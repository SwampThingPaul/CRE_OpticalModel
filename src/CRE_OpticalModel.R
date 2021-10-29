## 
## CRE Optical 'model'
##
##
##
## Code was compiled by Paul Julian
## contact info: pjulian@sccf.org

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape2)
library(zoo)
library(openxlsx)

# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)
library(tmap)

## Paths
wd="C:/Julian_LaCie/_GitHub/CRE_OpticalModel"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

GIS.path.gen=paste0(dirname(dirname(wd)),"/_GISData")

nad83.pro=CRS("+init=epsg:4269")
utm17=CRS("+init=epsg:26917")

tmap_mode("view")

# Data
# SFWMD - Chen et al ------------------------------------------------------
chen.dat=read.xlsx(paste0(data.path,"SFWMD/Chen_etal/CDOM_ms_Table2_sorted_kd_color_chl_turb_comparison_contribution.xlsx"),
                   sheet=1,cols=1:8,rows=1:72)
chen.dat$Dates=date.fun(convertToDate(chen.dat$Dates))

# dput(unique(chen.dat$Stations))
# dput(toupper(trimws(unique(chen.dat$Stations))))
Station.cleanup=data.frame(Stations=c("ces01", "ces02", "ces03", "ces04", "ces05", "ces06", "ces06a", 
                             "cES07", "CES08", "CES09", "SOUH6", "CES10", "CES11", " CES01", 
                             " CES02", "CES03", " CES04", "CES05", "CES06", "CES06A", "CES07", 
                             " CES08", "South06", " CES11", "CES01", "CES02", "CES04", "South6"),
                           Station.ID=c("CES01", "CES02", "CES03", "CES04", "CES05", "CES06", "CES06A", 
                                        "CES07", "CES08", "CES09", "SOUTH6", "CES10", "CES11", "CES01", 
                                        "CES02", "CES03", "CES04", "CES05", "CES06", "CES06A", "CES07", 
                                        "CES08", "SOUTH6", "CES11", "CES01", "CES02", "CES04", "SOUTH6"))
chen.dat=merge(chen.dat,Station.cleanup,"Stations")

# From spreadsheet - not sure origin of equation
chen.dat$color.est=with(chen.dat,(6.23-110.42)/(30.93-0.23)*(salinity-0.23)+110.42)

plot(salinity~color,chen.dat)
plot(color.est~color,chen.dat)
range(chen.dat$Kd,na.rm=T)

lmMod=lm(Kd~color+turb+chla,chen.dat)
summary(lmMod)
layout(matrix(1:4,2,2));plot(lmMod)
gvlma::gvlma(lmMod);
car::vif(lmMod)
shapiro.test(residuals(lmMod))

chen.dat$Kd.mod=predict(lmMod,chen.dat)
chen.dat$Kd.chen=with(chen.dat,0.15+0.02*color+0.07*turb+0.032*chla)

layout(matrix(1:4,2,2,byrow=T))
plot(Kd~Kd.mod,chen.dat);abline(0,1)
plot(Kd~Kd.chen,chen.dat);abline(0,1)
plot(Kd.mod~Kd.chen,chen.dat);abline(0,1)

# Data clean up
chen.dat2=chen.dat[,c("Station.ID","Dates","salinity","Kd","chla","color","turb")]
colnames(chen.dat2)=c("Station.ID","Date.EST","sal","K.par","Chla","color","Turb")

chen.melt=melt(chen.dat2,id.vars=c("Station.ID","Date.EST"))
chen.melt=subset(chen.melt,is.na(value)==F)
chen.melt$source="Chen et al 2015"

head(chen.melt)
colnames(chen.melt)=c("Station.ID", "Date.EST", "param", "HalfMDL", "source")
chen.melt=merge(chen.melt,data.frame(param=c("sal","K.par","Chla","color","Turb"),
                                     Units=c("PSU","1/m","ug/L","PCU","NTU")),"param")
chen.melt=chen.melt[,c( "Station.ID", "Date.EST","param", "Units", "HalfMDL", "source")]

## GIS
Chen.latlong=read.xlsx(paste0(data.path,"SFWMD/Chen_etal/CDOM_sample_stations_lats_lons.xlsx"),
                       sheet=1)
# dput(unique(Chen.latlong$station.label))
# dput(toupper(trimws(unique(Chen.latlong$station.label))))
Station.cleanup2=data.frame(station.label=c("Ces01", "CES02", "CES03", "CES04", "CES05 ", "CES06", "CES06A", 
                                            "CES07", "CES08", "CES09", "South 6A", "CES10", "CES11"),
                            Station.ID=c("CES01", "CES02", "CES03", "CES04", "CES05", "CES06", "CES06A", 
                                         "CES07", "CES08", "CES09", "SOUTH6", "CES10", "CES11"))
Chen.latlong=merge(Chen.latlong,Station.cleanup2,"station.label")
Chen.latlong=Chen.latlong[,c("Station.ID","lons.(decimal.degree)","lats.(decimal.degree)")]
colnames(Chen.latlong)=c("Station.ID","LONGITUDE","LATITUDE")

Chen.sp=spTransform(SpatialPointsDataFrame(Chen.latlong[,c("LONGITUDE","LATITUDE")],data=Chen.latlong,proj4string=nad83.pro),utm17)

tm_shape(Chen.sp)+tm_dots()


# SFWMD -------------------------------------------------------------------
## From Public Records Request
# wmd.dat=read.xlsx(paste0(data.path,"SFWMD/PPR_P007058-091421_Data.xlsx"),
#                    sheet=1,startRow = 8 )
# wmd.dat$DATE_COLLECTED=date.fun(convertToDate(wmd.dat$DATE_COLLECTED))
# 
# ddply(wmd.dat,c("STATION_ID"),summarise,N.val=N.obs("STATION_ID"))
# 
# 
# params.summary=ddply(wmd.dat,c("TEST_NUMBER","TEST_NAME"),summarise,N.val=N.obs("STATION_ID"))
# params=data.frame(TEST_NUMBER=c(11, 12, 13, 16, 61, 62, 112, 113, 178, 179, 180, 197),
#                   param=c("SD","Turb","color","TSS","Chla","Chlb","Chla.c","Chlc","Chla.sal","Chla.LC","Chlb.LC","PAR"))
# wmd.dat=merge(wmd.dat,params,"TEST_NUMBER")
# wmd.dat$HalfMDL=with(wmd.dat,ifelse(VALUE<0,abs(VALUE)/2,VALUE))
# 
# wmd.dat.xtab=dcast(wmd.dat,STATION_ID+DATE_COLLECTED~param,value.var = "HalfMDL",mean)
# plot(Chla.c~Chla,wmd.dat.xtab);abline(0,1);abline(lm(Chla.c~Chla,wmd.dat.xtab),col="red")
# plot(Chla.LC~Chla,wmd.dat.xtab);abline(0,1);abline(lm(Chla.LC~Chla,wmd.dat.xtab),col="red")
# plot(Chla.sal~Chla,wmd.dat.xtab);abline(0,1);abline(lm(Chla.LC~Chla,wmd.dat.xtab),col="red")
# 
# wmd.dat.xtab$Chla.CV=apply(wmd.dat.xtab[,c("Chla","Chla.c","Chla.LC","Chla.sal")],1,cv.per)*100
# range(wmd.dat.xtab$Chla.CV,na.rm=T)
# subset(wmd.dat.xtab,Chla.CV>50)
# range(wmd.dat.xtab$DATE_COLLECTED)
## DBHydro
# dates=date.fun(c("2008-05-01","2021-09-01"))
dates=date.fun(c("1999-05-01","2021-09-01"))
sites=data.frame(Station.ID=c("S79",paste0("CES0",1:9),"CES11","ROOK471"),
                 region=c(rep('FW',4),rep("Est",6),rep("Mar",2)))

# params=data.frame(Test.Number=c(21,20,18,80,61,179,25,23),param=c("TKN","NH4","NOx","TN","Chla","Chla","TP","SRP"))
params=data.frame(Test.Number=c(98,61,179,178,13,99,197,11,16,12,21,20,18,80,25,100),
                  param=c("sal","Chla","Chla","Chla","color","depth","K.par","secchi","TSS","Turb","TKN","NH4","NOx","TN","TP","TOC"))
wmd.dat2=data.frame()
for(i in 1:nrow(sites)){
  tmp=DBHYDRO_WQ(dates[1],dates[2],sites$Station.ID[i],params$Test.Number)
  wmd.dat2=rbind(tmp,wmd.dat2)
  print(i)
}
wmd.dat2=merge(wmd.dat2,params,"Test.Number")
wmd.dat2=merge(wmd.dat2,sites,"Station.ID")
ddply(wmd.dat2,c("param","Units"),summarise,N.val=N.obs(Value))
subset(wmd.dat2,Station.ID==sites$Station.ID[2])

# range(wmd.dat2$Depth,na.rm=T)
# sum(is.na(wmd.dat2$Depth))
# subset(wmd.dat2,Station.ID=="CES03"&Date.EST==date.fun("1999-05-12"))

# Depth average data
wmd.dat2.clean=ddply(wmd.dat2,c("Station.ID", "Date.EST","param", "Units"),summarise, HalfMDL=mean(HalfMDL))#,N.val=N.obs(param))
# subset(wmd.dat2.clean,N.val>1)
wmd.dat2.clean$source="SFWMD DBHYDRO"

# wmd.dat2.xtab=dcast(subset(wmd.dat2,Test.Number%in%params.summary$TEST_NUMBER),Station.ID+region+Date.EST~param,value.var="HalfMDL",fun.aggregate=function(x) mean(x,na.rm=T))
# nrow(wmd.dat.xtab)
# nrow(wmd.dat2.xtab)

wmd.dat2.xtab=dcast(wmd.dat2,Station.ID+region+Depth+Date.EST~param,value.var="HalfMDL",fun.aggregate=function(x) mean(x,na.rm=T))

## GIS
sites=data.frame(Station.ID=c("S79",paste0("CES0",2:9),"CES11","ROOK471"),
                 region=c(rep('FW',3),rep("Est",6),rep("Mar",2)))


wmd.mon=spTransform(readOGR(paste0(GIS.path.gen,"/SFWMD_Monitoring_20210119"),"DBHYDRO_SITE_STATION"),utm17)
head(wmd.mon)
wmd.mon2=subset(wmd.mon,STATION%in%sites$Station.ID&ACTIVITY_S=="Surface Water Grab")


# Lee County --------------------------------------------------------------
lee.dat=read.csv(paste0(data.path,"LeeCounty/20211025_GIS_Surface_Water.txt"))
unique(lee.dat$SAMPLE_LOCCODE)

# unique(lee.dat$RESULT_ANALYTE)
# unique(paste(lee.dat$RESULT_ACODE,lee.dat$AUNIT))
# unique(paste(lee.dat$RESULT_ANALYTE,lee.dat$AUNIT))
# dput(unique(lee.dat$RESULT_ANALYTE))
# param.xwalk=data.frame(RESULT_ACODE=c("$CHLOROA", "DOFIELD", "NOX", "SAL", "T-PO4", "TKN", "TN", 
#                                 "TURB", "COLOR465", "TOC", "TSS", "PAR", "SECCHI", "CONDF", "TURBF", 
#                                 "COLOR", "L_TURB"),
#                  Test.Name=c("Chla","DO.mgL","NOx","Sal","TP","TKN","TN","Turb","Color","TOC","TSS","PAR","SD",
#                              "SPC","Turb","Color","Turb"))
ddply(lee.dat,c("RESULT_ANALYTE","AUNIT"),summarise,N.val=N.obs(ID))
param.xwalk=data.frame(RESULT_ANALYTE=c("Chlorophyll a - corrected for Pheophytin", "Pheophytin", "Color", 
                                        "Specific Conductance, 25°C, Field", "Specific Conductance, 25 C, Field", 
                                        "Oxygen, Dissolved, Electrode", "OXYGEN, DISSOLVED, ELECTRODE", 
                                        "LCS Turbidity (Nephelometric)", "Nitrate + Nitrite", "NITRATE + NITRITE", 
                                        "Photosynthetically Active Radiation", "PHOTOSYNTHETICALLY ACTIVE RADIATION", 
                                        "Salinity by meter", "Water Clarity by Secchi Disk", "WATER CLARITY BY SECCHI DISK", 
                                        "Phosphorus, Total", "Nitrogen, Kjeldahl, Total", "Nitrogen, Total", 
                                        "Total Organic Carbon", "TOTAL ORGANIC CARBON", "Total Suspended Solids", 
                                        "Turbidity (Nephelometric)", "TURBIDITY (NEPHELOMETRIC)", "Turbidity (Nephelometric), field measure", 
                                        "TURBIDITY (NEPHELOMETRIC), FIELD MEASURE"),
                       param=c("Chla","Pheo","color",
                                   "SPC","SPC",
                                   "DO.mgL","DO.mgL",
                                   "Turb","NOx","NOx",
                                   "K.par","K.par",
                                   "sal","secchi","secchi",
                                   "TP","TKN","TN",
                                   "TOC","TOC","TSS",
                                   "Turb","Turb","Turb","Turb"),
                       Units=c("ug/L","ug/L","PCU",
                               "uS/cm","uS/cm",
                               rep("mg/L",2),
                               "NTU",rep("mg/L",2),
                               "1/m","1/m",
                               "PSU","meters","meters",
                               rep("mg/L",6),
                               rep("NTU",4)))

lee.dat=merge(lee.dat,param.xwalk,"RESULT_ANALYTE")
lee.dat$SAMPLE_COLDATE=date.fun(lee.dat$SAMPLE_COLDATE,form="%m/%d/%Y %H:%M")
lee.dat$Date.EST=date.fun(lee.dat$SAMPLE_COLDATE)
# lee.dat$Station.ID=lee.dat$SAMPLE_LOCCODE
lee.dat$HalfMDL=lee.dat$CALC_RESULT

# head(ddply(subset(wmd.dat2.clean,Station.ID=="CES03"),"Date.EST",summarise,N.val=N.obs(HalfMDL)))
# head(ddply(subset(lee.dat,SAMPLE_LOCCODE=="CES03SUR"),"Date.EST",summarise,N.val=N.obs(HalfMDL)))
# subset(wmd.dat2.clean,Station.ID=="CES03"&Date.EST==date.fun("1999-05-12"))
# subset(lee.dat,SAMPLE_LOCCODE=="CES03SUR"&Date.EST==date.fun("1999-05-12"))

site.xwalk=data.frame(SAMPLE_LOCCODE=c("CES03SUR","CES04SUR","CES06SUR","CES10SUR","PI-01","PI-02","PI-13","PI-14"),
                      Station.ID=c("CES03","CES04","CES06","CES10","PI-01","PI-02","PI-13","PI-14"))
lee.dat=merge(lee.dat,site.xwalk,"SAMPLE_LOCCODE")

# Depth average data
lee.dat.clean=ddply(lee.dat,c("Station.ID", "Date.EST","param", "Units"),summarise, HalfMDL=mean(HalfMDL,na.rm=T))#,N.val=N.obs(param))
# subset(lee.dat.clean,N.val>1)
lee.dat.clean$source="Lee County"

lee.dat.xtab=dcast(subset(lee.dat,SUSERFLDS_SAMP_TYPE=="SAMP"),Station.ID+Date.EST~Test.Name,value.var = "CALC_RESULT",mean)

# Quick Data Explore
sites=c("CES03SUR","CES04SUR","CES06SUR","CES10SUR","PI-01","PI-02","PI-13","PI-14")
par(family="serif",mar=c(1,1,0.5,0.25),oma=c(2,4,1,0.5));
layout(matrix(1:32,8,4,byrow=F))
for(i in 1:length(sites)){plot(Color~Date.EST,subset(lee.dat.xtab,Station.ID==sites[i]),type="b")}
for(i in 1:length(sites)){plot(PAR~Date.EST,subset(lee.dat.xtab,Station.ID==sites[i]),type="b")}
for(i in 1:length(sites)){plot(Chla~Date.EST,subset(lee.dat.xtab,Station.ID==sites[i]),type="b")}
for(i in 1:length(sites)){plot(Turb~Date.EST,subset(lee.dat.xtab,Station.ID==sites[i]),type="b")}


for(i in 1:length(sites)){plot(TP~Date.EST,subset(lee.dat.xtab,Station.ID==sites[i]),type="b")}
for(i in 1:length(sites)){plot(TN~Date.EST,subset(lee.dat.xtab,Station.ID==sites[i]),type="b")}
for(i in 1:length(sites)){plot(Sal~Date.EST,subset(lee.dat.xtab,Station.ID==sites[i]),type="b")}
for(i in 1:length(sites)){plot(DO.mgL~Date.EST,subset(lee.dat.xtab,Station.ID==sites[i]),type="b")}

## GIS
lee.latlong=data.frame(Station.ID=c("CES03SUR","CES04SUR","CES06SUR","CES10SUR","PI-01","PI-02","PI-13","PI-14"),
                       LONGITUDE=c(-81.76087,-81.83366,-81.91242,-81.99578,-82.01838, -82.04035,-82.09757, -82.04829),
                       LATITUDE=c(26.71663, 26.68165,26.57855,26.5287,26.50706, 26.5227,26.47175,26.49266))
lee.sp=spTransform(SpatialPointsDataFrame(lee.latlong[,c("LONGITUDE","LATITUDE")],data=lee.latlong,proj4string=nad83.pro),utm17)

tm_shape(Chen.sp)+tm_dots(col="dodgerblue1",alpha=0.5,size=0.075)+
  tm_shape(wmd.mon2)+tm_dots(col="green",alpha=0.5,size=0.075)+
  tm_shape(lee.sp)+tm_dots(col="red",alpha=0.5,size=0.075)


# merge datasets ----------------------------------------------------------
head(chen.melt)
head(wmd.dat2.clean)
head(lee.dat.clean)

all.dat=rbind(chen.melt,wmd.dat2.clean,lee.dat.clean)
# write.csv(all.dat,paste0(export.path,"20211029_masterdata.csv"),row.names=F)
dat.xtab=dcast(all.dat,Station.ID+source+Date.EST~param,value.var = "HalfMDL",mean)

par(mar=c(5,5,1,1))
plot(K.par~Date.EST,dat.xtab)
plot(Chla~Date.EST,dat.xtab)
plot(color~Date.EST,dat.xtab,ylab=c("Color (PCU)"))
