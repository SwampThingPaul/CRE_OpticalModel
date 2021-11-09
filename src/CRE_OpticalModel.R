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
                                        "CES07", "CES08", "CES09", "SOUTH6", "CES10", "ROOK471", "CES01", 
                                        "CES02", "CES03", "CES04", "CES05", "CES06", "CES06A", "CES07", 
                                        "CES08", "SOUTH6", "ROOK471", "CES01", "CES02", "CES04", "SOUTH6"))
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
                                         "CES07", "CES08", "CES09", "SOUTH6", "CES10", "ROOK471"))
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
params=data.frame(Test.Number=c(98,61,179,178,13,99,197,11,16,12,21,20,18,80,25,100,8,7,23,10,9),
                  param=c("sal","Chla","Chla","Chla","color","depth","K.par","secchi","TSS","Turb","TKN","NH4","NOx","TN","TP","TOC","DO.mgL","temp","SRP","pH","SPC"))
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

test=dcast(wmd.dat2,Station.ID+Date.EST+Sample.ID+Depth~param,value.var="HalfMDL",mean,na.rm=T)
sum(is.na(test$Depth))
summary(test$Depth)

nrow(subset(test,Depth<0.5|is.na(Depth)==T))
nrow(subset(test,Depth<2|is.na(Depth)==T))
test2=subset(test,Depth<2|is.na(Depth)==T)
subset(test,is.na(Depth)==T)
sum(is.na(test2$K.par)==F)
plot(K.par~color,test2)
plot(K.par~secchi,test2)


# range(wmd.dat2$Depth,na.rm=T)
# sum(is.na(wmd.dat2$Depth))
# subset(wmd.dat2,Station.ID=="CES03"&Date.EST==date.fun("1999-05-12"))

# Depth average data
wmd.dat2.clean=ddply(subset(wmd.dat2,Depth<2|is.na(Depth)==T),c("Station.ID", "Date.EST","param", "Units"),summarise, HalfMDL=mean(HalfMDL))#,N.val=N.obs(param))
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
lee.dat=read.csv(paste0(data.path,"LeeCounty/20211104_GIS_Surface_Water.txt"))
unique(lee.dat$SAMPLE_LOCCODE)

unique(lee.dat$RESULT_QUALIFY)
subset(lee.dat,RESULT_QUALIFY=="ELAB")
subset(lee.dat,RESULT_QUALIFY=="STL")
subset(lee.dat,RESULT_QUALIFY=="U")
# QA/QC qualifiers 
dat.qual=data.frame(QUALIFIER=c(NA,"!","A","D","E","F","I","R","T","U","*","?","B","H","J","K","L","M","N","O","Q","V","Y","Z"),
                    FATALYN=c("N",rep("N",9),rep("Y",14)))

quals=as.character(unique(lee.dat$RESULT_QUALIFY))
spl=strsplit(quals,split="")
quals=data.frame(RESULT_QUALIFY=quals,
                 q1=sapply(spl,"[",1),
                 q2=sapply(spl,"[",2),
                 q3=sapply(spl,"[",3),
                 q4=sapply(spl,"[",4),
                 q5=sapply(spl,"[",5),
                 q6=sapply(spl,"[",6))
quals$Fatal=with(quals,ifelse(q1%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER|
                                q2%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER|
                                q3%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER|
                                q4%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER|
                                q5%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER|
                                q6%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER,"Y","N"))
quals$Fatal=with(quals,ifelse(RESULT_QUALIFY%in%c("STL","ELAB"),"N",Fatal))

lee.dat=merge(lee.dat,quals[,c("RESULT_QUALIFY","Fatal")],"RESULT_QUALIFY")

# unique(lee.dat$RESULT_ANALYTE)
# unique(paste(lee.dat$RESULT_ACODE,lee.dat$AUNIT))
# unique(paste(lee.dat$RESULT_ANALYTE,lee.dat$AUNIT))
# dput(unique(lee.dat$RESULT_ANALYTE))
# param.xwalk=data.frame(RESULT_ACODE=c("$CHLOROA", "DOFIELD", "NOX", "SAL", "T-PO4", "TKN", "TN", 
#                                 "TURB", "COLOR465", "TOC", "TSS", "PAR", "SECCHI", "CONDF", "TURBF", 
#                                 "COLOR", "L_TURB"),
#                  Test.Name=c("Chla","DO.mgL","NOx","Sal","TP","TKN","TN","Turb","Color","TOC","TSS","PAR","SD",
#                              "SPC","Turb","Color","Turb"))
lee.analyte=ddply(lee.dat,c("RESULT_ANALYTE","AUNIT"),summarise,N.val=N.obs(ID))
#dput(lee.analyte$RESULT_ANALYTE)
param.xwalk=data.frame(
  RESULT_ANALYTE=c("Ammonia", "Biochemical Oxygen Demand 5 day", "Chlorophyll a - corrected for Pheophytin", 
                   "Color", "Enterococci", "Enterococci", "Field Temperature", "LCS Turbidity (Nephelometric)", 
                   "Nitrate", "NITRATE", "Nitrate + Nitrite", "NITRATE + NITRITE", 
                   "Nitrite", "NITRITE", "Nitrogen, Kjeldahl, Total", "Nitrogen, Organic", 
                   "Nitrogen, Total", "Oxygen, Dissolved, Electrode", "OXYGEN, DISSOLVED, ELECTRODE", 
                   "Oxygen, Dissolved, Percent Saturation", "pH      (electrometric)", 
                   "pH, Field (electrometric)", "Pheophytin", "Phosphorus, Ortho", 
                   "Phosphorus, Total", "Photosynthetically Active Radiation", "PHOTOSYNTHETICALLY ACTIVE RADIATION", 
                   "Salinity by meter", "Silica, disolved as SiO2", "Silica, dissolved as SiO2", 
                   "Specific Conductance, 25 C, Field", "Specific Conductance, 25°C", 
                   "Specific Conductance, 25°C, Field", "Total Organic Carbon", 
                   "TOTAL ORGANIC CARBON", "Total Suspended Solids", "Turbidity (Nephelometric)", 
                   "TURBIDITY (NEPHELOMETRIC)", "Turbidity (Nephelometric), field measure", 
                   "TURBIDITY (NEPHELOMETRIC), FIELD MEASURE", "Water Clarity by Secchi Disk", 
                   "WATER CLARITY BY SECCHI DISK"),
  param=c("NH4","BOD","Chla",
          "color","entero","entero","temp","Turb",
          "NO3","NO3","NOx","NOx",
          "NO2","NO2","TKN","TON",
          "TN","DO.mgL","DO.mgL",
          "DO.persat","pH",
          "pH","Pheo","SRP",
          "TP","K.par","K.par",
          "Sal","SiO2","SiO2",
          "SPC","SPC","SPC","TOC",
          "TOC","TSS","Turb","Turb","Turb","Turb","secchi","secchi"),
  Units=c("mg/L","mg/L","ug/L",
          "PCU","MPN/100mL","MPN/100mL","Deg C","NTU",
          rep("mg/L",11),"%","SU","SU",
          "ug/L","mg/L","mg/L","1/m","1/m","PSU","mg/L","mg/L",
          rep("uS/cm",3),"mg/L","mg/L","mg/L",
          rep("NTU",4),"m","m"))


lee.dat=merge(lee.dat,param.xwalk,"RESULT_ANALYTE")
lee.dat$SAMPLE_COLDATE=date.fun(lee.dat$SAMPLE_COLDATE,form="%m/%d/%Y %H:%M")
lee.dat$Date.EST=date.fun(lee.dat$SAMPLE_COLDATE)
# lee.dat$Station.ID=lee.dat$SAMPLE_LOCCODE
lee.dat$HalfMDL=lee.dat$CALC_RESULT

# head(ddply(subset(wmd.dat2.clean,Station.ID=="CES03"),"Date.EST",summarise,N.val=N.obs(HalfMDL)))
# head(ddply(subset(lee.dat,SAMPLE_LOCCODE=="CES03SUR"),"Date.EST",summarise,N.val=N.obs(HalfMDL)))
# subset(wmd.dat2.clean,Station.ID=="CES03"&Date.EST==date.fun("1999-05-12"))
# subset(lee.dat,SAMPLE_LOCCODE=="CES03SUR"&Date.EST==date.fun("1999-05-12"))

site.xwalk=data.frame(SAMPLE_LOCCODE=c("CES01SUR","CES03SUR","CES04SUR","CES06SUR","CES10SUR","PI-01","PI-02","PI-13","PI-14"),
                      Station.ID=c("CES01","CES03","CES04","CES06","CES10","PI-01","PI-02","PI-13","PI-14"))
lee.dat=merge(lee.dat,site.xwalk,"SAMPLE_LOCCODE")
unique(lee.dat$SUSERFLDS_SAMP_TYPE)
# Depth average data
lee.dat.clean=ddply(subset(lee.dat,Fatal=="N"&SUSERFLDS_SAMP_TYPE!="FD"),c("Station.ID", "Date.EST","param", "Units"),summarise, HalfMDL=mean(HalfMDL,na.rm=T))#,N.val=N.obs(param))
# subset(lee.dat.clean,N.val>1)
lee.dat.clean$source="Lee County"

lee.dat.xtab=dcast(subset(lee.dat,SUSERFLDS_SAMP_TYPE=="SAMP"),Station.ID+Date.EST~param,value.var = "CALC_RESULT",mean)

## GIS
lee.latlong=data.frame(SAMPLE_LOCCODE=c("CES01SUR","CES03SUR","CES04SUR","CES06SUR","CES10SUR","PI-01","PI-02","PI-13","PI-14"),
                       Station.ID=c("CES01","CES03","CES04","CES06","CES10","PI-01","PI-02","PI-13","PI-14"),
                       LONGITUDE=c(-81.69323,-81.76087,-81.83366,-81.91242,-81.99578,-82.01838, -82.04035,-82.09757, -82.04829),
                       LATITUDE=c(26.72133,26.71663, 26.68165,26.57855,26.5287,26.50706, 26.5227,26.47175,26.49266))
lee.sp=spTransform(SpatialPointsDataFrame(lee.latlong[,c("LONGITUDE","LATITUDE")],data=lee.latlong,proj4string=nad83.pro),utm17)

tm_shape(Chen.sp)+tm_dots(col="dodgerblue1",alpha=0.5,size=0.075)+
  tm_shape(wmd.mon2)+tm_dots(col="green",alpha=0.5,size=0.075)+
  tm_shape(lee.sp)+tm_dots(col="red",alpha=0.5,size=0.075)

### 
Chen.sp2=Chen.sp@data
lee.sp2=lee.sp@data
lee.sp2=lee.sp2[,2:4]

wmd.sp2=wmd.mon2@data
wmd.sp2=wmd.sp2[,c("STATION","LONG","LAT")]
wmd.sp2$Station.ID=wmd.sp2$STATION
wmd.sp2=wmd.sp2[,c(4,2,3)]
colnames(wmd.sp2)=names(Chen.sp2)

ddply(rbind(Chen.sp2,lee.sp2,wmd.sp2),"Station.ID",summarise,N.val=N.obs(LONGITUDE))

tmp.sp2=rbind(Chen.sp2,
              lee.sp2[!(lee.sp2$Station.ID%in%Chen.sp2$Station.ID),],
              wmd.sp2[!(wmd.sp2$Station.ID%in%Chen.sp2$Station.ID),])
# tmp.sp2=rbind(Chen.sp2,lee.sp2,wmd.sp2)
ddply(tmp.sp2,"Station.ID",summarise,N.val=N.obs(LONGITUDE))
dat.sp=ddply(tmp.sp2,"Station.ID",summarise,
             LONGITUDE=min(LONGITUDE,na.rm=T),
             LATITUDE=min(LATITUDE,na.rm=T))
dat.sp2=spTransform(SpatialPointsDataFrame(dat.sp[,c("LONGITUDE","LATITUDE")],data=dat.sp,proj4string=nad83.pro),utm17)

tm_shape(dat.sp2)+tm_dots(col="yellow",alpha=0.5,size=0.075)+
  tm_shape(Chen.sp)+tm_dots(col="dodgerblue1",alpha=0.5,size=0.075)+
  tm_shape(wmd.mon2)+tm_dots(col="green",alpha=0.5,size=0.075)+
  tm_shape(lee.sp)+tm_dots(col="red",alpha=0.5,size=0.075)

area=gBuffer(dat.sp2,width=5000)
areaR=raster(xmn=extent(area)[1],
             xmx=extent(area)[2],
             ymn=extent(area)[3],
             ymx=extent(area)[4],crs=utm17,vals=0)
areaR=mask(areaR,subset(dat.sp2,Station.ID=="S79"))
areaD=distance(areaR)
areaD=mask(areaD,area)
plot(areaD)

dat.sp2@data$EuDist.m=extract(areaD,dat.sp2)
dat.sp2@data
dat.sp2@data[dat.sp2@data$Station.ID=="CES01","EuDist.m"]=0
  # merge datasets ----------------------------------------------------------
head(chen.melt)
head(wmd.dat2.clean)
head(lee.dat.clean)

vars=c("color","DO.mgL", "NH4", "NOx", "sal","temp","pH","SPC", "secchi", 
       "TKN", "TOC","SRP", "TP", "TSS", "Turb", "Chla", "TN", "K.par")
all.dat=rbind(chen.melt,wmd.dat2.clean,subset(lee.dat.clean,param%in%vars))
# write.csv(all.dat,paste0(export.path,"20211029_masterdata.csv"),row.names=F)
# write.csv(all.dat,paste0(export.path,"20211104_masterdata.csv"),row.names=F) # Add DO to SFWMD data
dat.xtab=dcast(all.dat,Station.ID+source+Date.EST~param,value.var = "HalfMDL",mean)
dat.xtab$TN=with(dat.xtab,TN_Combine(NOx,TKN,TN))
dat.xtab$DIN=with(dat.xtab,NOx+NH4)
dat.xtab$sal.calc=with(dat.xtab,SalinityCalc(SPC,temp))
dat.xtab$sal=with(dat.xtab, ifelse(is.na(sal)==T,sal.calc,sal))

head(dat.xtab)

dev.off()
plot(sal~sal.calc,dat.xtab);abline(0,1)

par(mar=c(5,5,1,1))
plot(K.par~Date.EST,dat.xtab)
plot(Chla~Date.EST,dat.xtab)
plot(color~Date.EST,dat.xtab,ylab=c("Color (PCU)"))
plot(DIN~Date.EST,dat.xtab)

## Data inventory
vars=c("Station.ID","Date.EST", "sal","SPC", "K.par","secchi", "Chla", 
  "color", "Turb", "TSS", "TOC","SRP","TP","DIN","TN", "DO.mgL")
plot.vars=data.frame(variable=vars[3:length(vars)],plot.val=1:length(vars[3:length(vars)]))

dput(unique(dat.xtab$Station.ID))
site.ls=c( "S79","CES01", "CES02", "CES03", "CES04", "CES05", "CES06", "CES06A", 
          "CES07", "CES08", "CES09", "CES10", "CES11", "PI-01", "PI-02", 
          "PI-13", "PI-14", "ROOK471", "SOUTH6")

# png(filename=paste0(plot.path,"20211104_DataInventory.png"),width=6.5,height=7,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,1,1),oma=c(2,3,0.5,1),lwd=0.5);
layout(matrix(1:20,5,4,byrow=F))

xlim.val=dates;xmaj=seq(xlim.val[1],xlim.val[2],"5 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
for(i in 1:length(site.ls)){
tmp=reshape2::melt(subset(dat.xtab,Station.ID==site.ls[i])[,vars],id.vars=vars[1:2])
tmp=merge(tmp,plot.vars,"variable",all.y=T)
plot(plot.val~Date.EST,tmp,type="n",ann=F,axes=F,ylim=c(1,nrow(plot.vars)),xlim=xlim.val)
with(subset(tmp,is.na(value)==F),points(Date.EST,plot.val,pch=21,bg=adjustcolor("indianred1",0.25),col=adjustcolor("indianred1",0.5)))
if(i%in%c(1:5)){axis_fun(2,1:nrow(plot.vars),1:nrow(plot.vars),plot.vars$variable,cex=0.5)}else{axis_fun(2,1:nrow(plot.vars),1:nrow(plot.vars),NA)}     
if(i%in%c(5,10,15,19)){axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.75,cex=0.75)}else{axis_fun(1,xmaj,xmin,NA)}
box(lwd=1)
mtext(side=3,adj=0,site.ls[i],cex=0.75)
}
mtext(side=2,"Variable",line=1.5,outer=T)
mtext(side=1,"Date (Month-Year)",line=0.5,outer=T)
dev.off()

subset(dat.xtab,Station.ID=="PI-01")
tmp=reshape2::melt(subset(dat.xtab,Station.ID=="PI-01")[,vars],id.vars=vars[1:2])
unique(tmp$variable)
subset(tmp,variable=="TSS")


# duplicate Check ---------------------------------------------------------
vars=c("Station.ID", "Date.EST", "K.par", "Chla","color")
dup.check=dat.xtab[,vars[1:2]]
dup.check[duplicated(dup.check)| duplicated(dup.check, fromLast=TRUE),]
dat.xtab$dup=ifelse(duplicated(dup.check)| duplicated(dup.check, fromLast=TRUE),1,0)

subset(dat.xtab,dup==1)
subset(dat.xtab,Station.ID=="CES03"&Date.EST==date.fun("1999-05-12"))


dup.test=dcast(subset(dat.xtab,dup==1),Station.ID+Date.EST~source,value.var = "K.par",mean)
# dup.test=dcast(subset(dat.xtab,dup==1),Station.ID+Date.EST~source,value.var = "color",mean)
# dup.test=dcast(subset(dat.xtab,dup==1),Station.ID+Date.EST~source,value.var = "Turb",mean)
# dup.test=dcast(subset(dat.xtab,dup==1),Station.ID+Date.EST~source,value.var = "TN",mean)
# dup.test=dcast(subset(dat.xtab,dup==1),Station.ID+Date.EST~source,value.var = "TP",mean)
colnames(dup.test)=c("Station.ID","Date.EST","Chen","Lee","SFWMD")
plot(Chen~Lee,dup.test);abline(0,1)
plot(SFWMD~Lee,dup.test);abline(0,1)
plot(SFWMD~Chen,dup.test);abline(0,1)


# Discharge vs Color&Kd ---------------------------------------------------
dates
# Discharge Data ----------------------------------------------------------
CRE.site=data.frame(SITE=c(rep("S79",2),rep("S77",2)),
                    DBKEY=c("00865","P1023","15635","DJ235"),
                    Priority=c("P1","P2","P1","P2"))
CR.Q.dat=data.frame()
for(i in 1:nrow(CRE.site)){
  tmp=DBHYDRO_daily(dates[1],dates[2],CRE.site$DBKEY[i])
  tmp$DBKEY=as.character(CRE.site$DBKEY[i])
  CR.Q.dat=rbind(CR.Q.dat,tmp)
  print(i)
}
CR.Q.dat
CR.Q.dat=merge(CR.Q.dat,CRE.site,"DBKEY")
# CR.Q.dat=dcast(CR.Q.dat,SITE+Date~Priority,value.var="Data.Value",sum)
# CR.Q.dat$fflow.cfs=with(CR.Q.dat,ifelse(is.na(P1)==F|(P2=0&P1!=0),P1,P2))
# plot(fflow.cfs~Date,subset(CR.Q.dat,SITE=="S77"))
# plot(fflow.cfs~Date,subset(CR.Q.dat,SITE=="S79"))

CR.Q.dat.xtab=dcast(CR.Q.dat,Date~SITE,value.var = "Data.Value",mean,na.rm=T)
CR.Q.dat.xtab$WY=WY(CR.Q.dat.xtab$Date)

CR.Q.dat.xtab$S77BF=with(CR.Q.dat.xtab,ifelse(S77<0,abs(S77),0))
CR.Q.dat.xtab$S77=with(CR.Q.dat.xtab,ifelse(S77<0,0,S77))
CR.Q.dat.xtab$S79=with(CR.Q.dat.xtab,ifelse(S79<0,0,S79))
CR.Q.dat.xtab$C43=with(CR.Q.dat.xtab,ifelse(S79>S77,S79-S77,0))
CR.Q.dat.xtab$LOK_S79=apply(CR.Q.dat.xtab[,c("S79","S77")],1,min,na.rm=T);# from LOSOM LWL eval
CR.Q.dat.xtab$Date.EST=date.fun(CR.Q.dat.xtab$Date)
plot(S79~Date.EST,CR.Q.dat.xtab)

CR.Q.dat.xtab[,c("Date","S79")]
# If more than one sample per day - daily average was calculated. 
# See duplicate check section. Some of Lee Co and SFWMD data overlap
station.alias=data.frame(Station.ID=site.ls,
           Alias=c( "S79","S79", "CES02", "CES03", "CES04", "CES05", "CES06", "CES06A", 
                    "CES07", "CES08", "CES09", "CES10", "CES11", "PI-01", "PI-02", 
                    "PI-13", "PI-14", "ROOK471", "SOUTH6"),
           region=c(rep("FW",4),rep("Est",8),"Mar",rep("Est",2),"Mar","Est","Mar","Est"))
station.alias=merge(station.alias,dat.sp2@data,by.x="Station.ID",by.y="Station.ID",all.x=T)


dat.xtab2=dcast(merge(all.dat,station.alias,"Station.ID"),Alias+EuDist.m+LONGITUDE+LATITUDE+Date.EST~param,value.var = "HalfMDL",mean)
dat.xtab2=merge(dat.xtab2,CR.Q.dat.xtab[,c("Date.EST","S79")],"Date.EST",all.x=T)
dat.xtab2=subset(dat.xtab2,Date.EST%in%seq(dates[1],dates[2],"1 days"))
dat.xtab2
dat.xtab2$Alias=factor(dat.xtab2$Alias,levels=station.alias$Alias[2:19])
dat.xtab2$WY=WY(dat.xtab2$Date.EST)
dat.xtab2$DoWY=hydro.day(dat.xtab2$Date.EST)
## Data Explore
boxplot(sal~Alias,dat.xtab2,las=2)
boxplot(K.par~Alias,dat.xtab2)
boxplot(color~Alias,dat.xtab2)
boxplot(Chla~Alias,dat.xtab2,outline=F)
boxplot(Turb~Alias,dat.xtab2,outline=F)

plot(K.par~EuDist.m,dat.xtab2)
plot(color~EuDist.m,dat.xtab2)
plot(Chla~EuDist.m,dat.xtab2)

plot(color~S79,dat.xtab2)
plot(K.par~S79,dat.xtab2)

plot(K.par~S79,dat.xtab2,type="n")
with(subset(dat.xtab2,Alias=="CES02"),points(K.par~S79,pch=21,bg="Red"))
with(subset(dat.xtab2,Alias=="CES03"),points(K.par~S79,pch=21,bg="blue"))
with(subset(dat.xtab2,Alias=="CES04"),points(K.par~S79,pch=21,bg="green"))
with(subset(dat.xtab2,Alias=="CES05"),points(K.par~S79,pch=21,bg="yellow"))
with(subset(dat.xtab2,Alias=="CES11"),points(K.par~S79,pch=21,bg="lightblue"))
with(subset(dat.xtab2,Alias=="ROOK471"),points(K.par~S79,pch=21,bg="purple"))

plot(color~S79,dat.xtab2,type="n")
with(subset(dat.xtab2,Alias=="CES02"),points(color~S79,pch=21,bg="Red"))
with(subset(dat.xtab2,Alias=="CES03"),points(color~S79,pch=21,bg="blue"))
with(subset(dat.xtab2,Alias=="CES04"),points(color~S79,pch=21,bg="green"))
with(subset(dat.xtab2,Alias=="CES05"),points(color~S79,pch=21,bg="yellow"))
with(subset(dat.xtab2,Alias=="CES11"),points(color~S79,pch=21,bg="lightblue"))
with(subset(dat.xtab2,Alias=="ROOK471"),points(color~S79,pch=21,bg="purple"))


## nlme analysis
library(MuMIn)
library(car)
library(nlme)
library(lattice)
library(visreg)
library(piecewiseSEM)

lme.KdQ=lme(K.par~S79,
            data=na.omit(dat.xtab2[,c("K.par","S79","Alias")]),
            random=c(~1+S79|Alias),method="ML")

shapiro.test(residuals(lme.KdQ))
rsquared(lme.KdQ)
summary(lme.KdQ)
plot(lme.KdQ)

acf(residuals(lme.KdQ))
qqnorm(residuals(lme.KdQ));qqline(residuals(lme.KdQ))

visreg(lme.KdQ,"S79",type="conditional")

## GAM analysis
library(gratia)
library(mgcv)

knots <- list(DoY = c(0.5, 366.5))
m1=bam(log(K.par)~
         s(color)+
         s(Chla)+
         s(DoWY,bs="cc",k=150)+
         s(WY,k=20)+
         ti(DoWY,WY,bs=c("cc","tp"),k=c(80,20))+
         ti(LONGITUDE,LATITUDE,DoWY,d = c(2,1), bs = c('ds','cc'),
            m = list(c(1, 0.5), NA), k = c(25, 12))+
         ti(LONGITUDE,LATITUDE,WY,d = c(2,1), bs = c('ds','cc'),
            m = list(c(1, 0.5), NA), k = c(25, 12)),
       data=dat.xtab2, method = 'fREML',
       nthreads = c(4, 1), discrete = TRUE)
summary(m1)

nvar=4;layout(matrix(1:nvar,1,nvar))
gam.check(m1)
dev.off()

draw(m1)
plot(m1,residuals=T,pch=21)

pred.org=predict(m1,type="terms")
partial.resids<-pred.org+residuals(m1)

hist(partial.resids[,1])
shapiro.test(partial.resids[,1])
hist(partial.resids[,2])
shapiro.test(partial.resids[,2])
hist(partial.resids[,3])
shapiro.test(partial.resids[,3])
hist(partial.resids[,4])
shapiro.test(partial.resids[,4])
hist(partial.resids[,5])
shapiro.test(partial.resids[,5])
hist(partial.resids[,6])
shapiro.test(partial.resids[,6])
hist(partial.resids[,7])
shapiro.test(partial.resids[,7])
