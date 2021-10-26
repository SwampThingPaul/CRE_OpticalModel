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


# SFWMD - DBHydro ---------------------------------------------------------


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
                       Test.Name=c("Chla","Pheo","Color",
                                   "SPC","SPC",
                                   "DO.mgL","DO.mgL",
                                   "Turb","NOx","NOx",
                                   "PAR","PAR",
                                   "Sal","SD","SD",
                                   "TP","TKN","TN",
                                   "TOC","TOC","TSS",
                                   "Turb","Turb","Turb","Turb"))

lee.dat=merge(lee.dat,param.xwalk,"RESULT_ANALYTE")
lee.dat$Date.EST=date.fun(lee.dat$SAMPLE_COLDATE,form="%m/%d/%Y %H:%M")
lee.dat$Station.ID=lee.dat$SAMPLE_LOCCODE

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
