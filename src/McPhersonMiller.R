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

library(dataRetrieval)

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

# -------------------------------------------------------------------------
TB.bbox=extent(-82.89,-82.34,27.47,28.06)
TB.bbox.AOI=as(TB.bbox,"SpatialPolygons")

CH.bbox=extent(-82.42,-81.90,26.38,27.03)
CH.bbox.AOI=as(CH.bbox,"SpatialPolygons")

USGS.CH.sites=data.frame(siteID=c("264408082204800","263840082120500",
                                  "263840082114800","265355082075500",
                                  "262839082041200","262829082041200",
                                  "265522082064600","270055081590300"),
                         siteName=c("CH15.1","CH20",
                                    "CH20.SG","CH6",
                                    "CH24","CH24.SG",
                                    "CH5","CH29"))
USGS.TB.sites=data.frame(siteID=c("274837082314600","275701082375599","275530082383300",
                                  "274033082385300","273900082385300","273631082452600",
                                  "275134082270599"),
                         siteName=c("TB2","TB3-depth","TB3",
                                    "TB4","TB5","TB6","TB1"))
usgs.sites=rbind(USGS.CH.sites,USGS.TB.sites)
usgs.sites$MonitoringLocationIdentifier=paste0("USGS-",usgs.sites$siteID)

NWIS.siteinfo=readNWISsite(usgs.sites$siteID)
subset(NWIS.siteinfo,site_no=="262839082041200")

NWIS.siteinfo.shp=SpatialPointsDataFrame(coords=NWIS.siteinfo[,c("dec_long_va","dec_lat_va")],
                                         data=NWIS.siteinfo,
                                         proj4string = nad83.pro)
tm_basemap(leaflet::providers$Esri.WorldImagery,alpha=0.9)+
  tm_shape(NWIS.siteinfo.shp)+tm_dots(col="red",
                                      size=0.05,
                                      id="site_no",
                                      popup.vars=c("agency_cd","site_no","station_nm"
                                                   ,"huc_cd","drain_area_va"),
                                      group="USGS Sites")+
  tm_shape(TB.bbox.AOI)+tm_borders(lty="dashed",col="yellow")+
  tm_shape(CH.bbox.AOI)+tm_borders(lty="dashed",col="yellow")

## Missing CH11
## 
dates=date.fun(c("1989-10-01","1991-10-31"))
# params=data.frame(parameterCd=c("00003","00010","00076","00077","00080","00095","00680","70953","70971","00480"),
#                   param=c("depth","Temp","Turb","secchi","color","SPC","TOC","Chla","Kpar","Sal"))
dat=readWQPdata(siteNumbers=usgs.sites$MonitoringLocationIdentifier,startDate=dates[1],endDate=dates[2])
unique(dat$USGSPCode)
unique(dat$CharacteristicName)

pCode=c("00080","00076","70953","70957","70971")
param.xwalk=data.frame(parm_cd=pCode,
                       param=c("Color","Turb","Chla","Chla","Kpar"))
dat=readNWISqw(siteNumbers=usgs.sites$siteID,
                startDate=dates[1],endDate=dates[2],parameterCd=pCode)

dat=merge(dat,param.xwalk,"parm_cd")
dat=merge(dat,usgs.sites,by.x="site_no",by.y="siteID")
unique(dat$siteName)
dat$sample_dt=date.fun(dat$sample_dt)
dat.xtab=dcast(dat,siteName+sample_dt~param,value.var="result_va",mean)
dat.xtab

ddply(dat.xtab,c("siteName"),summarise,
      mean.Kd=mean(Kpar,na.rm=T),
      StDev=sd(Kpar,na.rm=T),
      N.val=N.obs(Kpar))

kpar.mod=lm(Kpar~Color+Turb+Chla,dat.xtab)
summary(kpar.mod)

layout(matrix(1:4,2,2))
plot(kpar.mod)
gvlma::gvlma(kpar.mod)

# Charlotte Harbor only
kpar.mod.ch=lm(Kpar~Color+Turb+Chla,subset(dat.xtab,siteName%in%USGS.CH.sites$siteName))
summary(kpar.mod.ch)

layout(matrix(1:4,2,2))
plot(kpar.mod.ch)
gvlma::gvlma(kpar.mod.ch)

# Tampa Bay only
kpar.mod.tb=lm(Kpar~Color+Turb+Chla,subset(dat.xtab,siteName%in%USGS.TB.sites$siteName))
summary(kpar.mod.tb)

layout(matrix(1:4,2,2))
plot(kpar.mod.tb)
gvlma::gvlma(kpar.mod.tb)


