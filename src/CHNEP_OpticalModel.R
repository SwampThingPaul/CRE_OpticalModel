## 
## CRE Optical 'model'
## CHNEP data
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

LB.dat=read.xlsx(paste0(data.path,"CHNEP/LemonBay_DataDownload_2420844_row.xlsx"))
LB.dat$SampleDate=date.fun(convertToDate(LB.dat$SampleDate))
head(LB.dat)

LB.loc=ddply(LB.dat,c("DataSource","StationID","Actual_Longitude","Actual_Latitude"),
             summarise,
             N.samp=N.obs(Result_Value),
             min.date=min(SampleDate),
             max.date=max(SampleDate))

LB.loc.sp=spTransform(SpatialPointsDataFrame(LB.loc[,c("Actual_Longitude","Actual_Latitude")],data=LB.loc,proj4string=nad83.pro),utm17)
unique(LB.loc$DataSource)
tm_shape(subset(LB.loc.sp,DataSource%in%c("STORET_21FLCHAR")))+tm_dots()

         