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

aoi.all=bind(TB.bbox.AOI,CH.bbox.AOI)
# proj4string(aoi.all)=nad83.pro
aoi.all=spTransform(aoi.all,utm17)

proj4string(TB.bbox.AOI)=nad83.pro
TB.bbox.AOI=spTransform(TB.bbox.AOI,utm17)
proj4string(CH.bbox.AOI)=nad83.pro
CH.bbox.AOI=spTransform(CH.bbox.AOI,utm17)

library(USAboundaries)
FL.shp=us_boundaries(resolution ="high",states="Florida")
FL.shp=as(FL.shp,"Spatial")
FL.shp=spTransform(FL.shp,utm17)

SW.US=c("Florida","Georgia","Alabama","South Carolina")
SW.US.shp=us_boundaries(resolution ="low",states=SW.US)
SW.US.shp=as(SW.US.shp,"Spatial")
SW.US.shp=spTransform(SW.US.shp,utm17)

NWIS.siteinfo.shp=spTransform(NWIS.siteinfo.shp,utm17)
NWIS.siteinfo.shp=merge(NWIS.siteinfo.shp,usgs.sites,by.x="site_no",by.y="siteID")

aoi.all=raster::extent(gBuffer(aoi.all,width=5000))
aoi.all=as(aoi.all,"SpatialPolygons")
proj4string(aoi.all)=utm17


# png(filename=paste0(plot.path,"McPhersonMiller_SamplingMap.png"),width=4,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(1,0.25,0.25,0.25),mar=c(0.1,0.1,0.1,0.1),xpd=F)
layout(matrix(c(1,1,2,3),2:2),widths=c(1,0.4))

bbox.lims=bbox(aoi.all)
plot(FL.shp,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(NWIS.siteinfo.shp,add=T,pch=19,col="red")

SG.sites=c("CH24.SG","CH20.SG",'TB3','TB6',"TB5","CH6","CH29")
text(subset(NWIS.siteinfo.shp,!(siteName%in%SG.sites)),subset(NWIS.siteinfo.shp,!(siteName%in%SG.sites))$siteName,pos=4,cex=0.5,offset=0.25)
text(subset(NWIS.siteinfo.shp,siteName%in%SG.sites),subset(NWIS.siteinfo.shp,siteName%in%SG.sites)$siteName,pos=1,cex=0.5,offset=0.25)
plot(TB.bbox.AOI,add=T,border="goldenrod",lty=2,bg=NA,lwd=2)
text(extent(TB.bbox.AOI)[1]+(extent(TB.bbox.AOI)[2]-extent(TB.bbox.AOI)[1])/2,
     extent(TB.bbox.AOI)[4],"Tampa Bay",pos=3,cex=0.8,offset=0.25)
plot(CH.bbox.AOI,add=T,border="goldenrod",lty=2,bg=NA,lwd=2)
text(extent(CH.bbox.AOI)[1]+(extent(CH.bbox.AOI)[2]-extent(CH.bbox.AOI)[1])/2,
     extent(CH.bbox.AOI)[4],"Charlotte Harbor",pos=3,cex=0.8,offset=0.25)
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=1,seg.len=4)
box(lwd=1)
mtext(side=1,line=-1,adj=1,font=3,"Missing data for CH11 ",cex=0.75)
mtext(side=1,adj=0,"McPherson & Miller (1994) Causes of Light Attenuation in Tampa Bay\nand Charlotte Harbor, Southwestern Florida. JAWRA 30:43–53",cex=0.30)

plot(SW.US.shp)
plot(aoi.all,border="red",lty=2,add=T)
plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=c("Study\nAreas","Monitoring\nLocation"),
       pch=c(NA,19),pt.bg=c(NA,NA),col=c("goldenrod","red"),lty=c(2,0),lwd=c(1.5,0),
       pt.cex=1.25,ncol=1,cex=0.8,bty="n",y.intersp=2,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)

dev.off()
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

subset(dat.xtab,siteName=="TB3-depth")
subset(dat.xtab,siteName=="TB3")

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



summary(dat.xtab)

quantile(dat.xtab$Color,probs=0.95,na.rm=T)

# Bootstrap example (work in progress)
# http://www.utstat.toronto.edu/~brunner/oldclass/appliedf12/lectures/2101f12BootstrapR.pdf
set.seed(123)
n=1000
col.sim=seq(5,150,length.out=n)#,2.5)
Chla.sim=seq(0.2,13.2,length.out=n)#,0.1)
Turb.sim=seq(0.1,12.5,length.out=n)#,0.5)
Kpar.sim=seq(0.22,4.32,length.out=n)

# simulate data (check distribution)
B=1000
mstar.col=NULL
mstar.Chl=NULL
mstar.Turb=NULL
for(i in 1:B){
  mstar.col=c(mstar.col,mean(col.sim[sample(1:n,size=n,replace=T)]))
  mstar.Chl=c(mstar.Chl,mean(Chla.sim[sample(1:n,size=n,replace=T)]))
  mstar.Turb=c(mstar.Turb,mean(Turb.sim[sample(1:n,size=n,replace=T)]))
}

layout(matrix(1:3,1,3))
hist(mstar.col)
hist(mstar.Chl)
hist(mstar.Turb)

qqnorm(mstar.col);qqline(mstar.col)
qqnorm(mstar.Chl);qqline(mstar.Chl)
qqnorm(mstar.Turb);qqline(mstar.Turb)


# 
# Kd=0.014*color+0.062*turb+0.049*chla+0.30
betahat=cbind(0.30,0.014,0.062,0.049)
# rownames(betahat)="betahat"
terms(Kd.sim.samp~col.sim.samp+Turb.sim.samp+Chla.sim.samp)
MM.mod=list("(Intercept)", "col.sim.samp"," Turb.sim.samp"," Chla.sim.samp")
class(MM.mod)<-c("lm")
MM.mod$coefficients=betahat

betahat=coef(MM.mod)

set.seed(1234)
B=1000
bstar=NULL
n2=500
for(i in 1:B){
col.sim.samp=col.sim[sample(1:n2,size=n2,replace=T)]
Chla.sim.samp=Chla.sim[sample(1:n2,size=n2,replace=T)]
Turb.sim.samp=Turb.sim[sample(1:n2,size=n2,replace=T)]
Kd.sim.samp=(0.30+0.014*col.sim.samp+0.062*Turb.sim.samp+0.049*Chla.sim.samp)
# Kd.sim.samp=Kpar.sim[sample(1:n2,size=n2,replace=T)]

dat.sim=data.frame(cbind(Kd.sim.samp,col.sim.samp,Turb.sim.samp,Chla.sim.samp))
mod.sim=lm(Kd.sim.samp~col.sim.samp+Turb.sim.samp+Chla.sim.samp,dat.sim)
bstar = rbind( bstar,coef(mod.sim) )

}

bstar

Vb = var(bstar) # Approximate asymptotic covariance matrix of betahat
Vb

# Test individual coefficients. H0: betaj=0
se = sqrt(diag(Vb));
Z=betahat/se;rownames(Z)="Z"

rbind(betahat,se,Z)

WaldTest = function(L,thetahat,Vn,h=0) # H0: L theta = h
# Note Vn is the asymptotic covariance matrix, so it's the
# Consistent estimator divided by n. For true Wald tests
# based on numerical MLEs, just use the inverse of the Hessian.
{
 WaldTest = numeric(3)
 names(WaldTest) = c("W","df","p-value")
  r = dim(L)[1]
 W = t(L%*%thetahat-h) %*% solve(L%*%Vn%*%t(L)) %*% (L%*%thetahat-h)
 W = as.numeric(W)
 pval = 1-pchisq(W,r)
 WaldTest[1] = W; WaldTest[2] = r; WaldTest[3] = pval
 WaldTest
} # End function WaldTest


Lprod = rbind( c(0,1,0,0),
             + c(0,0,1,0)
             + c(0,0,0,1))
WaldTest(Lprod,betahat,Vb)

##
set.seed(3201); alpha=2; beta=3
D <- round(rgamma(50,shape=alpha, scale=beta),2); D
momalpha <- mean(D)^2/var(D); momalpha
mombeta <- var(D)/mean(D); mombeta

gmll2 <- function(theta,datta)
  { gmll2 <- -sum(dgamma(datta,shape=theta[1],scale=theta[2],log=T))
    gmll2
  } # End of gmll2
# Maximum likelihood estimation
gamama = nlm(gmll2,c(momalpha,mombeta),hessian=T,datta=D)
thetahat = gamama$estimate; thetahat
kov = solve(gamama$hessian) # Inverse of (estimated) observed info
kov

# Test H0: alpha = beta
# LR test gave G2 = 4.2776, p = 0.039

LL = rbind(c(1,-1)); LL
WaldTest(LL,thetahat,kov)
