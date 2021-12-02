## 
## CRE Optical
## Part 1: evaluate changes in S79 discharge 
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

## Paths
wd="C:/Julian_LaCie/_GitHub/CRE_OpticalModel"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]


# -------------------------------------------------------------------------
dates=date.fun(c("1978-05-01","2021-04-30"))

## Caloosahatchee
CRE.site=data.frame(SITE=c(rep("S79",2),rep("S78",2),rep("S77",2)),
                    DBKEY=c(c("DJ237","00865"),
                            c("DJ236","00857"),
                            c("15635","DJ235")),
                    Priority=c("P1","P2",
                               "P1","P2",
                               "P1","P2"))

CRE.Q.dat=data.frame()
for(i in 1:nrow(CRE.site)){
  tmp=DBHYDRO_daily(dates[1],dates[2],CRE.site$DBKEY[i])
  tmp$DBKEY=as.character(CRE.site$DBKEY[i])
  CRE.Q.dat=rbind(CRE.Q.dat,tmp)
  print(i)
}
CRE.Q.dat
CRE.Q.dat=merge(CRE.Q.dat,CRE.site,"DBKEY")
CRE.Q.dat.xtab=dcast(CRE.Q.dat,SITE+Date~Priority,value.var = "Data.Value",mean,na.rm=T)
# CRE.Q.dat.xtab$P3=NA #filler

plot(P1~Date,subset(CRE.Q.dat.xtab,SITE=='S79'))
with(subset(CRE.Q.dat.xtab,SITE=='S79'),points(P2~Date,pch=19,col="grey"))
plot(P1~P2,subset(CRE.Q.dat.xtab,SITE=='S79'));abline(0,1)

plot(P1~Date,subset(CRE.Q.dat.xtab,SITE=='S78'))
with(subset(CRE.Q.dat.xtab,SITE=='S78'),points(P2~Date,pch=19,col="grey"))
plot(P1~P2,subset(CRE.Q.dat.xtab,SITE=='S78'));abline(0,1)

plot(P1~Date,subset(CRE.Q.dat.xtab,SITE=='S77'))
with(subset(CRE.Q.dat.xtab,SITE=='S77'),points(P2~Date,pch=19,col="grey"))
plot(P1~P2,subset(CRE.Q.dat.xtab,SITE=='S77'));abline(0,1)

# CRE.Q.dat.xtab$fflow.cfs=with(CRE.Q.dat.xtab,ifelse(is.na(P1)==T&is.na(P2)==T,P3,ifelse(is.na(P2)==T&is.na(P3)==T,P1,ifelse(is.na(P1)==T&is.na(P3)==T,P2,P1))));#final flow value for analysis
CRE.Q.dat.xtab$fflow.cfs=with(CRE.Q.dat.xtab,ifelse(is.na(P1),P2,P1));#final flow value for analysis
range(CRE.Q.dat.xtab$fflow.cfs,na.rm = T)

plot(fflow.cfs~Date,subset(CRE.Q.dat.xtab,SITE=='S77'))
plot(fflow.cfs~Date,subset(CRE.Q.dat.xtab,SITE=='S78'))
plot(fflow.cfs~Date,subset(CRE.Q.dat.xtab,SITE=='S79'))

# CRE.Q.dat.xtab$fflow.cfs=with(CRE.Q.dat.xtab,ifelse(fflow.cfs<0,0,fflow.cfs))# positive flow only
CRE.Q.dat.xtab=dcast(CRE.Q.dat.xtab,Date~SITE,value.var = "fflow.cfs",mean)
CRE.Q.dat.xtab$WY=WY(CRE.Q.dat.xtab$Date)

subset(CRE.Q.dat.xtab, is.na(S79))
subset(CRE.Q.dat.xtab, is.na(S77))
CRE.Q.dat.xtab$S79=with(CRE.Q.dat.xtab,ifelse(is.na(S79),0,S79)); #fill NAs with 0 ... not many values
CRE.Q.dat.xtab$S77=with(CRE.Q.dat.xtab,ifelse(is.na(S77),0,S77))

CRE.Q.dat.xtab$S77BF=with(CRE.Q.dat.xtab,ifelse(S77<0,abs(S77),0))
CRE.Q.dat.xtab$S77=with(CRE.Q.dat.xtab,ifelse(S77<0,0,S77))
CRE.Q.dat.xtab$S79=with(CRE.Q.dat.xtab,ifelse(S79<0,0,S79))
CRE.Q.dat.xtab$C43=with(CRE.Q.dat.xtab,ifelse(S79>S77,S79-S77,0))
CRE.Q.dat.xtab$LOK=apply(CRE.Q.dat.xtab[,c("S77","S79")],1,min,na.rm=T)
# CRE.Q.dat.xtab$C43_2=with(CRE.Q.dat.xtab,S79-LOK)
# plot(C43~C43_2,CRE.Q.dat.xtab);abline(0,1)

plot(S79~Date,CRE.Q.dat.xtab,type="l",col="Red")
lines(S78~Date,CRE.Q.dat.xtab,type="l",col="green")
lines(S77~Date,CRE.Q.dat.xtab,type="l",col="blue")

CRE.Q.dat.melt=melt(CRE.Q.dat.xtab,id.vars = c("Date",'WY'), value.name="Q.cfs")
head(CRE.Q.dat.melt)

CRE.Q.dat.melt.WY=dcast(CRE.Q.dat.melt,WY~variable,value.var="Q.cfs",fun.aggregate=function(x) sum(cfs.to.acftd(x),na.rm=T))
head(CRE.Q.dat.melt.WY)

ylim.val=c(0,4000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1979,2021);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
cols=adjustcolor(c("goldenrod","steelblue"),0.75)
# png(filename=paste0(plot.path,"ann_CREQ.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(2.5,2,1,0.5),lwd=0.5);

x=barplot(t(CRE.Q.dat.melt.WY[,c("C43","LOK")]/1000),space=0,col=cols,ylim=ylim.val,axes=F,ann=F,yaxs="i",xaxs="i")
axis_fun(1,x[seq(1,length(x),by.x)],x,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
legend("topleft",legend=c("C-43 Basin","Lake Okeechobee"),
       pch=c(22),lwd=c(0.1),lty=0,
       pt.bg=cols,pt.cex=1.5,ncol=3,
       cex=0.75,bty="n",y.intersp=1,
       x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,title.adj = 0,title=" Discharge Source")
mtext(side=3,adj=0,"S-79 (Freshwater discharge to Calooshatchee River Estuary)")
mtext(side=2,line=2.75,"Discharge (x1000 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=2.5,"Water Year\n(May - April)")
dev.off()

# Daily Q breakpoint ------------------------------------------------------
# https://www.r-bloggers.com/2021/03/detect-the-changes-in-timeseries-data/
library(changepoint)

CRE.Q.dat.xtab2=subset(CRE.Q.dat.xtab,WY%in%seq(2008,2021,1))
# subset(CRE.Q.dat.xtab2,is.na(S79)==T)

ansmean=cpt.mean(CRE.Q.dat.xtab2$S79)
plot(S79~Date,CRE.Q.dat.xtab2,type="l",col="Red")
abline(v=CRE.Q.dat.xtab2[ansmean@cpts[1],"Date"])
abline(h=6500)

ansmean2=cpt.meanvar(CRE.Q.dat.xtab2$S79)
# Expected given period of drought.
plot(S79~Date,CRE.Q.dat.xtab2,type="l",col="Red")
abline(v=CRE.Q.dat.xtab2[ansmean2@cpts[1],"Date"])

## evaluate return frequency 