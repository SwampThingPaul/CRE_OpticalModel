## seagrass nomogram
# Fig 5 & 6 of Corbett and Hale (2006)
# If you have fitted model as lm object
# https://www.rdocumentation.org/packages/rms/versions/6.2-0/topics/nomogram

kz=1.4; # optical depth stays constant based on attenuation target
z=c(0.5,0.75,1,1.5,2,2.5); # Depth segements

kd=kz/z; # associated kd for depth

chla=seq(0,50,10)
turb=0

# model Kd = 0.014*Color + 0.062*Turb + 0.049*Chla + 0.30
# example
# (0.63-(0.062*0+0.049*0+0.30))/0.014

z1=(kd[1]-(0.062*turb+0.049*chla+0.30))/0.014
z2=(kd[2]-(0.062*turb+0.049*chla+0.30))/0.014
z3=(kd[3]-(0.062*turb+0.049*chla+0.30))/0.014
z4=(kd[4]-(0.062*turb+0.049*chla+0.30))/0.014
z5=(kd[5]-(0.062*turb+0.049*chla+0.30))/0.014
z6=(kd[6]-(0.062*turb+0.049*chla+0.30))/0.014

# Chlorophyll and Color nomogram
plot(chla~z1,ylim=c(0,60),xlim=c(0,180),type="n",xaxs="i",yaxs="i")
lines(chla~z1,col="red",type="b",pch=21)
lines(chla~z2,col="blue",type="b",pch=22)
lines(chla~z3,col="green",type="b",pch=23)
lines(chla~z4,col="black",lty=2,type="b",pch=24)
lines(chla~z5,col="red",lty=2,type="b",pch=19)
lines(chla~z6,col="blue",lty=2,type="b",pch=20)


## 
# Turbidty and Color nomogram
chla= 0
turb=seq(0,50,length.out=10)

z1=(kd[1]-(0.062*turb+0.049*chla+0.30))/0.014
z2=(kd[2]-(0.062*turb+0.049*chla+0.30))/0.014
z3=(kd[3]-(0.062*turb+0.049*chla+0.30))/0.014
z4=(kd[4]-(0.062*turb+0.049*chla+0.30))/0.014
z5=(kd[5]-(0.062*turb+0.049*chla+0.30))/0.014
z6=(kd[6]-(0.062*turb+0.049*chla+0.30))/0.014

plot(turb~z1,ylim=c(0,50),xlim=c(0,180),type="n",xaxs="i",yaxs="i")
lines(turb~z1,col="red",type="b",pch=21)
lines(turb~z2,col="blue",type="b",pch=22)
lines(turb~z3,col="green",type="b",pch=23)
lines(turb~z4,col="black",lty=2,type="b",pch=24)
lines(turb~z5,col="red",lty=2,type="b",pch=19)
lines(turb~z6,col="blue",lty=2,type="b",pch=20)
