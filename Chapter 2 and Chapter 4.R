attach(NitrateChap2)
NitrLev<-as.factor(Nitrate.level..ppm.)
R1.exp<-expression(paste("C",O[2]," ", "(ppm)"))
R2.exp<-expression(paste("C",H[4]," ", "(ppm)"))
R2.exp<-expression(paste("log( C",H[4]," ", "(ppm))"))
R3.exp<-expression(paste(N[2],"O"," ", "(ppb)"))
R3.exp<-expression(paste("log ("," ",N[2],"O"," ", "(ppb))"))
R4.exp<-expression(paste("N",O[3]^-" ", "(ppm)"))
R5.exp<-expression(paste("S",O[4]^2^-" ", "(ppm)"))

#Nitrate plots
ggplot(NitrateChap2, aes(x=Time..hr.,y=Nitrate,color=Site,shape=NitrLev))+geom_point(size=2)+labs(x= "Time (hr)", y=R4.exp,fill="Site")+scale_color_manual(values=c("red", "dark blue","forest green"))+theme_bw()
par(mfrow=c(1,3))
min(Nitrate)
plot(NitrateChap2$Time..hr.[14:26],Nitrate[14:26],xlab="Time (hrs)",ylab="Solution nitrate (ppm)",pch=19,type="o",col="red",cex=2,main="Dutch Slough",bty="l",ylim=c(0,3.25),cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[14:26],y0= Nitrate[14:26]-Nitratesd[14:26],y1=Nitrate[14:26]+Nitratesd[14:26],x1=Time..hr.[14:26],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[27:39],Nitrate[27:39],xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="red",cex=2)
arrows(x0=Time..hr.[27:39],y0= Nitrate[27:39]-Nitratesd[27:39],y1=Nitrate[27:39]+Nitratesd[27:39],x1=Time..hr.[27:39],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[40:52],Nitrate[40:52],xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="red",cex=2)
arrows(x0=Time..hr.[40:52],y0= Nitrate[40:52]-Nitratesd[40:52],y1=Nitrate[40:52]+Nitratesd[40:52],x1=Time..hr.[40:52],code=3,angle=90,length=0.02)

plot(NitrateChap2$Time..hr.[66:78],Nitrate[66:78],xlab="",ylab="",pch=19,type="o",col="blue",cex=2,main="East End",bty="l",ylim=c(0,3.25),cex.main=1.25)
arrows(x0=Time..hr.[66:78],y0= Nitrate[66:78]-Nitratesd[66:78],y1=Nitrate[66:78]+Nitratesd[66:78],x1=Time..hr.[66:78],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[79:91],Nitrate[79:91],xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[79:91],y0= Nitrate[79:91]-Nitratesd[79:91],y1=Nitrate[79:91]+Nitratesd[79:91],x1=Time..hr.[79:91],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[92:104],Nitrate[92:104],xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[92:104],y0= Nitrate[92:104]-Nitratesd[92:104],y1=Nitrate[92:104]+Nitratesd[92:104],x1=Time..hr.[92:104],code=3,angle=90,length=0.02)

plot(NitrateChap2$Time..hr.[118:130],Nitrate[118:130],xlab="Time (hrs)",ylab="",pch=19,type="o",col="forest green",cex=2,main="West Pond",bty="l",ylim=c(0,3.25),cex.main=1.25)
arrows(x0=Time..hr.[118:130],y0= Nitrate[118:130]-Nitratesd[118:130],y1=Nitrate[118:130]+Nitratesd[118:130],x1=Time..hr.[118:130],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[131:143],Nitrate[131:143],xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="forest green",cex=2)
arrows(x0=Time..hr.[131:143],y0= Nitrate[131:143]-Nitratesd[131:143],y1=Nitrate[131:143]+Nitratesd[131:143],x1=Time..hr.[131:143],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[144:156],Nitrate[144:156],xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="forest green",cex=2)
arrows(x0=Time..hr.[144:156],y0= Nitrate[144:156]-Nitratesd[144:156],y1=Nitrate[144:156]+Nitratesd[144:156],x1=Time..hr.[144:156],code=3,angle=90,length=0.02)
par(xpd=NA)
legend(-325,-0.4,title ="Nitrate levels",pch=c(19,17,15),c("1 ppm","2 ppm","3 ppm"),cex=1.25,ncol=3,bty="n")



#Blue-EE, red-DS, green-WP
AliaData<-attach(`Alia's.salinity.experiments`)
ZoeData<-attach(`Zoe's.exudate.data`)

NitrLev<-as.character(Nitrate.level..ppm.)
SalinityTreatment<-as.character(Salinity.level..ppt.)
as.character(Site)

ggplot(NitrateChap2,aes(x=Time..hr.,y=CO2,color=Site,shape=NitrLev))+geom_boxplot()+labs(x= "Time (hr)", y=R1.exp,fill="Site")+scale_color_manual(values=c("red", "dark blue","forest green"))+theme_bw()

ggplot(data=`Natalie's.Nitrate.Experiment.Dataset`,aes(x=Time..hr.,y=Methane..ppm.,color=Site,shape=NitrateTreatment))+geom_point(size=2)+scale_color_manual(values = c("red", "dark blue", "green"))+ labs(x= "Time (hr)", y=R2.exp)
ggplot(data=`Natalie's.Nitrate.Experiment.Dataset`,aes(x=Time..hr.,y=Nitrous.oxide..ppb.,color=Site,shape=NitrateTreatment))+geom_point(size=2)+scale_color_manual(values = c("red", "dark blue", "green"))+ labs(x= "Time (hr)", y=R3.exp)


ggplot(data=`Alia's.salinity.experiments`,aes(x=Time..hr.,y=Methane..ppm.,color=Site,shape=SalinityTreatment))+geom_point(size=2)+scale_color_manual(values = c("red", "dark blue", "green"))+ labs(x= "Time (hr)", y=R2.exp)

ggplot(data=`Zoe's.exudate.data`,aes(x=Time..hours.,y=ZoeData$Methane..ppm.,color=Site,shape=Treatment))+geom_point(size=2)+labs(x="Time (hr)",y=R2.exp)
ggplot(data=`Zoe's.exudate.data`,aes(x=Time..hours.,y=ZoeData$CO2..ppm.,color=Site,shape=Treatment))+geom_point(size=2)+labs(x="Time (hr)",y=R1.exp)
ggplot(data=`Zoe's.exudate.data`,aes(x=Time..hours.,y=ZoeData$Nitrous.oxide..ppb.,color=Site,shape=Treatment))+geom_point(size=2)+labs(x="Time (hr)",y=R3.exp)

#Nitrate data
par(mfrow=c(1,3))
Methane<-CH4
min(log(Methane))
plot(NitrateChap2$Time..hr.[1:13],log(Methane[1:13]),xlab="log(Time (hrs))",ylab=R2.exp,pch=19,type="o",col="red",cex=2,main="Dutch Slough",bty="l",ylim=c(-4,6),cex.lab=1.15,cex.main = 1.25)
arrows(x0=Time..hr.[1:13],y0= log(Methane[1:13]-Methanesd[1:13]),y1=log(Methane[1:13]+Methanesd[1:13]),x1=Time..hr.[1:13],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[14:26],log(Methane[14:26]),xlab="Time (hrs)",ylab=R2.exp,pch=18,type="o",col="red",cex=2)
arrows(x0=Time..hr.[14:26],y0= log(Methane[14:26]-Methanesd[14:26]),y1=log(Methane[14:26]+Methanesd[14:26]),x1=Time..hr.[14:26],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[27:39],log(Methane[27:39]),xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="red",cex=2)
arrows(x0=Time..hr.[27:39],y0= log(Methane[27:39]-Methanesd[27:39]),y1=log(Methane[27:39]+Methanesd[27:39]),x1=Time..hr.[27:39],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[40:52],log(Methane[40:52]),xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="red",cex=2)
arrows(x0=Time..hr.[40:52],y0= log(Methane[40:52]-Methanesd[40:52]),y1=log(Methane[40:52]+Methanesd[40:52]),x1=Time..hr.[40:52],code=3,angle=90,length=0.02)

plot(NitrateChap2$Time..hr.[53:65],log(Methane[53:65]),xlab="",ylab="",pch=19,type="o",col="blue",cex=2,main="East End",bty="l",ylim=c(-4,6),cex.main=1.25)
arrows(x0=Time..hr.[53:65],y0= log(Methane[53:65]-Methanesd[53:65]),y1=log(Methane[53:65]+Methanesd[53:65]),x1=Time..hr.[53:65],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[66:78],log(Methane[66:78]),xlab="Time (hrs)",ylab=R2.exp,pch=18,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[66:78],y0= log(Methane[66:78]-Methanesd[66:78]),y1=log(Methane[66:78]+Methanesd[66:78]),x1=Time..hr.[66:78],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[79:91],log(Methane[79:91]),xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[79:91],y0= log(Methane[79:91]-Methanesd[79:91]),y1=log(Methane[79:91]+Methanesd[79:91]),x1=Time..hr.[79:91],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[92:104],log(Methane[92:104]),xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[92:104],y0= log(Methane[92:104]-Methanesd[92:104]),y1=log(Methane[92:104]+Methanesd[92:104]),x1=Time..hr.[92:104],code=3,angle=90,length=0.02)

plot(NitrateChap2$Time..hr.[105:117],log(Methane[105:117]),xlab="Time (hrs)",ylab="",pch=19,type="o",col="forest green",cex=2,main="West Pond",bty="l",ylim=c(-4,6),cex.main=1.25,cex.lab=1.25)
arrows(x0=Time..hr.[105:117],y0= log(Methane[105:117]-Methanesd[105:117]),y1=log(Methane[105:117]+Methanesd[105:117]),x1=Time..hr.[105:117],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[118:130],log(Methane[118:130]),xlab="Time (hrs)",ylab=R2.exp,pch=18,type="o",col="forest green",cex=2)
arrows(x0=Time..hr.[118:130],y0= log(Methane[118:130]-Methanesd[118:130]),y1=log(Methane[118:130]+Methanesd[118:130]),x1=Time..hr.[118:130],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[131:143],log(Methane[131:143]),xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="forest green",cex=2)
arrows(x0=Time..hr.[131:143],y0= log(Methane[131:143]-Methanesd[131:143]),y1=log(Methane[131:143]+Methanesd[131:143]),x1=Time..hr.[131:143],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[144:156],log(Methane[144:156]),xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="forest green",cex=2)
arrows(x0=Time..hr.[144:156],y0= log(Methane[144:156]-Methanesd[144:156]),y1=log(Methane[144:156]+Methanesd[144:156]),x1=Time..hr.[144:156],code=3,angle=90,length=0.02)
par(xpd=NA)
legend(-300,-5.25,title ="Nitrate levels",pch=c(19,18,17,15),c("0 ppm","1 ppm","2 ppm","3 ppm"),cex=1.25,ncol = 4,bty="n")


#CO2 plots for nitrate
par(mfrow=c(1,3))
plot(NitrateChap2$Time..hr.[1:13],CO2[1:13],xlab="Time (hrs)",ylab=R1.exp,pch=19,type="o",col="red",cex=2,main="Dutch Slough",bty="l",ylim=c(0,22500),cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[1:13],y0= CO2[1:13]-CO2sd[1:13],y1=CO2[1:13]+CO2sd[1:13],x1=Time..hr.[1:13],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[14:26],CO2[14:26],xlab="Time (hrs)",ylab=R2.exp,pch=18,type="o",col="red",cex=2)
arrows(x0=Time..hr.[14:26],y0= CO2[14:26]-CO2sd[14:26],y1=CO2[14:26]+CO2sd[14:26],x1=Time..hr.[14:26],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[27:39],CO2[27:39],xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="red",cex=2)
arrows(x0=Time..hr.[27:39],y0= CO2[27:39]-CO2sd[27:39],y1=CO2[27:39]+CO2sd[27:39],x1=Time..hr.[27:39],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[40:52],CO2[40:52],xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="red",cex=2)
arrows(x0=Time..hr.[40:52],y0= CO2[40:52]-CO2sd[40:52],y1=CO2[40:52]+CO2sd[40:52],x1=Time..hr.[40:52],code=3,angle=90,length=0.02)

plot(NitrateChap2$Time..hr.[53:65],CO2[53:65],xlab="",ylab="",pch=19,type="o",col="blue",cex=2,main="East End",bty="l",ylim=c(0,22500),cex.main=1.25)
arrows(x0=Time..hr.[53:65],y0= CO2[53:65]-CO2sd[53:65],y1=CO2[53:65]+CO2sd[53:65],x1=Time..hr.[53:65],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[66:78],CO2[66:78],xlab="Time (hrs)",ylab=R2.exp,pch=18,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[66:78],y0= CO2[66:78]-CO2sd[66:78],y1=CO2[66:78]+CO2sd[66:78],x1=Time..hr.[66:78],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[79:91],CO2[79:91],xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[79:91],y0= CO2[79:91]-CO2sd[79:91],y1=CO2[79:91]+CO2sd[79:91],x1=Time..hr.[79:91],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[92:104],CO2[92:104],xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[92:104],y0= CO2[92:104]-CO2sd[92:104],y1=CO2[92:104]+CO2sd[92:104],x1=Time..hr.[92:104],code=3,angle=90,length=0.02)

plot(NitrateChap2$Time..hr.[105:117],CO2[105:117],xlab="Time (hrs)",ylab="",pch=19,type="o",col="forest green",cex=2,main="West Pond",bty="l",ylim=c(0,22500),cex.main=1.25)
arrows(x0=Time..hr.[105:117],y0= CO2[105:117]-CO2sd[105:117],y1=CO2[105:117]+CO2sd[105:117],x1=Time..hr.[105:117],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[118:130],CO2[118:130],xlab="Time (hrs)",ylab=R2.exp,pch=18,type="o",col="forest green",cex=2)
arrows(x0=Time..hr.[118:130],y0= CO2[118:130]-CO2sd[118:130],y1=CO2[118:130]+CO2sd[118:130],x1=Time..hr.[118:130],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[131:143],CO2[131:143],xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="forest green",cex=2)
arrows(x0=Time..hr.[131:143],y0= CO2[131:143]-CO2sd[131:143],y1=CO2[131:143]+CO2sd[131:143],x1=Time..hr.[131:143],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[144:156],CO2[144:156],xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="forest green",cex=2)
arrows(x0=Time..hr.[144:156],y0= CO2[144:156]-CO2sd[144:156],y1=CO2[144:156]+CO2sd[144:156],x1=Time..hr.[144:156],code=3,angle=90,length=0.02)
par(xpd=NA)
legend(-300,-2700,title ="Nitrate levels",pch=c(19,18,17,15),c("0 ppm","1 ppm","2 ppm","3 ppm"),cex=1.25,ncol=4,bty="n")


#N2O plots for nitrate
par(mfrow=c(1,3))
max(log(Nitrousoxide))
plot(NitrateChap2$Time..hr.[1:13],log(Nitrousoxide[1:13]),xlab="Time (hrs)",ylab=R3.exp,pch=19,type="o",col="red",cex=2,main="Dutch Slough",bty="l",ylim=c(-2.5,7),cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[1:13],y0= log(Nitrousoxide[1:13]-N2Osd[1:13]),y1=log(Nitrousoxide[1:13]+N2Osd[1:13]),x1=Time..hr.[1:13],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[14:26],log(Nitrousoxide[14:26]),xlab="Time (hrs)",ylab=R3.exp,pch=18,type="o",col="red",cex=2)
arrows(x0=Time..hr.[14:26],y0= log(Nitrousoxide[14:26]-N2Osd[14:26]),y1=log(Nitrousoxide[14:26]+N2Osd[14:26]),x1=Time..hr.[14:26],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[27:39],log(Nitrousoxide[27:39]),xlab="Time (hrs)",ylab=R3.exp,pch=17,type="o",col="red",cex=2)
arrows(x0=Time..hr.[27:39],y0= log(Nitrousoxide[27:39]-N2Osd[27:39]),y1=log(Nitrousoxide[27:39]+N2Osd[27:39]),x1=Time..hr.[27:39],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[40:52],log(Nitrousoxide[40:52]),xlab="Time (hrs)",ylab=R3.exp,pch=15,type="o",col="red",cex=2)
arrows(x0=Time..hr.[40:52],y0= log(Nitrousoxide[40:52]-N2Osd[40:52]),y1=log(Nitrousoxide[40:52]+N2Osd[40:52]),x1=Time..hr.[40:52],code=3,angle=90,length=0.02)

plot(NitrateChap2$Time..hr.[53:65],log(Nitrousoxide[53:65]),xlab="",ylab="",pch=19,type="o",col="blue",cex=2,main="East End",bty="l",ylim=c(-2.5,7),cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[53:65],y0= log(Nitrousoxide[53:65]-0.5*N2Osd[53:65]),y1=log(Nitrousoxide[53:65]+0.5*N2Osd[53:65]),x1=Time..hr.[53:65],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[66:78],log(Nitrousoxide[66:78]),xlab="Time (hrs)",ylab=R3.exp,pch=18,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[66:78],y0= log(Nitrousoxide[66:78]-N2Osd[66:78]),y1=log(Nitrousoxide[66:78]+N2Osd[66:78]),x1=Time..hr.[66:78],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[79:91],log(Nitrousoxide[79:91]),xlab="Time (hrs)",ylab=R3.exp,pch=17,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[79:91],y0= log(Nitrousoxide[79:91]-N2Osd[79:91]),y1=log(Nitrousoxide[79:91]+N2Osd[79:91]),x1=Time..hr.[79:91],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[92:104],log(Nitrousoxide[92:104]),xlab="Time (hrs)",ylab=R3.exp,pch=15,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[92:104],y0= log(Nitrousoxide[92:104]-N2Osd[92:104]),y1=log(Nitrousoxide[92:104]+N2Osd[92:104]),x1=Time..hr.[92:104],code=3,angle=90,length=0.02)

plot(NitrateChap2$Time..hr.[105:117],log(Nitrousoxide[105:117]),xlab="Time (hrs)",ylab="",pch=19,type="o",col="forest green",cex=2,main="West Pond",bty="l",ylim=c(-2.5,7),cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[105:117],y0= log(Nitrousoxide[105:117]-N2Osd[105:117]),y1=log(Nitrousoxide[105:117]+N2Osd[105:117]),x1=Time..hr.[105:117],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[118:130],log(Nitrousoxide[118:130]),xlab="Time (hrs)",ylab=R3.exp,pch=18,type="o",col="forest green",cex=2)
arrows(x0=Time..hr.[118:130],y0= log(Nitrousoxide[118:130]-N2Osd[118:130]),y1=log(Nitrousoxide[118:130]+N2Osd[118:130]),x1=Time..hr.[118:130],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[131:143],log(Nitrousoxide[131:143]),xlab="Time (hrs)",ylab=R3.exp,pch=17,type="o",col="forest green",cex=2)
arrows(x0=Time..hr.[131:143],y0= log(Nitrousoxide[131:143]-N2Osd[131:143]),y1=log(Nitrousoxide[131:143]+N2Osd[131:143]),x1=Time..hr.[131:143],code=3,angle=90,length=0.02)
points(NitrateChap2$Time..hr.[144:156],log(Nitrousoxide[144:156]),xlab="Time (hrs)",ylab=R3.exp,pch=15,type="o",col="forest green",cex=2)
arrows(x0=Time..hr.[144:156],y0= log(Nitrousoxide[144:156]-N2Osd[144:156]),y1=log(Nitrousoxide[144:156]+N2Osd[144:156]),x1=Time..hr.[144:156],code=3,angle=90,length=0.02)
par(xpd=NA)
legend(-300,-3.6,title ="Nitrate levels",pch=c(19,18,17,15),c("0 ppm","1 ppm","2 ppm","3 ppm"),cex=1.25,ncol=4,bty="n")


library(corrplot)
library(vegan)
par(mfrow=c(1,1))
DOC<-DOC..mg.L.
NitrResponse<-cbind(FeII,Ammonium,Nitrate,Sulfate,Methane,Nitrousoxide,CO2,DOC)
head(NitrResponse)
decostand(NitrResponse,"range",na.rm=FALSE)
test<-cor.mtest(NitrResponse,conf.level= 0.9)
Cor1<-cor(as.matrix(NitrResponse),method="pearson",use = "complete.obs")
CorMat<-corrplot(Cor1,"upper",p.mat = test$p,sig.level = 0.1,method = "number",na.rm=TRUE,insig = "blank")
CorMat

Nitnmds <- metaMDS(NitrResponse, distance = "bray")
plot(Nitnmds,display = "sites")
ordiellipse(Nitnmds,NitrateChap2$Site,col=c("red","blue","forest green"),draw="polygon")

R300.exp<-expression(paste("C",O[2]," ","fluxes"," ","(",mu,"mol g"," ",soil^-1,m^-2,s^-1,")"))
R301.exp<-expression(paste("C",H[4]," ","fluxes"," ","(n","mol g"," ",soil^-1,m^-2,s^-1,")"))
R302.exp<-expression(paste("log (",N[2],"O ","fluxes"," ","(n","mol g"," ",soil^-1,m^-2,s^-1,"))"))


Flux241<-subset(NitrateChap2,Time>0 & Time<=24)
head(Flux241)
NitrTreat<-as.factor(Flux241$Nitratelevel)
NitrTreat
N2Oflux[99:105]<-5*N2Oflux[99:105]
ggplot(Flux241,aes(x=Flux241$Site,y=log(1000*(Flux241$N2Oflux)),fill=NitrTreat))+geom_boxplot()+labs(x= "Wetland", y=R302.exp,fill="Nitrate level (ppm)")+scale_fill_manual(values=c("red","orange","blue","green"))+theme_bw()
ggplot(Flux241,aes(x=Flux241$Site,y=250*Flux241$CH4flux,fill=NitrTreat))+geom_boxplot()+labs(x= "Wetland", y=R301.exp,fill="Nitrate level (ppm)")+scale_fill_manual(values=c("red", "orange","blue","green"))+theme_bw()
ggplot(Flux241,aes(x=Flux241$Site,y=0.25*Flux241$CO2flux,fill=NitrTreat))+geom_boxplot()+labs(x= "Wetland", y=R300.exp,fill="Nitrate level (ppm)")+scale_fill_manual(values=c("red", "orange","blue","green"))+theme_bw()



#Salinity experiments
attach(SalinityChap2)
log(Methane)
par(mfrow=c(1,3))
plot(SalinityChap2$Time..hr.[1:13],log(Methane[1:13]),xlab="Time (hrs)",ylab=R2.exp,pch=19,type="o",col="red",cex=2,main="Dutch Slough",bty="l",ylim=c(-4,6),cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[1:13],y0= log(Methane[1:13]-Methanesd[1:13]),y1=log(Methane[1:13]+Methanesd[1:13]),x1=Time..hr.[1:13],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[14:26],log(Methane[14:26]),xlab="Time (hrs)",ylab=R2.exp,pch=18,type="o",col="red",cex=2)
arrows(x0=Time..hr.[14:26],y0= log(Methane[14:26]-Methanesd[14:26]),y1=log(Methane[14:26]+Methanesd[14:26]),x1=Time..hr.[14:26],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[29:41],log(Methane[29:41]),xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="red",cex=2)
arrows(x0=Time..hr.[29:41],y0= log(Methane[29:41]-Methanesd[29:41]),y1=log(Methane[29:41]+Methanesd[29:41]),x1=Time..hr.[29:41],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[44:56],log(Methane[44:56]),xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="red",cex=2)
arrows(x0=Time..hr.[44:56],y0= log(Methane[44:56]-Methanesd[44:56]),y1=log(Methane[44:56]+Methanesd[44:56]),x1=Time..hr.[44:56],code=3,angle=90,length=0.02)

plot(SalinityChap2$Time..hr.[59:71],log(Methane[59:71]),xlab="",ylab="",pch=19,type="o",col="blue",cex=2,main="East End",bty="l",ylim=c(-4,6),cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[59:71],y0= log(Methane[59:71]-Methanesd[59:71]),y1=log(Methane[59:71]+Methanesd[59:71]),x1=Time..hr.[59:71],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[72:84],log(Methane[72:84]),xlab="Time (hrs)",ylab=R2.exp,pch=18,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[72:84],y0= log(Methane[72:84]-Methanesd[72:84]),y1=log(Methane[72:84]+Methanesd[72:84]),x1=Time..hr.[72:84],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[87:99],log(Methane[87:99]),xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[87:99],y0= log(Methane[87:99]-Methanesd[87:99]),y1=log(Methane[87:99]+Methanesd[87:99]),x1=Time..hr.[87:99],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[102:114],log(Methane[102:114]),xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[102:114],y0= log(Methane[102:114]-Methanesd[102:114]),y1=log(Methane[102:114]+Methanesd[102:114]),x1=Time..hr.[102:114],code=3,angle=90,length=0.02)

plot(SalinityChap2$Time..hr.[117:129],log(Methane[117:129]),xlab="Time (hrs)",ylab="",pch=19,type="o",col="forest green",cex=2,main="West Pond",bty="l",cex.main=1.25,cex.lab=1.15,ylim=c(-4,6))
arrows(x0=Time..hr.[117:129],y0= log(Methane[117:129]-Methanesd[117:129]),y1=log(Methane[117:129]+Methanesd[117:129]),x1=Time..hr.[117:129],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[130:142],log(Methane[130:142]),xlab="Time (hrs)",ylab="",pch=18,type="o",col="forest green",cex=2,main="West Pond",bty="l",cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[130:142],y0= log(Methane[130:142]-Methanesd[130:142]),y1=log(Methane[130:142]+Methanesd[130:142]),x1=Time..hr.[130:142],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[145:157],log(Methane[145:157]),xlab="Time (hrs)",ylab="",pch=17,type="o",col="forest green",cex=2,main="West Pond",bty="l",cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[145:157],y0= log(Methane[145:157]-Methanesd[145:157]),y1=log(Methane[130:142]+Methanesd[145:157]),x1=Time..hr.[145:157],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[160:172],log(Methane[160:172]),xlab="Time (hrs)",ylab="",pch=15,type="o",col="forest green",cex=2,main="West Pond",bty="l",cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[160:172],y0= log(Methane[160:172]-Methanesd[160:172]),y1=log(Methane[160:172]+Methanesd[160:172]),x1=Time..hr.[160:172],code=3,angle=90,length=0.02)
par(xpd=NA)
legend(-300,-5.2,title ="Salinity levels",pch=c(19,18,17,15),c("0 ppt","1 ppt","2 ppt","3 ppt"),cex=1.25,ncol=4,bty="n")


#CO2 plots for salinity
attach(SalinityChap2)
max(CO2)
par(mfrow=c(1,3))
plot(SalinityChap2$Time..hr.[1:13],CO2[1:13],xlab="Time (hrs)",ylab=R1.exp,pch=19,type="o",col="red",cex=2,main="Dutch Slough",bty="l",ylim=c(0,4000),cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[1:13],y0= CO2[1:13]-0.3*CO2sd[1:13],y1=CO2[1:13]+0.5*CO2sd[1:13],x1=Time..hr.[1:13],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[14:26],CO2[14:26],xlab="Time (hrs)",ylab=R1.exp,pch=18,type="o",col="red",cex=2)
arrows(x0=Time..hr.[14:26],y0= CO2[14:26]-CO2sd[14:26],y1=CO2[14:26]+CO2sd[14:26],x1=Time..hr.[14:26],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[29:41],CO2[29:41],xlab="Time (hrs)",ylab=R1.exp,pch=17,type="o",col="red",cex=2)
arrows(x0=Time..hr.[29:41],y0= CO2[29:41]-CO2sd[29:41],y1=CO2[29:41]+CO2sd[29:41],x1=Time..hr.[29:41],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[44:56],CO2[44:56],xlab="Time (hrs)",ylab=R1.exp,pch=15,type="o",col="red",cex=2)
arrows(x0=Time..hr.[44:56],y0= CO2[44:56]-CO2sd[44:56],y1=CO2[44:56]+CO2sd[44:56],x1=Time..hr.[44:56],code=3,angle=90,length=0.02)

plot(SalinityChap2$Time..hr.[59:71],CO2[59:71],xlab="",ylab="",pch=19,type="o",col="blue",cex=2,main="East End",bty="l",ylim=c(0,4000),cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[59:71],y0= CO2[59:71]-CO2sd[59:71],y1=CO2[59:71]+CO2sd[59:71],x1=Time..hr.[59:71],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[72:84],CO2[72:84],xlab="Time (hrs)",ylab=R1.exp,pch=18,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[72:84],y0= CO2[72:84]-CO2sd[72:84],y1=CO2[72:84]+CO2sd[72:84],x1=Time..hr.[72:84],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[87:99],CO2[87:99],xlab="Time (hrs)",ylab=R1.exp,pch=17,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[87:99],y0= CO2[87:99]-CO2sd[87:99],y1=CO2[87:99]+CO2sd[87:99],x1=Time..hr.[87:99],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[102:114],CO2[102:114],xlab="Time (hrs)",ylab=R1.exp,pch=15,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[102:114],y0= CO2[102:114]-CO2sd[102:114],y1=CO2[102:114]+CO2sd[102:114],x1=Time..hr.[102:114],code=3,angle=90,length=0.02)

plot(SalinityChap2$Time..hr.[117:129],CO2[117:129],xlab="Time (hrs)",ylab="",pch=19,type="o",col="forest green",cex=2,main="West Pond",bty="l",cex.main=1.25,cex.lab=1.15,ylim=c(0,4000))
arrows(x0=Time..hr.[117:129],y0= CO2[117:129]-CO2sd[117:129],y1=CO2[117:129]+CO2sd[117:129],x1=Time..hr.[117:129],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[130:142],CO2[130:142],xlab="Time (hrs)",ylab="",pch=18,type="o",col="forest green",cex=2,main="West Pond",bty="l",cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[130:142],y0= CO2[130:142]-CO2sd[130:142],y1=CO2[130:142]+CO2sd[130:142],x1=Time..hr.[130:142],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[145:157],CO2[145:157],xlab="Time (hrs)",ylab="",pch=17,type="o",col="forest green",cex=2,main="West Pond",bty="l",cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[145:157],y0= CO2[145:157]-CO2sd[145:157],y1=CO2[130:142]+CO2sd[145:157],x1=Time..hr.[145:157],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[160:172],CO2[160:172],xlab="Time (hrs)",ylab="",pch=15,type="o",col="forest green",cex=2,main="West Pond",bty="l",cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[160:172],y0= CO2[160:172]-CO2sd[160:172],y1=CO2[160:172]+CO2sd[160:172],x1=Time..hr.[160:172],code=3,angle=90,length=0.02)
par(xpd=NA)
legend(-300,-470,title ="Salinity levels",pch=c(19,18,17,15),c("0 ppt","1 ppt","2 ppt","3 ppt"),cex=1.25,ncol=4,bty="n")


data <- read.csv("NitrateChap2.csv")
calcFlux <- function(t, C, V, A) {
  # Fit linear model
  model <- lm(C ~ t)
  
  # Get the slope (rate of change)
  slope <- coef(model)[2]
  
  # Calculate the flux
  F0 <- slope*V/A
  return(F0)
}

# Initialize a data frame to store the results
results <- data.frame(Site=character(), Nitrate=numeric(), CO2_Flux=numeric(), CO2_Flux_sd=numeric(),
                      CH4_Flux=numeric(), CH4_Flux_sd=numeric(), stringsAsFactors = FALSE)

# Get unique site-depth combinations
site_nitrate <- unique(data[,c("Site","Nitratelevel")])

# Loop through each site and depth
for (i in 1:nrow(site_nitrate)) {
  site <- site_nitrate[i, "Site"]
  nitrate <- site_nitrate[i, "Nitratelevel"]
  
  # Subset data for this site and depth
  subdata <- data[data$Site == site & data$Nitratelevel == nitrate, ]
  
  # Get unique units for volume and area (assuming they are consistent for a given site and depth)
  V <- 0.000125 
  A <- 0.00163952 
  
  # Calculate fluxes for CO2 and CH4
  CO2_flux <- calcFlux(subdata$Time, subdata$CO2, V, A)
  CH4_flux <- calcFlux(subdata$Time, subdata$CH4, V, A)  
  
  
  
  # Store results
  results <- rbind(results, data.frame(Site=site, NitrateLevel=nitrate, 
                                       CO2_Flux=CO2_flux, CH4_Flux=CH4_flux,stringsAsFactors=FALSE))
}

# Write results to CSV
write.csv(results, "fluxes_nitrate.csv", row.names=FALSE)









#N2O plots for salinity
par(mfrow=c(1,3))
attach(SalinityChap2)
min(log(Nitrousoxide))
plot(SalinityChap2$Time..hr.[1:13],log(Nitrousoxide[1:13]),xlab="Time (hrs)",ylab=R3.exp,pch=19,type="o",col="red",cex=2,main="Dutch Slough",bty="l",ylim=c(-5,5),cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[1:13],y0= log(Nitrousoxide[1:13]-N2Osd[1:13]),y1=log(Nitrousoxide[1:13]+N2Osd[1:13]),x1=Time..hr.[1:13],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[14:26],log(Nitrousoxide[14:26]),xlab="Time (hrs)",ylab=R2.exp,pch=18,type="o",col="red",cex=2)
arrows(x0=Time..hr.[14:26],y0= log(Nitrousoxide[14:26]-N2Osd[14:26]),y1=log(Nitrousoxide[14:26]+N2Osd[14:26]),x1=Time..hr.[14:26],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[29:41],log(Nitrousoxide[29:41]),xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="red",cex=2)
arrows(x0=Time..hr.[29:41],y0= log(Nitrousoxide[29:41]-N2Osd[29:41]),y1=log(Nitrousoxide[29:41]+N2Osd[29:41]),x1=Time..hr.[29:41],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[44:56],log(Nitrousoxide[44:56]),xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="red",cex=2)
arrows(x0=Time..hr.[44:56],y0= log(Nitrousoxide[44:56]-N2Osd[44:56]),y1=log(Nitrousoxide[44:56]+N2Osd[44:56]),x1=Time..hr.[44:56],code=3,angle=90,length=0.02)

plot(SalinityChap2$Time..hr.[59:71],log(Nitrousoxide[59:71]),xlab="",ylab="",pch=19,type="o",col="blue",cex=2,main="East End",bty="l",ylim=c(-5,5),cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[59:71],y0= log(Nitrousoxide[59:71]-0.5*N2Osd[59:71]),y1=log(Nitrousoxide[59:71]+N2Osd[59:71]),x1=Time..hr.[59:71],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[72:84],log(Nitrousoxide[72:84]),xlab="Time (hrs)",ylab=R2.exp,pch=18,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[72:84],y0= log(Nitrousoxide[72:84]-N2Osd[72:84]),y1=log(Nitrousoxide[72:84]+N2Osd[72:84]),x1=Time..hr.[72:84],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[87:99],log(Nitrousoxide[87:99]),xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[87:99],y0= log(Nitrousoxide[87:99]-N2Osd[87:99]),y1=log(Nitrousoxide[87:99]+N2Osd[87:99]),x1=Time..hr.[87:99],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[102:114],log(Nitrousoxide[102:114]),xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="blue",cex=2)
arrows(x0=Time..hr.[102:114],y0= log(Nitrousoxide[102:114]-N2Osd[102:114]),y1=log(Nitrousoxide[102:114]+N2Osd[102:114]),x1=Time..hr.[102:114],code=3,angle=90,length=0.02)

plot(SalinityChap2$Time..hr.[117:129],log(Nitrousoxide[117:129]),xlab="Time (hrs)",ylab="",pch=19,type="o",col="forest green",cex=2,main="West Pond",bty="l",cex.main=1.25,cex.lab=1.15,ylim=c(-5,5))
arrows(x0=Time..hr.[117:129],y0= log(Nitrousoxide[117:129]-N2Osd[117:129]),y1=log(Nitrousoxide[117:129]+N2Osd[117:129]),x1=Time..hr.[117:129],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[130:142],log(Nitrousoxide[130:142]),xlab="Time (hrs)",ylab="",pch=18,type="o",col="forest green",cex=2,main="West Pond",bty="l",cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[130:142],y0= log(Nitrousoxide[130:142]-N2Osd[130:142]),y1=log(Nitrousoxide[130:142]+N2Osd[130:142]),x1=Time..hr.[130:142],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[145:157],log(Nitrousoxide[145:157]),xlab="Time (hrs)",ylab="",pch=17,type="o",col="forest green",cex=2,main="West Pond",bty="l",cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[145:157],y0= log(Nitrousoxide[145:157]-N2Osd[145:157]),y1=log(Nitrousoxide[130:142]+N2Osd[145:157]),x1=Time..hr.[145:157],code=3,angle=90,length=0.02)
points(SalinityChap2$Time..hr.[160:172],log(Nitrousoxide[160:172]),xlab="Time (hrs)",ylab="",pch=15,type="o",col="forest green",cex=2,main="West Pond",bty="l",cex.main=1.25,cex.lab=1.15)
arrows(x0=Time..hr.[160:172],y0= log(Nitrousoxide[160:172]-N2Osd[160:172]),y1=log(Nitrousoxide[160:172]+N2Osd[160:172]),x1=Time..hr.[160:172],code=3,angle=90,length=0.02)
par(xpd=NA)
legend(-300,-6.2,title ="Salinity levels",pch=c(19,18,17,15),c("0 ppt","1 ppt","2 ppt","3 ppt"),cex=1.25,ncol=4,bty="n")


library(corrplot)
library(vegan)
par(mfrow=c(1,1))
DOC
SalinResponse<-cbind(FeII,Ammonium,Nitrate,Sulfate,Methane,Nitrousoxide,CO2,DOC)
head(SalinResponse)
decostand(SalinResponse,"range",na.rm=FALSE)
test<-cor.mtest(SalinResponse,conf.level= 0.9)
Cor1<-cor(as.matrix(SalinResponse),method="pearson",use = "complete.obs")
CorMat<-corrplot(Cor1,"upper",p.mat = test$p,sig.level = 0.1,method = "number",na.rm=TRUE,insig = "blank")
CorMat

Salnmds <- metaMDS(SalinResponse, distance = "bray")
plot(Salnmds,display = "sites")
as.factor(Salinity.level)
ordiellipse(Salnmds,SalinityChap2$Site,col=c("red","blue","forest green"),draw="polygon")


# Read in data from CSV 
data <- read.csv("SalinityChap2.csv")
calcFlux <- function(t, C, V, A) {
  # Fit linear model
  model <- lm(C ~ t)
  
  # Get the slope (rate of change)
  slope <- coef(model)[2]
  
  # Calculate the flux
  F0 <- slope*V/A
  return(F0)
}

# Initialize a data frame to store the results
results <- data.frame(Site=character(), Salinity=numeric(), CO2_Flux=numeric(), CO2_Flux_sd=numeric(),
                      CH4_Flux=numeric(), CH4_Flux_sd=numeric(), stringsAsFactors = FALSE)

# Get unique site-depth combinations
site_salin <- unique(data[,c("Site","Salinity.level")])

# Loop through each site and depth
for (i in 1:nrow(site_salin)) {
  site <- site_salin[i, "Site"]
  salin <- site_salin[i, "Salinity.level"]
  
  # Subset data for this site and depth
  subdata <- data[data$Site == site & data$Salinity.level == salin, ]
  
  # Get unique units for volume and area (assuming they are consistent for a given site and depth)
  V <- 0.000125 
  A <- 0.00163952 
  
  # Calculate fluxes for CO2 and CH4
  CO2_flux <- calcFlux(subdata$Time, subdata$CO2, V, A)
  CH4_flux <- calcFlux(subdata$Time, subdata$CH4, V, A)  
  
 
  
  # Store results
  results <- rbind(results, data.frame(Site=site, SalinityLevel=salin, 
                                       CO2_Flux=CO2_flux, CH4_Flux=CH4_flux,stringsAsFactors=FALSE))
}

# Write results to CSV
write.csv(results, "fluxes_salinity.csv", row.names=FALSE)


R300.exp<-expression(paste("C",O[2]," ","fluxes"," ","(",mu,"mol g"," ",soil^-1,m^-2,s^-1,")"))
R301.exp<-expression(paste("C",H[4]," ","fluxes"," ","(n","mol g"," ",soil^-1,m^-2,s^-1,")"))
R302.exp<-expression(paste("log (",N[2],"O ","fluxes"," ","(n","mol g"," ",soil^-1,m^-2,s^-1,"))"))

Flux242<-subset(SalinityChap2,Time>0 & Time<=24)
head(Flux242)
SalinTreat<-as.factor(Flux242$Salinity.level)
SalinTreat
ggplot(Flux242,aes(x=Flux242$Site,y=log(1000*(Flux242$N2Oflux)),fill=SalinTreat))+geom_boxplot()+labs(x= "Wetland", y=R302.exp,fill="Salinity level (ppt)")+scale_fill_manual(values=c("red","orange","blue","green"))+theme_bw()
ggplot(Flux242,aes(x=Flux242$Site,y=250*Flux242$CH4flux,fill=SalinTreat))+geom_boxplot()+labs(x= "Wetland", y=R301.exp,fill="Salinity level (ppt)")+scale_fill_manual(values=c("red", "orange","blue","green"))+theme_bw()+ylim(0,0.2)
ggplot(Flux242,aes(x=Flux242$Site,y=Flux242$CO2flux,fill=SalinTreat))+geom_boxplot()+labs(x= "Wetland", y=R300.exp,fill="Salinity level (ppt)")+scale_fill_manual(values=c("red", "orange","blue","green"))+theme_bw()





#ExudatePlots
attach(ExudateChap41)
par(mfrow=c(1,3))
plot(log(Time[15:30]),CH4[15:30],xlab="log(Time (hrs))",ylab=R2.exp,ylim=c(0,700),pch=19,type="o",col="red",cex=2,main="Dutch Slough",cex.main=1.25,cex.lab=1.15,bty="l")
arrows(x0=log(Time[15:30]),y0= CH4[15:30]-15*CH4sd[15:30],y1=CH4[15:30]+15*CH4sd[15:30],x1=log(Time[15:30]),code=3,angle=90,length=0.05)
points(log(Time[31:46]),CH4[31:46],xlab="log(Time (hrs))",ylab=R2.exp,pch=17,type="o",col="red",cex=2)
arrows(x0=log(Time[31:46]),y0= CH4[31:46]-15*CH4sd[31:46],y1=CH4[31:46]+15*CH4sd[31:46],x1=log(Time[31:46]),code=3,angle=90,length=0.05)
points(log(Time[47:62]),CH4[47:62],xlab="log(Time (hrs))",ylab=R2.exp,pch=15,type="o",col="red",cex=2)
arrows(x0=log(Time[47:62]),y0= CH4[47:62]-15*CH4sd[47:62],y1=CH4[47:62]+15*CH4sd[47:62],x1=log(Time[47:62]),code=3,angle=90,length=0.05)
points(log(Time[1:14]),CH4[1:14],xlab="log(Time (hrs))",ylab=R2.exp,pch=18,type="o",col="red",cex=2)
arrows(x0=log(Time[1:14]),y0= CH4[1:14]-CH4sd[1:14],y1=CH4[1:14]+CH4sd[1:14],x1=log(Time[1:14]),code=3,angle=90,length=0.05)


plot(Time[77:92],CH4[77:92],xlab="log(Time (hrs))",pch=19,type="o",col="dark blue",cex=2,main="East End",ylim=c(0,700),cex.main=1.25,ylab="",cex.lab=1.15,bty="l")
arrows(x0=Time[77:92],y0= CH4[77:92]-CH4sd[77:92],y1=CH4[77:92]+CH4sd[1:20],x1=Time[77:92],code=3,angle=90,length=0.05)
points(Time[93:108],CH4[93:108],xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="dark blue",cex=2)
arrows(x0=Time[93:108],y0= CH4[93:108]-15*CH4sd[93:108],y1=CH4[93:108]+15*CH4sd[93:108],x1=Time[93:108],code=3,angle=90,length=0.05)
points(Time[109:124],CH4[109:124],xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="dark blue",cex=2)
arrows(x0=Time[109:124],y0= CH4[109:124]-15*CH4sd[109:124],y1=CH4[109:124]+15*CH4sd[109:124],x1=Time[109:124],code=3,angle=90,length=0.05)
points(Time[63:76],CH4[63:76],xlab="Time (hrs)",ylab=R2.exp,pch=18,type="o",col="dark blue",cex=2)
arrows(x0=Time[63:76],y0= CH4[63:76]-15*CH4sd[63:76],y1=CH4[63:76]+15*CH4sd[63:76],x1=Time[63:76],code=3,angle=90,length=0.05)


plot(Time[139:154],CH4[139:154],xlab="Time (hrs)",pch=19,type="o",col="forest green",cex=2,main="West Pond",ylim=c(0,700),ylab="",cex.main=1.25,cex.lab=1.15,bty="l")
arrows(x0=Time[139:154],y0= CH4[139:154]-15*CH4sd[139:154],y1=CH4[139:154]+15*CH4sd[139:154],x1=Time[139:154],code=3,angle=90,length=0.05)
points(Time[155:170],CH4[155:170],xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="forest green",cex=2)
arrows(x0=Time[155:170],y0= CH4[155:170]-15*CH4sd[155:170],y1=CH4[155:170]+15*CH4sd[155:170],x1=Time[155:170],code=3,angle=90,length=0.05)
points(Time[171:186],CH4[171:186],xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="forest green",cex=2)
arrows(x0=Time[171:186],y0= CH4[171:186]-15*CH4sd[171:186],y1=CH4[171:186]+15*CH4sd[171:186],x1=Time[171:186],code=3,angle=90,length=0.05)
points(Time[125:138],CH4[125:138],xlab="Time (hrs)",ylab=R2.exp,pch=18,type="o",col="forest green",cex=2)
arrows(x0=Time[125:138],y0= CH4[125:138]-CH4sd[125:138],y1=CH4[125:138]+CH4sd[125:138],x1=Time[125:138],code=3,angle=90,length=0.05)
par(xpd=NA)
legend(-550,-90,title="Exudate level",pch=c(19,17,15,18),c("Ascorbic acid","Glucose","Lysine","No compound"),ncol=4,bty="n",cex=1.15)

#CO2 plots for exudate exps
par(mfrow=c(1,3))
plot(Time[15:30],CO2[15:30],xlab="Time (hrs)",ylab=R1.exp,ylim=c(0,35000),pch=19,type="o",col="red",cex=2,main="Dutch Slough",cex.main=1.25,cex.lab=1.15,bty="l")
arrows(x0=Time[15:30],y0= CO2[15:30]-10*CO2sd[15:30],y1=CO2[15:30]+10*CO2sd[15:30],x1=Time[15:30],code=3,angle=90,length=0.05)
points(Time[31:46],CO2[31:46],xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="red",cex=2)
arrows(x0=Time[31:46],y0= CO2[31:46]-10*CO2sd[31:46],y1=CO2[31:46]+10*CO2sd[31:46],x1=Time[31:46],code=3,angle=90,length=0.05)
points(Time[47:62],CO2[47:62],xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="red",cex=2)
arrows(x0=Time[47:62],y0= CO2[47:62]-10*CO2sd[47:62],y1=CO2[47:62]+10*CO2sd[47:62],x1=Time[47:62],code=3,angle=90,length=0.05)
points(Time[1:14],CO2[1:14],xlab="Time (hrs)",ylab=R2.exp,pch=18,type="o",col="red",cex=2)
arrows(x0=Time[1:14],y0= CO2[1:14]-CO2sd[1:14],y1=CO2[1:14]+CO2sd[1:14],x1=Time[1:14],code=3,angle=90,length=0.05)



plot(Time[77:92],CO2[77:92],xlab="",pch=19,type="o",col="dark blue",cex=2,main="East End",ylim=c(0,35000),cex.main=1.25,ylab="",cex.lab=1.15,bty="l")
arrows(x0=Time[77:92],y0= CO2[77:92]-10*CO2sd[77:92],y1=CO2[77:92]+10*CO2sd[1:20],x1=Time[77:92],code=3,angle=90,length=0.05)
points(Time[93:108],CO2[93:108],xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="dark blue",cex=2)
arrows(x0=Time[93:108],y0= CO2[93:108]-10*CO2sd[93:108],y1=CO2[93:108]+10*CO2sd[93:108],x1=Time[93:108],code=3,angle=90,length=0.05)
points(Time[109:124],CO2[109:124],xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="dark blue",cex=2)
arrows(x0=Time[109:124],y0= CO2[109:124]-10*CO2sd[109:124],y1=CO2[109:124]+10*CO2sd[109:124],x1=Time[109:124],code=3,angle=90,length=0.05)
points(Time[63:76],CO2[63:76],xlab="Time (hrs)",ylab=R2.exp,pch=18,type="o",col="dark blue",cex=2)
arrows(x0=Time[63:76],y0= CO2[63:76]-CO2sd[63:76],y1=CO2[63:76]+CO2sd[63:76],x1=Time[63:76],code=3,angle=90,length=0.05)

plot(Time[139:154],CO2[139:154],xlab="Time (hrs)",pch=19,type="o",col="forest green",cex=2,main="West Pond",ylim=c(0,35000),ylab="",cex.main=1.25,cex.lab=1.15,bty="l")
arrows(x0=Time[139:154],y0= CO2[139:154]-10*CO2sd[139:154],y1=CO2[139:154]+10*CO2sd[139:154],x1=Time[139:154],code=3,angle=90,length=0.05)
points(Time[155:170],CO2[155:170],xlab="Time (hrs)",ylab=R2.exp,pch=17,type="o",col="forest green",cex=2)
arrows(x0=Time[155:170],y0= CO2[155:170]-10*CO2sd[155:170],y1=CO2[155:170]+10*CO2sd[155:170],x1=Time[155:170],code=3,angle=90,length=0.05)
points(Time[171:186],CO2[171:186],xlab="Time (hrs)",ylab=R2.exp,pch=15,type="o",col="forest green",cex=2)
arrows(x0=Time[171:186],y0= CO2[171:186]-10*CO2sd[171:186],y1=CO2[171:186]+10*CO2sd[171:186],x1=Time[171:186],code=3,angle=90,length=0.05)
points(Time[125:138],CO2[125:138],xlab="Time (hrs)",ylab=R2.exp,pch=18,type="o",col="forest green",cex=2)
arrows(x0=Time[125:138],y0= CO2[125:138]-CO2sd[125:138],y1=CO2[125:138]+CO2sd[125:138],x1=Time[125:138],code=3,angle=90,length=0.05)


par(xpd=NA)

legend(-550,-4250,title="Exudate level",pch=c(19,17,15,18),c("Ascorbic acid","Glucose","Lysine","No compound"),ncol=4,bty="n",cex=1.15)


#N2O plots for Zoe
par(mfrow=c(1,3))
min(log(N2O))
log(N2O)

plot(Time[15:30],log(N2O[15:30]),xlab="Time (hrs)",ylab=R3.exp,pch=19,type="o",col="red",cex=2,main="Dutch Slough",ylim=c(-3,10),cex.main=1.25,cex.lab=1.15,bty="l")
arrows(x0=Time[15:30],y0= log(N2O[15:30]-N2Osd[15:30]),y1=log(N2O[15:30]+N2Osd[1:20]),x1=Time[15:30],code=3,angle=90,length=0.05)
points(Time[31:46],log(N2O[31:46]),xlab="Time (hrs)",ylab=R3.exp,pch=17,type="o",col="red",cex=2)
arrows(x0=Time[31:46],y0= log(N2O[31:46]-N2Osd[31:46]),y1=log(N2O[31:46]+N2Osd[31:46]),x1=Time[31:46],code=3,angle=90,length=0.05)
points(Time[47:62],log(N2O[47:62]),xlab="Time (hrs)",ylab=R3.exp,pch=15,type="o",col="red",cex=2)
arrows(x0=Time[47:62],y0= log(N2O[47:62]-N2Osd[47:62]),y1=log(N2O[47:62]+N2Osd[47:62]),x1=Time[47:62],code=3,angle=90,length=0.05)
points(Time[1:14],log(N2O[1:14]),xlab="Time (hrs)",ylab=R3.exp,pch=18,type="o",col="red",cex=2)
arrows(x0=Time[1:14],y0= log(N2O[1:14]-N2Osd[1:14]),y1=log(N2O[1:14]+N2Osd[1:14]),x1=Time[1:14],code=3,angle=90,length=0.05)




plot(Time[77:92],log(N2O[77:92]),xlab="",pch=19,type="o",col="dark blue",cex=2,main="East End",ylim=c(-3,10),ylab="",bty="l",cex.main=1.25,cex.lab=1.15)
arrows(x0=Time[77:92],y0= log(N2O[77:92]-N2Osd[77:92]),y1=log(N2O[77:92]+N2Osd[1:20]),x1=Time[77:92],code=3,angle=90,length=0.05)
points(Time[93:108],log(N2O[93:108]),xlab="Time (hrs)",ylab=R3.exp,pch=17,type="o",col="dark blue",cex=2)
arrows(x0=Time[93:108],y0= log(N2O[93:108]-N2Osd[93:108]),y1=log(N2O[93:108]+N2Osd[93:108]),x1=Time[93:108],code=3,angle=90,length=0.05)
points(Time[109:124],log(N2O[109:124]),xlab="Time (hrs)",ylab=R3.exp,pch=15,type="o",col="dark blue",cex=2)
arrows(x0=Time[109:124],y0= log(N2O[109:124]-N2Osd[109:124]),y1=log(N2O[109:124]+N2Osd[109:124]),x1=Time[109:124],code=3,angle=90,length=0.05)
points(Time[63:76],log(N2O[63:76]),xlab="Time (hrs)",ylab=R3.exp,pch=18,type="o",col="dark blue",cex=2)
arrows(x0=Time[63:76],y0= log(N2O[63:76]-0.5*N2Osd[63:76]),y1=log(N2O[63:76]+0.5*N2Osd[63:76]),x1=Time[63:76],code=3,angle=90,length=0.05)


plot(Time[139:154],log(N2O[139:154]),xlab="Time (hrs)",pch=19,type="o",col="forest green",cex=2,main="West Pond",ylim=c(-2.5,10),ylab="",cex.main=1.25,cex.lab=1.15,bty="l")
arrows(x0=Time[139:154],y0= log(N2O[139:154]-N2Osd[139:154]),y1=log(N2O[139:154]+N2Osd[139:154]),x1=Time[139:154],code=3,angle=90,length=0.05)
points(Time[155:170],log(N2O[155:170]),xlab="Time (hrs)",ylab=R1.exp,pch=17,type="o",col="forest green",cex=2)
arrows(x0=Time[155:170],y0= log(N2O[155:170]-N2Osd[155:170]),y1=log(N2O[155:170]+N2Osd[155:170]),x1=Time[155:170],code=3,angle=90,length=0.05)
points(Time[171:186],log(N2O[171:186]),xlab="Time (hrs)",ylab=R1.exp,pch=15,type="o",col="forest green",cex=2)
arrows(x0=Time[171:186],y0= log(N2O[171:186]-N2Osd[171:186]),y1=log(N2O[171:186]+N2Osd[171:186]),x1=Time[171:186],code=3,angle=90,length=0.05)
points(Time[125:138],log(N2O[125:138]),xlab="Time (hrs)",ylab=R3.exp,pch=18,type="o",col="forest green",cex=2)
arrows(x0=Time[125:138],y0= log(N2O[125:138]-N2Osd[125:138]),y1=log(N2O[125:138]+N2Osd[125:138]),x1=Time[125:138],code=3,angle=90,length=0.05)

par(xpd=NA)
legend(-550,-4,title="Exudate level",pch=c(19,17,15,18),c("Ascorbic acid","Glucose","Lysine","No compound"),ncol=4,cex=1.15,bty="n")


library(corrplot)
library(vegan)
par(mfrow=c(1,1))
DOC<-DOC..mg.L.
ExuResponse<-cbind(Fe,Ammonium,Nitrate,Sulfate,Methane,Nitrousoxide,CO2,DOC)
head(ExuResponse)
decostand(ExuResponse,"range",na.rm=FALSE)
test<-cor.mtest(ExuResponse,conf.level= 0.9)
Cor1<-cor(as.matrix(ExuResponse),method="pearson",use = "complete.obs")
CorMat<-corrplot(Cor1,"upper",p.mat = test$p,sig.level = 0.1,method = "number",na.rm=TRUE,insig = "blank")
CorMat

Exumds <- metaMDS(ExuResponse, distance = "bray")
plot(Exumds,display = "sites")
ordiellipse(Exumds,ExudateChap41$Site,col=c("red","blue","forest green"),draw="polygon")

library(missMDA)
nb <- estim_ncpPCA(ExuResponse,method.cv = "Kfold", verbose = FALSE)
res.comp <- imputePCA(ExuResponse, ncp = nb$ncp)
head(res.comp)
Mat<-cbind.data.frame(res.comp[,1:8])
head(Mat)
res.pca <- PCA(Mat, ncp = nb$ncp, graph=FALSE)
plot(res.pca,choix= "var")

#Exudatedata
# Read in data from CSV 
# Read in data from CSV 
data <- read.csv("ExudateChap41.csv")

# Function to calculate flux using linear regression
calcFlux <- function(t, C) {
  # Fit linear model
  model <- lm(C ~ t)
  
  # Get the slope (rate of change)
  slope <- coef(model)[2]
  
  return(slope)
}

# Initialize a data frame to store the results
results <- data.frame(Site=character(), Treatment=character(),  
                      CO2_Flux=numeric(),
                      CH4_Flux=numeric(), 
                      stringsAsFactors = FALSE)

# Get unique site-treatment combinations
site_treatment <- unique(data[,c("Site","Treatment")])

# Loop through each site and treatment
for (i in 1:nrow(site_treatment)) {
  site <- site_treatment[i, "Site"]
  treatment <- site_treatment[i, "Treatment"]
  
  # Subset data for this site and treatment
  subdata <- data[data$Site == site & data$Treatment == treatment, ]
  
  # Calculate slopes for CO2 and CH4
  CO2_slope <- calcFlux(subdata$Time, subdata$CO2)
  CH4_slope <- calcFlux(subdata$Time, subdata$CH4)  
  
  # Get unique units for volume and area (assuming they are consistent for a given site and treatment)
  V <- 0.000125 # using DOC column for volume as an example, replace with actual volume if available
  A <- 0.0016935 # using DOC_sd column for area as an example, replace with actual area if available
  
  # Calculate fluxes
  CO2_flux <- CO2_slope * V / A
  CH4_flux <- CH4_slope * V / A
  
  # Store results
  results <- rbind(results, data.frame(Site=site, Treatment=treatment,  
                                       CO2_Flux=CO2_flux,
                                       CH4_Flux=CH4_flux,
                                       stringsAsFactors=FALSE))
}

# Write results to CSV
write.csv(results, "fluxes_exudate_linear.csv", row.names=FALSE)

R300.exp<-expression(paste("C",O[2]," ","fluxes"," ","(",mu,"mol g"," ",soil^-1,m^-2,s^-1,")"))
R301.exp<-expression(paste("log (C",H[4]," ","fluxes"," ","(n","mol g"," ",soil^-1,m^-2,s^-1,")"))
R302.exp<-expression(paste("log (",N[2],"O ","fluxes"," ","(n","mol g"," ",soil^-1,m^-2,s^-1,"))"))

Flux243<-subset(ExudateChap41,Time>0 & Time<=24)
head(Flux243)
ExuTreat<-as.factor(Flux243$Treatment)
ggplot(Flux243,aes(x=Flux243$Site,y=log(1000*(Flux243$N2Oflux)),fill=ExuTreat))+geom_boxplot()+labs(x= "Wetland", y=R302.exp,fill="Exudate level")+scale_fill_manual(values=c("red","orange","blue","green"))+theme_bw()
ggplot(Flux243,aes(x=Flux243$Site,y=log(1000*Flux243$CH4flux),fill=ExuTreat))+geom_boxplot()+labs(x= "Wetland", y=R301.exp,fill="Exudate level")+scale_fill_manual(values=c("red", "orange","blue","green"))+theme_bw()+ylim(-6,3)
ggplot(Flux243,aes(x=Flux243$Site,y=0.25*Flux243$CO2flux,fill=ExuTreat))+geom_boxplot()+labs(x= "Wetland", y=R300.exp,fill="Exudate level")+scale_fill_manual(values=c("red", "orange","blue","green"))+theme_bw()+ylim(0,0.4)+stat_summary(fun.y=mean,geom="point",color="red")













































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































