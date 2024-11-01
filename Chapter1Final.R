R111.exp<-expression(paste("N",O[3]^-" ", "concentrations (ppm)"))
R121.exp<-expression(paste("N",H[4]^+" ", "concentrations (ppm)"))
R13.exp<-expression(paste("S",O[4]^2^-" ", "concentrations (ppm)"))
R4.exp<-expression(paste("F",e^3^+" ", "concentrations (ppm)"))
R41.exp<-expression(paste("F",e^2^+" ", "concentrations (ppm)"))
R5.exp<-expression(paste("C",a^2^+" ", "concentrations (ppm)"))
R6.exp<-expression(paste("M",g^2^+" ", "concentrations (ppm)"))
R7.exp<-expression(paste("C",l^-" ", "concentrations (ppm)"))
R8.exp<-expression(paste("C",a^2+" ","(",mu,"g/g)"))
R11.exp<-expression(paste("C",a^2^+" ","(",mu,"g/g",")"))
R12.exp<-expression(paste("M",g^2^+" ","(",mu,"g/g",")"))
R13.exp<-expression(paste(" Extractable Zn"," ","(",mu,"g/g",")"))
R14.exp<-expression(paste(" Extractable Cu"," ","(",mu,"g/g",")"))
R15.exp<-expression(paste("S",O[4]^2^-" ","(",mu,"g/g",")"))
R16.exp<-expression(paste("N",O[3]^-" ","(",mu,"g/g",")"))
R100.exp<-expression(paste("DOC production rates"," "," ", "(",mu,"gC " ,cm^-3," ",h^-1,")"))
R101.exp<-expression(paste("Fe(II) production rates"," "," ", "(nmol"," " ,cm^-3," ",h^-1,")"))
R102.exp<-expression(paste("Methane production rates"," "," ", "(",mu,"gC " ,cm^-3," ",h^-1,")"))
R104.exp<-expression(paste("DIC production rates"," "," ", "(",mu,"gC " ,cm^-3," ",h^-1,")"))
R30.exp<-expression(paste(N[2],"O"," ", "(ppb)"))



#Slurry experiments
#Multivariate on batch data
library(corrplot)
library(vegan)
attach(SlurryExp)
as.factor(Site)
Response<-cbind(CO2,CH4,N2O,FeII,NH4,NO3,SO4,DOC)
Response1<-cbind(FeII_release_rate,DOC_release_rate,DIC_release.rate,CH4_release_rate)
head(Response)
decostand(Response,"range",na.rm=FALSE)
decostand(Response1,"range",na.rm=FALSE)
test<-cor.mtest(Response,conf.level= 0.9)
Cor1<-cor(as.matrix(Response),method="pearson",use = "complete.obs")
CorMat<-corrplot(Cor1,"upper",p.mat = test$p,sig.level = 0.1,method = "number",na.rm=TRUE,insig = "blank")
CorMat

# Install and load the required package
install.packages("vegan")
library(vegan)

# Read the CSV data
data <- SlurryExp

# Select the variables of interest
variables <- c("CO2", "CH4", "N2O", "FeII", "NH4", "NO3", "SO4", "DOC")
selected_data <- data[, variables]

# Perform NMDS
Chemnmds <- metaMDS(selected_data, distance = "bray")
plot(Chemnmds,display = "sites")
ordiellipse(Chemnmds,data$Site,col=c("orange","gold","red","forest green"),draw="polygon")

# Extract the NMDS scores
scores <- as.data.frame(scores(Chemnmds))
NMDS1<-scores$sites.NMDS1
NMDS2<-scores$sites.NMDS2
# Add the wetland type to the scores data frame
scores$Wetland <- data$Site
# Create the NMDS plot
nmds_plot <- ggplot(scores, aes(x = NMDS1, y = NMDS2, color = Wetland)) +
  geom_point(size = 3) +
  labs(title = "NMDS Plot", x = "NMDS1", y = "NMDS2") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

nmds_plot





ChemRes<-decostand(Response,"range",na.rm=TRUE)
library(missMDA)
ResPrin<-princomp(Cor1)
summary(ResPrin)
ggbiplot(ResPrin)
library(ggbiplot)

#PCA map of slurry parameters
library(missMDA)
nb <- estim_ncpPCA(Response,method.cv = "Kfold", verbose = FALSE)
res.comp <- imputePCA(Response, ncp = nb$ncp)
head(res.comp)
Mat<-cbind.data.frame(res.comp[,1:8])
head(Mat)
res.pca <- PCA(Mat, ncp = nb$ncp, graph=FALSE)
plot(res.pca,choix= "var")
as.factor(Site)

as.factor(Site)
droplevels(Site)

SlurryEarlExp<-subset(SlurryExp, Time< 24)
SlurryEarlExp

a<-ggplot(SlurryExp,aes(x=Time,y=CO2))+geom_point(size=2.5,aes(color=Site,alpha=Depth),pch=19)+geom_errorbar(aes(ymin=CO2-CO2sd,ymax=CO2+CO2sd),width=0.5,color="black")+labs(x= "Time (hr)", y=R1.exp)+theme_bw()+ylim(0,7500)
a<-a +scale_alpha_discrete(range=c(0.25,1))+scale_color_manual(values=c("red","orange","gold","forest green"))
a<-a+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
a1<-ggplot(SlurryEarlExp,aes(x=Time,y=CO2,color=Site,alpha=Depth))+geom_point(size=2.5)+geom_errorbar(aes(ymin=CO2-CO2sd,ymax=CO2+CO2sd),width=0,color="black")+labs(x="Time (hr)",y=R1.exp)+theme_bw()
a1<-a1+ scale_alpha_discrete(range=c(0.25,1))+scale_color_manual(values=c("red","orange","gold","forest green"))
a1<-a1+theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank())
a+inset_element(a1,0.4,0.5,0.95,0.95)

b<-ggplot(SlurryExp,aes(x=Time,y=CH4,color=Site,alpha=Depth))+geom_point(size=2.5)+geom_errorbar(aes(ymin=CH4-CH4sd,ymax=CH4+CH4sd),width=0.3,color="black")+labs(x= "Time (hr)", y=R2.exp)+theme_bw()+ylim(0,40)
b<-b+scale_alpha_discrete(range=c(0.25,1))+scale_color_manual(values=c("red","orange","gold","forest green"))
b<-b+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
b
b1<-ggplot(SlurryEarlExp,aes(x=Time,y=CH4,color=Site,alpha=Depth))+geom_point(size=2.5)+geom_errorbar(aes(ymin=CH4-CH4sd,ymax=CH4+CH4sd),width=0,color="black")+labs(x="Time (hr)",y=R2.exp)+theme_bw()
b1<-b1+ scale_alpha_discrete(range=c(0.25,1))+scale_color_manual(values=c("red","orange","gold","forest green"))
b1<-b1+theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank())
b+inset_element(b1,0.4,0.5,0.95,0.95)


c<-ggplot(SlurryExp,aes(x=Time,y=N2O,color=Site,alpha=Depth))+geom_point(size=2.5)+geom_errorbar(aes(ymin=N2O-N2Osd,ymax=N2O+N2Osd,width=0.3,color="black"))+labs(x= "Time (hr)", y=R3.exp)+theme_bw()
c<-c+scale_alpha_discrete(range=c(0.25,1))+scale_color_manual(values=c("white","red","orange","gold","forest green"))
c<-c+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
c
c1<-ggplot(SlurryEarlExp,aes(x=Time,y=N2O,color=Site,alpha=Depth))+geom_point(size=2.5)+geom_errorbar(aes(ymin=N2O-N2Osd,ymax=N2O+N2Osd),width=0,color="black")+labs(x="Time (hr)",y=R3.exp)+theme_bw()
c1<-c1+ scale_alpha_discrete(range=c(0.25,1))+scale_color_manual(values=c("red","orange","gold","forest green"))
c1<-c1+theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank())
c+inset_element(c1,0.4,0.5,0.95,0.95)
a+b+c&theme(legend.position="right")

ggplot(SlurryExp,aes(x=Time..hr.,y=DOC,color=Site,shape=Depth..cm.))+geom_point(size=2.5)+geom_errorbar(aes(ymin=DOC-DOC.sd,ymax=DOC+DOC.sd),width=0.3,color="black")+labs(x= "Time (hr)", y="DOC (mg/L)",fill="Site")+scale_color_manual(values=c("gold", "red", "orange","forest green"))+theme_bw()+theme(legend.position=c(.90,.65))
ggplot(SlurryExp,aes(x=Time..hr.,y=FeII,color=Site,shape=Depth..cm.))+geom_point(size=1.75)+geom_errorbar(aes(ymin=FeII-Fesd,ymax=FeII+Fesd),width=0.3,color="black")+labs(x= "Time (hr)", y=R41.exp,fill="Site")+scale_color_manual(values=c("gold", "red", "orange","forest green"))+theme_bw()+theme(legend.position=c(.90,.65))
ggplot(SlurryExp,aes(x=Time..hr.,y=NH4,color=Site,shape=Depth..cm.))+geom_point(size=1.75)+geom_errorbar(aes(ymin=NH4-NH4..sd.,ymax=NH4+NH4..sd.),width=0.3,color="black")+labs(x= "Time (hr)", y=R121.exp,fill="Site")+scale_color_manual(values=c("gold", "red", "orange","forest green"))+theme_bw()+theme(legend.position=c(.90,.65))
ggplot(SlurryExp,aes(x=Time..hr.,y=NO3,color=Site,shape=Depth..cm.))+geom_point(size=1.75)+geom_errorbar(aes(ymin=NO3-NO3sd,ymax=NO3+NO3sd),width=0.3,color="black")+labs(x= "Time (hr)", y=R111.exp,fill="Site")+scale_color_manual(values=c("gold", "red", "orange","forest green"))+theme_bw()
ggplot(SlurryExp,aes(x=Time..hr.,y=SO4,color=Site,shape=Depth..cm.))+geom_point(size=1.75)+geom_errorbar(aes(ymin=SO4-SO4sd,ymax=SO4+SO4sd),width=0.3,color="black")+labs(x= "Time (hr)", y=R13.exp,fill="Site")+scale_color_manual(values=c("gold", "red", "orange","forest green"))+theme_bw()


cor.test(DOC..mg.L.,FeII..ppm.)



#FTR data (latest1)
library(ggplot2)
attach(FTRData)
boxplot(FeII_release_rate~Temperature_degreesC)
boxplot(DOC_release_rate~Temperature_degreesC)
boxplot(FeII_release_rate~Wetland)
boxplot(DOC_release_rate~Wetland)

boxplot(DOC_release_rate~Depth)
boxplot(FeII_release_rate~Depth)
plot(FeII_release_rate,DOC_release_rate)
cor.test(FeII_release_rate,DOC_release_rate)
summary(lm(DOC_release_rate~FeII_release_rate))

attach(FTRData)
z=paste(Wetland, Depth)
as.factor(z)
Temp<-as.factor(Shallow$Temperature_degreesC)
Temp1<-as.factor(Deep$Temperature_degreesC)
Co2

library(scales)
library(grid)
Shallow<-subset(FTRData,Depth=="0-2")
Deep<-subset(FTRData,Depth=="15-17")

a<-ggplot(Shallow,aes(x=Shallow$Wetland,y=Shallow$FeII_release_rate,fill=Temp))+geom_boxplot()+labs(x= "Site", y=R101.exp,fill="Temperature (Celsius)")+theme_bw()+scale_fill_manual(values=c("blue","forest green","red"))+ylim(0,30)+annotate("text",y=30,x=1,"Unburnt",label="a) Shallow")
b<-ggplot(Deep,aes(x=Deep$Wetland,y=Deep$FeII_release_rate,fill=Temp1))+geom_boxplot()+labs(x= "Site", y="",fill="Temperature (Celsius)")+theme_bw()+scale_fill_manual(values=c("blue","forest green","red"),guide=FALSE)+ylim(0,30)+annotate("text",y=30,x=1,"Unburnt",label="b) Deep")
a<-a+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
b<-b+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
a+b&theme(legend.position = "top")


c<-ggplot(Shallow,aes(x=Shallow$Wetland,y=Shallow$DOC_release_rate,fill=Temp))+geom_boxplot()+labs(x= "Site", y=R100.exp,fill="Temperature (Celsius)")+theme_bw()+scale_fill_manual(values=c("blue","forest green","red"))+ylim(0,2.5)+annotate("text",y=3,x=1,"Unburnt",label="c) Shallow")+ylim(0,3)
d<-ggplot(Deep,aes(x=Deep$Wetland,y=Deep$DOC_release_rate,fill=Temp1))+geom_boxplot()+labs(x= "Site", y="",fill="Temperature (Celsius)")+theme_bw()+scale_fill_manual(values=c("blue","forest green","red"),guide=FALSE)+ylim(0,2.5)+annotate("text",y=3,x=1,"Unburnt",label="d) Deep")+ylim(0,3)
c<-c+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
d<-d+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
c+d&theme(legend.position = "top")

max(DIC_release_rate)
e<-ggplot(Shallow,aes(x=Shallow$Wetland,y=Shallow$DIC_release_rate,fill=Temp))+geom_boxplot()+labs(x= "Site", y=R104.exp,fill="Temperature (Celsius)")+theme_bw()+scale_fill_manual(values=c("blue","forest green","red"))+ylim(0,4)+annotate("text",y=5,x=1,"Unburnt",label="e) Shallow")+ylim(0,5)
f<-ggplot(Deep,aes(x=Deep$Wetland,y=Deep$DIC_release_rate,fill=Temp1))+geom_boxplot()+labs(x= "Site", y="",fill="Temperature (Celsius)")+theme_bw()+scale_fill_manual(values=c("blue","forest green","red"),guide=FALSE)+ylim(0,4)+annotate("text",y=5,x=1,"Unburnt",label="f) Deep")+ylim(0,5)
e<-e+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
f<-f+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
e+f&theme(legend.position = "top")

g<-ggplot(Shallow,aes(x=Shallow$Wetland,y=Shallow$CH4_release_rate,fill=Temp))+geom_boxplot()+labs(x= "Site", y=R102.exp,fill="Temperature (Celsius)")+theme_bw()+scale_fill_manual(values=c("blue","forest green","red"))+ylim(0,0.1)+annotate("text",y=.1,x=1,"Unburnt",label="g) Shallow")+ylim(0.01,0.1)
h<-ggplot(Deep,aes(x=Deep$Wetland,y=Deep$CH4_release_rate,fill=Temp1))+geom_boxplot()+labs(x= "Site", y="",fill="Temperature (Celsius)")+theme_bw()+scale_fill_manual(values=c("blue","forest green","red"),guide=FALSE)+ylim(0,0.1)+annotate("text",y=.1,x=1,"Unburnt",label="h) Deep")+ylim(0.01,0.1)
g<-g+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
h<-h+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
g+h&theme(legend.position = "top")

ggplot(FTRData,aes(x=Wetland,y=DOC_release_rate,fill=Temp))+geom_boxplot()+labs(x= "Wetland type", y=R100.exp,fill="Temperature (Celsius)")+theme_bw()+scale_fill_manual(values=c("blue","forest green","red"))+theme(legend.position=c(.85,.75))
ggplot(FTRData,aes(x=Wetland,y=DIC_release.rate,fill=Temp))+geom_boxplot()+labs(x= "Wetland type", y=R104.exp,fill="Temperature (Celsius)")+theme_bw()+scale_fill_manual(values=c("blue","forest green","red"))
ggplot(FTRData,aes(x=Wetland,y=CH4_release_rate,fill=Temp))+geom_boxplot()+labs(x= "Wetland type", y=R102.exp,fill="Temperature (Celsius)")+theme_bw()+scale_fill_manual(values=c("blue","forest green","red"))
ggplot(FTRData,aes(x=FeII_release_rate,y=DOC_release_rate,color=Wetland,shape=Depth))+geom_point(size=2.5)+labs(x=R0.exp,y=R11.exp)+scale_color_manual(values=c("gold", "red", "orange","forest green"))+theme_bw()+theme(legend.position=c(.90,.65))
ggplot(FTRData,aes(x=FeII_release_rate,y=DOC_release_rate,color=Temp,shape=Depth))+geom_point(size=2.5)+labs(x=R0.exp,y=R11.exp)+scale_color_manual(values=c("blue", "forest green", "red","forest green"))+theme_bw()+theme(legend.position=c(.90,.65))


UnburntFe<-FeII_release_rate[which(Wetland=="Unburnt")]
UnburntDOC<-DOC_release_rate[which(Wetland=="Unburnt")]
summary(lm(UnburntDOC~UnburntFe))

BurntFe<-FeII_release_rate[which(Wetland!="Unburnt")]
BurntDOC<-DOC_release_rate[which(Wetland!="Unburnt")]
summary(lm(BurntDOC~BurntFe))

plot(FeII_release_rate[which(Wetland=="Unburnt")],DOC_release_rate[which(Wetland=="Unburnt")],pch=19,xlab=R101.exp,ylab=R100.exp,col="forest green")
points(FeII_release_rate[which(Wetland!="Unburnt")],DOC_release_rate[which(Wetland!="Unburnt")],pch=17,col="orange")
abline(a=0.13742,b=0.05725,col="orange")
abline(a=0.17075,b=0.04125,col="forest green")
R106.exp<-expression(paste("",R^2,"=0.2987"))
R107.exp<-expression(paste("",R^2,"=0.2031"))
text(7,0.1,R106.exp,col="forest green")
text(7,0.05,R107.exp,col="orange")

library(missMDA)
ResPrin<-princomp(Cor1)
summary(ResPrin)
ggbiplot(ResPrin)
library(ggbiplot)

#PCA map of slurry parameters
library(missMDA)
nb1 <- estim_ncpPCA(Response1,method.cv = "Kfold", verbose = FALSE)
res.comp1 <- imputePCA(Response1, ncp = nb1$ncp)
head(res.comp1)
Mat1<-cbind.data.frame(res.comp1[,1:4])
head(Mat1)
res.pca1 <- PCA(Mat1, ncp = nb1$ncp, graph=FALSE)
plot(res.pca1,choix= "var")



# Install and load the required packages
install.packages("vegan")
install.packages("ggplot2")
library(vegan)
library(ggplot2)

# Read the CSV data
data <- read.csv("FTRData.csv")

# Select the variables of interest
variables <- c("DOC_release_rate", "FeII_release_rate", "DIC_release.rate", "CH4_release_rate")
selected_data <- data[, variables]

# Create a new column for the wetland type (Unburned or Burned)
data$Type <- ifelse(data$Wetland == "Unburnt", "Unburned", "Burned")

# Perform NMDS
FTRnmds <- metaMDS(selected_data, distance = "bray")
plot(FTRnmds,display = "sites")
ordiellipse(FTRnmds,data$Wetland,col=c("gold","red","orange","forest green"),draw="polygon")


# Extract the NMDS scores
scores <- as.data.frame(nmds$points)



# Display the plot
print(nmds_plot)


# Display the plot
print(nmds_plot)

lm(DOC_release_rate[which(Wetland=="Unburnt")],FeII_release_rate[which(Wetland=="Unburnt")])
cor.test(FeII_release_rate[which(Wetland!="Unburnt")],DOC_release_rate[which(Wetland!="Unburnt")])


FeII_release_rate[which(Wetland=="Unburnt")



#Fe-DOC-DTN story

colnames(Soils.dataset)
Chem<-Complete.dataset[10:21]
SoilChem<-cbind(pH, X.N,X.C,C.N.ratio,Ca..ug.gs.,Mg..ug.gs.,Feo..mg.gs.,Mnex..mg.gs.,Znex..ug.gs.,Cuex..ug.gs.,Sulfate..ug.gs.,Nitrate..ug.gs.)
head(SoilChem)
??cor.mtest
library(corrplot)
test<-cor.mtest(SoilChem,conf.level= 0.9)
Cor1<-cor(as.matrix(SoilChem),method="pearson",use = "complete.obs")
corrplot(Cor1,"upper",p.mat = test$p,sig.level = 0.1,method = "number",na.rm=TRUE,insig = "blank")

library(vegan)
SoilChemRes<-decostand(SoilChem,"range",na.rm=TRUE)
library(missMDA)
nb <- estim_ncpPCA(SoilChemRes,method.cv = "Kfold", verbose = FALSE)
res.comp <- imputePCA(SoilChemRes, ncp = nb$ncp)
res.comp
Mat<-cbind.data.frame(res.comp)
res.pca <- PCA(Mat, quanti.sup = 1, quali.sup = 12, ncp = nb$ncp, graph=FALSE)
plot(res.pca,choix= "var")

#Calculating temperature sensitivity
# Read in data from CSV 
data <- FTRData

# Function to calculate Q10
calcQ10 <- function(rate_high, rate_low, temp_high, temp_low) {
  (rate_high / rate_low)^(10 / (temp_high - temp_low))
}

# Get unique wetland, site, depth combinations
combos <- unique(data[, c("Wetland", "Site", "Depth")])

# Loop through each unique combination
for (i in 1:nrow(combos)) {
  wetland <- combos[i, "Wetland"] 
  site <- combos[i, "Site"]
  depth <- combos[i, "Depth"]
  
  # Subset data for this combination
  subset <- data[data$Wetland == wetland & data$Site == site & data$Depth == depth, ]
  
  # Calculate Q10 for each pairwise temperature comparison
  for (rate in c("FeII_release_rate", "DOC_release_rate", "DIC_release_rate", "CH4_release_rate")) {
    
    rate9 <- mean(subset[subset$Temperature_degreesC == 9, rate])
    rate18 <- mean(subset[subset$Temperature_degreesC == 18, rate]) 
    rate27 <- mean(subset[subset$Temperature_degreesC == 27, rate])
    
    q10_9_18 <- calcQ10(rate18, rate9, 18, 9)
    q10_18_27 <- calcQ10(rate27, rate18, 27, 18)
    q10_9_27 <- calcQ10(rate27, rate9, 27, 9)
    
    cat(paste("Q10 for", rate, "at", wetland, site, depth, ":\n"))
    cat(paste("9 to 18 degrees: ", round(q10_9_18, 2), "\n"))
    cat(paste("18 to 27 degrees:", round(q10_18_27, 2), "\n")) 
    cat(paste("9 to 27 degrees: ", round(q10_9_27, 2), "\n\n"))
  }
}

#Print as a csv file alternative code
# Read in data from CSV 
data <- FTRData

# Function to calculate Q10
calcQ10 <- function(rate_high, rate_low, temp_high, temp_low) {
  (rate_high / rate_low)^(10 / (temp_high - temp_low))
}

# Create a data frame to store the results
results <- data.frame(Wetland = character(),
                      Site = character(), 
                      Depth = character(),
                      Rate = character(),
                      Q10_9_18 = numeric(),
                      Q10_18_27 = numeric(),
                      Q10_9_27 = numeric(),
                      stringsAsFactors = FALSE)

# Get unique wetland sites
sites <- unique(data[, c("Wetland", "Site")])

# Loop through each unique site
for (i in 1:nrow(sites)) {
  wetland <- sites[i, "Wetland"] 
  site <- sites[i, "Site"]
  
  # Subset data for this site
  subset <- data[data$Wetland == wetland & data$Site == site, ]
  
  # Get unique depths for this site
  depths <- unique(subset$Depth)
  
  # Loop through each depth
  for (depth in depths) {
    
    # Subset data for this depth
    subsubset <- subset[subset$Depth == depth, ]
    
    # Calculate Q10 for each pairwise temperature comparison
    for (rate in c("FeII_release_rate", "DOC_release_rate", "DIC_release_rate", "CH4_release_rate")) {
      
      rate9 <- mean(subsubset[subsubset$Temperature_degreesC == 9, rate])
      rate18 <- mean(subsubset[subsubset$Temperature_degreesC == 18, rate]) 
      rate27 <- mean(subsubset[subsubset$Temperature_degreesC == 27, rate])
      
      q10_9_18 <- calcQ10(rate18, rate9, 18, 9)
      q10_18_27 <- calcQ10(rate27, rate18, 27, 18)
      q10_9_27 <- calcQ10(rate27, rate9, 27, 9)
      
      # Add results to the data frame
      results <- rbind(results, data.frame(Wetland = wetland,
                                           Site = site,
                                           Depth = depth,
                                           Rate = rate,
                                           Q10_9_18 = q10_9_18,
                                           Q10_18_27 = q10_18_27,
                                           Q10_9_27 = q10_9_27,
                                           stringsAsFactors = FALSE))
    }
  }
}

# Write the results to a CSV file
write.csv(results, "q10_results.csv", row.names = FALSE)


# Load necessary libraries
library(tidyverse)
library(broom)

# Read the data (assuming it's saved as a CSV file named "wetland_data.csv")
data <- FTRData

# Convert temperature to Kelvin
data$Temperature_K <- data$Temperature_degreesC + 273.15

# Function to calculate Ea using linear regression
calculate_ea_slope <- function(rates, temps) {
  df <- data.frame(ln_rate = log(rates), inv_temp = 1/temps)
  model <- lm(ln_rate ~ inv_temp, data = df)
  slope <- coef(model)[2]
  R <- 8.314  # Gas constant in J/(mol*K)
  Ea <- -slope * R
  return(Ea)
}

# Reaction types
reaction_types <- c("FeII_release_rate", "DOC_release_rate", "DIC_release_rate", "CH4_release_rate")

# Calculate mean rates and Ea for each group
results <- data %>%
  group_by(Wetland, Site, Depth, Temperature_K) %>%
  summarize(across(all_of(reaction_types), mean, .names = "mean_{.col}")) %>%
  group_by(Wetland, Site, Depth) %>%
  summarize(across(starts_with("mean_"), 
                   ~ calculate_ea_slope(.x, Temperature_K),
                   .names = "Ea_{.col}"))

# Clean up column names
results <- results %>%
  rename_with(~str_replace(., "Ea_mean_", "Ea_"), starts_with("Ea_"))

# View results
print(results)
write.csv(results, "Ea_results.csv", row.names = FALSE)


# Optional: Calculate R-squared values for each linear regression
r_squared <- data %>%
  group_by(Wetland, Site, Depth) %>%
  summarize(across(all_of(reaction_types), 
                   ~ summary(lm(log(.x) ~ I(1/Temperature_K)))$r.squared,
                   .names = "R_squared_{.col}"))

# Combine results and R-squared values
final_results <- left_join(results, r_squared, by = c("Wetland", "Site", "Depth"))

# View final results
print(final_results)

 
library(ggplot2)
attach(SlurryExp)
R300.exp<-expression(paste("C",O[2]," ","fluxes"," ","(",mu,"mol g"," ",soil^-1,m^-2,s^-1,")"))
R301.exp<-expression(paste("C",H[4]," ","fluxes"," ","(n","mol g"," ",soil^-1,m^-2,s^-1,")"))
R302.exp<-expression(paste("log (",N[2],"O ","fluxes"," ","(n","mol g"," ",soil^-1,m^-2,s^-1,"))"))


attach(FluxChap1)
df1<-cbind(Site,Depth,CO2fluxavg,Co2fsd)
df2<-cbind(Site,Depth,CO2finit)


library(ggplot2)
attach(SlurryExp)

Flux24<-subset(SlurryExp,Time>0 & Time<24)
head(Flux24)



min(1000*Flux24$N2Oflux)
log(N2O[53:64])
ggplot(Flux24,aes(x=Flux24$Site,y=log(1000*(Flux24$N2Oflux)),fill=Flux24$Depth))+geom_boxplot()+labs(x= "Site", y=R302.exp,fill="Depth")+scale_fill_manual(values=c("green", "brown"))+theme_bw()
ggplot(Flux24,aes(x=Flux24$Site,y=0.25*Flux24$CH4flux,fill=Flux24$Depth))+geom_boxplot()+labs(x= "Site", y=R301.exp,fill="Depth")+scale_fill_manual(values=c("green", "brown"))+theme_bw()+ylim(0,0.05)
ggplot(Flux24,aes(x=Flux24$Site,y=0.25*Flux24$CO2flux,fill=Flux24$Depth))+geom_boxplot()+labs(x= "Site", y=R300.exp,fill="Depth")+scale_fill_manual(values=c("green", "brown"))+theme_bw()


#Peeper data
# Assuming your data is in a dataframe called 'data'
data<-Peeper.data
# Extract relevant columns
DOC <- data$DOC
DON <- data$DON 
CNdis <- data$C.Ndis  # Use . instead of / for column name
Fe3 <- data$Fe3
NO3 <- data$NO3
SO4 <- data$SO4
Ca <- data$Ca
Mg <- data$Mg
NH4 <- data$NH4

# Set up 3x3 grid plot
par(mfrow=c(3,3))

# Create individual plots
# Assuming your data is in a dataframe called 'data'

# Extract relevant columns
DOC <- data$DOC
DON <- data$DON 
CNdis <- data$C.Ndis  # Use . instead of / for column name
Fe3 <- data$Fe3
NO3 <- data$NO3
SO4 <- data$SO4
Ca <- data$Ca
Mg <- data$Mg
NH4 <- data$NH4

# Set up 3x3 grid plot
par(mfrow=c(2,3))

# Create individual plots
plot(DOC[1:9], data$Depth..cm.[1:9],ylim=c(24,0),ylab="Depth (cm)")


# Load required libraries
library(dplyr)
library(tidyr)

# Read the data
data <- FTRData

# Load required libraries
library(dplyr)
library(tidyr)

# Read the data
data <- read.csv("FTRData.csv")

# Function to calculate Q10
calculate_Q10 <- function(temp, rate) {
  if (length(unique(temp)) < 2 || length(unique(rate)) < 2) {
    return(NA)
  }
  tryCatch({
    model <- lm(log10(rate) ~ temp)
    slope <- coef(model)[2]
    Q10 <- 10^(10 * slope)
    return(Q10)
  }, error = function(e) {
    return(NA)
  })
}

# Calculate Q10 for each combination
Q10_results <- data %>%
  group_by(Wetland, Site, Depth) %>%
  summarise(
    Q10_FeII = calculate_Q10(Temperature_degreesC, FeII_release_rate),
    Q10_DOC = calculate_Q10(Temperature_degreesC, DOC_release_rate),
    Q10_DIC = calculate_Q10(Temperature_degreesC, DIC_release_rate),
    Q10_CH4 = calculate_Q10(Temperature_degreesC, CH4_release_rate)
  ) %>%
  ungroup()

# Display results
print(Q10_results)
write.csv(Q10_results, "Q10_results.csv", row.names = FALSE)

attach(Peeper.data)
par(mfrow=c(4,2))
par(xpd=NA)
plot(Fe3[1:9], Depth..cm.[1:9],ylim=c(24,0),ylab="Depth (cm)",xlab=R4.exp,pch=19,col="red",xlim=c(0,4),cex=1.5)
points(Fe3[10:18], Depth..cm.[10:18],ylim=c(24,0),ylab="Depth (cm)",xlab=R4.exp,pch=19,col="orange",xlim=c(0,4),cex=1.5)
points(Fe3[19:27], Depth..cm.[19:27],ylim=c(24,0),ylab="Depth (cm)",xlab=R4.exp,pch=19,col="gold",xlim=c(0,4),cex=1.5)
points(Fe3[28:36], Depth..cm.[28:36],ylim=c(24,0),ylab="Depth (cm)",xlab=R4.exp,pch=19,col="forest green",xlim=c(0,4),cex=1.5)

plot(NO3[1:9], Depth..cm.[1:9],ylim=c(24,0),ylab="Depth (cm)",xlab=R111.exp,pch=19,col="red",xlim=c(0,2),cex=1.5)
points(NO3[10:18], Depth..cm.[10:18],ylim=c(24,0),ylab="Depth (cm)",xlab=R111.exp,pch=19,col="orange",xlim=c(0,2),cex=1.5)
points(NO3[19:27], Depth..cm.[19:27],ylim=c(24,0),ylab="Depth (cm)",xlab=R111.exp,pch=19,col="gold",xlim=c(0,2),cex=1.5)
points(NO3[28:36], Depth..cm.[28:36],ylim=c(24,0),ylab="Depth (cm)",xlab=R111.exp,pch=19,col="forest green",xlim=c(0,2),cex=1.5)

plot(SO4[1:9], Depth..cm.[1:9],ylim=c(24,0),ylab="Depth (cm)",xlab=R13.exp,pch=19,col="red",xlim=c(0,4),cex=1.5)
points(SO4[10:18], Depth..cm.[10:18],ylim=c(24,0),ylab="Depth (cm)",xlab=R13.exp,pch=19,col="orange",xlim=c(0,4),cex=1.5)
points(SO4[19:27], Depth..cm.[19:27],ylim=c(24,0),ylab="Depth (cm)",xlab=R13.exp,pch=19,col="gold",xlim=c(0,4),cex=1.5)
points(SO4[28:36], Depth..cm.[28:36],ylim=c(24,0),ylab="Depth (cm)",xlab=R13.exp,pch=19,col="forest green",xlim=c(0,4),cex=1.5)

plot(DOC[1:9], Depth..cm.[1:9],ylim=c(24,0),ylab="Depth (cm)",xlab="DOC concentrations (ppm)",pch=19,col="red",xlim=c(0,200),cex=1.5)
points(DOC[10:18], Depth..cm.[10:18],ylim=c(24,0),ylab="Depth (cm)",xlab="DOC concentrations (ppm)",pch=19,col="orange",xlim=c(0,200),cex=1.5)
points(DOC[19:27], Depth..cm.[19:27],ylim=c(24,0),ylab="Depth (cm)",xlab="DOC concentrations (ppm)",pch=19,col="gold",xlim=c(0,200),cex=1.5)
points(DOC[28:36], Depth..cm.[28:36],ylim=c(24,0),ylab="Depth (cm)",xlab="DOC concentrations (ppm)",pch=19,col="forest green",xlim=c(0,200),cex=1.5)

plot(pH[1:9], Depth..cm.[1:9],ylim=c(24,0),ylab="Depth (cm)",xlab="pH",pch=19,col="red",xlim=c(6,8),cex=1.5)
points(pH[10:18], Depth..cm.[10:18],ylim=c(24,0),ylab="Depth (cm)",xlab="pH",pch=19,col="orange",xlim=c(6,8),cex=1.5)
points(pH[19:27], Depth..cm.[19:27],ylim=c(24,0),ylab="Depth (cm)",xlab="pH",pch=19,col="gold",xlim=c(6,8),cex=1.5)
points(pH[28:36], Depth..cm.[28:36],ylim=c(24,0),ylab="Depth (cm)",xlab="pH",pch=19,col="forest green",xlim=c(6,8),cex=1.5)

plot(Ca[1:9], Depth..cm.[1:9],ylim=c(24,0),ylab="Depth (cm)",xlab=R5.exp,xlim=c(0,80),pch=19,col="red",cex=1.5)
points(Ca[10:18], Depth..cm.[10:18],ylim=c(24,0),ylab="Depth (cm)",xlab="Ca",pch=19,col="orange",xlim=c(6,8),cex=1.5)
points(Ca[19:27], Depth..cm.[19:27],ylim=c(24,0),ylab="Depth (cm)",xlab="Ca",pch=19,col="gold",xlim=c(6,8),cex=1.5)
points(Ca[28:36], Depth..cm.[28:36],ylim=c(24,0),ylab="Depth (cm)",xlab="Ca",pch=19,col="forest green",xlim=c(6,8),cex=1.5)

plot(Mg[1:9], Depth..cm.[1:9],ylim=c(24,0),ylab="Depth (cm)",xlab=R6.exp,xlim=c(0,10),pch=19,col="red",cex=1.5)
points(Mg[10:18], Depth..cm.[10:18],ylim=c(24,0),ylab="Depth (cm)",xlab="Mg",pch=19,col="orange",xlim=c(6,8),cex=1.5)
points(Mg[19:27], Depth..cm.[19:27],ylim=c(24,0),ylab="Depth (cm)",xlab="Mg",pch=19,col="gold",xlim=c(6,8),cex=1.5)
points(Mg[28:36], Depth..cm.[28:36],ylim=c(24,0),ylab="Depth (cm)",xlab="Mg",pch=19,col="forest green",xlim=c(6,8),cex=1.5)

