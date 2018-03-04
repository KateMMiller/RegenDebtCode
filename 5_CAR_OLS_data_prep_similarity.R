#--------------------------------------------
# Spatial Regression on beta diversity
#--------------------------------------------
setwd('C:/temp/GIS')
library(rgdal)
library(raster)
#library(sp)
#library(maptools)
library(dplyr)
options("scipen"=100, "digits"=4)
rasterOptions(timer=TRUE,progress='text', tmpdir='G:/raster_temp') #shows a timer for processes run from raster package

file=('C:/temp/GIS')

#-----------------------------------
# Relate response shapefile point locations to explanatory variables 
#-----------------------------------
# Seedling/canopy similarity
seedbeta<-readOGR(dsn=file,layer="Tree_seedling_stems_m2_beta")
seeds.bdf<-data.frame(seedbeta)[,-9]
names(seeds.bdf)

# read in raster and shapefiles to join data with
prec<-raster('Pct_Change_Precip.tif') # raster files not kernel smoothed
tmax<-raster('Pct_Change_Tmax.tif')
deer.den<-readOGR(dsn=file, layer='deer_density_UTMalbers')
deer.den$deer.index<-ifelse(deer.den$deer_dnsty=="red",4,ifelse(deer.den$deer_dnsty=="orange",3,ifelse(deer.den$deer_dnsty=="yellow",2,ifelse(deer.den$deer_dnsty=="green",1,NA))))
deer.sm<-raster('Browse_Impact_kern10k.tif')
inv.sm<-raster('Invasive_Cover_kern10k.tif')
hmod.lc<-raster('nlcd300_utm_clip.tif')
plots<-readOGR(dsn=file, layer="FIA_plots_for_analysis")
relabund.tr<-readOGR(dsn=file, layer="relabund_trees")
relabund.sd<-readOGR(dsn=file, layer="relabund_seedlings")
plots.df<-data.frame(plots)[,-13]
relab.tr.df<-data.frame(relabund.tr)[,-(17:19)] #remove coordinates and optional to keep from getting double copies in merged df
relab.sd.df<-data.frame(relabund.sd)[,-(17:19)] #remove coordinates and optional fields

colnames(relab.sd.df)<-c("PLT_CN","INVYR","ACERUB.sd","ACESAC.sd","CARYA.sd", "EARSUC.sd","FAGGRA.sd","FRAX.sd",
                         "INVASIVE.sd","LIRTUL.sd","OTHER_SPP.sd","PICEA.sd","PINUS.sd","QUERCUS.sd","SHORT.sd","ULMUS.sd")

# extract values of rasters for each point location in seeds and join point data
prec.ex<-extract(prec,seedbeta,method='bilinear')
tmax.ex<-extract(tmax,seedbeta,method='bilinear')
deer.sm.ex<-extract(deer.sm,seedbeta,method='bilinear')
inv.sm.ex<-extract(inv.sm,seedbeta,method='bilinear')
hmod.ex<-extract(hmod.lc,seedbeta,method='simple')
deer.ex<-sp::over(seedbeta, deer.den)

seeds2<-left_join(seeds.bdf,plots.df[,c("PLT_CN","INVYR","STDSZCD","CANOPY_")], by=c("PLT_CN"))
seeds3<-left_join(seeds2,relab.tr.df, by=c("PLT_CN","INVYR"))
seeds4<-left_join(seeds3,relab.sd.df, by=c("PLT_CN","INVYR"))

# combine datasets  
seeds5<-data.frame(seeds4,prec.ex,tmax.ex, deer.sm.ex, inv.sm.ex,hmod.ex, deer.ex[,2]) 
head(seeds5)
values<-c("train","test")
seeds5$type<-sample(values, nrow(seeds5), TRUE, prob = c(.95,.05)) # split data into training and testing dataset
table(seeds5$type)

coordinates(seeds5)<-~coords.x1+coords.x2
proj4string(seeds5)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
file<-'C:/temp/GIS'
writeOGR(seeds5, dsn=file, layer="Seedlings_m2_beta_with_predictors", driver="ESRI Shapefile", overwrite_layer=T)

#------------------------
# Seedling/canopy similarity
sapbeta<-readOGR(dsn=file,layer="Tree_sapling_stems_m2_beta")
bbox<-readOGR(dsn=file, layer="Bounding_Box")
saps.bdf<-data.frame(sapbeta)[,-9] #remove optional
names(saps.bdf)

# read in sapling relative abundance shapefile
relabund.sap<-readOGR(dsn=file, layer="relabund_saplings")
relab.sap.df<-data.frame(relabund.sap)[,-(17:19)]
colnames(relab.sap.df)<-c("PLT_CN","INVYR","ACERUB.sap","ACESAC.sap","CARYA.sap", "EARSUC.sap","FAGGRA.sap","FRAX.sap",
                         "INVASIVE.sap","LIRTUL.sap","OTHER_SPP.sap","PICEA.sap","PINUS.sap","QUERCUS.sap","SHORT.sap","ULMUS.sap")
names(relab.sap.df)

# extract values of rasters for each point location in seeds and join point data
prec.ex<-extract(prec,sapbeta,method='bilinear')
tmax.ex<-extract(tmax,sapbeta,method='bilinear')
deer.sm.ex<-extract(deer.sm,sapbeta,method='bilinear')
inv.sm.ex<-extract(inv.sm,sapbeta,method='bilinear')
hmod.ex<-extract(hmod.lc,sapbeta,method='simple')
deer.ex<-sp::over(sapbeta, deer.den)

saps2<-left_join(saps.bdf,plots.df[,c("PLT_CN","INVYR","STDSZCD","CANOPY_")], by=c("PLT_CN"))
saps3<-left_join(saps2,relab.tr.df, by=c("PLT_CN","INVYR"))
saps4<-left_join(saps3,relab.sap.df, by=c("PLT_CN","INVYR"))

# combine datasets  
saps5<-data.frame(saps4,prec.ex,tmax.ex, deer.sm.ex, inv.sm.ex,hmod.ex, deer.ex[,2]) 
values<-c("train","test")
saps5$type<-sample(values, nrow(saps5), TRUE, prob = c(.95,.05)) # split data into training and testing dataset
table(saps5$type)

coordinates(saps5)<-~coords.x1+coords.x2
proj4string(saps5)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
file<-'C:/temp/GIS'
writeOGR(saps5, dsn=file, layer="Saplings_m2_beta_with_predictors", driver="ESRI Shapefile", overwrite_layer=T)

#---------------------------------------
# Clip datasets to mid-Atlantic region for analysis
#---------------------------------------
# Regen without deer impacts
#---------------------------------------
# Clip seedling data to primary area of interest in analysis
seeds<-readOGR(dsn=file,layer="Seedlings_m2_beta_with_predictors")
bbox<-readOGR(dsn=file, layer="Bounding_Box")
seeds.c<-raster::crop(seeds,bbox, snap='in')
plot(seeds.c, col='blue', pch=16)
plot(bbox,add=T)
names(seeds.c)
#View(seeds2)

names(seeds.c)
seeds2<-seeds.c[,c("PLT_CN","INVYR","Sor","Hor","STDSZCD","CANOPY_","ACERUB","ACESAC","CARYA","EARSUC","FAGGRA","FRAX","INVASIVE","LIRTUL",
                   "OTHER_SPP","PICEA","PINUS","QUERCUS","SHORT","ULMUS","ACERUB_","ACESAC_", "CARYA_s","EARSUC_",
                   "FAGGRA_","FRAX_sd","INVASIVE_","LIRTUL_","OTHER_SPP_","PICEA_s","PINUS_s","QUERCUS_","SHORT_s","ULMUS_s",
                   "prec_ex","tmax_ex","inv_sm_","hmod_ex","d____2_","type" )]
names(seeds2)[names(seeds2)=="d____2_"]<-"deer_den"
seeds2$X<-seeds2$coords.x1
seeds2$Y<-seeds2$coords.x2
seeds3<-na.omit(seeds2@data)
nrow(seeds2);nrow(seeds3)
coordinates(seeds3)<-~X+Y
proj4string(seeds3)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeOGR(seeds3, dsn=file, layer='Seedlings_m2_beta_with_pred_clip',driver="ESRI Shapefile",overwrite_layer=T)
seeds.df<-as.data.frame(seeds3)
write.csv(seeds.df, "Seedlings_m2_beta_with_pred_df.csv")

# Clip sapling data to primary area of interest in analysis
saps<-readOGR(dsn=file,layer="Saplings_m2_beta_with_predictors")
bbox<-readOGR(dsn=file, layer="Bounding_Box")
saps.c<-raster::crop(saps,bbox, snap='in')
plot(saps.c, col='blue', pch=16)
plot(bbox,add=T)
names(saps.c)

saps2<-saps.c[,c("PLT_CN","INVYR","Sor","Hor","STDSZCD","CANOPY_","ACERUB","ACESAC","CARYA","EARSUC","FAGGRA","FRAX","INVASIVE","LIRTUL",
                   "OTHER_SPP","PICEA","PINUS","QUERCUS","SHORT","ULMUS","ACERUB_","ACESAC_", "CARYA_s","EARSUC_",
                   "FAGGRA_","FRAX_sp","INVASIVE_","LIRTUL_","OTHER_SPP_","PICEA_s","PINUS_s","QUERCUS_","SHORT_s","ULMUS_s",
                   "prec_ex","tmax_ex","inv_sm_","hmod_ex","d____2_","type" )]
names(saps2)[names(saps2)=="d____2_"]<-"deer_den"
saps2$X<-saps2$coords.x1
saps2$Y<-saps2$coords.x2
saps3<-na.omit(saps2@data)
nrow(saps2)-nrow(saps3) # only losing 23 records
coordinates(saps3)<-~X+Y
proj4string(saps3)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeOGR(saps3, dsn=file, layer='Saplings_m2_beta_with_pred_clip',driver="ESRI Shapefile",overwrite_layer=T)
saps.df<-as.data.frame(saps3)
write.csv(saps.df, "Saplings_m2_beta_with_pred_df.csv")

#---------------------------------------
# Regen with deer impacts
#---------------------------------------
# Clip seedling data to primary area of interest in analysis
seeds<-readOGR(dsn=file,layer="Seedlings_m2_beta_with_predictors")
bbox<-readOGR(dsn=file, layer="Bounding_Box")
seeds.c<-raster::crop(seeds,bbox, snap='in')
plot(seeds.c, col='blue', pch=16)
plot(bbox,add=T)
names(seeds.c)

names(seeds.c)
seeds2d<-seeds.c[,c("PLT_CN","INVYR","Sor","Hor","STDSZCD","CANOPY_","ACERUB","ACESAC","CARYA","EARSUC","FAGGRA","FRAX","INVASIVE","LIRTUL",
                    "OTHER_SPP","PICEA","PINUS","QUERCUS","SHORT","ULMUS","ACERUB_","ACESAC_", "CARYA_s","EARSUC_",
                    "FAGGRA_","FRAX_sd","INVASIVE_","LIRTUL_","OTHER_SPP_","PICEA_s","PINUS_s","QUERCUS_","SHORT_s","ULMUS_s",
                    "prec_ex","tmax_ex","inv_sm_","hmod_ex","type", "dr_sm_x", "d____2_")]
names(seeds2d)[names(seeds2d)=="d____2_"]<-"deer_den"
seeds2d$X<-seeds2d$coords.x1
seeds2d$Y<-seeds2d$coords.x2
seeds3d<-na.omit(seeds2d@data)
nrow(seeds2)-nrow(seeds3d) #1400 plots lost with deer data included
coordinates(seeds3d)<-~X+Y
proj4string(seeds3d)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeOGR(seeds3d, dsn=file, layer='Seedlings_m2_beta_with_deer_pred_clip',driver="ESRI Shapefile",overwrite_layer=T)
seeds.dfd<-as.data.frame(seeds3d)
write.csv(seeds.dfd, "Seedlings_m2_beta_with_deer_pred_df.csv")

# Clip sapling data to primary area of interest in analysis
saps<-readOGR(dsn=file,layer="Saplings_m2_beta_with_predictors")
bbox<-readOGR(dsn=file, layer="Bounding_Box")
saps.c<-raster::crop(saps,bbox, snap='in')
plot(saps.c, col='blue', pch=16)
plot(bbox,add=T)
names(saps.c)

saps2d<-saps.c[,c("PLT_CN","INVYR","Sor","Hor","STDSZCD","CANOPY_","ACERUB","ACESAC","CARYA","EARSUC","FAGGRA","FRAX","INVASIVE","LIRTUL",
                  "OTHER_SPP","PICEA","PINUS","QUERCUS","SHORT","ULMUS","ACERUB_","ACESAC_", "CARYA_s","EARSUC_",
                  "FAGGRA_","FRAX_sp","INVASIVE_","LIRTUL_","OTHER_SPP_","PICEA_s","PINUS_s","QUERCUS_","SHORT_s","ULMUS_s",
                  "prec_ex","tmax_ex","inv_sm_","hmod_ex","type", "dr_sm_x", "d____2_")]
names(saps2d)[names(saps2d)=="d____2_"]<-"deer_den"
saps2d$X<-saps2d$coords.x1
saps2d$Y<-saps2d$coords.x2
saps3d<-na.omit(saps2d@data)
nrow(saps2d)-nrow(saps3d) #1391
coordinates(saps3d)<-~X+Y
proj4string(saps3d)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeOGR(saps3d, dsn=file, layer='Saplings_m2_beta_with_deer_pred_clip',driver="ESRI Shapefile",overwrite_layer=T)
saps.dfd<-as.data.frame(saps3d)
write.csv(saps.dfd, "Saplings_m2_beta_with_deer_pred_df.csv")

#-----------------------
# Close R and reopen to clear RAM and temporary files
#----------------------
# START HERE FOR Spatial Regress
#------------------
setwd('C:/temp/GIS')
library(rgdal)
library(raster)
library(spdep)
library(car)
#library(GWmodel)

options("scipen"=100, "digits"=4)
rasterOptions(timer=TRUE,progress='text', tmpdir='G:/raster_temp') #shows a timer for processes run from raster package

file=('C:/temp/GIS')
#-----------------------------
# Seedling vs. Canopy similarity without deer impacts
#-----------------------------
seeds<-read.csv('Seedlings_m2_beta_with_pred_df.csv')[,-c(1)]
seeds2<-subset(seeds,type=="train" ) # select points that are only in the training dataset

names(seeds2)
seeds2$meso.tr<-seeds2$ACERUB+seeds2$ACESAC+seeds2$FAGGRA
seeds2$maple.tr<-seeds2$ACERUB+seeds2$ACESAC
seeds2$oakhick.tr<-seeds2$QUERCUS+seeds2$CARYA

seeds2$meso.sd<-seeds2$ACERUB_+seeds2$ACESAC_+seeds2$FAGGRA_
seeds2$maple.sd<-seeds2$ACERUB_+seeds2$ACESAC_
seeds2$oakhick.sd<-seeds2$QUERCUS_+seeds2$CARYA_s
seeds2$oakhick.sub<-seeds2$oakhick.tr-seeds2$oakhick.sd
seeds2$Sor.lgt<-logit(seeds2$Sor)
seeds2$Hor.lgt<-logit(seeds2$Hor)
seeds2$cancov<-seeds2$CANOPY_/100
#View(seeds2)

# Run OLS model first
names(seeds2)
#View(seeds2)
Sor.lm<-lm(Sor.lgt~SHORT_s+maple.sd+QUERCUS+QUERCUS_+FAGGRA+FAGGRA_+PINUS+PINUS_s+INVASIVE_+deer_den, data=seeds2)

summary(Sor.lm) #r2=0.113
AIC(Sor.lm) #19326
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(Sor.lm)
par(mfrow=c(1,1))

Hor.lm<-lm(Hor.lgt~tmax_ex+SHORT_s+maple.sd+QUERCUS+QUERCUS_+FAGGRA+FAGGRA_+PINUS+PINUS_s, data=seeds2)

summary(Hor.lm) #r2=0.254
AIC(Hor.lm) #20886
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(Hor.lm)
par(mfrow=c(1,1))


library(car)
vif(Sor.lm)
vif(Hor.lm) # all are under 2, so no issues of colinearity

library(gstat) #for bubble plot
names(seeds2)
autocor<-data.frame(seeds2$X,seeds2$Y,resids=Sor.lm$residuals)
names(autocor)
coordinates(autocor)<-~seeds2.X+seeds2.Y
proj4string(autocor)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
bubble(autocor,zcol='resids')
var.mod<-variogram(resids~1, data=autocor, alpha=c(0,90,180,270))
plot(var.mod)

autocorH<-data.frame(seeds2$X,seeds2$Y,resids=Hor.lm$residuals)
coordinates(autocorH)<-~seeds2.X+seeds2.Y
proj4string(autocorH)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
bubble(autocorH,zcol='resids')
var.modH<-variogram(resids~1, data=autocorH, alpha=c(0,90,180,270))
plot(var.modH)

#Calc Moran.I
library(ape)
plt.dist<-as.matrix(dist(cbind(seeds2$X,seeds2$Y)))
plt.dist.inv<-1/plt.dist
diag(plt.dist.inv)<-0
Moran.I(autocorH$resids,plt.dist.inv) #checking if residuals are autocorrelated
# significant p-value

#++++++++++++++++++++++++++++++++
#-----------------------------
# Seedling data with deer impacts
#-----------------------------
#++++++++++++++++++++++++++++++++
seedsd<-read.csv('Seedlings_m2_beta_with_deer_pred_df.csv')[,-c(1)]
seeds2d<-subset(seedsd,type=="train" ) # select points that are only in the training dataset
names(seeds2d)
seeds2d$meso.tr<-seeds2d$ACERUB+seeds2d$ACESAC+seeds2d$FAGGRA
seeds2d$maple.tr<-seeds2d$ACERUB+seeds2d$ACESAC
seeds2d$oakhick.tr<-seeds2d$QUERCUS+seeds2d$CARYA

seeds2d$meso.sd<-seeds2d$ACERUB_+seeds2d$ACESAC_+seeds2d$FAGGRA_
seeds2d$maple.sd<-seeds2d$ACERUB_+seeds2d$ACESAC_
seeds2d$oakhick.sd<-seeds2d$QUERCUS_+seeds2d$CARYA_s
seeds2d$Sor.lgt<-logit(seeds2d$Sor)
seeds2d$Hor.lgt<-logit(seeds2d$Hor)

# Run OLS model for Sorenson similarity
Sor.lmd<-lm(Sor.lgt~SHORT_s+maple.sd+QUERCUS+QUERCUS_+FAGGRA+FAGGRA_+PINUS+INVASIVE_+dr_sm_x+deer_den, data=seeds2d)
summary(Sor.lmd) #r2=0.117
AIC(Sor.lmd) #15336
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(Sor.lmd)
par(mfrow=c(1,1))

# OLS model for Horn similarity
Hor.lmd<-lm(Hor.lgt~tmax_ex+SHORT_s+maple.sd+QUERCUS+QUERCUS_+FAGGRA+FAGGRA_+PINUS+PINUS_s+deer_den, data=seeds2d) #deer dens better than impact
summary(Hor.lmd) #r2=0.265
AIC(Hor.lmd) #16503
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(Hor.lm)
par(mfrow=c(1,1))

library(gstat) #for bubble plot
names(seeds2d)
length(glob.lm$residuals)
autocor<-data.frame(seeds2d$X,seeds2d$Y,resids=glob.lm$residuals)
names(autocor)
coordinates(autocor)<-~seeds2d.X+seeds2d.Y
proj4string(autocor)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
names(autocor)

bubble(autocor,zcol='resids')
var.mod<-variogram(resids~1, data=autocor, alpha=c(0,90,180,270))
plot(var.mod)

#Calc Moran.I
library(ape)
plt.dist<-as.matrix(dist(cbind(seeds2d$X,seeds2d$Y)))
plt.dist.inv<-1/plt.dist
diag(plt.dist.inv)<-0
Moran.I(autocor$resids,plt.dist.inv) #checking if residuals are autocorrelated
# significant p-value

#----------------------------------
# Sapling data without deer impacts
#----------------------------------
saps<-read.csv('Saplings_m2_beta_with_pred_df.csv')[,-c(1)]
saps2<-subset(saps,type=="train" ) # select points that are only in the training dataset

names(saps2)
saps2$meso.tr<-saps2$ACERUB+saps2$ACESAC+saps2$FAGGRA
saps2$maple.tr<-saps2$ACERUB+saps2$ACESAC
saps2$oakhick.tr<-saps2$QUERCUS+saps2$CARYA

saps2$meso.sd<-saps2$ACERUB_+saps2$ACESAC_+saps2$FAGGRA_
saps2$maple.sap<-saps2$ACERUB_+saps2$ACESAC_
saps2$oakhick.sd<-saps2$QUERCUS_+saps2$CARYA_s
saps2$oakhick.sub<-saps2$oakhick.tr-saps2$oakhick.sd
saps2$Sor.lgt<-logit(saps2$Sor)
saps2$Hor.lgt<-logit(saps2$Hor)
saps2$cancov<-saps2$CANOPY_/100

# OLS model for Sorenson similarity
names(saps2)
Sor.sap.lm<-lm(Sor.lgt~tmax_ex+INVASIVE_+SHORT_s+QUERCUS+QUERCUS_+CARYA+maple.sap+FAGGRA+PINUS_s+STDSZCD+cancov, data=saps2)
summary(Sor.sap.lm) #r2=0.153
AIC(Sor.sap.lm) #18397
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(Sor.sap.lm)
par(mfrow=c(1,1))

library(gstat) #for bubble plot
autocor<-data.frame(saps2$X,saps2$Y,resids=Sor.sap.lm$residuals)
names(autocor)
coordinates(autocor)<-~saps2.X+saps2.Y
proj4string(autocor)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
names(autocor)

library(car)
bubble(autocor,zcol='resids')
var.mod<-variogram(resids~1, data=autocor, alpha=c(0,90,180,270))
plot(var.mod)

#Calc Moran.I
library(ape)
names(saps2)
plt.dist<-as.matrix(dist(cbind(saps2$X,saps2$Y)))
plt.dist.inv<-1/plt.dist
diag(plt.dist.inv)<-0
Moran.I(autocor$resids,plt.dist.inv) #checking if residuals are autocorrelated
# significant p-value

# OLS model for Horn similarity
names(saps2)
Hor.sap.lm<-lm(Hor.lgt~tmax_ex+INVASIVE_+SHORT_s+QUERCUS+QUERCUS_+CARYA+maple.sap+FAGGRA+FAGGRA_+PINUS+PINUS_s+STDSZCD+cancov, data=saps2)
summary(Hor.sap.lm) #r2=0.274
AIC(Hor.sap.lm) #20773
vif(Hor.sap.lm)


#----------------------------------
# Sapling data with deer impacts
#----------------------------------
saps<-read.csv('Saplings_m2_beta_with_deer_pred_df.csv')[,-c(1)]
saps2<-subset(saps,type=="train" ) # select points that are only in the training dataset

saps2$maple.tr<-saps2$ACERUB+saps2$ACESAC
saps2$maple.sap<-saps2$ACERUB_+saps2$ACESAC_
saps2$Sor.lgt<-logit(saps2$Sor)
saps2$Hor.lgt<-logit(saps2$Hor)
saps2$cancov<-saps2$CANOPY_/100

# OLS model for Sorenson similarity
names(saps2)
Sor.sap.lm<-lm(Sor.lgt~tmax_ex+INVASIVE_+SHORT_s+QUERCUS+QUERCUS_+CARYA+maple.sap+FAGGRA+FAGGRA_+PINUS_s+STDSZCD+cancov, data=saps2)
summary(Sor.sap.lm) #r2=0.168
AIC(Sor.sap.lm) #14234
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(Sor.sap.lm)
par(mfrow=c(1,1))

Hor.sap.lm<-lm(Hor.lgt~tmax_ex+INVASIVE_+SHORT_s+QUERCUS+QUERCUS_+CARYA+maple.sap+FAGGRA+FAGGRA_+PINUS+PINUS_s+STDSZCD+cancov, data=saps2)
summary(Hor.sap.lm) #r2=0.277
AIC(Hor.sap.lm) #16273

