#--------------------------------------------
# Spatial Regression on regeneration densities
#--------------------------------------------
setwd('C:/temp/GIS')
library(rgdal)
library(raster)
library(rgeos)
library(dplyr)
options("scipen"=100, "digits"=4)
rasterOptions(timer=TRUE,progress='text', tmpdir='G:/raster_temp') #shows a timer for processes run from raster package

file=('C:/temp/GIS')

#create bounding box for analysis
coords=matrix(c(1154979, 2450153,2158160, 2450153,2158160, 1679168, 1154979, 1679168), ncol=2, byrow=T)
bbox = Polygon(coords)
bboxsp = SpatialPolygons(list(Polygons(list(bbox), 1)), proj4string=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))
plot(bboxsp, axes = TRUE)
bbox2<-as(bboxsp,"SpatialPolygonsDataFrame")
writeOGR(bbox2, dsn=file, layer="Bounding_Box", driver="ESRI Shapefile", overwrite_layer=T)

#-----------------------------------
# Relate response shapefile point locations to explanatory variables 
#-----------------------------------
# Seedling abudance
seeds<-readOGR(dsn=file,layer="numseedlings_albers")
seeds.df<-data.frame(seeds)

# read in raster and shapefiles to join data with
prec<-raster('Pct_Change_Precip.tif') # raster files not kernel smoothed
tmax<-raster('Pct_Change_Tmax.tif')
deer.den<-readOGR(dsn=file, layer='deer_density_UTMalbers')
deer.den$deer.index<-ifelse(deer.den$deer_dnsty=="red",4,ifelse(deer.den$deer_dnsty=="orange",3,ifelse(deer.den$deer_dnsty=="yellow",2,ifelse(deer.den$deer_dnsty=="green",1,NA))))
deer.sm<-raster('Browse_Impact_kern10k.tif')
inv<-raster('Ave_Cov_Invasive_20km.tif')
inv.sm<-raster('Invasive_Cover_kern10k.tif')
hmod.lc<-raster('nlcd300_utm_clip.tif')
oak<-raster('Tree_relabund_QUERCUS_kern10k.tif')
rmaple<-raster('Tree_relabund_ACERUB_kern10k.tif')
smaple<-raster('Tree_relabund_ACESAC_kern10k.tif')
beech<-raster('Tree_relabund_FAGGRA_kern10k.tif')
plots<-readOGR(dsn=file, layer="FIA_plots_for_analysis")
relabund<-readOGR(dsn=file, layer="relabund_trees")
relabundBA<-readOGR(dsn=file, layer="relabund_trees_BA")
plots.df<-data.frame(plots)
relab.df<-data.frame(relabund)
relab.df<-relab.df[,c(1:16)] #don't include coords or optional, b/c already in seeds.df
names(relab.df)
relabBA.df<-data.frame(relabundBA) # remove optional field and create dataframe from sp object
relabBA.df<-relabBA.df[,1:16]
colnames(relabBA.df)<-c("PLT_CN","INVYR","ACERUB.BA","ACESAC.BA","CARYA.BA","EARSUC.BA","FAGGRA.BA","FRAX.BA",
                        "INVASIVE.BA","LIRTUL.BA","OTHER_SPP.BA","PICEA.BA","PINUS.BA","QUERCUS.BA","SHORT.BA",
                        "ULMUS.BA")
# extract values of rasters for each point location in seeds
prec.ex<-extract(prec,seeds,method='bilinear')
tmax.ex<-extract(tmax,seeds,method='bilinear')
deer.ex<-sp::over(seeds, deer.den)
deer.sm.ex<-extract(deer.sm,seeds,method='bilinear')
inv.ex<-extract(inv,seeds,method='bilinear')
inv.sm.ex<-extract(inv.sm,seeds,method='bilinear')
hmod.ex<-extract(hmod.lc,seeds,method='simple')
oak.ex<-extract(oak, seeds,method='simple')
rmap.ex<-extract(rmaple, seeds, method='simple')
smap.ex<-extract(smaple,seeds,method='simple')
beech.ex<-extract(beech, seeds, method='simple')

seeds2<-left_join(seeds.df,plots.df[,c("PLT_CN","INVYR","STDSZCD","CANOPY_")], by=c("PLT_CN",'INVYR'))
seeds3<-left_join(seeds2,relab.df, by=c("PLT_CN","INVYR"))
seeds4<-left_join(seeds3,relabBA.df, by=c("PLT_CN","INVYR"))

# combine datasets  
seeds5<-data.frame(seeds4,prec.ex,tmax.ex,  deer.ex[,2], deer.sm.ex, inv.ex, inv.sm.ex,hmod.ex, oak.ex, rmap.ex,smap.ex,beech.ex) 
head(seeds5)
values<-c("train","test")
seeds5$type<-sample(values, nrow(seeds5), TRUE, prob = c(.95,.05)) # split data into training and testing dataset
table(seeds5$type)

coordinates(seeds5)<-~coords.x1+coords.x2
proj4string(seeds5)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
file<-'C:/temp/GIS'
writeOGR(seeds5, dsn=file, layer="Seedlings_m2_with_predictors", driver="ESRI Shapefile", overwrite_layer=T)

#------------------------
# Sapling abudance
saps<-readOGR(dsn=file,layer="numsaplings_albers")
saps.df<-data.frame(saps)

# extract raster values for each point location in saps
prec.ex<-extract(prec,saps,method='bilinear')
tmax.ex<-extract(tmax,saps,method='bilinear')
deer.ex<-sp::over(seeds, deer.den)
deer.sm.ex<-extract(deer.sm,saps,method='bilinear')
inv.ex<-extract(inv,saps,method='bilinear')
inv.sm.ex<-extract(inv.sm,saps,method='bilinear')
hmod.ex<-extract(hmod.lc,saps,method='simple')
oak.ex<-extract(oak, seeds,method='simple')
rmap.ex<-extract(rmaple, seeds, method='simple')
smap.ex<-extract(smaple,seeds,method='simple')
beech.ex<-extract(beech, seeds, method='simple')

saps2<-left_join(saps.df,plots.df[,c("PLT_CN","INVYR","STDSZCD","CANOPY_")], by=c("PLT_CN",'INVYR'))
saps3<-left_join(saps2,relab.df, by=c("PLT_CN","INVYR"))
saps4<-left_join(saps3,relabBA.df, by=c("PLT_CN","INVYR"))
nrow(plots);nrow(saps); nrow(saps4)

# combine datasets
saps5<-data.frame(saps4,prec.ex,tmax.ex,deer.ex[,2],deer.sm.ex,inv.ex,inv.sm.ex,hmod.ex, oak.ex, rmap.ex,smap.ex,beech.ex) 
head(saps5)

# add field that randomly assigns each point to a training or testing dataset
values<-c("train","test")
saps5$type<-sample(values, nrow(saps5), TRUE, prob = c(.95,.05)) # split data into training and testing dataset
coordinates(saps5)<-~coords.x1+coords.x2
proj4string(saps5)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
file<-'C:/temp/GIS'

writeOGR(saps5, dsn=file, layer="Saplings_m2_with_predictors", driver="ESRI Shapefile", overwrite_layer=T)

#---------------------------------------
# Regen without deer impacts metric
#---------------------------------------
# Clip seedling data to primary area of interest in analysis
seeds<-readOGR(dsn=file,layer="Seedlings_m2_with_predictors")
bbox<-readOGR(dsn=file, layer="Bounding_Box")
seeds.c<-raster::crop(seeds,bbox, snap='in')
plot(seeds.c, col='blue', pch=16)
plot(bbox,add=T)
names(seeds.c)

names(seeds.c)
seeds2<-seeds.c[,c("PLT_CN","INVYR","seds_m2","STDSZCD","CANOPY_","ACERUB","ACESAC","CARYA","EARSUC","FAGGRA","FRAX","INVASIVE","LIRTUL",
                   "OTHER_SPP","PICEA","PINUS","QUERCUS","SHORT","ULMUS","ACERUB_","ACESAC_", "CARYA_B","EARSUC_",
                   "FAGGRA_","FRAX_BA","INVASIVE_","LIRTUL_","OTHER_SPP_","PICEA_B","PINUS_B","QUERCUS_","SHORT_B","ULMUS_B",
                   "prec_ex","tmax_ex","inv_sm_","hmod_ex","oak_ex","rmap_ex","smap_ex","beech_x","d____2_","type")]
names(seeds2)[names(seeds2)=="d____2_"]<-"deer_den"
seeds2$X<-seeds2$coords.x1
seeds2$Y<-seeds2$coords.x2
seeds3<-na.omit(seeds2@data)
nrow(seeds2);nrow(seeds3)
coordinates(seeds3)<-~X+Y
proj4string(seeds3)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeOGR(seeds3, dsn=file, layer='Seedlings_m2_with_pred_clip',driver="ESRI Shapefile",overwrite_layer=T)
seeds.df<-as.data.frame(seeds3)
write.csv(seeds.df, "Seedlings_m2_with_pred_df.csv")

# Clip sapling data to primary area of interest in analysis
saps<-readOGR(dsn=file,layer="Saplings_m2_with_predictors")
bbox<-readOGR(dsn=file, layer="Bounding_Box")
saps.c<-raster::crop(saps,bbox, snap='in')
plot(saps.c, col='blue', pch=16)
plot(bbox,add=T)
names(saps.c)

saps2<-saps.c[,c("PLT_CN","INVYR","saps_m2","STDSZCD","CANOPY_","ACERUB","ACESAC","CARYA","EARSUC","FAGGRA","FRAX","INVASIVE",
                 "LIRTUL","OTHER_SPP","PICEA","PINUS","QUERCUS","SHORT","ULMUS","ACERUB_","ACESAC_", "CARYA_B","EARSUC_","FAGGRA_",
                 "FRAX_BA","INVASIVE_","LIRTUL_","OTHER_SPP_","PICEA_B","PINUS_B","QUERCUS_","SHORT_B","ULMUS_B",
                 "prec_ex","tmax_ex","inv_sm_","hmod_ex","oak_ex","rmap_ex","smap_ex","beech_x","d____2_","type"   )]
names(saps2)[names(saps2)=="d____2_"]<-"deer_den"
saps2$X<-saps2$coords.x1
saps2$Y<-saps2$coords.x2
saps3<-na.omit(saps2@data)
coordinates(saps3)<-~X+Y
proj4string(saps3)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeOGR(saps3, dsn=file, layer='Saplings_m2_with_pred_clip',driver="ESRI Shapefile",overwrite_layer=T)
saps.df<-as.data.frame(saps3)
write.csv(saps.df, "Saplings_m2_with_pred_df.csv")

#---------------------------------------
# Regen with deer impacts
#---------------------------------------
# Clip seedling data to primary area of interest in analysis
seeds<-readOGR(dsn=file,layer="Seedlings_m2_with_predictors")
bbox<-readOGR(dsn=file, layer="Bounding_Box")
seeds.c<-raster::crop(seeds,bbox, snap='in')
plot(seeds.c, col='blue', pch=16)
plot(bbox,add=T)
names(seeds.c)

str(seeds.c)
seeds2d<-seeds.c[,c("PLT_CN","INVYR","seds_m2","STDSZCD","CANOPY_","ACERUB","ACESAC","CARYA","EARSUC","FAGGRA","FRAX","INVASIVE","LIRTUL",
                    "OTHER_SPP","PICEA","PINUS","QUERCUS","SHORT","ULMUS","ACERUB_","ACESAC_", "CARYA_B","EARSUC_","FAGGRA_",
                    "FRAX_BA","INVASIVE_","LIRTUL_","OTHER_SPP_","PICEA_B","PINUS_B","QUERCUS_","SHORT_B","ULMUS_B",
                    "prec_ex","tmax_ex","inv_sm_","hmod_ex","oak_ex","rmap_ex","smap_ex","beech_x","type", "dr_sm_x","d____2_" )]
names(seeds2d)[names(seeds2d)=="d____2_"]<-"deer_den"
seeds2d$X<-seeds2d$coords.x1
seeds2d$Y<-seeds2d$coords.x2
seeds3d<-na.omit(seeds2d@data)
coordinates(seeds3d)<-~X+Y
proj4string(seeds3d)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeOGR(seeds3d, dsn=file, layer='Seedlings_m2_with_deer_pred_clip',driver="ESRI Shapefile",overwrite_layer=T)
seeds.dfd<-as.data.frame(seeds3d)
write.csv(seeds.dfd, "Seedlings_m2_with_deer_pred_df.csv")

# Clip sapling data to primary area of interest in analysis
saps<-readOGR(dsn=file,layer="Saplings_m2_with_predictors")
bbox<-readOGR(dsn=file, layer="Bounding_Box")
saps.c<-raster::crop(saps,bbox, snap='in')
plot(saps.c, col='blue', pch=16)
plot(bbox,add=T)
names(saps.c)

saps2d<-saps.c[,c("PLT_CN","INVYR","saps_m2","STDSZCD","CANOPY_","ACERUB","ACESAC","CARYA","EARSUC","FAGGRA","FRAX","INVASIVE","LIRTUL",
                  "OTHER_SPP","PICEA","PINUS","QUERCUS","SHORT","ULMUS","ACERUB_","ACESAC_", "CARYA_B","EARSUC_","FAGGRA_",
                  "FRAX_BA","INVASIVE_","LIRTUL_","OTHER_SPP_","PICEA_B","PINUS_B","QUERCUS_","SHORT_B","ULMUS_B",
                  "prec_ex","tmax_ex","inv_sm_","hmod_ex","oak_ex","rmap_ex","smap_ex","beech_x","type", "dr_sm_x","d____2_"  )]
names(saps2d)[names(saps2d)=="d____2_" ]<-"deer_den"
saps2d$X<-saps2d$coords.x1
saps2d$Y<-saps2d$coords.x2
saps3d<-na.omit(saps2d@data)
coordinates(saps3d)<-~X+Y
proj4string(saps3d)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeOGR(saps3d, dsn=file, layer='Saplings_m2_with_deer_pred_clip',driver="ESRI Shapefile",overwrite_layer=T)
saps.dfd<-as.data.frame(saps3d)
write.csv(saps.dfd, "Saplings_m2_with_deer_pred_df.csv")

#-----------------------
# Close R and reopen to clear RAM and temporary files
#----------------------
# START HERE FOR Spatial Regress
#------------------
setwd('C:/temp/GIS')
library(rgdal)
library(raster)
library(spdep)

options("scipen"=100, "digits"=4)
rasterOptions(timer=TRUE,progress='text', tmpdir='G:/raster_temp') #shows a timer for processes run from raster package

file=('C:/temp/GIS')
#-----------------------------
# Seedling data without deer impacts
#-----------------------------
seeds<-read.csv('Seedlings_m2_with_pred_df.csv')[,-c(1)]
seeds2<-subset(seeds,type=="train" ) # select points that are only in the training dataset

names(seeds2)
seeds2$meso<-seeds2$ACERUB+seeds2$ACESAC+seeds2$FAGGRA
seeds2$maple<-seeds2$ACERUB+seeds2$ACESAC
seeds2$sqseeds<-sqrt(seeds2$seds_m2)
seeds2$oakhick<-seeds2$QUERCUS+seeds2$CARYA
seeds2$oakhickBA<-seeds2$QUERCUS_+seeds2$CARYA_B
head(seeds2)

# Run OLS model first
glob.lm7<-lm(sqseeds~prec_ex+tmax_ex+inv_sm_+hmod_ex+oakhick+FAGGRA+PINUS+STDSZCD+CANOPY_, data=seeds2)
summary(glob.lm7) #r2=0.0599
AIC(glob.lm7) #8508
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(glob.lm7)
par(mfrow=c(1,1))

library(car)
vif(glob.lm7) # all are under 1.3, so no issues of colinearity

library(gstat) #for bubble plot
names(seeds2)
length(glob.lm7$residuals)
autocor<-data.frame(seeds2$X,seeds2$Y,resids=glob.lm7$residuals)
names(autocor)
coordinates(autocor)<-~seeds2.X+seeds2.Y
proj4string(autocor)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
bubble(autocor,zcol='resids')
var.mod<-variogram(resids~1, data=autocor, alpha=c(0,90,180,270))
plot(var.mod)

#Calc Moran.I
library(ape)
plt.dist<-as.matrix(dist(cbind(seeds2$X,seeds2$Y)))
plt.dist.inv<-1/plt.dist
diag(plt.dist.inv)<-0
Moran.I(autocor$resids,plt.dist.inv) #checking if residuals are autocorrelated
# significant p-value

#-----------------------------
# Seedling data with deer impacts
#-----------------------------
seedsd<-read.csv('Seedlings_m2_with_deer_pred_df.csv')[,-c(1)]
seeds2d<-subset(seedsd,type=="train" ) # select points that are only in the training dataset

names(seeds2d)
seeds2d$meso<-seeds2d$ACERUB+seeds2d$ACESAC+seeds2d$FAGGRA
seeds2d$maple<-seeds2d$ACERUB+seeds2d$ACESAC
seeds2d$sqseeds<-sqrt(seeds2d$seds_m2)
seeds2d$oakhick<-seeds2d$QUERCUS+seeds2d$CARYA

# Run global model first
glob.lm<-lm(sqseeds~prec_ex+tmax_ex+inv_sm_+hmod_ex+dr_sm_x+oakhick+FAGGRA+PINUS+STDSZCD+CANOPY_, data=seeds2d)
summary(glob.lm) #r2=0.0826
AIC(glob.lm) #7216
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(glob.lm)
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
# Sapling data
#----------------------------------
saps<-read.csv('Saplings_m2_with_pred_df.csv')[,-c(1)]
saps2<-subset(saps,type=="train" ) # select points that are only in the training dataset

names(saps2)
saps2$meso<-saps2$ACERUB+saps2$ACESAC+saps2$FAGGRA
saps2$maple<-saps2$ACERUB+saps2$ACESAC
saps2$mapleBA<-saps2$ACERUB_+saps2$ACESAC_
saps2$sqsaps<-sqrt(saps2$saps_m2)
saps2$oakhick<-saps2$QUERCUS+saps2$CARYA
saps2$oakhickBA<-saps2$QUERCUS_+saps2$CARYA_B

# Run global model first
glob.lm<-lm(sqsaps~prec_ex+inv_sm_+hmod_ex+oakhick+maple+FAGGRA+PINUS+STDSZCD+CANOPY_, data=saps2)
summary(glob.lm) #r2=0.124
AIC(glob.lm) #-8840
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(glob.lm)
par(mfrow=c(1,1))

library(gstat) #for bubble plot
length(glob.lm$residuals)
autocor<-data.frame(saps2$X,saps2$Y,resids=glob.lm$residuals)
names(autocor)
coordinates(autocor)<-~saps2.X+saps2.Y
proj4string(autocor)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
names(autocor)

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








