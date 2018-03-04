#--------------------------------------------
# STEP 4: Build Beta Diversity matrics for seedlings and trees
#--------------------------------------------
setwd('C:/temp/FIA_Data')
library(vegan)
library(rgdal)
library(raster)
options("scipen"=100)

tree<-read.csv("Tree_stems_m2_siteXspp.csv")[,c(-1)]
regen<-read.csv("Seedling_stems_m2_siteXspp.csv")[,c(-1)]
#----------------
# Tree stems vs. seedlings (both stems/ha)
#----------------
# Calculate beta diversity for Sorensen, Jaccard, Bsim, Morisita, Horn 
beta.mat.sor<-matrix(NA,nrow(tree),3,dimnames=list(c(paste(tree$PLT_CN)), c("SiteT","SiteR","Sor")))

for(i in 1:nrow(tree)){
  x<-rbind(tree[i,-c(1:3)],regen[i,-c(1:3)]) # create a data frame with an individual site containing the regen and tree species data, leaving out X,Y fields
  beta.mat.sor[i,1]<-tree[i,1] # name the SiteT column using the tree site column.
  beta.mat.sor[i,2]<-regen[i,1] # names the SiteR column using the regen site column.This will let me check whether they match, just in case the row order changed
  beta.mat.sor[i,3]<-betadiver(x,method="sor") # calculate sorensen similarity between the regen and tree layer 
  } 

beta.mat.jac<-matrix(NA,nrow(tree),3,dimnames=list(c(paste(tree$PLT_CN)), c("SiteT","SiteR","Jac")))

for(i in 1:nrow(tree)){
  x<-rbind(tree[i,-c(1:3)],regen[i,-c(1:3)]) # create a data frame with an individual site containing the regen and tree species data, leaving out X,Y fields
  beta.mat.jac[i,1]<-tree[i,1] # name the SiteT column using the tree site column.
  beta.mat.jac[i,2]<-regen[i,1] # names the SiteR column using the regen site column.This will let me check whether they match, just in case the row order changed
  beta.mat.jac[i,3]<-betadiver(x,method="j") # calculate jaccard similarity between the regen and tree layer 
} 

beta.mat.sim<-matrix(NA,nrow(tree),3,dimnames=list(c(paste(tree$PLT_CN)), c("SiteT","SiteR","Sim")))

for(i in 1:nrow(tree)){
  x<-rbind(tree[i,-c(1:3)],regen[i,-c(1:3)]) # create a data frame with an individual site containing the regen and tree species data, leaving out X,Y fields
  beta.mat.sim[i,1]<-tree[i,1] # name the SiteT column using the tree site column.
  beta.mat.sim[i,2]<-regen[i,1] # names the SiteR column using the regen site column.This will let me check whether they match, just in case the row order changed
  beta.mat.sim[i,3]<-betadiver(x,method="sim") # calculate BSim similarity between the regen and tree layer 
} 

beta.mat.mor<-matrix(NA,nrow(tree),1) #Morisita requires integers
colnames(beta.mat.mor)<-"Mor"

for(i in 1:nrow(tree)){
  x<-rbind(tree[i,-c(1:3)],regen[i,-c(1:3)]) # create a data frame with an individual site containing the regen and tree species data
  beta.mat.mor[i,1]<-(vegdist(x,method="morisita", na.rm=T)) # calculate morisita similarity between the regen and tree layer
} # need to subtract 1 from Mor to make similarity

beta.mat.hor<-matrix(NA,nrow(tree),1) #Morisita requires integers
colnames(beta.mat.hor)<-"Hor"

for(i in 1:nrow(tree)){
  x<-rbind(tree[i,-c(1:3)],regen[i,-c(1:3)]) # create a data frame with an individual site containing the regen and tree species data
  beta.mat.hor[i,1]<-(vegdist(x,method="horn", diag=T)) # calculate horn similarity between the regen and tree layer 
} # need to subtract 1 from Mor to make similarity


beta.mat.sor<-as.data.frame(beta.mat.sor)
beta.mat.jac<-as.data.frame(beta.mat.jac)
beta.mat.sim<-as.data.frame(beta.mat.sim)
beta.mat.hor<-as.data.frame(beta.mat.hor)
beta.mat.mor<-as.data.frame(beta.mat.mor)

beta.mat.simr$Sim.sim<-with(beta.mat.sim,(ifelse(!is.na(Sim), 1-Sim,NA)))
beta.mat.hor$Hor.sim<-with(beta.mat.hor,(ifelse(!is.na(Hor), 1-Hor,NA)))
beta.mat.mor$Mor.sim<-with(beta.mat.mor,(ifelse(!is.na(Mor), 1-Mor,NA)))

beta.mat<-as.data.frame(cbind(tree[,1:3], beta.mat.sor$Sor,beta.mat.jac$Jac, beta.mat.sim$Sim.sim, beta.mat.hor$Hor.sim,beta.mat.mor$Mor.sim))
head(beta.mat)
nrow(beta.mat.jac)

colnames(beta.mat)<-c("PLT_CN","X_Coord","Y_Coord","Sor","Jac","Sim","Hor","Mor")
write.csv(beta.mat,"Seedling_TreeStems_m2_beta_diversity_matrix.csv")

# Convert dataframe to shapefile and raster
coordinates(beta.mat)<-~X_Coord+Y_Coord
proj4string(beta.mat)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
file<-'C:/temp/GIS'
writeOGR(beta.mat, dsn=file,layer="Tree_seedling_stems_m2_beta",driver="ESRI Shapefile")
#beta.mat.shp<-readOGR(dsn=file, layer="Tree_seedling_stems_m2_beta")

r <- raster(extent(beta.mat))
res(r)=20000 #20km= 20,000m
sor <- rasterize(beta.mat, field="Sor", fun=mean, background=NA, r)
writeRaster(sor, filename="../GIS/Tree_seedling_stems_m2_SOR.tif",format="GTiff", overwrite=T)

jac <- rasterize(beta.mat, field="Jac", fun=mean,background=NA, r)
writeRaster(jac, filename="../GIS/Tree_seedling_stems_m2_JAC.tif",format="GTiff", overwrite=T)

sim <- rasterize(beta.mat, field="Sim", fun=mean,background=NA, r)
writeRaster(sim, filename="../GIS/Tree_seedling_stems_m2_SIM.tif",format="GTiff", overwrite=T)

hor <- rasterize(beta.mat, field="Hor", fun=mean,background=NA, r)
writeRaster(hor, filename="../GIS/Tree_seedling_stems_m2_HOR.tif",format="GTiff", overwrite=T)

mor <- rasterize(beta.mat, field="Mor", fun=mean,background=NA,r)
writeRaster(mor, filename="../GIS/Tree_seedling_stems_m2_MOR.tif",format="GTiff", overwrite=T)
