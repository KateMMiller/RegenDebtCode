#--------------------------------------------
# STEP 3: R queries to compile sapling data for analysis
#--------------------------------------------
setwd('C:/temp/FIA_Data')
library(dplyr)
library(rgdal)
library(reshape)
library(reshape2)
library(raster)
options("scipen"=100, "digits"=22)

# Read in data compiled for analysis in step 2
plots<-read.csv("FIA_plots_for_analysis.csv")
REF_SPECIES<-read.csv("./raw_data/REF_SPECIES.csv")[,c("SPCD","COMMON_NAME","GENUS","SPECIES","SPECIES_SYMBOL")]
TREE<-read.csv("./raw_data/all_states_TREES.csv")[,c("CN", "PLT_CN","INVYR","SUBP","TREE","STATUSCD","SPCD","DIA","CCLCD")]
TREE2<-filter(TREE, STATUSCD==1 & DIA>=5)# only want live trees and trees >5" DBH 
SAP<-filter(TREE, STATUSCD==1 & DIA<5)
rm(TREE)
# Summarize saplings per m2
sap2<-left_join(plots, SAP, by=c("PLT_CN","INVYR"))
sap3<-left_join(sap2,REF_SPECIES, by=c("SPCD"))
sap3$DBHcm<-sap3$DIA*2.54
sap3$STEM<-ifelse(!is.na(sap3$DBHcm),1,0)

# summarise seedlings at the plot level by species
sap4<-sap3 %>% group_by(PLT_CN,INVYR,X_Coord, Y_Coord, GENUS,SPCD, SPECIES,SPECIES_SYMBOL) %>% 
                  summarise(sapcount=sum(STEM),MICRPROP_UNADJ=first(MICRPROP_UNADJ),SUBPPROP_UNADJ=first(SUBPPROP_UNADJ)) 

# remove plots that didn't have any microplots or subplots sampled, and sum the number of saplings at plot level
sap5<- sap4 %>% filter(MICRPROP_UNADJ!=0 & SUBPPROP_UNADJ!=0) %>% group_by(PLT_CN, INVYR, X_Coord, Y_Coord) %>%
                  summarise(sapcount=sum(sapcount),MICRPROP_UNADJ=first(MICRPROP_UNADJ),SUBPPROP_UNADJ=first(SUBPPROP_UNADJ))

micro4area<-((2.07264^2)*pi)*4
sap5$saps.m2<-(sap5$sapcount)/(sap5$MICRPROP_UNADJ*micro4area)
numsaplings<-sap5

# Convert dataframe to shapefile
coordinates(numsaplings)<-~X_Coord+Y_Coord
proj4string(numsaplings)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
file<-'C:/temp/GIS'
writeOGR(numsaplings, dsn=file,layer="numsaplings_albers",driver="ESRI Shapefile")
r <- raster(extent(numsaplings))
res(r)=20000 #20km= 20,000m
r <- rasterize(numsaplings, field="saps.m2", fun=mean,background=NA, r)
plot(r)
writeRaster(r, filename="../GIS/NumSaplings_m2_20km.tif",format="GTiff", overwrite=T)
#------------------------

#------------------------
# Sapling and tree species composition queries
#------------------------
# Now only using plots with MICRPROP_UNADJ=1 and SUBPPROP_UNADJ=1, cast the sapling data
data.fullp<-sap4 %>% filter(MICRPROP_UNADJ==1 & SUBPPROP_UNADJ==1) #Want only fully sampled micro and subplots for diversity analyses
head(data.fullp)
data.fullp$Latin<-paste(data.fullp$GENUS,data.fullp$SPECIES, sep=" ")
splist<-unique(data.fullp[,c("Latin","SPECIES_SYMBOL")])
write.csv(splist, "sapling_species_list.csv")

table(data.fullp2$Latin)

# Create species groupings by SPECIES_SYMBOL
ACERUB<-c("ACRU")
ACESAC<-c("ACSA3")
CARYA<-c('CAAL27','CAAQ2','CACA38','CACO15', 'CAGL8', 'CAIL2', 'CALA21', 'CAOV3', 'CAOV2', 'CAPA24', 'CARYA')
EARSUC<-c('BEPA', 'BEPO', 'POGR4', 'POTR5')
FAGGRA<-c('FAGR')
FRAX<-c('FRAM2','FRCA3','FRNI','FRPE','FRPR','FRQU','FRAXI')
INVASIVE<-c('ACPL','AIAL', 'ALJU','ALGL2','MEAZ','MOAL','PATO2','ROPS', 'TRSE6')
LIRTUL<-c('LITU')
PICEA<-c('PIGL','PIMA','PIRU')
PINUS<-c('PIEC2','PIEL','PIGL2','PIPA2','PIPU5','PIRE','PIRI','PISE','PIST','PITA','PIVI2')
QUERCUS<-c('QUAL', 'QUBI', 'QUCO2', 'QUFA', 'QUIL', 'QUIM', 'QUIN', 'QULA2', 'QULA3', 'QULY', 'QUMA2', 'QUMA6', 'QUMA3', 'QUMI', 'QUMU', 'QUNI',
           'QUPA5', 'QUPA2', 'QUPH', 'QUPR', 'QUPR2', 'QURU', 'QUSH', 'QUERC', 'QUST', 'QUTE', 'QUVE', 'QUVI')
ULMUS<-c('ULAL','ULAM','ULRU')
SHORT<-c('ACPE','ASTR','CACA18','CELA', 'CEOC', 'CECA4', 'COFL2', 'CRCR2', 'CRMO2', 'CRATA', 'GLTR', 'GOLA', 'HACA3', 'ILOP', 'MAPO', 'MAVI2', 
         'MAAN3', 'MACO5', 'MALUS', 'OSVI', 'OXAR', 'PEBO', 'PRAM', 'PRVI', 'SABE2', 'SAAL5', 'SILAL3', 'SOAM3', 'SORBU')
data.fullp$SPGRP<-with(data.fullp, ifelse(SPECIES_SYMBOL%in% ACERUB, 'ACERUB',ifelse(SPECIES_SYMBOL%in% ACESAC, 'ACESAC',ifelse(SPECIES_SYMBOL%in% CARYA,'CARYA',
                                        ifelse(SPECIES_SYMBOL%in% EARSUC,'EARSUC',ifelse(SPECIES_SYMBOL%in% FAGGRA,'FAGGRA', ifelse(SPECIES_SYMBOL%in% FRAX,'FRAX',
                                            ifelse(SPECIES_SYMBOL%in% INVASIVE,'INVASIVE', ifelse(SPECIES_SYMBOL%in% LIRTUL,'LIRTUL',ifelse(SPECIES_SYMBOL%in% PICEA,'PICEA',
                                                ifelse(SPECIES_SYMBOL%in% PINUS,'PINUS', ifelse(SPECIES_SYMBOL%in% QUERCUS,'QUERCUS',
                                                    ifelse(SPECIES_SYMBOL%in% ULMUS,'ULMUS',ifelse(SPECIES_SYMBOL %in% SHORT, 'SHORT','OTHER_SPP'))))))))))))))

table(data.fullp$SPGRP) 
names(data.fullp)
data.spgrp<-data.fullp %>% group_by(PLT_CN,INVYR, X_Coord,Y_Coord, SPGRP) %>% summarise(sapcount=sum(sapcount, na.rm=T))
head(data.spgrp)

melt.spgrp<-melt(data.spgrp, id=c('PLT_CN','INVYR','X_Coord','Y_Coord','SPGRP'),measure.vars="sapcount") #site by species matrics for seedlings/4 microplots
head(melt.spgrp)
sXsppgrp.sap<-dcast(melt.spgrp, PLT_CN+INVYR+X_Coord+Y_Coord~SPGRP, fun.aggregated=sum)
head(sXsppgrp.sap)
sXsppgrp.sap[,5:18][is.na(sXsppgrp.sap[,5:18])]<-0

rownames(sXsppgrp.sap)<-sXsppgrp.sap$PLT_CN # change row names to PLT_CN to make sure cbind works
relabund<-sweep(sXsppgrp.sap[,5:18],1,rowSums(sXsppgrp.sap[,5:18]),'/')
table(rowSums(relabund)) #all =1, so relative abundance worked
relabund.sap<-cbind(sXsppgrp.sap[,1:4], relabund)

# Convert dataframe to shapefile
coordinates(relabund.sap)<-~X_Coord+Y_Coord
proj4string(relabund.sap)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
file<-'C:/temp/GIS'
writeOGR(relabund.sap, dsn=file,layer="relabund_saplings",driver="ESRI Shapefile")
names(relabund.sap)

r <- raster(extent(relabund.sap))
res(r)=20000 #20km= 20,000m
ACERUB <- rasterize(relabund.sap, field="ACERUB", fun=mean,background=NA, r)
writeRaster(ACERUB, filename="../GIS/RELABU_SAP_ACERUB_20km.tif",format="GTiff", overwrite=T)

ACESAC <- rasterize(relabund.sap, field="ACESAC", fun=mean,background=NA, r)
writeRaster(ACESAC, filename="../GIS/RELABU_SAP_ACESAC_20km.tif",format="GTiff", overwrite=T)

CARYA <- rasterize(relabund.sap, field="CARYA", fun=mean,background=NA, r)
writeRaster(CARYA, filename="../GIS/RELABU_SAP_CARYA_20km.tif",format="GTiff", overwrite=T)

EARSUC <- rasterize(relabund.sap, field="EARSUC", fun=mean,background=NA, r)
writeRaster(EARSUC, filename="../GIS/RELABU_SAP_EARSUC_20km.tif",format="GTiff", overwrite=T)

FAGGRA <- rasterize(relabund.sap, field="FAGGRA", fun=mean,background=NA, r)
writeRaster(FAGGRA, filename="../GIS/RELABU_SAP_FAGGRA_20km.tif",format="GTiff", overwrite=T)

FRAX <- rasterize(relabund.sap, field="FRAX", fun=mean,background=NA, r)
writeRaster(FRAX, filename="../GIS/RELABU_SAP_FRAX_20km.tif",format="GTiff", overwrite=T)

INVASIVE <- rasterize(relabund.sap, field="INVASIVE", fun=mean,background=NA, r)
writeRaster(INVASIVE, filename="../GIS/RELABU_SAP_INVASIVE_20km.tif",format="GTiff", overwrite=T)

LIRTUL <- rasterize(relabund.sap, field="LIRTUL", fun=mean,background=NA, r)
writeRaster(LIRTUL, filename="../GIS/RELABU_SAP_LIRTUL_20km.tif",format="GTiff", overwrite=T)

PICEA <- rasterize(relabund.sap, field="PICEA", fun=mean,background=NA, r)
writeRaster(PICEA, filename="../GIS/RELABU_SAP_PICEA_20km.tif",format="GTiff", overwrite=T)

PINUS <- rasterize(relabund.sap, field="PINUS", fun=mean,background=NA, r)
writeRaster(PINUS, filename="../GIS/RELABU_SAP_PINUS_20km.tif",format="GTiff", overwrite=T)

QUERCUS <- rasterize(relabund.sap, field="QUERCUS", fun=mean,background=NA, r)
writeRaster(QUERCUS, filename="../GIS/RELABU_SAP_QUERCUS_20km.tif",format="GTiff", overwrite=T)

ULMUS <- rasterize(relabund.sap, field="ULMUS", fun=mean,background=NA, r)
writeRaster(ULMUS, filename="../GIS/RELABU_SAP_ULMUS_20km.tif",format="GTiff", overwrite=T)

SHORT <- rasterize(relabund.sap, field="SHORT", fun=mean,background=NA, r)
writeRaster(SHORT, filename="../GIS/RELABU_SAP_SHORT_20km.tif",format="GTiff", overwrite=T)

OTHERSPP <- rasterize(relabund.sap, field="OTHER_SPP", fun=mean,background=NA, r)
writeRaster(OTHERSPP, filename="../GIS/RELABU_SAP_OTHERSPP_20km.tif",format="GTiff", overwrite=T)

#------------------------------
# Create sapling site by species matrix
sap.melt<-melt(data.fullp, id=c('PLT_CN','X_Coord','Y_Coord','SPECIES_SYMBOL'),measure.vars="sapcount") #site by species matrics for seedlings/4 microplots
sXspp.sap<-dcast(sap.melt, PLT_CN+X_Coord+Y_Coord~SPECIES_SYMBOL, fun.aggregated=sum)
write.csv(sXspp.sap,"Sapling_site_by_species.csv")
ncol(sXspp.sap) #170
sXspp.sap[,4:170][is.na(sXspp.sap[,4:170])]<-0 
  # NOTE: sapling counts are in the original 4 microplot area densities

# Compile the tree data
pre.plots<-data.fullp[,c("PLT_CN","INVYR","X_Coord","Y_Coord","MICRPROP_UNADJ","SUBPPROP_UNADJ")]
plots<-unique(pre.plots) #19667 plots
plotarea<-(pi*(7.3152^2))*4 # equals the full area of the 4 subplots
data.tr<-left_join(plots,TREE2, by=c("PLT_CN","INVYR"))
data.tr$DBHcm<-data.tr$DIA*2.54
data.tr$STEM<-ifelse(!is.na(data.tr$DBHcm),1,0)
data.tr2<- data.tr %>% group_by(PLT_CN,INVYR,X_Coord, Y_Coord, SPCD) %>% summarise(numstems=sum(STEM))

# Make site by species matrix for tree stems/full 4 subplots
data.tr3<-left_join(data.tr2,REF_SPECIES, by="SPCD")
data.tr.melt<-melt(data.tr3,id=c('PLT_CN','X_Coord','Y_Coord','SPECIES_SYMBOL'),measure.vars="numstems")
sXspp.tr<-dcast(data.tr.melt, PLT_CN+X_Coord+Y_Coord~SPECIES_SYMBOL, fun.aggregated=sum)
nrow(sXspp.tr) #19667
ncol(sXspp.tr) #188
sXspp.tr[,4:188][is.na(sXspp.tr[,4:188])]<-0 
  # NOTE: tree counts are in the original 4 subplot area densities

# creating site by species matrices that have the same species list
new.tr.sp <- names(sXspp.tr[, which(!names(sXspp.tr) %in% names(sXspp.sap) )]) # species on tree list but not in seedling list
new.tr.sp #23 species to add to seedling list
new.sd.sp<- names(sXspp.sap[, which(!names(sXspp.sap) %in% names(sXspp.tr) )]) # species on seedling list but not in tree
new.sd.sp #6 species to add to tree list

# Creating dataframes to hold the species that need to be added to each list
new.trees.df <- data.frame(matrix(0,nrow = nrow(sXspp.tr), ncol = length(new.sd.sp)));colnames(new.trees.df)<-new.sd.sp #creating new dataframe with new tree species
sXspp.tr.full<-as.data.frame(cbind(sXspp.tr,new.trees.df)) #binding the two dataframes

new.saps.df <- data.frame(matrix(0,nrow = nrow(sXspp.sap), ncol = length(new.tr.sp)));colnames(new.saps.df)<-new.tr.sp # creating new dataframe for new seedling species
sXspp.sap.full<-as.data.frame(cbind(sXspp.sap,new.saps.df)) #binding two dataframs for seedlings/4 subplots

#----------------
# Sorting the species columns, so both tree and seedling dataframes are in the same order
#----------------
  #---TREE STEMS  
ncol(sXspp.tr.full) #194 cols
sp<-names(sXspp.tr.full[,4:194])#create list of species, then sort it 
sp2<-sort(sp, decreasing=F) 
sXspp.tr.full2<-sXspp.tr.full[,c("PLT_CN","X_Coord","Y_Coord",sp2)] # sort columns by the species list
names(sXspp.tr.full2)
sXspp.tr.full2<-sXspp.tr.full2[,-97] #removing the NA column
sXspp.tr.full2[is.na(sXspp.tr.full2)]<-0 # convert blanks to 0

  #---SAPLINGS per 4 microplots in m2
sXspp.sap.full2<-sXspp.sap.full[,c("PLT_CN","X_Coord","Y_Coord",sp2)] # sort columns by the species list
sXspp.sap.full2<-sXspp.sap.full2[,-97] #removing the NA column
sXspp.sap.full2[is.na(sXspp.sap.full2)]<-0  # convert blanks to 0
# Tree stems/4 subplots and saplings/4microplots in m2 all have the same fields and order, now need to remove sites that either don't have any saplings or don't have any trees.

#--------------
# Removing plots with no seedlings or no trees, and matching up the plot list 
  #---Trees/4 subplots
sXspp.tr.full3<-sXspp.tr.full2
sXspp.tr.full2$tottrs<-rowSums(sXspp.tr.full2[,4:193]) #have to remove plots that don't have any seedlings or trees
summary(sXspp.tr.full2$tottrs)  
  #---Saplings/4 microplots
sXspp.sap.full3<-sXspp.sap.full2
sXspp.sap.full2$totsaps<-rowSums(sXspp.sap.full2[,4:193]) #have to remove plots that don't have any seedlings or trees
summary(sXspp.sap.full2$totsaps)  #median 24

sites_no_trees<-sXspp.tr.full2[which(sXspp.tr.full2$tottrs==0),"PLT_CN"] 
length(sites_no_trees) 
sites_no_saps<-sXspp.sap.full2[which(sXspp.sap.full2$totsaps==0),"PLT_CN"]
length(sites_no_saps)

sites_to_drop<-unique(append(sites_no_trees,sites_no_saps)) #append list of plots from 0 trees and 0 seedlings together

sXspp.tr.final<-sXspp.tr.full3[!sXspp.tr.full3$PLT_CN %in% sites_to_drop,]
sXspp.sap.final<-sXspp.sap.full3[!sXspp.sap.full3$PLT_CN %in% sites_to_drop,]

nrow(sXspp.tr.final);nrow(sXspp.sap.final)
ncol(sXspp.tr.final);ncol(sXspp.sap.final)

plt<-as.data.frame(cbind(sXspp.tr.final$PLT_CN,sXspp.sap.final$PLT_CN))
plt$check<-with(plt, ifelse(V1==V2,0,1))
table(plt$check) #all 17798 records are 0, so plot order matches across the datasets

write.csv(sXspp.tr.final,"Tree_stems_m2_for_saplings_siteXspp.csv")
write.csv(sXspp.sap.final,"Sapling_stems_m2_siteXspp.csv")
