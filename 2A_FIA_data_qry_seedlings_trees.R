#--------------------------------------------
# STEP 2: R queries to compile FIA Tree and Seedling data for analysis
#--------------------------------------------
setwd('C:/temp/FIA_Data')
library(dplyr)
library(rgdal)
library(reshape)
library(reshape2)
library(raster)
options("scipen"=100, "digits"=4)

#-------------------------------------#
# Compile the FIA data #
#-------------------------------------#
PLOTSNAP<-read.csv("./raw_data/all_states_PLOTSNAP.csv")[,c("CN","INVYR","STATECD","PLOT","PLOT_STATUS_CD","LAT","LON","KINDCD","DESIGNCD","SUBP_EXAMINE_CD","QA_STATUS",
                                                 "MANUAL","MANUAL_DB","INTENSITY","EVAL_GRP_CN","EVAL_GRP")] # Read in the PLOTSNAP table taking only the necessary fields
names(PLOTSNAP)[names(PLOTSNAP)=="CN"]<-"PLT_CN" # Changed name so easier to merge with rest of tables, which use PLT_CN for the same field
PLOTSNAP$STATECD<-as.factor(PLOTSNAP$STATECD)
PLOTSNAP2<-PLOTSNAP %>% group_by(STATECD) %>% filter(EVAL_GRP==max(EVAL_GRP), QA_STATUS==1,INTENSITY==1, PLOT_STATUS_CD==1) # Only taking the most recent evaluation group of plots for each state, non-qaqc sampling events, plots that are part of the national frame, and plots that are sampled forestland
nrow(PLOTSNAP); nrow(PLOTSNAP2)
names(PLOTSNAP2)
PLOTSNAP2<-droplevels(PLOTSNAP2)
table(PLOTSNAP2$INVYR) # inventory years range from 2009-2016.
table(PLOTSNAP2$MANUAL)
COND<-read.csv("./raw_data/all_states_COND.csv")[,c("CN","PLT_CN","INVYR","CONDID","COND_STATUS_CD","PROP_BASIS","CONDPROP_UNADJ",
                                                    "MICRPROP_UNADJ","SUBPPROP_UNADJ", "STDSZCD", "LIVE_CANOPY_CVR_PCT")];names(COND)
data<-left_join(PLOTSNAP2, COND, by=c("PLT_CN","INVYR"))            
data$STDSZCD[data$STDSZCD==5]<-4 # change 5= non-stocked to 4, so scale of 1-4 with 1 being large diamater forest and 4 being young forest
table(data$STDSZCD)
head(data[,12:25])
names(data)
data2<-data %>% filter(COND_STATUS_CD==1) %>% group_by(PLT_CN, INVYR) %>% 
  summarise(STATECD=first(STATECD), PLOT=first(PLOT),LAT=first(LAT),LON=first(LON),EVAL_GRP=first(EVAL_GRP),MANUAL=first(MANUAL),
            STDSZCD.wa=sum(STDSZCD*CONDPROP_UNADJ)/sum(CONDPROP_UNADJ), CANOPY_COV_wa=sum(LIVE_CANOPY_CVR_PCT*CONDPROP_UNADJ)/sum(CONDPROP_UNADJ),
            MICRPROP_UNADJ=sum(MICRPROP_UNADJ), SUBPPROP_UNADJ=sum(SUBPPROP_UNADJ))

# checkinng weighted averages worked across plots with multiple conditions
data.test<-subset(data, PLT_CN==14262943020004)
data.test[,c(1,2,3,4,19,20,21,24,25)]
data.test2<-subset(data2, PLT_CN==14262943020004)
data.test2

#Convert coordinates from DD to UTM
coordinates(data2)<-~LON+LAT
proj4string(data2)=CRS("+init=epsg:4269")
CRS.albers <- CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
plots.UTM<-spTransform(data2, CRS=CRS.albers)
plots.UTM.df<-data.frame(plots.UTM)
str(plots.UTM.df)  
names(plots.UTM.df)[names(plots.UTM.df)=="LON"]<-"X_Coord"
names(plots.UTM.df)[names(plots.UTM.df)=="LAT"]<-"Y_Coord"
names(plots.UTM.df)
plots<-plots.UTM.df[,-13] #remove optional field

  # Checking that previous step resulted in correct area for a plot with multiple conditions and only partially sampled.
  plot.test<-c(171866963020004)
  data.test<-plots[plots$PLT_CN %in% plot.test,]# MICRPROP_UNADJ should be 0.5, SUBPPROP_UNADJ should be 0.433195- check

coordinates(plots)<-~X_Coord+Y_Coord
proj4string(plots)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
file<-'C:/temp/GIS'
writeOGR(plots, dsn=file,layer="FIA_plots_for_analysis",driver="ESRI Shapefile", overwrite_layer=T)
write.csv(plots, "FIA_plots_for_analysis.csv")

# Join plot data to seedling data
    #plots<-read.csv("FIA_plots_for_analysis.csv")
SEEDLING<-read.csv("./raw_data/all_states_SEEDLING.csv")[,c("CN", "PLT_CN","INVYR","PLOT","SUBP","CONDID","SPCD","STOCKING","TREECOUNT","TREECOUNT_CALC","TPA_UNADJ")]
names(SEEDLING)[names(SEEDLING)=="CN"]<-"CN_SEEDLING"
nrow(SEEDLING)
REF_SPECIES<-read.csv("./raw_data/REF_SPECIES.csv")[,c("SPCD","COMMON_NAME","GENUS","SPECIES","SPECIES_SYMBOL")]
nrow(REF_SPECIES)

data3<-left_join(plots.UTM.df[,-13],SEEDLING, by=c("PLT_CN","INVYR"))
names(data3)
data4<-left_join(data3,REF_SPECIES, by=c("SPCD"))
data4$TREECOUNT[is.na(data4$TREECOUNT)]<-0 # sets blanks, where seedlings weren't found to 0

data5<-data4 %>% group_by(PLT_CN,INVYR,X_Coord, Y_Coord, GENUS,SPCD, SPECIES,SPECIES_SYMBOL) %>% 
  summarise(TREECOUNT=sum(TREECOUNT),MICRPROP_UNADJ=first(MICRPROP_UNADJ),SUBPPROP_UNADJ=first(SUBPPROP_UNADJ)) 
  # summarise seedlings at the plot level by species

data6<- data5 %>% filter(MICRPROP_UNADJ!=0 & SUBPPROP_UNADJ!=0) %>% group_by(PLT_CN, INVYR, X_Coord, Y_Coord) %>%
              summarise(TREECOUNT=sum(TREECOUNT),MICRPROP_UNADJ=first(MICRPROP_UNADJ),SUBPPROP_UNADJ=first(SUBPPROP_UNADJ))
  # remove plots that didn't have any microplots or subplots sampled, and sum the number of seedlings at plot level
micro4area<-((2.07264^2)*pi)*4
data6$seeds.ha<-(data6$TREECOUNT*10000)/(data6$MICRPROP_UNADJ*micro4area)
data6$seeds.m2<-(data6$TREECOUNT)/(data6$MICRPROP_UNADJ*micro4area)
numseedlings<-data6

# Convert dataframe to shapefile
coordinates(numseedlings)<-~X_Coord+Y_Coord
proj4string(numseedlings)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
file<-'C:/temp/GIS'
writeOGR(numseedlings, dsn=file,layer="numseedlings_albers",driver="ESRI Shapefile", overwrite_layer=T)

names(numseedlings)
r <- raster(extent(numseedlings))
res(r)=20000 #20km= 20,000m
r <- rasterize(numseedlings, field="seeds.m2", fun=mean,background=NA, r)
writeRaster(r, filename="../GIS/NumSeedlings_m2_20km.tif",format="GTiff", overwrite=T)
#--------------------------------------------

#--------------------------------------------
# Regen and tree species composition queries
#--------------------------------------------
# Now only using plots with MICRPROP_UNADJ=1 and SUBPPROP_UNADJ=1, summarise the seedling data
data.fullp<-data5 %>% filter(MICRPROP_UNADJ==1 & SUBPPROP_UNADJ==1) #select only fully sampled micro and subplots for diversity analyses
data.fullp2<-data.fullp[which(data.fullp$TREECOUNT>0),] #remove plots without and seedlings 
data.fullp2$seeds.ha<-(data.fullp2$TREECOUNT*10000)/(data.fullp2$MICRPROP_UNADJ*micro4area)
nrow(data.fullp); nrow(data.fullp2)

#--------------------------------------------
# Cast the seedling data into a site by species matrix for beta diversity analyses
data.melt<-melt(data.fullp2, id=c('PLT_CN','X_Coord','Y_Coord','SPECIES_SYMBOL'),measure.vars="seeds.ha") # site by species matrix for seedlings/ha
sXspp.seed<-dcast(data.melt, PLT_CN+X_Coord+Y_Coord~SPECIES_SYMBOL, fun.aggregated=sum)

data.melt.m2<-melt(data.fullp2, id=c('PLT_CN','X_Coord','Y_Coord','SPECIES_SYMBOL'),measure.vars="TREECOUNT") #site by species matrics for seedlings/4 microplots
sXspp.seed.m2<-dcast(data.melt.m2, PLT_CN+X_Coord+Y_Coord~SPECIES_SYMBOL, fun.aggregated=sum)

  #write.csv(sXspp.seed,"Seedling_site_by_species.csv")

#-----------------------------------
# Calculate the relative abundance of species or species groups of interest for seedlings
head(data.fullp2)
data.fullp2$Latin<-paste(data.fullp2$GENUS,data.fullp2$SPECIES, sep=" ")
splist<-unique(data.fullp2[,c("Latin","SPECIES_SYMBOL")])
write.csv(splist, "seedling_species_list.csv")

table(data.fullp2$Latin)

# Create species groupings by SPECIES_SYMBOL
ACERUB<-c("ACRU")
ACESAC<-c("ACSA3")
CARYA<-c('CAAL27','CAAQ2','CACO15', 'CAGL8', 'CAIL2', 'CALA21', 'CAOV3', 'CAOV2', 'CAPA24', 'CARYA')
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
data.fullp2$SPGRP<-with(data.fullp2, ifelse(SPECIES_SYMBOL%in% ACERUB, 'ACERUB',ifelse(SPECIES_SYMBOL%in% ACESAC, 'ACESAC',ifelse(SPECIES_SYMBOL%in% CARYA,'CARYA',
                                        ifelse(SPECIES_SYMBOL%in% EARSUC,'EARSUC',ifelse(SPECIES_SYMBOL%in% FAGGRA,'FAGGRA', ifelse(SPECIES_SYMBOL%in% FRAX,'FRAX',
                                            ifelse(SPECIES_SYMBOL%in% INVASIVE,'INVASIVE', ifelse(SPECIES_SYMBOL%in% LIRTUL,'LIRTUL',ifelse(SPECIES_SYMBOL%in% PICEA,'PICEA',
                                                ifelse(SPECIES_SYMBOL%in% PINUS,'PINUS', ifelse(SPECIES_SYMBOL%in% QUERCUS,'QUERCUS',
                                                    ifelse(SPECIES_SYMBOL%in% ULMUS,'ULMUS',ifelse(SPECIES_SYMBOL %in% SHORT, 'SHORT','OTHER_SPP'))))))))))))))

table(data.fullp2$SPGRP) 
names(data.fullp2)
data.spgrp<-data.fullp2 %>% group_by(PLT_CN,INVYR, X_Coord,Y_Coord, SPGRP) %>% summarise(TREECOUNT=sum(TREECOUNT, na.rm=T))
head(data.spgrp)

melt.spgrp<-melt(data.spgrp, id=c('PLT_CN','INVYR','X_Coord','Y_Coord','SPGRP'),measure.vars="TREECOUNT") #site by species matrics for seedlings/4 microplots
head(melt.spgrp)
sXsppgrp.seed<-dcast(melt.spgrp, PLT_CN+INVYR+X_Coord+Y_Coord~SPGRP, fun.aggregated=sum)
head(sXsppgrp.seed)
names(sXsppgrp.seed)
sXsppgrp.seed[,5:18][is.na(sXsppgrp.seed[,5:18])]<-0

rownames(sXsppgrp.seed)<-sXsppgrp.seed$PLT_CN # change row names to PLT_CN to make sure cbind works
relabund<-sweep(sXsppgrp.seed[,5:18],1,rowSums(sXsppgrp.seed[,5:18]),'/')
table(rowSums(relabund)) #all =1, so relative abundance worked
relabund.seed<-cbind(sXsppgrp.seed[,1:4], relabund)

# Convert dataframe to shapefile
coordinates(relabund.seed)<-~X_Coord+Y_Coord
proj4string(relabund.seed)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
file<-'C:/temp/GIS'
writeOGR(relabund.seed, dsn=file,layer="relabund_seedlings",driver="ESRI Shapefile", overwrite_layer=T)
names(relabund.seed)

r <- raster(extent(relabund.seed))
res(r)=20000 #20km= 20,000m
ACERUB <- rasterize(relabund.seed, field="ACERUB", fun=mean,background=NA, r)
writeRaster(ACERUB, filename="../GIS/RELABU_SEED_ACERUB_20km.tif",format="GTiff", overwrite=T)

ACESAC <- rasterize(relabund.seed, field="ACESAC", fun=mean,background=NA, r)
writeRaster(ACESAC, filename="../GIS/RELABU_SEED_ACESAC_20km.tif",format="GTiff", overwrite=T)

CARYA <- rasterize(relabund.seed, field="CARYA", fun=mean,background=NA, r)
writeRaster(CARYA, filename="../GIS/RELABU_SEED_CARYA_20km.tif",format="GTiff", overwrite=T)

EARSUC <- rasterize(relabund.seed, field="EARSUC", fun=mean,background=NA, r)
writeRaster(EARSUC, filename="../GIS/RELABU_SEED_EARSUC_20km.tif",format="GTiff", overwrite=T)

FAGGRA <- rasterize(relabund.seed, field="FAGGRA", fun=mean,background=NA, r)
writeRaster(FAGGRA, filename="../GIS/RELABU_SEED_FAGGRA_20km.tif",format="GTiff", overwrite=T)

FRAX <- rasterize(relabund.seed, field="FRAX", fun=mean,background=NA, r)
writeRaster(FRAX, filename="../GIS/RELABU_SEED_FRAX_20km.tif",format="GTiff", overwrite=T)

INVASIVE <- rasterize(relabund.seed, field="INVASIVE", fun=mean,background=NA, r)
writeRaster(INVASIVE, filename="../GIS/RELABU_SEED_INVASIVE_20km.tif",format="GTiff", overwrite=T)

LIRTUL <- rasterize(relabund.seed, field="LIRTUL", fun=mean,background=NA, r)
writeRaster(LIRTUL, filename="../GIS/RELABU_SEED_LIRTUL_20km.tif",format="GTiff", overwrite=T)

PICEA <- rasterize(relabund.seed, field="PICEA", fun=mean,background=NA, r)
writeRaster(PICEA, filename="../GIS/RELABU_SEED_PICEA_20km.tif",format="GTiff", overwrite=T)

PINUS <- rasterize(relabund.seed, field="PINUS", fun=mean,background=NA, r)
writeRaster(PINUS, filename="../GIS/RELABU_SEED_PINUS_20km.tif",format="GTiff", overwrite=T)

QUERCUS <- rasterize(relabund.seed, field="QUERCUS", fun=mean,background=NA, r)
writeRaster(QUERCUS, filename="../GIS/RELABU_SEED_QUERCUS_20km.tif",format="GTiff", overwrite=T)

ULMUS <- rasterize(relabund.seed, field="ULMUS", fun=mean,background=NA, r)
writeRaster(ULMUS, filename="../GIS/RELABU_SEED_ULMUS_20km.tif",format="GTiff", overwrite=T)

SHORT <- rasterize(relabund.seed, field="SHORT", fun=mean,background=NA, r)
writeRaster(SHORT, filename="../GIS/RELABU_SEED_SHORT_20km.tif",format="GTiff", overwrite=T)

OTHERSPP <- rasterize(relabund.seed, field="OTHER_SPP", fun=mean,background=NA, r)
writeRaster(OTHERSPP, filename="../GIS/RELABU_SEED_OTHERSPP_20km.tif",format="GTiff", overwrite=T)

#----------------------------------------

#----------------------------
# Read in the tree data
TREE<-read.csv("./raw_data/all_states_TREES.csv")[,c("CN", "PLT_CN","INVYR","SUBP","TREE","STATUSCD","SPCD","DIA","CCLCD")]
TREE2<-filter(TREE, STATUSCD==1 & DIA>=5)# only want live trees and trees >5" DBH 
  #join to the same list of plots in data.fullp, which includes all of the plots with fully sampled subplots
rm(TREE)
plots<-read.csv("FIA_plots_for_analysis.csv")

plots2<-plots[,c("PLT_CN","INVYR","X_Coord","Y_Coord","MICRPROP_UNADJ","SUBPPROP_UNADJ")]
length(unique(plots2$PLT_CN)) #29512- using these data for the seedling/sapling stem densities, so included all forested sampled plots, even if partial.
plotarea<-(pi*(7.3152^2))*4 # equals the full area of the 4 subplots
data.tr<-left_join(plots2,TREE2, by=c("PLT_CN","INVYR"))
data.tr$DBHcm<-data.tr$DIA*2.54
data.tr$STEM<-ifelse(!is.na(data.tr$DBHcm),1,0)
data.tr$BA_msq<-with(data.tr, ifelse(!is.na(DBHcm), ((pi*(DBHcm/200)^2)/plotarea)*10000,0))
names(data.tr)

data.tr2<- data.tr %>% group_by(PLT_CN,INVYR,X_Coord, Y_Coord, SPCD) %>% summarise(BA_msq_ha=sum(BA_msq),numstems=sum(STEM))

#-----------------------------------
# Calculate the relative abundance of species or species groups of interest for trees
head(data.tr2)
data.tr3<-left_join(data.tr2,REF_SPECIES[,c("SPCD","SPECIES_SYMBOL", "GENUS","SPECIES")], by=c("SPCD"))
data.tr3$Latin<-paste(data.tr3$GENUS,data.tr3$SPECIES, sep=" ")
splist<-unique(data.tr3[,c("Latin","SPECIES_SYMBOL")])
write.csv(splist, "tree_species_list.csv")

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
PINUS<-c('PIBA2','PIEC2','PIEL','PIGL2','PIPA2','PIPU5','PIRE','PIRI','PISE','PIST','PITA','PIVI2')
QUERCUS<-c('QUAL', 'QUBI', 'QUCO2', 'QUEL','QUFA', 'QUIL', 'QUIM', 'QUIN', 'QULA2', 'QULA3', 'QULY', 'QUMA2', 'QUMA6', 'QUMA3', 'QUMI', 'QUMU', 'QUNI',
           'QUPA5', 'QUPA2', 'QUPH', 'QUPR', 'QUPR2', 'QURU', 'QUSH', 'QUERC', 'QUST', 'QUTE', 'QUVE', 'QUVI')
ULMUS<-c('ULAM','ULPU','ULRU','ULTH')
SHORT<-c('ACPE','ASTR','CACA18','CELA', 'CEOC', 'CECA4', 'COFL2', 'CRCR2', 'CRMO2', 'CRATA', 'GLTR', 'GOLA', 'HACA3', 'ILOP', 'MAPO', 'MAVI2', 
         'MAAN3', 'MACO5', 'MALUS', 'OSVI', 'OXAR', 'PEBO', 'PRAM', 'PRVI', 'SABE2', 'SAAL5', 'SILAL3', 'SOAM3', 'SORBU')

data.tr3$SPGRP<-with(data.tr3, ifelse(SPECIES_SYMBOL%in% ACERUB, 'ACERUB',ifelse(SPECIES_SYMBOL%in% ACESAC, 'ACESAC',ifelse(SPECIES_SYMBOL%in% CARYA,'CARYA',
                                        ifelse(SPECIES_SYMBOL%in% EARSUC,'EARSUC',ifelse(SPECIES_SYMBOL%in% FAGGRA,'FAGGRA', ifelse(SPECIES_SYMBOL%in% FRAX,'FRAX',
                                           ifelse(SPECIES_SYMBOL%in% INVASIVE,'INVASIVE', ifelse(SPECIES_SYMBOL%in% LIRTUL,'LIRTUL',ifelse(SPECIES_SYMBOL%in% PICEA,'PICEA',
                                              ifelse(SPECIES_SYMBOL%in% PINUS,'PINUS', ifelse(SPECIES_SYMBOL%in% QUERCUS,'QUERCUS',
                                                  ifelse(SPECIES_SYMBOL %in% ULMUS,'ULMUS',ifelse(SPECIES_SYMBOL %in% SHORT, 'SHORT','OTHER_SPP'))))))))))))))

table(data.tr3$SPGRP) 
names(data.tr3)
tree.spgrp<-data.tr3 %>% group_by(PLT_CN,INVYR, X_Coord,Y_Coord, SPGRP) %>% summarise(numstems=sum(numstems, na.rm=T), BA=sum(BA_msq_ha, na.rm=T))
head(tree.spgrp)
# For stem density
melt.tr.spgrp<-melt(tree.spgrp, id=c('PLT_CN','INVYR','X_Coord','Y_Coord','SPGRP'),measure.vars="numstems") #site by species matrics for seedlings/4 microplots
head(melt.tr.spgrp)
sXsppgrp.tree<-dcast(melt.tr.spgrp, PLT_CN+INVYR+X_Coord+Y_Coord~SPGRP, fun.aggregated=sum)
head(sXsppgrp.tree)
sXsppgrp.tree[,5:18][is.na(sXsppgrp.tree[,5:18])]<-0

rownames(sXsppgrp.tree)<-sXsppgrp.tree$PLT_CN # change row names to PLT_CN to make sure cbind works
relabund.tr<-sweep(sXsppgrp.tree[,5:18],1,rowSums(sXsppgrp.tree[,5:18]),'/')
table(rowSums(relabund.tr)) #all =1, so relative abundance worked
relabund.tr<-cbind(sXsppgrp.tree[,1:4], relabund.tr)

# For basal area
melt.trBA.spgrp<-melt(tree.spgrp, id=c('PLT_CN','INVYR','X_Coord','Y_Coord','SPGRP'),measure.vars="BA") #site by species matrics for seedlings/4 microplots
head(melt.trBA.spgrp)
sXsppgrp.treeBA<-dcast(melt.trBA.spgrp, PLT_CN+INVYR+X_Coord+Y_Coord~SPGRP, fun.aggregated=sum)
head(sXsppgrp.treeBA)
sXsppgrp.treeBA[,5:18][is.na(sXsppgrp.treeBA[,5:18])]<-0

rownames(sXsppgrp.treeBA)<-sXsppgrp.treeBA$PLT_CN # change row names to PLT_CN to make sure cbind works
relabund.trBA<-sweep(sXsppgrp.treeBA[,5:18],1,rowSums(sXsppgrp.treeBA[,5:18]),'/')
table(rowSums(relabund.trBA)) #all =1, so relative abundance worked
relabund.trBA<-cbind(sXsppgrp.treeBA[,1:4], relabund.trBA)

# Convert dataframe to shapefile
coordinates(relabund.trBA)<-~X_Coord+Y_Coord
proj4string(relabund.trBA)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
file<-'C:/temp/GIS'
writeOGR(relabund.trBA, dsn=file,layer="relabund_trees_BA",driver="ESRI Shapefile", overwrite_layer=T)
names(relabund.trBA)

# Create rasters from the relative abundance stem densities
tr <- raster(extent(relabund.tr))
res(tr)=20000 #20km= 20,000m
ACERUB <- rasterize(relabund.tr, field="ACERUB", fun=mean,background=NA, tr)
writeRaster(ACERUB, filename="../GIS/RELABU_TREE_ACERUB_20km.tif",format="GTiff", overwrite=T)

ACESAC <- rasterize(relabund.tr, field="ACESAC", fun=mean,background=NA, tr)
writeRaster(ACESAC, filename="../GIS/RELABU_TREE_ACESAC_20km.tif",format="GTiff", overwrite=T)

CARYA <- rasterize(relabund.tr, field="CARYA", fun=mean,background=NA, tr)
writeRaster(CARYA, filename="../GIS/RELABU_TREE_CARYA_20km.tif",format="GTiff", overwrite=T)

EARSUC <- rasterize(relabund.tr, field="EARSUC", fun=mean,background=NA, tr)
writeRaster(EARSUC, filename="../GIS/RELABU_TREE_EARSUC_20km.tif",format="GTiff", overwrite=T)

FAGGRA <- rasterize(relabund.tr, field="FAGGRA", fun=mean,background=NA, tr)
writeRaster(FAGGRA, filename="../GIS/RELABU_TREE_FAGGRA_20km.tif",format="GTiff", overwrite=T)

FRAX <- rasterize(relabund.tr, field="FRAX", fun=mean,background=NA, tr)
writeRaster(FRAX, filename="../GIS/RELABU_TREE_FRAX_20km.tif",format="GTiff", overwrite=T)

INVASIVE <- rasterize(relabund.tr, field="INVASIVE", fun=mean,background=NA, tr)
writeRaster(INVASIVE, filename="../GIS/RELABU_TREE_INVASIVE_20km.tif",format="GTiff", overwrite=T)

LIRTUL <- rasterize(relabund.tr, field="LIRTUL", fun=mean,background=NA, tr)
writeRaster(LIRTUL, filename="../GIS/RELABU_TREE_LIRTUL_20km.tif",format="GTiff", overwrite=T)

PICEA <- rasterize(relabund.tr, field="PICEA", fun=mean,background=NA, tr)
writeRaster(PICEA, filename="../GIS/RELABU_TREE_PICEA_20km.tif",format="GTiff", overwrite=T)

PINUS <- rasterize(relabund.tr, field="PINUS", fun=mean,background=NA, tr)
writeRaster(PINUS, filename="../GIS/RELABU_TREE_PINUS_20km.tif",format="GTiff", overwrite=T)

QUERCUS <- rasterize(relabund.tr, field="QUERCUS", fun=mean,background=NA, tr)
writeRaster(QUERCUS, filename="../GIS/RELABU_TREE_QUERCUS_20km.tif",format="GTiff", overwrite=T)

ULMUS <- rasterize(relabund.tr, field="ULMUS", fun=mean,background=NA, tr)
writeRaster(ULMUS, filename="../GIS/RELABU_TREE_ULMUS_20km.tif",format="GTiff", overwrite=T)

SHORT <- rasterize(relabund.tr, field="SHORT", fun=mean,background=NA, tr)
writeRaster(SHORT, filename="../GIS/RELABU_TREE_SHORT_20km.tif",format="GTiff", overwrite=T)

OTHERSPP <- rasterize(relabund.tr, field="OTHER_SPP", fun=mean,background=NA, tr)
writeRaster(OTHERSPP, filename="../GIS/RELABU_TREE_OTHERSPP_20km.tif",format="GTiff", overwrite=T)

#----------------------------
  # Make site by species matrix for tree BA
data.tr3<-left_join(data.tr2,REF_SPECIES, by="SPCD")
data.tr.melt.BA<-melt(data.tr3,id=c('PLT_CN','X_Coord','Y_Coord','SPECIES_SYMBOL'),measure.vars="BA_msq_ha")
sXspp.trBA<-dcast(data.tr.melt.BA, PLT_CN+X_Coord+Y_Coord~SPECIES_SYMBOL, fun.aggregated=sum)
sXspp.trBA[,4:186][is.na(sXspp.trBA[,4:186])]<-0 
nrow(sXspp.trBA) #29512

  # Make site by species matrix for tree stems/ha
data.tr3$numstem_ha<-(data.tr3$numstems*10000)/plotarea
data.tr.melt.TR<-melt(data.tr3,id=c('PLT_CN','X_Coord','Y_Coord','SPECIES_SYMBOL'),measure.vars="numstem_ha")
sXspp.tr<-dcast(data.tr.melt.TR, PLT_CN+X_Coord+Y_Coord~SPECIES_SYMBOL, fun.aggregated=sum)
nrow(sXspp.tr) #29512
sXspp.tr[,4:186][is.na(sXspp.tr[,4:186])]<-0 

  # Make site by species matrix for tree stems/full 4 subplots
data.tr.melt.m2<-melt(data.tr3,id=c('PLT_CN','X_Coord','Y_Coord','SPECIES_SYMBOL'),measure.vars="numstems")
sXspp.tr.m2<-dcast(data.tr.melt.m2, PLT_CN+X_Coord+Y_Coord~SPECIES_SYMBOL, fun.aggregated=sum)
nrow(sXspp.tr.m2) #29512
sXspp.tr.m2[,4:186][is.na(sXspp.tr.m2[,4:186])]<-0 

# creating site by species matrices that have the same species list
new.tr.sp <- names(sXspp.tr[, which(!names(sXspp.tr) %in% names(sXspp.seed) )]) # species on tree list but not in seedling list
new.tr.sp #16 species to add to seedling list
new.sd.sp<- names(sXspp.seed[, which(!names(sXspp.seed) %in% names(sXspp.tr) )]) # species on seedling list but not in tree
new.sd.sp #7 species to add to tree list

# Creating dataframes to hold the species that need to be added to each list
new.trees.df <- data.frame(matrix(0,nrow = nrow(sXspp.tr), ncol = length(new.sd.sp)));colnames(new.trees.df)<-new.sd.sp #creating new dataframe with new tree species
sXspp.tr.full<-as.data.frame(cbind(sXspp.tr,new.trees.df)) #binding the two dataframes
new.treesBA.df <- data.frame(matrix(0,nrow = nrow(sXspp.tr), ncol = length(new.sd.sp)));colnames(new.treesBA.df)<-new.sd.sp #creating new dataframe with new tree species
sXspp.trBA.full<-as.data.frame(cbind(sXspp.trBA,new.treesBA.df)) #binding the two dataframes
new.trees.m2.df <- data.frame(matrix(0,nrow = nrow(sXspp.tr), ncol = length(new.sd.sp)));colnames(new.trees.m2.df)<-new.sd.sp #creating new dataframe with new tree species
sXspp.tr.m2.full<-as.data.frame(cbind(sXspp.tr.m2,new.trees.m2.df)) #binding the two dataframes

new.seeds.df <- data.frame(matrix(0,nrow = nrow(sXspp.seed), ncol = length(new.tr.sp)));colnames(new.seeds.df)<-new.tr.sp # creating new dataframe for new seedling species
sXspp.sd.full<-as.data.frame(cbind(sXspp.seed,new.seeds.df)) # binding two dataframes for seedlings/ha
sXspp.sd.m2.full<-as.data.frame(cbind(sXspp.seed.m2,new.seeds.df)) #binding two dataframs for seedlings/4 subplots

#----------------
# Sorting the species columns, so both tree and seedling dataframes are in the same order
#----------------
  #---TREE STEMS  
ncol(sXspp.tr.full)
sp<-names(sXspp.tr.full[,4:193])#create list of species, then sort it 
sp2<-sort(sp, decreasing=F) 
sXspp.tr.full2<-sXspp.tr.full[,c("PLT_CN","X_Coord","Y_Coord",sp2)] # sort columns by the species list
sXspp.tr.full2<-sXspp.tr.full2[,-95] #removing the NA column
sXspp.tr.full2[is.na(sXspp.tr.full2)]<-0 # convert blanks to 0

  #---TREE BA  
sXspp.trBA.full2<-sXspp.trBA.full[,c("PLT_CN","X_Coord","Y_Coord",sp2)] # sort columns by the species list
sXspp.trBA.full2<-sXspp.trBA.full2[,-95] #removing the NA column
sXspp.trBA.full2[is.na(sXspp.trBA.full2)]<-0 # convert blanks to 0

  #---TREE stems in full 4 suplots m2  
sXspp.tr.m2.full2<-sXspp.tr.m2.full[,c("PLT_CN","X_Coord","Y_Coord",sp2)] # sort columns by the species list
sXspp.tr.m2.full2<-sXspp.tr.m2.full2[,-95] #removing the NA column
sXspp.tr.m2.full2[is.na(sXspp.tr.m2.full2)]<-0 # convert blanks to 0

  #---SEEDLINGS per ha
sp<-names(sXspp.sd.full[,4:193]) #create list of species, then sort it
sp2<-sort(sp, decreasing=F)
sXspp.sd.full2<-sXspp.sd.full[,c("PLT_CN","X_Coord","Y_Coord",sp2)] # sort columns by the species list
sXspp.sd.full2<-sXspp.sd.full2[,-95] #removing the NA column
sXspp.sd.full2[is.na(sXspp.sd.full2)]<-0  # convert blanks to 0

  #---SEEDLINGS per 4 microplots in m2
sp<-names(sXspp.sd.m2.full[,4:193]) #create list of species, then sort it
sp2<-sort(sp, decreasing=F)
sXspp.sd.m2.full2<-sXspp.sd.m2.full[,c("PLT_CN","X_Coord","Y_Coord",sp2)] # sort columns by the species list
sXspp.sd.m2.full2<-sXspp.sd.m2.full2[,-95] #removing the NA column
sXspp.sd.m2.full2[is.na(sXspp.sd.m2.full2)]<-0  # convert blanks to 0
# Tree stems/ha, tree stems/4 subplots, Tree BA, seedlings/ha and seedlings/4microplots in m2 all have the same fields and order, now need to remove sites that either don't have any seedlings or don't have any trees.

#--------------
# Removing plots with no seedlings or no trees, and matching up the plot list 
  #---Trees/ha
sXspp.tr.full3<-sXspp.tr.full2
sXspp.tr.full2$tottrs<-rowSums(sXspp.tr.full2[,4:192]) #have to remove plots that don't have any seedlings or trees
summary(sXspp.tr.full2$tottrs)  
  #---Tree BA
sXspp.trBA.full3<-sXspp.trBA.full2
sXspp.trBA.full2$tottrs<-rowSums(sXspp.trBA.full2[,4:192]) #have to remove plots that don't have any seedlings or trees
summary(sXspp.trBA.full2$tottrs)  
  #---Trees/4 subplots
sXspp.tr.m2.full3<-sXspp.tr.m2.full2
sXspp.tr.m2.full2$tottrs<-rowSums(sXspp.tr.m2.full2[,4:192]) #have to remove plots that don't have any seedlings or trees
summary(sXspp.tr.m2.full2$tottrs)  # median 26
  #---Seedlings/ha
sXspp.sd.full3<-sXspp.sd.full2
sXspp.sd.full2$totsds<-rowSums(sXspp.sd.full2[,4:192]) #have to remove plots that don't have any seedlings or trees
summary(sXspp.sd.full2$totsds)  
  #---Seedlings/4 microplots
sXspp.sd.m2.full3<-sXspp.sd.m2.full2
sXspp.sd.m2.full2$totsds<-rowSums(sXspp.sd.m2.full2[,4:192]) #have to remove plots that don't have any seedlings or trees
summary(sXspp.sd.m2.full2$totsds)  #median 24

sites_no_trees<-sXspp.tr.full2[which(sXspp.tr.full2$tottrs==0),"PLT_CN"] 
length(sites_no_trees) 
sites_no_regen<-sXspp.sd.full2[which(sXspp.sd.full2$totsds==0),"PLT_CN"]
length(sites_no_regen)

sites_to_drop<-unique(append(sites_no_trees,sites_no_regen)) #append list of plots from 0 trees and 0 seedlings together

sXspp.tr.final<-sXspp.tr.full3[!sXspp.tr.full3$PLT_CN %in% sites_to_drop,]
sXspp.trBA.final<-sXspp.tr.full3[!sXspp.tr.full3$PLT_CN %in% sites_to_drop,]
sXspp.tr.m2.final<-sXspp.tr.m2.full3[!sXspp.tr.m2.full3$PLT_CN %in% sites_to_drop,]
sXspp.sd.final<-sXspp.sd.full3[!sXspp.sd.full3$PLT_CN %in% sites_to_drop,]
sXspp.sd.m2.final<-sXspp.sd.m2.full3[!sXspp.sd.m2.full3$PLT_CN %in% sites_to_drop,]

nrow(sXspp.tr.final);nrow(sXspp.trBA.final);nrow(sXspp.sd.final);nrow(sXspp.tr.m2.final);nrow(sXspp.sd.m2.final)
ncol(sXspp.tr.final);ncol(sXspp.trBA.final);ncol(sXspp.sd.final);ncol(sXspp.tr.m2.final);ncol(sXspp.sd.m2.final)

plt<-as.data.frame(cbind(sXspp.tr.final$PLT_CN,sXspp.trBA.final$PLT_CN,sXspp.sd.final$PLT_CN,sXspp.sd.m2.final$PLT_CN,sXspp.tr.m2.final$PLT_CN))
plt$check<-with(plt, ifelse(V1==V2 &V2==V3 &V3==V4 &V4==V5,0,1))
table(plt$check) #all 18420 records are 0, so plot order matches across the datasets

write.csv(sXspp.tr.final,"Tree_stemsha_siteXspp.csv")
write.csv(sXspp.tr.m2.final,"Tree_stems_m2_siteXspp.csv")
write.csv(sXspp.trBA.final, "Tree_BA_siteXspp.csv")
write.csv(sXspp.sd.final,"Seedling_stemsha_siteXspp.csv")
write.csv(sXspp.sd.m2.final,"Seedling_stems_m2_siteXspp.csv")
