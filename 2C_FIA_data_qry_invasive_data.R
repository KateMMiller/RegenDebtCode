#--------------------------------------------
# STEP 2: R queries to compile FIA Tree and Seedling data for analysis
#--------------------------------------------
setwd('C:/temp/FIA_Data')
library(rgdal)
library(raster)
library(dplyr)

options("scipen"=100, "digits"=4)

#-------------------------------------#
# Compile the FIA data #
#-------------------------------------#
plots<-read.csv("FIA_plots_for_analysis.csv")[,-1]
PLOT<-read.csv("./raw_data/all_states_PLOT.csv")[,c("CN","INVYR","INVASIVE_SAMPLING_STATUS_CD")] # Read in the PLOTSNAP table taking only the necessary fields
names(PLOT)[names(PLOT)=="CN"]<-"PLT_CN" # Changed name so easier to merge with rest of tables, which use PLT_CN for the same field
SUBP<-read.csv('./raw_data/all_states_SUBPLOT.csv')[,c("CN","PLT_CN","INVYR","SUBP","SUBP_STATUS_CD","INVASIVE_SUBP_STATUS_CD")]
names(SUBP)[names(SUBP)=="CN"]<-"CN_SUBP" 
INV<-read.csv('./raw_data/all_states_SUBP_INV_SRS.csv')[,c("CN","PLT_CN","INVYR","SUBP","VEG_SPCD","COVER_PCT")]
names(INV)[names(INV)=="CN"]<-"CN_INV" 

names(INV)
plots2<-left_join(plots,PLOT,by=c("PLT_CN","INVYR"))
plots3<-left_join(plots2,SUBP, by=c("PLT_CN","INVYR"))
plots4<-left_join(plots3,INV,by=c("PLT_CN","INVYR","SUBP") )
names(plots4)

table(plots4$STATECD, plots4$INVASIVE_SUBP_STATUS_CD)

plots5<-plots4[which(plots4$INVASIVE_SAMPLING_STATUS_CD==1),] # subset to only include plots that were sampled for invasive species
plots6<-plots5[which(plots5$INVASIVE_SUBP_STATUS_CD==1|plots5$INVASIVE_SUBP_STATUS_CD==2),]
table(plots6$INVASIVE_SUBP_STATUS_CD)
plots6$COVER_PCT[is.na(plots6$COVER_PCT)]<-0
names(plots6)

table(plots6$STATECD,plots6$MANUAL) # SRS states only include data from Manual 6.0 or higher.

plots7<- plots6 %>% group_by(PLT_CN,INVYR,SUBP, X_Coord, Y_Coord) %>% summarise(SUBPPROP_UNADJ=first(SUBPPROP_UNADJ),subp.cover=sum(COVER_PCT)) 
plots8<- plots7 %>% group_by(PLT_CN,INVYR, X_Coord,Y_Coord) %>% summarise(SUBPROP_UNADJ=first(SUBPPROP_UNADJ),num.subp=length(SUBP),plot.cover=sum(subp.cover))
plots8$avecov<-plots8$plot.cover/plots8$num.subp
summary(plots8$avecov)

# Convert dataframe to shapefile
coordinates(plots8)<-~X_Coord+Y_Coord
proj4string(plots8)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
file<-'C:/temp/GIS'
writeOGR(plots8, dsn=file,layer="FIA_invasive_cover",driver="ESRI Shapefile", overwrite_layer=T)

names(plots8)
r <- raster(extent(plots8))
res(r)=20000 #20km= 20,000m
r <- rasterize(plots8, field="avecov", fun=mean,background=NA, r)
plot(r)
writeRaster(r, filename="../GIS/Ave_Cov_Invasive_20km.tif",format="GTiff", overwrite=T)



