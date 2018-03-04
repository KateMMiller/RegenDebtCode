#--------------------------------------------
# STEP ##: R queries to compile deer browse data for analysis
#--------------------------------------------
setwd('C:/temp/FIA_Data')
library(dplyr)
library(reshape)
library(reshape2)
options("scipen"=100, "digits"=4)

# Browse impact data are only available in states in the Northern Research Station. Before smoothing the data,
# the states outside of that region need to be removed from the masked regional shapefile

library(raster)
library(rgdal)
file<-'C:/temp/GIS'

# Creating a regional boundary to mask the kernel smooth
us<-getData('GADM', country='USA', level=2) #get the county-level US GIS layer
CRS.albers <- CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
USUTM<-spTransform(us, CRS=CRS.albers) #reprojecting from WGS84 to UTM albers
library(maptools)
usstates <- unionSpatialPolygons(USUTM, USUTM@data$ID_1)# dissolve to county-level to state level
states=c(7,8,20,22,21,30,31,33,36,39,40,46,49) #numbers associated with each state in usstates
neast.states<-USUTM[USUTM$ID_1 %in% states,]
neast.states2<-neast.states[neast.states$ENGTYPE_2 !="Water body",] #remove water bodies from layer
neast.union <- unionSpatialPolygons(neast.states2, neast.states2@data$ID_1)
neast.uniondf <- SpatialPolygonsDataFrame(spTransform(neast.union, CRS=CRS.albers),
                                         data.frame(US=names(neast.union), row.names=names(neast.union), stringsAsFactors=FALSE))
neast.union2 <- unionSpatialPolygons(neast.states2, neast.states2@data$ISO) #dissolve the study area to 1 polygon for kernal mask later
neast.mask <- SpatialPolygonsDataFrame(spTransform(neast.union2, CRS=CRS.albers),
                                      data.frame(US=names(neast.union2), row.names=names(neast.union2), stringsAsFactors=FALSE))
writeOGR(neast.mask, dsn=file, layer="Northeast_States_diss_UTMalbers",driver="ESRI Shapefile")

# Read in data compiled for analysis in step 2
plots<-read.csv("FIA_plots_for_analysis.csv")[,-1]
browse<-read.csv('./raw_data/all_states_PLOT_REGEN.csv')[,c("PLT_CN","INVYR","BROWSE_IMPACT")]
data<-left_join(plots, browse, by=c("PLT_CN","INVYR"))
data.check<-data[,c("PLT_CN", "INVYR")]
data.check2<-unique(data.check)
nrow(data.check) ; nrow(data.check2) #29512- Both are the same number of rows, so only 1 record per plot
data2<-data[!is.na(data$BROWSE_IMPACT),]
nrow(data2) #only 1962 points
data3<-data2[data2$BROWSE_IMPACT>1,]
table(data3$BROWSE_IMPACT)

# Convert dataframe to shapefile
coordinates(data3)<-~X_Coord+Y_Coord
proj4string(data3)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
file<-'C:/temp/GIS'
writeOGR(data3, dsn=file,layer="Browse_Impact_plots",driver="ESRI Shapefile", overwrite_layer=T)
r <- raster(extent(data3))
res(r)=20000 #20km= 20,000m
r <- rasterize(data3, field="BROWSE_IMPACT", fun=mean,background=NA, r)
plot(r)
writeRaster(r, filename="../GIS/Browse_Impact_20km.tif",format="GTiff", overwrite=T)
#------------------------
