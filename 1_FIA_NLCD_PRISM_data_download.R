#-----------------------------------------------------
# STEP 1: Download FIA data tables for analysls, NLCD impervious and PRISM data
#-----------------------------------------------------
setwd('C:/temp/FIA_Data/raw_data')
options("scipen"=100, "digits"=22) #Change settings to that CN numbers aren't converted to scientific and digits aren't lost.

# FILES DOWNLOADED ON 11/21/17
# Download PLOTSNAP table by state directly from FIA website
states=c('CT','DE','KY','MA','MD','ME','NC','NH','NJ','NY','OH','PA','RI','SC','TN','VA','VT','WV')
for(i in 1:length(states)){
  state=states[i]
  j=assign(paste(state,'.PLOTSNAP',sep=""),read.csv(paste('http://apps.fs.usda.gov/fia/datamart/CSV/',state,'_PLOTSNAP.CSV',sep='')))
  print(c(i, nrow(j), ncol(j))) #all nrow match # records in table on FIA website, and have same # columns
}

all.PLOTSNAP=rbind(CT.PLOTSNAP,DE.PLOTSNAP,KY.PLOTSNAP,MA.PLOTSNAP,MD.PLOTSNAP,ME.PLOTSNAP,NC.PLOTSNAP,NH.PLOTSNAP,NJ.PLOTSNAP,NY.PLOTSNAP,OH.PLOTSNAP,PA.PLOTSNAP,RI.PLOTSNAP,SC.PLOTSNAP,TN.PLOTSNAP,VA.PLOTSNAP,VT.PLOTSNAP,WV.PLOTSNAP )
table(all.PLOTSNAP$STATECD,all.PLOTSNAP$INVYR)
nrow(all.PLOTSNAP)
write.csv(all.PLOTSNAP, 'all_states_PLOTSNAP.csv')
rm(list=ls())

# Download PLOT table by state directly from FIA website for invasive data
states=c('CT','DE','KY','MA','MD','ME','NC','NH','NJ','NY','OH','PA','RI','SC','TN','VA','VT','WV')
for(i in 1:length(states)){
  state=states[i]
  j=assign(paste(state,'.PLOT',sep=""),read.csv(paste('http://apps.fs.usda.gov/fia/datamart/CSV/',state,'_PLOT.CSV',sep='')))
  print(c(i, nrow(j), ncol(j))) #all nrow match # records in table on FIA website, and have same # columns
}

all.PLOT=rbind(CT.PLOT,DE.PLOT,KY.PLOT,MA.PLOT,MD.PLOT,ME.PLOT,NC.PLOT,NH.PLOT,NJ.PLOT,NY.PLOT,OH.PLOT,PA.PLOT,RI.PLOT,SC.PLOT,TN.PLOT,VA.PLOT,VT.PLOT,WV.PLOT )
table(all.PLOT$STATECD,all.PLOT$INVYR)
nrow(all.PLOT)
write.csv(all.PLOT, 'all_states_PLOT.csv')
rm(list=ls())

#Download COND table by state
states=c('CT','DE','KY','MA','MD','ME','NC','NH','NJ','NY','OH','PA','RI','SC','TN','VA','VT','WV')
for(i in 1:length(states)){
  state=states[i]
  j=assign(paste(state,'.COND',sep=''),read.csv(paste('http://apps.fs.usda.gov/fia/datamart/CSV/',state,'_COND.CSV',sep='')))
  print(c(i, nrow(j), ncol(j))) #all nrow match # records in table on FIA website, and have same # columns
}
all.COND=rbind(CT.COND,DE.COND,KY.COND,MA.COND,MD.COND,ME.COND,NC.COND,NH.COND,
                    NJ.COND,NY.COND,OH.COND,PA.COND,RI.COND,SC.COND,TN.COND,VA.COND,VT.COND,
                    WV.COND )
table(all.COND$STATECD,all.COND$INVYR)
nrow(COND)
write.csv(all.COND, "all_states_COND.csv")
rm(list=ls())

#Download SUBPLOT table by state
states=c('CT','DE','KY','MA','MD','ME','NC','NH','NJ','NY','OH','PA','RI','SC','TN','VA','VT','WV')
for(i in 1:length(states)){
  state=states[i]
  j=assign(paste(state,'.SUBPLOT',sep=''),read.csv(paste('http://apps.fs.usda.gov/fia/datamart/CSV/',state,'_SUBPLOT.CSV',sep='')))
  print(c(i, nrow(j), ncol(j))) #all nrow match # records in table on FIA website, and have same # columns
}

all.SUBPLOT=rbind(CT.SUBPLOT,DE.SUBPLOT,KY.SUBPLOT,MA.SUBPLOT,MD.SUBPLOT,ME.SUBPLOT,NC.SUBPLOT,NH.SUBPLOT,NJ.SUBPLOT,
                  NY.SUBPLOT,OH.SUBPLOT,PA.SUBPLOT,RI.SUBPLOT,SC.SUBPLOT,TN.SUBPLOT,VA.SUBPLOT,VT.SUBPLOT,WV.SUBPLOT )
write.csv(all.SUBPLOT, 'all_states_SUBPLOT.csv')
table(all.SUBPLOT$STATECD,all.SUBPLOT$INVYR)

rm(list=ls())

#Download SEEDLING table by state
states=c('CT','DE','KY','MA','MD','ME','NC','NH','NJ','NY','OH','PA','RI','SC','TN','VA','VT','WV')
for(i in 1:length(states)){
  state=states[i]
  j=assign(paste(state,'.SEEDLING',sep=''),read.csv(paste('http://apps.fs.usda.gov/fia/datamart/CSV/',state,'_SEEDLING.CSV',sep='')))
  print(c(i, nrow(j), ncol(j))) #all nrow match # records in table on FIA website, and have same # columns
}
all.SEEDLING=rbind(CT.SEEDLING,DE.SEEDLING,KY.SEEDLING,MA.SEEDLING,MD.SEEDLING,ME.SEEDLING,NC.SEEDLING,NH.SEEDLING,
                    NJ.SEEDLING,NY.SEEDLING,OH.SEEDLING,PA.SEEDLING,RI.SEEDLING,SC.SEEDLING,TN.SEEDLING,VA.SEEDLING,VT.SEEDLING,
                    WV.SEEDLING)
table(all.SEEDLING$STATECD,all.SEEDLING$INVYR)
nrow(all.SEEDLING)
write.csv(all.SEEDLING, "all_states_SEEDLING.csv")
rm(list=ls())
table(seeds$STATECD,seeds$INVYR)

# Download PLOT_REGEN table by state directly from FIA website
states=c('CT','DE','KY','MA','MD','ME','NC','NH','NJ','NY','OH','PA','RI','SC','TN','VA','VT','WV')
for(i in 1:length(states)){
  state=states[i]
  j=assign(paste(state,'.PLOT_REGEN',sep=""),read.csv(paste('http://apps.fs.usda.gov/fia/datamart/CSV/',state,'_PLOT_REGEN.CSV',sep='')))
  print(c(i, nrow(j), ncol(j))) #all nrow match # records in table on FIA website, and have same # columns
}

all.PLOT_REGEN=rbind(CT.PLOT_REGEN,DE.PLOT_REGEN,KY.PLOT_REGEN,MA.PLOT_REGEN,MD.PLOT_REGEN,ME.PLOT_REGEN,NC.PLOT_REGEN,NH.PLOT_REGEN,NJ.PLOT_REGEN,NY.PLOT_REGEN,OH.PLOT_REGEN,PA.PLOT_REGEN,RI.PLOT_REGEN,SC.PLOT_REGEN,TN.PLOT_REGEN,VA.PLOT_REGEN,VT.PLOT_REGEN,WV.PLOT_REGEN )
table(all.PLOT_REGEN$STATECD,all.PLOT_REGEN$INVYR)
nrow(all.PLOT_REGEN)
write.csv(all.PLOT_REGEN, 'all_states_PLOT_REGEN.csv')
rm(list=ls())

#Download Invasive Subplot Species table by state
# NOTE THAT INVASIVE DATA ARE NOT POSTED ON FIA DATA MART FOR SOUTHERN RESEARCH STATION STATES (KY,NC,SC,TN,VA). THEY HAVE TO 
# BE REQUESTED FROM SRS STAFF AND ARE ONLY COMPATIBLE WITH THE LATEST METHODS IF MANUAL 6.0 WAS SAMPLED ON THAT PLOT.

states=c('CT','DE','KY','MA','MD','ME','NC','NH','NJ','NY','OH','PA','RI','SC','TN','VA','VT','WV')
for(i in 1:length(states)){
  state=states[i]
  j=assign(paste(state,'.SUBP_INV',sep=''),read.csv(paste('http://apps.fs.usda.gov/fia/datamart/CSV/',state,'_INVASIVE_SUBPLOT_SPP.CSV',sep='')))
  print(c(i, nrow(j), ncol(j))) #all nrow match # records in table on FIA website, and have same # columns
}
all.SUBP_INV=rbind(CT.SUBP_INV,DE.SUBP_INV,KY.SUBP_INV,MA.SUBP_INV,MD.SUBP_INV,ME.SUBP_INV,NC.SUBP_INV,NH.SUBP_INV,
                   NJ.SUBP_INV,NY.SUBP_INV,OH.SUBP_INV,PA.SUBP_INV,RI.SUBP_INV,SC.SUBP_INV,TN.SUBP_INV,VA.SUBP_INV,VT.SUBP_INV,
                   WV.SUBP_INV)
table(all.SUBP_INV$STATECD,all.SUBP_INV$INVYR)
nrow(all.SUBP_INV)
write.csv(all.SUBP_INV, "all_states_SUBP_INV.csv")
inv<-read.csv("all_states_SUBP_INV.csv")[,-17]
inv.srs<-read.csv('SRS_states_SUBP_INV.csv')[,-17] # data provided by SRS staff
inv2<-rbind(inv,inv.srs)
head(inv)
write.csv(inv2,"all_states_SUBP_INV_SRS.csv")

#rm(list=ls())

# TREE tables are too big to download all at once in a for loop. The code below downloads each state tree table individually. 
CT.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/CT_TREE.CSV')
nrow(CT.TREES)
write.csv(CT.TREES,"CT_TREES.csv")

DE.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/DE_TREE.CSV')
nrow(DE.TREES)
write.csv(DE.TREES,"DE_TREES.csv")

KY.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/KY_TREE.CSV')
nrow(KY.TREES)
write.csv(KY.TREES,"KY_TREES.csv")

MA.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/MA_TREE.CSV')
nrow(MA.TREES)
write.csv(MA.TREES,"MA_TREES.csv")

MD.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/MD_TREE.CSV')
nrow(MD.TREES)
write.csv(MD.TREES,"MD_TREES.csv")

ME.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/ME_TREE.CSV')
nrow(ME.TREES)
write.csv(ME.TREES,"ME_TREES.csv")

NC.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/NC_TREE.CSV')
nrow(NC.TREES)
write.csv(NC.TREES,"NC_TREES.csv")

NH.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/NH_TREE.CSV')
nrow(NH.TREES)
write.csv(NH.TREES,"NH_TREES.csv")

NJ.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/NJ_TREE.CSV')
nrow(NJ.TREES)
write.csv(NJ.TREES,"NJ_TREES.csv")

NY.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/NY_TREE.CSV')
nrow(NY.TREES)
write.csv(NY.TREES,"NY_TREES.csv")

OH.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/OH_TREE.CSV')
nrow(OH.TREES)
write.csv(OH.TREES,"OH_TREES.csv")

PA.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/PA_TREE.CSV')
nrow(PA.TREES)
write.csv(PA.TREES,"PA_TREES.csv")

RI.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/RI_TREE.CSV')
nrow(RI.TREES)
write.csv(RI.TREES,"RI_TREES.csv")

SC.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/SC_TREE.CSV')
nrow(SC.TREES)
write.csv(SC.TREES,"SC_TREES.csv")

TN.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/TN_TREE.CSV')
nrow(TN.TREES)
write.csv(TN.TREES,"TN_TREES.csv")

VA.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/VA_TREE.CSV')
nrow(VA.TREES)
write.csv(VA.TREES,"VA_TREES.csv")

VT.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/VT_TREE.CSV')
nrow(VT.TREES)
write.csv(VT.TREES,"VT_TREES.csv")

WV.TREES=read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/WV_TREE.CSV')
nrow(WV.TREES)
write.csv(WV.TREES,"WV_TREES.csv")

all.TREES=rbind(CT.TREES,DE.TREES,KY.TREES,MA.TREES,MD.TREES,ME.TREES,NC.TREES,NH.TREES,NJ.TREES,NY.TREES,OH.TREES,PA.TREES, RI.TREES,SC.TREES,
                TN.TREES,VA.TREES,VT.TREES,WV.TREES)
nrow(all.TREES)

table(all.TREES$STATECD,all.TREES$INVYR) # make sure the years by state make sense

write.csv(all.TREES, 'all_states_TREES.csv')

write.csv(read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/REF_SPECIES.csv'),"REF_SPECIES.csv")
write.csv(read.csv('http://apps.fs.usda.gov/fia/datamart/CSV/REF_PLANT_DICTIONARY.csv'),"REF_PLANT_DICTIONARY.csv")

#--------------------------------------------
# Create mask layer of US states in analysis 
#--------------------------------------------
setwd('C:/temp')
library(raster)
library(rgdal)
options("scipen"=100)
file<-'C:/temp/GIS'

# Creating a regional boundary to mask the kernel smooth
us<-getData('GADM', country='USA', level=2) #get the county-level US GIS layer
CRS.albers <- CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
USUTM<-spTransform(us, CRS=CRS.albers) #reprojecting from WGS84 to UTM albers
library(maptools)
View(USUTM)
states=c(7,8,9,18,20,22,21,30,31,33,34,36,39,40,41,43,46,47,49) #numbers associated with each state in usstates; Had to scale back states due to data availability

east.states<-USUTM[USUTM$ID_1 %in% states,]
east.states2<-east.states[east.states$ENGTYPE_2 !="Water body",] #remove water bodies from layer
east.union <- unionSpatialPolygons(east.states2, east.states2@data$ID_1)
east.uniondf <- SpatialPolygonsDataFrame(spTransform(east.union, CRS=CRS.albers),
                                         data.frame(US=names(east.union), row.names=names(east.union), stringsAsFactors=FALSE))
east.union2 <- unionSpatialPolygons(east.states2, east.states2@data$ISO) #dissolve the study area to 1 polygon for kernal mask later
east.mask <- SpatialPolygonsDataFrame(spTransform(east.union2, CRS=CRS.albers),
                                      data.frame(US=names(east.union2), row.names=names(east.union2), stringsAsFactors=FALSE))
writeOGR(east.mask, dsn=file, layer="East_States_diss_UTMalbers",driver="ESRI Shapefile", overwrite_layer=T)

# Create a regional boundary for states with invasive and deer browse data
nestates=c(7,8,9,20,22,21,30,31,33,36,39,40,46,49) #numbers associated with each state in usstates plus DC
ne.states<-USUTM[USUTM$ID_1 %in% nestates,]
ne.states2<-ne.states[ne.states$ENGTYPE_2 !="Water body",] #remove water bodies from layer
ne.union <- unionSpatialPolygons(ne.states2, ne.states2@data$ID_1)
ne.uniondf <- SpatialPolygonsDataFrame(spTransform(ne.union, CRS=CRS.albers),
                                         data.frame(US=names(ne.union), row.names=names(ne.union), stringsAsFactors=FALSE))
ne.union2 <- unionSpatialPolygons(ne.states2, ne.states2@data$ISO) #dissolve the study area to 1 polygon for kernal mask later
ne.mask <- SpatialPolygonsDataFrame(spTransform(ne.union2, CRS=CRS.albers),
                                      data.frame(US=names(ne.union2), row.names=names(ne.union2), stringsAsFactors=FALSE))
writeOGR(ne.mask, dsn=file, layer="Northeast_States_diss_UTMalbers",driver="ESRI Shapefile", overwrite_layer=T)

#--------------------------------------------
# Download NLCD iland cover layers and process for analysis
#--------------------------------------------
library(rgdal)
library(raster)
#library(beepr)
rasterOptions(timer=TRUE,progress='text', tmpdir='G:/raster_temp') #shows a timer for processes run from raster package
#rasterOptions(timer=TRUE,progress='text') #shows a timer for processes run from raster package

setwd('C:/temp')
file.path<-"C:/temp/GIS/nlcd_2011_landcover_2011_edition_2014_10_10.zip"

# Land cover raster
download.file('http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2011&FNAME=nlcd_2011_landcover_2011_edition_2014_10_10.zip', destfile=file.path)
zipLC<-paste(file, "nlcd_2011_landcover_2011_edition_2014_10_10.zip", sep="/")
unzip(zipLC,exdir=file)

# Read in shapefile to mask the raster file with
east.mask<-readOGR(dsn=file, layer="East_States_diss_UTMalbers")
plot(east.mask)

# Prepare NLCD for analysis
r<-raster('C:/temp/GIS/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img')
lc.mat<-matrix(c(10,13,0,20,25,1,30,53,0,70,83,1,89,96,0))
nlcd.bin<-reclassify(nlcd,lc.mat)
nlcd.bin2<-na.omit(nlcd.bin)
cellStats(nlcd.bin,max) #max is 1
cellStats(nlcd.bin,min) # min is 0
writeRaster(nlcd.bin, './GIS/nlcdbin.tif', format="GTiff")
nlcd.bin<-raster('./GIS/nlcdbin.tif')

# Aggregate to 300m resolution
nlcd300<-raster::aggregate(nlcd.bin, fact=10,fun=mean,na.rm=T)
writeRaster(nlcd300,'./GIS/nlcd300_hmod_preproj.tif',format="GTiff",overwrite=T) #save as backup

nlcd300<-raster('./GIS/nlcd300_hmod_preproj.tif')
mask.r3<-raster(extent(east.mask)) # create blank raster to use as clip in projectRaster
res(mask.r3)<-300
proj4string(mask.r3)<-CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
nlcd.utm3<-projectRaster(nlcd300, crs=CRS.albers, mask.r3)

nlcd.utm3<-raster('./GIS/nlcd300_utm_preclip.tif')
crop.nlcd3<-crop(nlcd.utm3,east.mask, snap='near')
mask.nlcd3<-mask(crop.utm3,east.mask, snap='near')
writeRaster(mask.nlcd3,'./GIS/nlcd300_hmod_clip.tif',format="GTiff",overwrite=T)

#---------------------------------------------
# Download PRISM climate data 
#---------------------------------------------
library(prism)
library(gdalUtils)
library(raster)
library(rgdal)
prismfile<-("C:/temp/GIS/prism")
setwd(prismfile)
options(prism.path = prismfile)

# Download climate data
get_prism_normals('ppt', "4km", annual = TRUE, mon=4:9, keepZip = FALSE) # monthly 30yr average precip for April to Sept
get_prism_normals('tmax','4km',annual=T,keepZip=FALSE) # 30 year average max temp
get_prism_annual('tmax', years = 1911:1982, keepZip=FALSE) #annual max temp. Note: prism package that requires 1982 to be the end year
get_prism_monthlys('ppt',years= 1911:1982, mon=4:9, keepZip=F) #monthly historic precip 

#------------------------------------
# Calculate 30year average precip during growing season for 1911-1940 to compare with 30year normals for 1981-2010

# Sum growing season precipitation by year for historic data
years<-1911:1940
for(i in 1:length(years)){
  year<-years[i]
  prism.fold<-paste("/PRISM_ppt_stable_4kmM2_",year,"_all_bil", sep="")
  prism.ppt<-list.files(path=paste("./",prism.fold,sep=""),"*.bil$")[5:10]
    loc<-paste(prismfile,prism.fold,"/",prism.ppt,sep="")
    r1<-raster(loc[1])
    r2<-raster(loc[2])
    r3<-raster(loc[3])
    r4<-raster(loc[4])
    r5<-raster(loc[5])
    r6<-raster(loc[6])
    ppt.s<-stack(r1,r2,r3,r4,r5,r6)
    sum.gr.ppt<-calc(ppt.s, sum, na.rm=T)
    writeRaster(sum.gr.ppt,paste("sum_ppt_",year,'.tif', sep=""),format="GTiff",overwrite=T)
      }

# Stack output of loop and average stack to come up with 30-year normal for growing season precip: 1911-1940
sum.files<-list.files('.', "\\.tif$")
sum.s<-stack(sum.files)
avg.gr.ppt.1140<-calc(sum.s,mean,na.rm=T)

# Average growing season precipation for the 30year normals data
months<-c("04","05","06","07","08","09")

for(i in 1:length(months)){
  month<-months[i]
  prism.fold[i]<-paste("/PRISM_ppt_30yr_normal_4kmM2_",month,"_bil", sep="")
  prism.ppt<-list.files(path=paste("./",prism.fold,sep=""),"*.bil$")
  loc2<-paste(prismfile,prism.fold,"/",prism.ppt,sep="")
  ar1<-raster(loc2[1])
  ar2<-raster(loc2[2])
  ar3<-raster(loc2[3])
  ar4<-raster(loc2[4])
  ar5<-raster(loc2[5])
  ar6<-raster(loc2[6])
  ppt.as<-stack(ar1,ar2,ar3,ar4,ar5,ar6)
  sum.gr.30yr.ppt<-calc(ppt.as, sum, na.rm=T)
  writeRaster(sum.gr.30yr.ppt,"avg_growing_season_ppt_30yr.tif",format="GTiff",overwrite=T)
}

# Convert average growing season precip. for 1911-1940 to UTM albers
CRS.albers <- CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
avg.gr.ppt.1140.utm<-projectRaster(avg.gr.ppt.1140, crs=CRS.albers) #convert data the UTM albers
writeRaster(avg.gr.ppt.1140.utm,"avg_growing_season_ppt_1911-40_utm.tif",format="GTiff",overwrite=T)

avg.gr.ppt.1140.utm<-raster("avg_growing_season_ppt_1911-40_utm.tif")

# Convert average growing season 30year normals for 1981-2010 to UTM albers
avg.gr.30yr.ppt.utm<-projectRaster(sum.gr.30yr.ppt, crs=CRS.albers) #convert data the UTM albers
writeRaster(avg.gr.30yr.ppt.utm,"avg_growing_season_ppt_30yr_utm.tif",format="GTiff",overwrite=T)

avg.gr.30yr.ppt.utm<-raster("avg_growing_season_ppt_30yr_utm.tif")

# Calculate the percent change between the two 30year periods
pct.ppt.change<-(avg.gr.30yr.ppt.utm-avg.gr.ppt.1140.utm)/(avg.gr.30yr.ppt.utm)
file<-"C:/temp/GIS"
east.mask<-readOGR(dsn=file, layer="East_States_diss_UTMalbers") #crop by the study region
#clip<-mask(pct.ppt.change,east.mask,snap="near")
clip.ppt<-mask(pct.ppt.change,east.mask)
clip.ppt2<-crop(clip.ppt,east.mask, snap='near')
plot(clip.ppt2)
writeRaster(clip,filename='../ave_pch_ppt_1911-1940_to_1980-2010_UTM.tif', format="GTiff", overwrite=T )

#---------------------------------------------
# Calculate average tmax for 1911-1940 period

# Average maximum temperature for 1911-1940 period by first selecting all of the files involved, each of which is in its own folder
tmax<-stack()
years<-1911:1940
for(i in 1:length(years)){
  year=years[i]  
  prism.fold[i]<-paste("/PRISM_tmax_stable_4kmM2_",year,"_all_bil", sep="")
  prism.tmax[i]<-list.files(path=paste("./",prism.fold[i],sep=""),"*.bil$")[1]
  loc.t<-paste(prismfile,prism.fold,"/",prism.tmax,sep="")}

tmax.s<-stack(loc.t) # stack the rasters in the loc.t list of files
avg.tmax.1140<-calc(tmax.s,mean,na.rm=T)

# Convert average tmax for 1911-1940 to UTM albers
CRS.albers <- CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
avg.tmax.1140.utm<-projectRaster(avg.tmax.1140, crs=CRS.albers) #convert data the UTM albers
writeRaster(avg.tmax.1140.utm,"avg_tmax_1911-40_utm.tif",format="GTiff",overwrite=T)

avg.tmax.1140.utm<-raster("avg_tmax_1911-40_utm.tif")

# Read in average tmax 30year normals for 1981-2010, and convert to UTM albers
avg.30yr.tmax<-raster("./PRISM_tmax_30yr_normal_4kmM2_annual_bil/PRISM_tmax_30yr_normal_4kmM2_annual_bil.bil")
avg.30yr.tmax.utm<-projectRaster(avg.30yr.tmax, crs=CRS.albers) #convert data the UTM albers
writeRaster(avg.30yr.tmax.utm,"avg_tmax_30yr_utm.tif",format="GTiff",overwrite=T)

avg.30yr.tmax.utm<-raster("avg_tmax_30yr_utm.tif")

# Calculate the percent change between the two 30year periods
pct.tmax.change<-(avg.30yr.tmax.utm-avg.tmax.1140.utm)/(avg.30yr.tmax.utm)
east.mask<-readOGR(dsn=file, layer="East_States_diss_UTMalbers") #crop by the study region
clip.tmax<-mask(pct.tmax.change,east.mask)
clip.t<-crop(clip.tmax,east.mask,snap="near")
writeRaster(clip.t,filename='../ave_pch_tmax_1911-1940_to_1980-2010_UTM.tif', format="GTiff", overwrite=T )

#--------------------------------
# Download county-level deer density data
#--------------------------------
library(rgdal)
filepath<-"C:/temp/GIS/deer_density_QDMA.zip"
file<-"C:/temp/GIS"
download.file('https://conservancy.umn.edu/bitstream/handle/11299/178246/deer_density_QDMA.zip?sequence=1&isAllowed=y', destfile=filepath)
zipDD<-paste(file, "deer_density_QDMA.zip", sep="/")
unzip(zipDD,exdir=file)

deer<-readOGR(dsn=file, layer="deer_density_aea")
CRS.albers <- CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
deer.proj<-spTransform(deer, CRS=CRS.albers) #reprojecting from WGS84 to UTM albers
writeOGR(deer.proj, dsn=file, layer="deer_density_UTMalbers", driver="ESRI Shapefile", overwrite_layer=T)

#--------------------------------
# Little Range Maps
#--------------------------------
# Range maps were downloaded from https://github.com/wpetry/USTreeAtlas and unzipped before running the processes below.
library(raster)
library(rgdal)
library(tools)

setwd("C:/temp/GIS/")
infile<-"C:/temp/GIS/Little_Maps/preproject"
outfile<-"C:/temp/GIS/Little_Maps/projected"

# Batch project shapefiles by first defining EPSG of 4267 and then converting to UTM Albers
CRS.albers <- CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 

file.names<-file_path_sans_ext(list.files(path=infile,"*.shp"))
file.names
for(i in 1:length(file.names)){
  spp<-file.names[i]
  spp1<-readOGR(dsn=infile, layer=spp)
  proj4string(spp1)<-CRS("+init=epsg:4267")
  spp.proj<-spTransform(spp1,CRS=CRS.albers)
  writeOGR(spp.proj, dsn=outfile,layer=spp,driver="ESRI Shapefile", overwrite_layer=T)
}

