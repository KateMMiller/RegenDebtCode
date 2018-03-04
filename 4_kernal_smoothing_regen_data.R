#---------------------------
# STEP 6: Kernel smoothing point data
#---------------------------
#----------------------------------
# Clipping climate and nlcd rasters to same extent as regen data
#----------------------------------
library(spatstat)
library(rgeos)
library(maptools)
library(raster)
library(rgdal)
setwd('C:/temp')
file<-'C:/temp/GIS'
rasterOptions(timer=TRUE,progress='text') #shows a timer for processes run from raster package
options("scipen"=100, "digits"=4)

east.mask<-readOGR(dsn=file, layer="East_States_diss_UTMalbers")
ne.mask<-readOGR(dsn=file, layer="Northeast_States_diss_UTMalbers")
bbox<-as.owin(east.mask)
bbox2<-as.owin(ne.mask)

# Stand Size Class 
szclass<-readOGR(dsn=file,layer="FIA_plots_for_analysis")
sz.bb<-as(szclass, 'ppp')
sz.ppp<-ppp(szclass$coords.x1,szclass$coords.x2,window=bbox, marks=szclass$STDSZCD)
kern<-Smooth.ppp(sz.ppp, kernel='gaussian', sigma=10000, scalekernel=T, edge=T, diggle=T)
kern.ras<-raster(kern)
proj4string(kern.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(kern.ras, filename="./GIS/STDSZCD_kern10k.tif",format="GTiff", overwrite=T)

# Canopy cover
can.ppp<-ppp(szclass$coords.x1,szclass$coords.x2,window=bbox, marks=szclass$CANOPY_)
kern<-Smooth.ppp(can.ppp, kernel='gaussian', sigma=10000, scalekernel=T, edge=T, diggle=T)
kern.ras<-raster(kern)
proj4string(kern.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(kern.ras, filename="./GIS/Canopy_cover_kern10k.tif",format="GTiff", overwrite=T)

# Pct max. temp. change from 1911:1940 to 1981:2010
tmax<-raster("./GIS/ave_pch_tmax_1911-1940_to_1980-2010_UTM.tif")
tmax2<-mask(tmax,east.mask)
writeRaster(tmax2, './GIS/Pct_Change_TMax.tif',format="GTiff",overwrite=T)
tmax.im<-as.im(tmax2)
kern.tmax<-Smooth(tmax.im, kernel='gaussian', sigma=10000, bleed=F, edge=T)
tmax.ras<-raster(kern.tmax)
writeRaster(tmax.ras, './GIS/tmax_kern10k.tif', format="GTiff", overwrite=T)

# Pct growing season precip change from 1911:1940 to 1981:2010
ppt<-raster("./GIS/ave_pch_ppt_1911-1940_to_1980-2010_UTM.tif")
ppt2<-mask(ppt,east.mask)
writeRaster(ppt2, './GIS/Pct_Change_Precip.tif',format="GTiff",overwrite=T)
ppt.im<-as.im(ppt2)
kern.ppt<-Smooth(ppt.im, kernel='gaussian',sigma=10000,bleed=F,edge=T)
ppt.ras<-raster(kern.ppt)
writeRaster(ppt.ras, './GIS/precip_kern10k.tif', format="GTiff", overwrite=T)

# Deer browse impacts
browse.shp<-readOGR(dsn=file,layer="Browse_Impact_plots")
br.bb<-as(browse.shp, 'ppp')
br.ppp<-ppp(browse.shp$coords.x1,browse.shp$coords.x2,window=bbox2, marks=browse.shp$BROWSE_)
kern<-Smooth.ppp(br.ppp, kernel='gaussian', sigma=10000, scalekernel=T, edge=T, diggle=T)
kern.ras<-raster(kern)
proj4string(kern.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(kern.ras, filename="./GIS/Browse_Impact_kern10k.tif",format="GTiff", overwrite=T)

# Invasive species cover
invcov.shp<-readOGR(dsn=file,layer="FIA_invasive_cover")
inv.bb<-as(invcov.shp, 'ppp')
inv.ppp<-ppp(invcov.shp$coords.x1,invcov.shp$coords.x2,window=bbox, marks=invcov.shp$avecov)
kern<-Smooth.ppp(inv.ppp, kernel='gaussian', sigma=10000, scalekernel=T, edge=T, diggle=T)
kern.ras<-raster(kern)
proj4string(kern.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(kern.ras, filename="./GIS/Invasive_Cover_kern10k.tif",format="GTiff", overwrite=T)

#-----------------------
# Kernel Smoothing on regeneration data
# Seedling density
numseeds<-readOGR(dsn=file,layer="numseedlings_albers")
sds.bb<-as(numseeds, 'ppp')
sds.ppp<-ppp(numseeds$coords.x1,numseeds$coords.x2,window=bbox, marks=numseeds$seds_m2)
#bw<-bw.smoothppp(sds.ppp) #12002.94
kern<-Smooth.ppp(sds.ppp, kernel='gaussian', sigma=10000, scalekernel=T, edge=T)
kern.ras<-raster(kern)
proj4string(kern.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(kern.ras, filename="./GIS/Seedling_m2_kern_opt10k.tif",format="GTiff", overwrite=T)
# Create polygon layer of very low seedling densities

numseeds.r<-raster("./GIS/Seedling_m2_kern10k.tif")
seeds.poly<-rasterToPolygons(numseeds.r,fun=function(x){x<=0.25}, n=16, na.rm=T, dissolve=T)# creates polygon of raster cells with <= 0.25seedlings/m2 for easier plotting.
plot(seeds.poly)
writeOGR(seeds.poly, dsn=file, layer="Seedlings_me_poly_p25", driver="ESRI Shapefile", overwrite_layer=T)

# Sapling density
numsaps<-readOGR(dsn=file,layer="numsaplings_albers")
saps.bb<-as(numsaps, 'ppp')
saps.ppp<-ppp(numsaps$coords.x1,numsaps$coords.x2,window=bbox, marks=numsaps$saps_m2)
kern<-Smooth.ppp(saps.ppp, kernel='gaussian', sigma=10000, scalekernel=T, edge=T)
kern.ras<-raster(kern)
proj4string(kern.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(kern.ras, filename="./GIS/Sapling_m2_kern10k.tif",format="GTiff", overwrite=T)

numsaps.r<-raster("./GIS/Sapling_m2_kern10k.tif")
saps.poly<-rasterToPolygons(numsaps.r,fun=function(x){x<=0.1}, n=16, na.rm=T, dissolve=T)# creates polygon of raster cells with <= 0.1 seedlings/m2 for easier plotting.
plot(saps.poly)
writeOGR(saps.poly, dsn=file, layer="Saplings_m2_poly_p10", driver="ESRI Shapefile", overwrite_layer=T)


# Beta diversity metrics for trees and seedlings
beta.shp<-readOGR(dsn=file, layer="Tree_seedling_stems_m2_beta")
beta.bb<-as(beta.shp,'ppp')
beta.ppp<-ppp(beta.shp$coords.x1,beta.shp$coords.x2,window=bbox, marks=beta.shp$Sor)
kern.sor<-Smooth.ppp(beta.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
sor.ras<-raster(kern.sor)
proj4string(sor.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(sor.ras, filename="./GIS/Tree_seedling_Sor_kern10k.tif",format="GTiff", overwrite=T)

beta.ppp<-ppp(beta.shp$coords.x1,beta.shp$coords.x2,window=bbox, marks=beta.shp$Jac)
kern.jac<-Smooth.ppp(beta.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
jac.ras<-raster(kern.jac)
proj4string(jac.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(jac.ras, filename="./GIS/Tree_seedling_Jac_kern10k.tif",format="GTiff", overwrite=T)

beta.ppp<-ppp(beta.shp$coords.x1,beta.shp$coords.x2,window=bbox, marks=beta.shp$Sim)
kern.sim<-Smooth.ppp(beta.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
sim.ras<-raster(kern.sim)
proj4string(sim.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(sim.ras, filename="./GIS/Tree_seedling_BSim_kern10k.tif",format="GTiff", overwrite=T)

beta.ppp<-ppp(beta.shp$coords.x1,beta.shp$coords.x2,window=bbox, marks=beta.shp$Hor)
kern.hor<-Smooth.ppp(beta.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
hor.ras<-raster(kern.hor)
proj4string(hor.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(hor.ras, filename="./GIS/Tree_seedling_Hor_kern10k.tif",format="GTiff", overwrite=T)

beta.ppp<-ppp(beta.shp$coords.x1,beta.shp$coords.x2,window=bbox, marks=beta.shp$Mor)
kern.mor<-Smooth.ppp(beta.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
mor.ras<-raster(kern.mor)
proj4string(mor.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(mor.ras, filename="./GIS/Tree_seedling_Mor_kern10k.tif",format="GTiff", overwrite=T)

# Beta diversity metrics for trees and saplings
beta.shp<-readOGR(dsn=file, layer="Tree_sapling_stems_m2_beta")
beta.bb<-as(beta.shp,'ppp')
beta.ppp<-ppp(beta.shp$coords.x1,beta.shp$coords.x2,window=bbox, marks=beta.shp$Sor)
kern.sor<-Smooth.ppp(beta.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
sor.ras<-raster(kern.sor)
proj4string(sor.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(sor.ras, filename="./GIS/Tree_sapling_Sor_kern10k.tif",format="GTiff", overwrite=T)

beta.ppp<-ppp(beta.shp$coords.x1,beta.shp$coords.x2,window=bbox, marks=beta.shp$Jac)
kern.jac<-Smooth.ppp(beta.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
jac.ras<-raster(kern.jac)
proj4string(jac.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(jac.ras, filename="./GIS/Tree_sapling_Jac_kern10k.tif",format="GTiff", overwrite=T)

beta.ppp<-ppp(beta.shp$coords.x1,beta.shp$coords.x2,window=bbox, marks=beta.shp$Sim)
kern.sim<-Smooth.ppp(beta.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
sim.ras<-raster(kern.sim)
proj4string(sim.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(sim.ras, filename="./GIS/Tree_sapling_BSim_kern10k.tif",format="GTiff", overwrite=T)

beta.ppp<-ppp(beta.shp$coords.x1,beta.shp$coords.x2,window=bbox, marks=beta.shp$Hor)
kern.hor<-Smooth.ppp(beta.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
hor.ras<-raster(kern.hor)
proj4string(hor.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(hor.ras, filename="./GIS/Tree_sapling_Hor_kern10k.tif",format="GTiff", overwrite=T)

beta.ppp<-ppp(beta.shp$coords.x1,beta.shp$coords.x2,window=bbox, marks=beta.shp$Mor)
kern.mor<-Smooth.ppp(beta.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
mor.ras<-raster(kern.mor)
proj4string(mor.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(mor.ras, filename="./GIS/Tree_sapling_Mor_kern10k.tif",format="GTiff", overwrite=T)

#-----------------------------------
# Relative abundance by tree, sapling and seedling strata
#-----------------------------------
# Tree strata
tree.shp<-readOGR(dsn=file, layer="relabund_trees")
head(tree.shp)
tr.bb<-as(tree.shp,'ppp')
names(tree.shp)

tree.shp$maple<-tree.shp$ACERUB+tree.shp$ACESAC
tree.shp$oakhick<-tree.shp$QUERCUS+tree.shp$CARYA
head(tree.shp)

maple.tr.ppp<-ppp(tree.shp$coords.x1,tree.shp$coords.x2,window=bbox, marks=tree.shp$maple)
kern.maple.tr<-Smooth.ppp(maple.tr.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
maple.ras<-raster(kern.maple.tr)
proj4string(maple.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(maple.ras, filename="./GIS/Tree_relabund_ACER_kern10k.tif",format="GTiff", overwrite=T)

oakhick.tr.ppp<-ppp(tree.shp$coords.x1,tree.shp$coords.x2,window=bbox, marks=tree.shp$oakhick)
kern.oakhick.tr<-Smooth.ppp(oakhick.tr.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
oakhick.ras<-raster(kern.oakhick.tr)
proj4string(oakhick.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(oakhick.ras, filename="./GIS/Tree_relabund_oakhick_kern10k.tif",format="GTiff", overwrite=T)

ACERUB.tr.ppp<-ppp(tree.shp$coords.x1,tree.shp$coords.x2,window=bbox, marks=tree.shp$ACERUB)
kern.ACERUB.tr<-Smooth.ppp(ACERUB.tr.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
ACERUB.ras<-raster(kern.ACERUB.tr)
proj4string(ACERUB.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(ACERUB.ras, filename="./GIS/Tree_relabund_ACERUB_kern10k.tif",format="GTiff", overwrite=T)

ACESAC.tr.ppp<-ppp(tree.shp$coords.x1,tree.shp$coords.x2,window=bbox, marks=tree.shp$ACESAC)
kern.ACESAC.tr<-Smooth.ppp(ACESAC.tr.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
ACESAC.ras<-raster(kern.ACESAC.tr)
proj4string(ACESAC.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(ACESAC.ras, filename="./GIS/Tree_relabund_ACESAC_kern10k.tif",format="GTiff", overwrite=T)

CARYA.tr.ppp<-ppp(tree.shp$coords.x1,tree.shp$coords.x2,window=bbox, marks=tree.shp$CARYA)
kern.CARYA.tr<-Smooth.ppp(CARYA.tr.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
CARYA.ras<-raster(kern.CARYA.tr)
proj4string(CARYA.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(CARYA.ras, filename="./GIS/Tree_relabund_CARYA_kern10k.tif",format="GTiff", overwrite=T)

FAGGRA.tr.ppp<-ppp(tree.shp$coords.x1,tree.shp$coords.x2,window=bbox, marks=tree.shp$FAGGRA)
kern.FAGGRA.tr<-Smooth.ppp(FAGGRA.tr.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
FAGGRA.ras<-raster(kern.FAGGRA.tr)
proj4string(FAGGRA.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(FAGGRA.ras, filename="./GIS/Tree_relabund_FAGGRA_kern10k.tif",format="GTiff", overwrite=T)

FRAX.tr.ppp<-ppp(tree.shp$coords.x1,tree.shp$coords.x2,window=bbox, marks=tree.shp$FRAX)
kern.FRAX.tr<-Smooth.ppp(FRAX.tr.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
FRAX.ras<-raster(kern.FRAX.tr)
proj4string(FRAX.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(FRAX.ras, filename="./GIS/Tree_relabund_FRAX_kern10k.tif",format="GTiff", overwrite=T)

PINUS.tr.ppp<-ppp(tree.shp$coords.x1,tree.shp$coords.x2,window=bbox, marks=tree.shp$PINUS)
kern.PINUS.tr<-Smooth.ppp(PINUS.tr.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
PINUS.ras<-raster(kern.PINUS.tr)
proj4string(PINUS.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(PINUS.ras, filename="./GIS/Tree_relabund_PINUS_kern10k.tif",format="GTiff", overwrite=T)

QUERCUS.tr.ppp<-ppp(tree.shp$coords.x1,tree.shp$coords.x2,window=bbox, marks=tree.shp$QUERCUS)
kern.QUERCUS.tr<-Smooth.ppp(QUERCUS.tr.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
QUERCUS.ras<-raster(kern.QUERCUS.tr)
proj4string(QUERCUS.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(QUERCUS.ras, filename="./GIS/Tree_relabund_QUERCUS_kern10k.tif",format="GTiff", overwrite=T)

SHORT.tr.ppp<-ppp(tree.shp$coords.x1,tree.shp$coords.x2,window=bbox, marks=tree.shp$SHORT)
kern.SHORT.tr<-Smooth.ppp(SHORT.tr.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
SHORT.ras<-raster(kern.SHORT.tr)
proj4string(SHORT.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(SHORT.ras, filename="./GIS/Tree_relabund_SHORT_kern10k.tif",format="GTiff", overwrite=T)

INVASIVE.tr.ppp<-ppp(tree.shp$coords.x1,tree.shp$coords.x2,window=bbox, marks=tree.shp$INVASIVE)
kern.INVASIVE.tr<-Smooth.ppp(INVASIVE.tr.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
INVASIVE.ras<-raster(kern.INVASIVE.tr)
proj4string(INVASIVE.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(INVASIVE.ras, filename="./GIS/Tree_relabund_INVASIVE_kern10k.tif",format="GTiff", overwrite=T)

# Sapling strata
saps.shp<-readOGR(dsn=file, layer="relabund_saplings")
head(saps.shp)
saps.bb<-as(saps.shp,'ppp')
names(saps.shp)

saps.shp$maple<-saps.shp$ACERUB+saps.shp$ACESAC
maple.sap.ppp<-ppp(saps.shp$coords.x1,saps.shp$coords.x2,window=bbox, marks=saps.shp$maple)
kern.maple.sap<-Smooth.ppp(maple.sap.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
maple.ras<-raster(kern.maple.sap)
proj4string(maple.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(maple.ras, filename="./GIS/Sapling_relabund_ACER_kern10k.tif",format="GTiff", overwrite=T)

ACERUB.sap.ppp<-ppp(saps.shp$coords.x1,saps.shp$coords.x2,window=bbox, marks=saps.shp$ACERUB)
kern.ACERUB.sap<-Smooth.ppp(ACERUB.sap.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
ACERUB.ras<-raster(kern.ACERUB.sap)
proj4string(ACERUB.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(ACERUB.ras, filename="./GIS/Sapling_relabund_ACERUB_kern10k.tif",format="GTiff", overwrite=T)

ACESAC.sap.ppp<-ppp(saps.shp$coords.x1,saps.shp$coords.x2,window=bbox, marks=saps.shp$ACESAC)
kern.ACESAC.sap<-Smooth.ppp(ACESAC.sap.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
ACESAC.ras<-raster(kern.ACESAC.sap)
proj4string(ACESAC.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(ACESAC.ras, filename="./GIS/Sapling_relabund_ACESAC_kern10k.tif",format="GTiff", overwrite=T)

CARYA.sap.ppp<-ppp(saps.shp$coords.x1,saps.shp$coords.x2,window=bbox, marks=saps.shp$CARYA)
kern.CARYA.sap<-Smooth.ppp(CARYA.sap.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
CARYA.ras<-raster(kern.CARYA.sap)
proj4string(CARYA.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(CARYA.ras, filename="./GIS/Sapling_relabund_CARYA_kern10k.tif",format="GTiff", overwrite=T)

FAGGRA.sap.ppp<-ppp(saps.shp$coords.x1,saps.shp$coords.x2,window=bbox, marks=saps.shp$FAGGRA)
kern.FAGGRA.sap<-Smooth.ppp(FAGGRA.sap.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
FAGGRA.ras<-raster(kern.FAGGRA.sap)
proj4string(FAGGRA.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(FAGGRA.ras, filename="./GIS/Sapling_relabund_FAGGRA_kern10k.tif",format="GTiff", overwrite=T)

FRAX.sap.ppp<-ppp(saps.shp$coords.x1,saps.shp$coords.x2,window=bbox, marks=saps.shp$FRAX)
kern.FRAX.sap<-Smooth.ppp(FRAX.sap.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
FRAX.ras<-raster(kern.FRAX.sap)
proj4string(FRAX.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(FRAX.ras, filename="./GIS/Sapling_relabund_FRAX_kern10k.tif",format="GTiff", overwrite=T)

PINUS.sap.ppp<-ppp(saps.shp$coords.x1,saps.shp$coords.x2,window=bbox, marks=saps.shp$PINUS)
kern.PINUS.sap<-Smooth.ppp(PINUS.sap.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
PINUS.ras<-raster(kern.PINUS.sap)
proj4string(PINUS.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(PINUS.ras, filename="./GIS/Sapling_relabund_PINUS_kern10k.tif",format="GTiff", overwrite=T)

QUERCUS.sap.ppp<-ppp(saps.shp$coords.x1,saps.shp$coords.x2,window=bbox, marks=saps.shp$QUERCUS)
kern.QUERCUS.sap<-Smooth.ppp(QUERCUS.sap.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
QUERCUS.ras<-raster(kern.QUERCUS.sap)
proj4string(QUERCUS.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(QUERCUS.ras, filename="./GIS/Sapling_relabund_QUERCUS_kern10k.tif",format="GTiff", overwrite=T)

SHORT.sap.ppp<-ppp(saps.shp$coords.x1,saps.shp$coords.x2,window=bbox, marks=saps.shp$SHORT)
kern.SHORT.sap<-Smooth.ppp(SHORT.sap.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
SHORT.ras<-raster(kern.SHORT.sap)
proj4string(SHORT.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(SHORT.ras, filename="./GIS/Sapling_relabund_SHORT_kern10k.tif",format="GTiff", overwrite=T)

INVASIVE.sap.ppp<-ppp(saps.shp$coords.x1,saps.shp$coords.x2,window=bbox, marks=saps.shp$INVASIVE)
kern.INVASIVE.sap<-Smooth.ppp(INVASIVE.sap.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
INVASIVE.ras<-raster(kern.INVASIVE.sap)
proj4string(INVASIVE.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(INVASIVE.ras, filename="./GIS/Sapling_relabund_INVASIVE_kern10k.tif",format="GTiff", overwrite=T)

# Seedling strata
seed.shp<-readOGR(dsn=file, layer="relabund_seedlings")
head(seed.shp)
seed.bb<-as(seed.shp,'ppp')
names(seed.shp)

seed.shp$maple<-seed.shp$ACERUB+seed.shp$ACESAC
maple.sd.ppp<-ppp(seed.shp$coords.x1,seed.shp$coords.x2,window=bbox, marks=seed.shp$maple)
kern.maple.sd<-Smooth.ppp(maple.sd.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
maple.ras<-raster(kern.maple.sd)
proj4string(maple.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(maple.ras, filename="./GIS/Seedling_relabund_ACER_kern10k.tif",format="GTiff", overwrite=T)

ACERUB.seed.ppp<-ppp(seed.shp$coords.x1,seed.shp$coords.x2,window=bbox, marks=seed.shp$ACERUB)
kern.ACERUB.seed<-Smooth.ppp(ACERUB.seed.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
ACERUB.ras<-raster(kern.ACERUB.seed)
proj4string(ACERUB.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(ACERUB.ras, filename="./GIS/Seedling_relabund_ACERUB_kern10k.tif",format="GTiff", overwrite=T)

ACESAC.seed.ppp<-ppp(seed.shp$coords.x1,seed.shp$coords.x2,window=bbox, marks=seed.shp$ACESAC)
kern.ACESAC.seed<-Smooth.ppp(ACESAC.seed.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
ACESAC.ras<-raster(kern.ACESAC.seed)
proj4string(ACESAC.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(ACESAC.ras, filename="./GIS/Seedling_relabund_ACESAC_kern10k.tif",format="GTiff", overwrite=T)

CARYA.seed.ppp<-ppp(seed.shp$coords.x1,seed.shp$coords.x2,window=bbox, marks=seed.shp$CARYA)
kern.CARYA.seed<-Smooth.ppp(CARYA.seed.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
CARYA.ras<-raster(kern.CARYA.seed)
proj4string(CARYA.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(CARYA.ras, filename="./GIS/Seedling_relabund_CARYA_kern10k.tif",format="GTiff", overwrite=T)

FAGGRA.seed.ppp<-ppp(seed.shp$coords.x1,seed.shp$coords.x2,window=bbox, marks=seed.shp$FAGGRA)
kern.FAGGRA.seed<-Smooth.ppp(FAGGRA.seed.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
FAGGRA.ras<-raster(kern.FAGGRA.seed)
proj4string(FAGGRA.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(FAGGRA.ras, filename="./GIS/Seedling_relabund_FAGGRA_kern10k.tif",format="GTiff", overwrite=T)

FRAX.seed.ppp<-ppp(seed.shp$coords.x1,seed.shp$coords.x2,window=bbox, marks=seed.shp$FRAX)
kern.FRAX.seed<-Smooth.ppp(FRAX.seed.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
FRAX.ras<-raster(kern.FRAX.seed)
proj4string(FRAX.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(FRAX.ras, filename="./GIS/Seedling_relabund_FRAX_kern10k.tif",format="GTiff", overwrite=T)

PINUS.seed.ppp<-ppp(seed.shp$coords.x1,seed.shp$coords.x2,window=bbox, marks=seed.shp$PINUS)
kern.PINUS.seed<-Smooth.ppp(PINUS.seed.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
PINUS.ras<-raster(kern.PINUS.seed)
proj4string(PINUS.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(PINUS.ras, filename="./GIS/Seedling_relabund_PINUS_kern10k.tif",format="GTiff", overwrite=T)

QUERCUS.seed.ppp<-ppp(seed.shp$coords.x1,seed.shp$coords.x2,window=bbox, marks=seed.shp$QUERCUS)
kern.QUERCUS.seed<-Smooth.ppp(QUERCUS.seed.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
QUERCUS.ras<-raster(kern.QUERCUS.seed)
proj4string(QUERCUS.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(QUERCUS.ras, filename="./GIS/Seedling_relabund_QUERCUS_kern10k.tif",format="GTiff", overwrite=T)

SHORT.seed.ppp<-ppp(seed.shp$coords.x1,seed.shp$coords.x2,window=bbox, marks=seed.shp$SHORT)
kern.SHORT.seed<-Smooth.ppp(SHORT.seed.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
SHORT.ras<-raster(kern.SHORT.seed)
proj4string(SHORT.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(SHORT.ras, filename="./GIS/Seedling_relabund_SHORT_kern10k.tif",format="GTiff", overwrite=T)

INVASIVE.seed.ppp<-ppp(seed.shp$coords.x1,seed.shp$coords.x2,window=bbox, marks=seed.shp$INVASIVE)
kern.INVASIVE.seed<-Smooth.ppp(INVASIVE.seed.ppp, kernel='gaussian',sigma=10000,scalekernel=T,edge=T)
INVASIVE.ras<-raster(kern.INVASIVE.seed)
proj4string(INVASIVE.ras)=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
writeRaster(INVASIVE.ras, filename="./GIS/Seedling_relabund_INVASIVE_kern10k.tif",format="GTiff", overwrite=T)
