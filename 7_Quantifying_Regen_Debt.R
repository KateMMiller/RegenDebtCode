#---------------------
# Quantifying regeneration debt based on thresholds for each regen metric
#---------------------
library(rgdal)
library(raster)
library(tidyverse)

setwd('C:/temp/GIS')

options("scipen"=100, "digits"=6)

file=('C:/temp/GIS')

# Seedling Density raster
seeds.r<-raster('Seedling_m2_kern10k.tif')
seed.den <- c(0, 0.25, 1,  0.25, 20, 0)
seed.debt <- reclassify(seeds.r, seed.den)

#Sapling density raster
saps.r<-raster('Sapling_m2_kern10k.tif')
sap.den <- c(0, 0.1, 1,  0.1, 20, 0)
sap.debt <- reclassify(saps.r, sap.den)

# Similarity rasters
seed.Hor<-raster('Tree_seedling_Hor_kern10k.tif')
sap.Hor<-raster('Tree_sapling_Hor_kern10k.tif')

beta<-c(0,0.2,1,0.2,1,0)
seed.Hor.debt<-reclassify(seed.Hor, beta)
sap.Hor.debt<-reclassify(sap.Hor, beta)

# Combine rasters to get overall debt score
regen_debt<-seed.debt+sap.debt+seed.Hor.debt+sap.Hor.debt
writeRaster(regen_debt, filename="Regen_debt_Hor_kern10k_25.tif",format="GTiff", overwrite=T)

# Summarize each debt score by area covered for full area
debt.tbl<-data.frame(table(regen_debt@data@values))
tot<-sum(debt.tbl$Freq,na.rm=T)
debt.tbl<-debt.tbl %>% mutate(Pct=Freq/tot*100)
colnames(debt.tbl)<-c('Debt_Level','Freq','Pct')
debt.tbl

# Summarize debt for analysis area
bbox<-readOGR(dsn=file, layer="Bounding_Box")
dbt.crop<-raster::crop(regen_debt,bbox, snap='in')

debt.tbl2<-data.frame(table(dbt.crop@data@values))
tot2<-sum(debt.tbl2$Freq,na.rm=T)

debt.tbl2<-debt.tbl2 %>% mutate(Pct=Freq/tot2*100)
colnames(debt.tbl2)<-c('Debt_Level','Freq','Pct')

debt.table<-cbind(debt.tbl,debt.tbl2[,2:3])
colnames(debt.table)<-c("Debt_Level","Full_Freq","Full_Pct","Analysis_Freq","Analysis_Pct")

write.csv(debt.table,'regen_debt_summaries.csv')

