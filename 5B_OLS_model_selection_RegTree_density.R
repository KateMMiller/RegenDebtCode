#--------------------------------------------
# Model Selection for regeneration densities
#--------------------------------------------
setwd("C:/temp/GIS")
library(AICcmodavg)
library(rpart)
library(rpart.plot)

options("scipen"=100, "digits"=4)

file=('C:/temp/GIS')
#-----------------------------
# Seedling Density Model Selection 
#-----------------------------
seeds<-read.csv('Seedlings_m2_with_pred_df.csv')[,-c(1)]
seeds2<-subset(seeds,type=="train" ) # select points that are only in the training dataset

seeds2$meso<-seeds2$ACERUB+seeds2$ACESAC+seeds2$FAGGRA
seeds2$maple<-seeds2$ACERUB+seeds2$ACESAC
seeds2$sqseeds<-sqrt(seeds2$seds_m2)
seeds2$oakhick<-seeds2$QUERCUS+seeds2$CARYA
seeds2$inv.dc<-seeds2$inv_sm_/100
seeds2$cancov<-seeds2$CANOPY_/100
head(seeds2)
names(seeds2)

# Model selection 
sdlm1<-lm(sqseeds~prec_ex*tmax_ex+inv_sm_*hmod_ex+hmod_ex*deer_den, data=seeds2) #Stressors only with interactions
sdlm2<-lm(sqseeds~prec_ex+tmax_ex+inv_sm_+hmod_ex+deer_den, data=seeds2)#Stressors only no interactions
sdlm3<-lm(sqseeds~prec_ex+inv_sm_+hmod_ex+deer_den,data=seeds2) # Stressor model without tmax
sdlm4<-lm(sqseeds~QUERCUS+CARYA+FAGGRA+maple+PINUS, data=seeds2)#Genus canopy composition only 
sdlm5<-lm(sqseeds~STDSZCD+CANOPY_,data=seeds2)# stand metrics only
sdlm6<-lm(sqseeds~prec_ex+tmax_ex+inv_sm_+hmod_ex+deer_den+STDSZCD+CANOPY_+oakhick+meso, data=seeds2)#stressors + grouped canopy composition 
sdlm7<-lm(sqseeds~prec_ex+tmax_ex+inv_sm_+hmod_ex+deer_den+STDSZCD+CANOPY_+QUERCUS+PINUS+CARYA, data=seeds2)#stressors + dry canopy composition 
sdlm8<-lm(sqseeds~prec_ex+tmax_ex+inv_sm_+hmod_ex+deer_den+STDSZCD+CANOPY_+FAGGRA+maple, data=seeds2)#stressors + mesic canopy composition 
sdlm9<-lm(sqseeds~prec_ex+tmax_ex+inv_sm_+hmod_ex+deer_den+STDSZCD+CANOPY_+FAGGRA+QUERCUS+PINUS, data=seeds2)#Most abundant genera 
sdlm10<-lm(sqseeds~prec_ex*tmax_ex+inv_sm_*deer_den+hmod_ex*deer_den+STDSZCD+CANOPY_+FAGGRA+QUERCUS+PINUS, data=seeds2)#Most abundant genera 

mods<-list(sdlm1,sdlm2,sdlm3,sdlm4,sdlm5,sdlm6,sdlm7,sdlm8, sdlm9, sdlm10)
aictab(mods)

#----------------------------------
# Sapling Density Model Selection
#----------------------------------
saps<-read.csv('Saplings_m2_with_pred_df.csv')[,-c(1)]
saps2<-subset(saps,type=="train" ) # select points that are only in the training dataset

saps2$meso<-saps2$ACERUB+saps2$ACESAC+saps2$FAGGRA
saps2$maple<-saps2$ACERUB+saps2$ACESAC
saps2$mapleBA<-saps2$ACERUB_+saps2$ACESAC_
saps2$sqsaps<-sqrt(saps2$saps_m2)
saps2$oakhick<-saps2$QUERCUS+saps2$CARYA
saps2$inv.dc<-saps2$inv_sm_/100
saps2$cancov<-saps2$CANOPY_/100

# Model selection 
saplm1<-lm(sqsaps~prec_ex*tmax_ex+inv_sm_*hmod_ex+hmod_ex*deer_den, data=saps2) #Stressors only with interactions
saplm2<-lm(sqsaps~prec_ex+tmax_ex+inv_sm_+hmod_ex+deer_den, data=saps2)#Stressors only no interactions
saplm3<-lm(sqsaps~prec_ex+inv_sm_+hmod_ex+deer_den, data=saps2) # Stressor model without tmax
saplm4<-lm(sqsaps~QUERCUS+CARYA+FAGGRA+maple+PINUS, data=saps2)#Genus canopy composition only 
saplm5<-lm(sqsaps~STDSZCD+CANOPY_,data=saps2)# stand metrics only
saplm6<-lm(sqsaps~prec_ex+inv_sm_+hmod_ex+deer_den+STDSZCD+CANOPY_+oakhick+meso, data=saps2)#stressors + grouped canopy composition 
saplm7<-lm(sqsaps~prec_ex+inv_sm_+hmod_ex+deer_den+STDSZCD+CANOPY_+QUERCUS+PINUS+CARYA, data=saps2)#stressors + dry canopy composition 
saplm8<-lm(sqsaps~prec_ex+inv_sm_+hmod_ex+deer_den+STDSZCD+CANOPY_+FAGGRA+maple, data=saps2)#stressors + mesic canopy composition 
saplm9<-lm(sqsaps~prec_ex+inv_sm_+hmod_ex+deer_den+STDSZCD+CANOPY_+FAGGRA+QUERCUS+PINUS+maple, data=saps2)#Most abundant genera 
saplm10<-lm(sqsaps~prec_ex+inv_sm_*deer_den+hmod_ex*deer_den+STDSZCD+CANOPY_+FAGGRA+QUERCUS+PINUS, data=saps2)#mod9 with interactions 

mods<-list(saplm1,saplm2,saplm3,saplm4,saplm5,saplm6,saplm7,saplm8, saplm9, saplm10)
aictab(mods)

#-----------------------------
# Seedling Density Regression Tree
#-----------------------------
seed.rt<-rpart(sqseeds~STDSZCD+cancov+CARYA+FAGGRA+INVASIVE+PINUS+QUERCUS+prec_ex+tmax_ex+hmod_ex+deer_den+maple+oakhick+inv.dc, data=seeds2, method='anova',control=rpart.control(cp=0.0001))
bestcp<-seed.rt$cptable[which.min(seed.rt$cptable[,"xerror"]),"CP"]
bestcp 
seed.rtp<-prune(seed.rt,cp=0.003)
prp(seed.rtp, type=1, extra=0,box.palette=c('#dadbd9','#acdaa0','#72c459','#50923a'),faclen=0,space=0.4,cex=0.8, uniform=T, compress=T, ycompress=T)

seed.pred<-predict(seed.rtp, seeds.test, type='vector')
seeds.test2<-cbind(seeds.test,seed.pred)
summary(lm(sqseeds~seed.pred, data=seeds.test2))

#-----------------------------
# Sapling Density Regression Tree
#-----------------------------
sap.rt<-rpart(sqsaps~STDSZCD+cancov+CARYA+FAGGRA+INVASIVE+PINUS+QUERCUS+prec_ex+tmax_ex+hmod_ex+deer_den+maple+oakhick+inv.dc, data=saps2, method='anova',control=rpart.control(cp=0.0001))
bestcp<-sap.rt$cptable[which.min(sap.rt$cptable[,"xerror"]),"CP"]
bestcp 
sap.rtp<-prune(sap.rt,cp=0.005)
prp(sap.rtp, type=1, extra=0,box.palette=c('#dadbd9','#acdaa0','#72c459','#50923a'),faclen=0,space=0.4,cex=0.8, uniform=T, compress=T, ycompress=T)

sap.pred<-predict(sap.rtp, saps.test, type='vector')
saps.test2<-cbind(saps.test,sap.pred)
summary(lm(sqsaps~sap.pred, data=saps.test2))

