#--------------------------------------------
# Model Selection for Regeneration Vs. Tree Similarity
#--------------------------------------------
setwd("C:/temp/GIS")
library(AICcmodavg)
library(car)
library(rpart)
library(rpart.plot)

options("scipen"=100, "digits"=4)

#-----------------------------
# Seedling vs. Tree Similarity Model Selection
#-----------------------------
seeds<-read.csv('Seedlings_m2_beta_with_pred_df.csv')[,-c(1)]
seeds2<-subset(seeds,type=="train" ) # select points that are only in the training dataset

seeds2$meso.tr<-seeds2$ACERUB+seeds2$ACESAC+seeds2$FAGGRA
seeds2$maple.tr<-seeds2$ACERUB+seeds2$ACESAC
seeds2$oakhick.tr<-seeds2$QUERCUS+seeds2$CARYA

seeds2$meso.sd<-seeds2$ACERUB_+seeds2$ACESAC_+seeds2$FAGGRA_
seeds2$maple.sd<-seeds2$ACERUB_+seeds2$ACESAC_
seeds2$oakhick.sd<-seeds2$QUERCUS_+seeds2$CARYA_s
seeds2$oakhick.sub<-seeds2$oakhick.tr-seeds2$oakhick.sd
seeds2$Sor.lgt<-logit(seeds2$Sor)
seeds2$Hor.lgt<-logit(seeds2$Hor)
seeds2$cancov<-seeds2$CANOPY_/100

# Model Selection: Seedling vs. Tree- Sorensen
names(seeds2)
Sor.sd.lm1<-lm(Sor.lgt~prec_ex*tmax_ex+inv_sm_*hmod_ex+hmod_ex*deer_den, data=seeds2) #Interactions, stressors only
Sor.sd.lm2<-lm(Sor.lgt~prec_ex+tmax_ex+inv_sm_+hmod_ex+deer_den, data=seeds2) #No interactions, stressors only
Sor.sd.lm3<-lm(Sor.lgt~prec_ex+tmax_ex+INVASIVE_+INVASIVE+hmod_ex+deer_den, data=seeds2) #No interactions, stressors only. Invasive tree spp.
Sor.sd.lm4<-lm(Sor.lgt~STDSZCD+cancov, data=seeds2) # Stand metrics only
Sor.sd.lm5<-lm(Sor.lgt~STDSZCD+cancov+INVASIVE+QUERCUS+FAGGRA+PINUS+SHORT+maple.tr+CARYA,data=seeds2) # tree RA only
Sor.sd.lm6<-lm(Sor.lgt~STDSZCD+cancov+INVASIVE_+QUERCUS_+FAGGRA_+PINUS_s+SHORT_s+maple.sd+CARYA_s, data=seeds2) # seedling RA only
Sor.sd.lm7<-lm(Sor.lgt~STDSZCD+cancov+INVASIVE_+QUERCUS+QUERCUS_+FAGGRA+FAGGRA_+PINUS+PINUS_s+SHORT_s+maple.tr+maple.sd+CARYA,data=seeds2) # combinations of strata
Sor.sd.lm8<-lm(Sor.lgt~STDSZCD+cancov+deer_den+INVASIVE_+QUERCUS+QUERCUS_+FAGGRA+FAGGRA_+PINUS+PINUS_s+SHORT_s+maple.sd,data=seeds2) # Chip away to simplify
Sor.sd.lm9<-lm(Sor.lgt~deer_den+INVASIVE_+QUERCUS+QUERCUS_+FAGGRA+FAGGRA_+PINUS+PINUS_s+SHORT_s+maple.sd, data=seeds2) #Does deer density improve?
Sor.sd.lm10<-lm(Sor.lgt~prec_ex+tmax_ex+STDSZCD+cancov+deer_den+hmod_ex+FAGGRA_+INVASIVE_+SHORT_s+maple.sd+QUERCUS_+PINUS_s+FAGGRA+QUERCUS+PINUS, data=seeds2)
 # Do stressors improve?

mods<-list(Sor.sd.lm1,Sor.sd.lm2,Sor.sd.lm3, Sor.sd.lm4, Sor.sd.lm5, Sor.sd.lm6, Sor.sd.lm7, Sor.sd.lm8, Sor.sd.lm9, Sor.sd.lm10)
aictab(mods)

# Model Selection: Seedling vs. Tree- Horn
names(seeds2)
Hor.sd.lm1<-lm(Hor.lgt~prec_ex*tmax_ex+inv_sm_*hmod_ex+hmod_ex*deer_den, data=seeds2) #Interactions, stresHors only
Hor.sd.lm2<-lm(Hor.lgt~prec_ex+tmax_ex+inv_sm_+hmod_ex+deer_den, data=seeds2) #No interactions, stresHors only
Hor.sd.lm3<-lm(Hor.lgt~prec_ex+tmax_ex+INVASIVE_+INVASIVE+hmod_ex+deer_den, data=seeds2) #No interactions, stressors only. Invasive tree spp.
Hor.sd.lm4<-lm(Hor.lgt~STDSZCD+cancov, data=seeds2) # Stand metrics only
Hor.sd.lm5<-lm(Hor.lgt~STDSZCD+cancov+INVASIVE+QUERCUS+FAGGRA+PINUS+SHORT+maple.tr+CARYA,data=seeds2) # tree RA only
Hor.sd.lm6<-lm(Hor.lgt~STDSZCD+cancov+INVASIVE_+QUERCUS_+FAGGRA_+PINUS_s+SHORT_s+maple.sd+CARYA_s, data=seeds2) # seedling RA only
Hor.sd.lm7<-lm(Hor.lgt~STDSZCD+cancov+deer_den+QUERCUS+QUERCUS_+FAGGRA+FAGGRA_+PINUS+PINUS_s+SHORT_s+maple.sd,data=seeds2) # combinations of strata
Hor.sd.lm8<-lm(Hor.lgt~deer_den+QUERCUS+QUERCUS_+FAGGRA+FAGGRA_+PINUS+PINUS_s+SHORT_s+maple.sd,data=seeds2) # Chip away to see if we can simplify
Hor.sd.lm9<-lm(Hor.lgt~tmax_ex+QUERCUS+QUERCUS_+FAGGRA+FAGGRA_+PINUS+PINUS_s+SHORT_s+maple.sd, data=seeds2) #Tmax instead of deer density?
Hor.sd.lm10<-lm(Hor.lgt~prec_ex+tmax_ex+hmod_ex+STDSZCD+cancov+deer_den+FAGGRA_+INVASIVE_+SHORT_s+maple.sd+QUERCUS_+PINUS_s+FAGGRA+QUERCUS+PINUS, data=seeds2)
# Do stressors improve?

mods<-list(Hor.sd.lm1,Hor.sd.lm2,Hor.sd.lm3, Hor.sd.lm4, Hor.sd.lm5, Hor.sd.lm6, Hor.sd.lm7, Hor.sd.lm8, Hor.sd.lm9, Hor.sd.lm10)
aictab(mods)

#-----------------------------
# Sapling vs. Tree Similarity Model Selection
#-----------------------------
saps<-read.csv(paste(file, 'Saplings_m2_beta_with_pred_df.csv',sep='/'))[,-c(1)]
saps2<-subset(saps,type=="train" ) # select points that are only in the training dataset

saps2$meso.tr<-saps2$ACERUB+saps2$ACESAC+saps2$FAGGRA
saps2$maple.tr<-saps2$ACERUB+saps2$ACESAC
saps2$oakhick.tr<-saps2$QUERCUS+saps2$CARYA

saps2$meso.sd<-saps2$ACERUB_+saps2$ACESAC_+saps2$FAGGRA_
saps2$maple.sap<-saps2$ACERUB_+saps2$ACESAC_
saps2$oakhick.sd<-saps2$QUERCUS_+saps2$CARYA_s
saps2$oakhick.sub<-saps2$oakhick.tr-saps2$oakhick.sd
saps2$Sor.lgt<-logit(saps2$Sor)
saps2$Hor.lgt<-logit(saps2$Hor)
saps2$cancov<-saps2$CANOPY_/100

# Model Selection: Sapling vs. Tree- Sorensen
Sor.sap.lm1<-lm(Sor.lgt~prec_ex*tmax_ex+inv_sm_*hmod_ex+hmod_ex*deer_den, data=saps2) #Interactions, stressors only
Sor.sap.lm2<-lm(Sor.lgt~prec_ex+tmax_ex+inv_sm_+hmod_ex+deer_den, data=saps2) #No interactions, stressors only
Sor.sap.lm3<-lm(Sor.lgt~prec_ex+tmax_ex+INVASIVE_+INVASIVE+hmod_ex+deer_den, data=saps2) #No interactions, stressors only. Invasive tree spp.
Sor.sap.lm4<-lm(Sor.lgt~STDSZCD+cancov, data=saps2) # Stand metrics only
Sor.sap.lm5<-lm(Sor.lgt~STDSZCD+cancov+INVASIVE+QUERCUS+FAGGRA+PINUS+SHORT+maple.tr+CARYA,data=saps2) # tree RA only
Sor.sap.lm6<-lm(Sor.lgt~STDSZCD+cancov+INVASIVE_+QUERCUS_+FAGGRA_+PINUS_s+SHORT_s+maple.sap+CARYA_s, data=saps2) # sapling RA only
Sor.sap.lm7<-lm(Sor.lgt~STDSZCD+cancov+tmax_ex+INVASIVE_+QUERCUS+QUERCUS_+FAGGRA+PINUS_s+SHORT_s+maple.sap+CARYA,data=saps2) # combinations of strata
Sor.sap.lm8<-lm(Sor.lgt~STDSZCD+cancov+deer_den+INVASIVE_+QUERCUS+QUERCUS_+FAGGRA+PINUS_s+SHORT_s+maple.sap+CARYA,data=saps2) # Chip away to simplify
Sor.sap.lm9<-lm(Sor.lgt~deer_den+INVASIVE_+QUERCUS+QUERCUS_+FAGGRA+PINUS_s+SHORT_s+maple.sap+CARYA, data=saps2) #Does deer density improve?
Sor.sap.lm10<-lm(Sor.lgt~prec_ex+tmax_ex+STDSZCD+cancov+deer_den+hmod_ex+INVASIVE_+QUERCUS+QUERCUS_+FAGGRA+PINUS_s+SHORT_s+maple.sap+CARYA, data=saps2)

# Do stressors improve?
mods<-list(Sor.sap.lm1,Sor.sap.lm2,Sor.sap.lm3, Sor.sap.lm4, Sor.sap.lm5, Sor.sap.lm6, Sor.sap.lm7, Sor.sap.lm8, Sor.sap.lm9, Sor.sap.lm10)
aictab(mods)

# Model Selection: Sapling vs. Tree- Horn
Hor.sap.lm1<-lm(Hor.lgt~prec_ex*tmax_ex+inv_sm_*hmod_ex+hmod_ex*deer_den, data=saps2) #Interactions, stressors only
Hor.sap.lm2<-lm(Hor.lgt~prec_ex+tmax_ex+inv_sm_+hmod_ex+deer_den, data=saps2) #No interactions, stresHors only
Hor.sap.lm3<-lm(Hor.lgt~prec_ex+tmax_ex+INVASIVE_+INVASIVE+hmod_ex+deer_den, data=saps2) #No interactions, stresHors only. Invasive tree spp.
Hor.sap.lm4<-lm(Hor.lgt~STDSZCD+cancov, data=saps2) # Stand metrics only
Hor.sap.lm5<-lm(Hor.lgt~STDSZCD+cancov+INVASIVE+QUERCUS+FAGGRA+PINUS+SHORT+maple.tr+CARYA,data=saps2) # tree RA only
Hor.sap.lm6<-lm(Hor.lgt~STDSZCD+cancov+INVASIVE_+QUERCUS_+FAGGRA_+PINUS_s+SHORT_s+maple.sap+CARYA_s, data=saps2) # sapling RA only
Hor.sap.lm7<-lm(Hor.lgt~STDSZCD+cancov+tmax_ex+INVASIVE_+FAGGRA+FAGGRA_+QUERCUS+QUERCUS_+SHORT_s+maple.sap+PINUS+PINUS_s+CARYA,data=saps2) # combinations of strata
Hor.sap.lm8<-lm(Hor.lgt~STDSZCD+cancov+deer_den+INVASIVE_+FAGGRA+FAGGRA_+QUERCUS+QUERCUS_+SHORT_s+maple.sap+PINUS+PINUS_s+CARYA,data=saps2) # Chip away to simplify
Hor.sap.lm9<-lm(Hor.lgt~deer_den+INVASIVE_+FAGGRA+FAGGRA_+QUERCUS+QUERCUS_+SHORT_s+maple.sap+PINUS+PINUS_s+CARYA, data=saps2) #Does deer density improve?
Hor.sap.lm10<-lm(Hor.lgt~prec_ex+tmax_ex+STDSZCD+cancov+deer_den+hmod_ex+INVASIVE_+FAGGRA+FAGGRA_+QUERCUS+QUERCUS_+SHORT_s+maple.sap+PINUS+PINUS_s+CARYA, data=saps2)# Do stressors improve?

mods<-list(Hor.sap.lm1,Hor.sap.lm2,Hor.sap.lm3, Hor.sap.lm4, Hor.sap.lm5, Hor.sap.lm6, Hor.sap.lm7, Hor.sap.lm8, Hor.sap.lm9, Hor.sap.lm10)
aictab(mods)

#----------------------------------
# Seedling Vs. Tree Similarity Regression Tree
#----------------------------------

# Seedling vs. Tree- Sorensen
seed.sor.rt<-rpart(Sor.lgt~STDSZCD+cancov+CARYA+FAGGRA+INVASIVE+PINUS+QUERCUS+maple.sd+CARYA_s+FAGGRA_+INVASIVE_+PINUS_s+QUERCUS_+SHORT_s+prec_ex+tmax_ex+hmod_ex+deer_den+maple.tr+inv.dc, data=seeds2, method='anova',control=rpart.control(cp=0.0001))
bestcp.sds<-seed.sor.rt$cptable[which.min(seed.sor.rt$cptable[,"xerror"]),"CP"]
bestcp.sds 
seed.sor.rtp<-prune(seed.sor.rt,cp=0.005)

prp(seed.sor.rtp, type=0, extra=0,box.palette=c('#dadbd9','#acdaa0','#72c459','#50923a'),
  cex=0.7,gap=4,ygap=4,space=0.1, nn.space=1.2,nspace=1,uniform=T, compress=T, ycompress=T, xcompact=T, ycompact=T, do.par=1)

seed.sor.pred<-predict(seed.sor.rtp, seedsim.test, type='vector')
seeds.test2<-cbind(seedsim.test,seed.sor.pred)
summary(lm(Sor.lgt~seed.sor.pred, data=seedsim.test)) 

# Seedling vs. Tree- Horn
seed.hor.rt<-rpart(Hor.lgt~STDSZCD+cancov+CARYA+FAGGRA+INVASIVE+PINUS+QUERCUS+maple.sd+CARYA_s+FAGGRA_+INVASIVE_+PINUS_s+QUERCUS_+SHORT_s+prec_ex+tmax_ex+hmod_ex+deer_den+maple.tr+inv.dc, data=seeds2, method='anova',control=rpart.control(cp=0.0001))
bestcp.sds<-seed.hor.rt$cptable[which.min(seed.hor.rt$cptable[,"xerror"]),"CP"]
bestcp.sds 
seed.hor.rtp<-prune(seed.hor.rt,cp=0.005)

prp(seed.hor.rtp, type=0, extra=0,box.palette=c('#dadbd9','#acdaa0','#72c459','#50923a'),
  cex=0.7,gap=4,ygap=4,space=0.1, nn.space=1.2,nspace=1,uniform=T, compress=T, ycompress=T, xcompact=T, ycompact=T, do.par=1)

seed.hor.pred<-predict(seed.hor.rtp, seedsim.test, type='vector')
seeds.test2<-cbind(seedsim.test,seed.hor.pred)
summary(lm(hor.lgt~seed.hor.pred, data=seedsim.test)) 

#----------------------------------
# Sapling Vs. Tree Similarity Regression Tree
#----------------------------------

# Sapling vs. Tree- Sorensen
sap.sor.rt<-rpart(Sor.lgt~STDSZCD+cancov+CARYA+FAGGRA+INVASIVE+PINUS+QUERCUS+maple.sap+CARYA_s+FAGGRA_+INVASIVE_+PINUS_s+QUERCUS_+SHORT_s+prec_ex+tmax_ex+hmod_ex+deer_den+maple.tr+inv.dc, data=sap2, method='anova',control=rpart.control(cp=0.0001))
bestcp.sps<-sap.sor.rt$cptable[which.min(sap.sor.rt$cptable[,"xerror"]),"CP"]
bestcp.sps 
sap.sor.rtp<-prune(sap.sor.rt,cp=0.007)
prp(sap.sor.rtp, type=0, extra=0,box.palette=c('#dadbd9','#acdaa0','#72c459','#50923a'),
  cex=0.8,gap=4,ygap=4,space=0.1, nn.space=1.2,nspace=1,uniform=T, compress=T, ycompress=T, xcompact=T, ycompact=T, do.par=1)

sap.sor.pred<-predict(sap.sor.rtp, sapsim.test, type='vector')
sapsim.test2<-cbind(sapsim.test,sap.sor.pred)
summary(lm(Sor.lgt~sap.sor.pred, data=sapsim.test2)) 

# Saplings vs. Tree- Horn
sap.hor.rt<-rpart(Hor.lgt~STDSZCD+cancov+CARYA+FAGGRA+INVASIVE+PINUS+QUERCUS+maple.sap+CARYA_s+FAGGRA_+INVASIVE_+PINUS_s+QUERCUS_+SHORT_s+prec_ex+tmax_ex+hmod_ex+deer_den+maple.tr+inv.dc, data=sapsim2, method='anova',control=rpart.control(cp=0.0001))
bestcp.sph<-sap.hor.rt$cptable[which.min(sap.hor.rt$cptable[,"xerror"]),"CP"]
bestcp.sph 
sap.hor.rtp<-prune(sap.hor.rt,cp=0.007)
prp(sap.hor.rtp, type=0, extra=0,box.palette=c('#dadbd9','#acdaa0','#72c459','#50923a'),shift=10,
  cex=0.6,space=0.1, uniform=T,gap=0.1, nspace=4,compress=T, ycompress=T, xcompact=T, ycompact=T, do.par=1, yshift=-1)

sap.hor.pred<-predict(sap.hor.rtp, sapsim.test, type='vector')
saps.test3<-cbind(sapsim.test2,sap.hor.pred)
summary(lm(Hor.lgt~sap.hor.pred, data=saps.test3)) 