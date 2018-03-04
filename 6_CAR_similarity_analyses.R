#-------------------
# Conditionally autoregressive regression
#-------------------
# Seedling densities
#-------------------
setwd('C:/temp/GIS')
library(rgdal)
library(spdep)
library(car)
library(rpart)
library(rpart.plot)
options("scipen"=100, "digits"=4)
file=('C:/temp/GIS')

#----------------------------------
# Seedling Similarity 
#----------------------------------
seeds<-read.csv('Seedlings_m2_beta_with_pred_df.csv')[,-c(1)]
seeds$maple.sd<-seeds$ACERUB_+seeds$ACESAC_
seeds$maple.tr<-seeds$ACERUB+seeds$ACESAC
seeds$inv.dc<-seeds$inv_sm_/100
seeds$cancov<-seeds$CANOPY_/100
seeds$Sor.lgt<-logit(seeds$Sor)
seeds$Hor.lgt<-logit(seeds$Hor)

seeds2<-subset(seeds,type=="train" ) # select points that are only in the training dataset
seeds.test<-subset(seeds, type=='test')

# OLS model for Sorenson similarity
Sor.lm<-lm(Sor.lgt~SHORT_s+maple.sd+QUERCUS+QUERCUS_+FAGGRA+FAGGRA_+PINUS+PINUS_s+INVASIVE_+deer_den, data=seeds2)
summary(Sor.lm) #r2=0.113
AIC(Sor.lm) #19325

coordinates(seeds2)<-~X+Y # set X/Y coordinates as the coordinates for the dataset
coords<-coordinates(seeds2)

IDs<-row.names(as(seeds2,"data.frame"))
Sor_kd20<-dnearneigh(coords,d1=0, d2=20000, row.names=IDs)
Sor_kd20w_w<-nb2listw(Sor_kd20,style='W', zero.policy=TRUE)

car.Sor.mod<-spautolm(Sor.lgt~scale(SHORT_s)+scale(maple.sd)+scale(QUERCUS)+scale(QUERCUS_)+scale(FAGGRA)+scale(FAGGRA_)+scale(PINUS)+
                        scale(PINUS_s)+scale(INVASIVE_)+scale(deer_den), 
                  data = seeds2, listw=Sor_kd20w_w, family = "CAR", method="eigen", verbose = TRUE,zero.policy = TRUE)

summary(car.Sor.mod, Nagelkerke=T) #0.1213

# OLS model for Horn similarity
Hor.lm<-lm(Hor.lgt~tmax_ex+SHORT_s+maple.sd+QUERCUS+QUERCUS_+FAGGRA+FAGGRA_+PINUS+PINUS_s, data=seeds2)
summary(Hor.lm) #r2=0.256
AIC(Hor.lm) #20966

coordinates(seeds2)<-~X+Y # set X/Y coordinates as the coordinates for the dataset
coords<-coordinates(seeds2)

IDs<-row.names(as(seeds2,"data.frame"))
Hor_kd20<-dnearneigh(coords,d1=0, d2=20000, row.names=IDs)
Hor_kd20w_w<-nb2listw(Hor_kd20,style='W', zero.policy=TRUE)

car.Hor2.mod<-spautolm(Hor.lgt~scale(tmax_ex)+scale(SHORT_s)+scale(maple.sd)+scale(QUERCUS)+scale(QUERCUS_)+scale(FAGGRA)+
    scale(FAGGRA_)+scale(PINUS)+scale(PINUS_s),data = seeds2, listw=Hor_kd20w_w, family = "CAR", method="eigen", 
    verbose = TRUE,zero.policy = TRUE)

summary(car.Hor2.mod, Nagelkerke=T) #0.2574

#------------------------------
# Sapling vs. Canopy Similarity 
#------------------------------
saps<-read.csv('Saplings_m2_beta_with_pred_df.csv')[,-c(1)]
saps$maple.sap<-saps$ACERUB_+saps$ACESAC_
saps$maple.tr<-saps$ACERUB+saps$ACESAC
saps$Sor.lgt<-logit(saps$Sor)
saps$Hor.lgt<-logit(saps$Hor)
saps$inv.dc<-saps$inv_sm_/100
saps$cancov<-saps$CANOPY_/100
saps2<-subset(saps,type=="train" ) # select points that are only in the training dataset
saps.test<-subset(saps, type="test")

# OLS model for Sorenson
sap.lm<-lm(Sor.lgt~tmax_ex+INVASIVE_+SHORT_s+QUERCUS+QUERCUS_+CARYA+maple.sap+FAGGRA+PINUS_s+STDSZCD+cancov, data=saps2)
summary(sap.lm) #r2=0.157
AIC(sap.lm) #18520
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(sap.lm)
par(mfrow=c(1,1))
vif(sap.lm) # all are below 2

coordinates(saps2)<-~X+Y # set X/Y coordinates as the coordinates for the dataset
coords<-coordinates(saps2)

# CAR model with 20km weights
IDs<-row.names(as(saps2,"data.frame"))
saps_kd20<-dnearneigh(coords,d1=0, d2=20000, row.names=IDs)
saps_kd20_w<-nb2listw(saps_kd20,style='W', zero.policy=TRUE)
plot(saps_kd20_w,coords)

car.Sor.sap<-spautolm(Sor.lgt~scale(tmax_ex)+scale(INVASIVE_)+scale(SHORT_s)+scale(QUERCUS)+scale(QUERCUS_)+scale(CARYA)+
    scale(maple.sap)+scale(FAGGRA)+scale(PINUS_s)+scale(STDSZCD)+scale(cancov),
    data=saps2, listw=saps_kd20_w, family = "CAR", method="eigen", verbose = TRUE,zero.policy = TRUE)

summary(car.Sor.sap, Nagelkerke=T, correlation=F) #0.1622, AIC=-9297

saps2$fit<-car.mod.sap$fit$fitted.values
saps2$resid<-car.mod.sap$fit$residuals

plot(saps2$resid~saps2$fit)
abline(lm(saps2$resid~saps2$fit), col='red')

# OLS model for Horn
Hor.sap.lm<-lm(Hor.lgt~tmax_ex+INVASIVE_+SHORT_s+QUERCUS+QUERCUS_+CARYA+maple.sap+FAGGRA+FAGGRA_+PINUS+PINUS_s+STDSZCD+cancov, data=saps2)
AIC(Hor.sap.lm) #20773

car.Hor.sap<-spautolm(Hor.lgt~scale(tmax_ex)+scale(INVASIVE_)+scale(SHORT_s)+scale(QUERCUS)+scale(QUERCUS_)+scale(CARYA)+
    scale(maple.sap)+scale(FAGGRA)+scale(FAGGRA_)+scale(PINUS)+scale(PINUS_s)+scale(STDSZCD)+scale(cancov), data=saps2,
    listw=saps_kd20_w, family = "CAR", method="eigen", verbose = TRUE,zero.policy = TRUE)

summary(car.Hor.sap, Nagelkerke=T, correlation=F) #0.1622, AIC=-9297

