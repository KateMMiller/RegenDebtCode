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
# Seedling Densities 
#----------------------------------
seeds<-read.csv('Seedlings_m2_with_pred_df.csv')[,-c(1)]
seeds$maple<-seeds$ACERUB+seeds$ACESAC
seeds$sqseeds<-sqrt(seeds$seds_m2)
seeds$oakhick<-seeds$QUERCUS+seeds$CARYA
seeds$inv.dc<-seeds$inv_sm_/100
seeds$cancov<-seeds$CANOPY_/100
seeds2<-subset(seeds,type=="train" ) # select points that are only in the training dataset
seeds.test<-subset(seeds, type='test')

# Run OLS model first
seed.lm<-lm(sqseeds~scale(prec_ex)+scale(tmax_ex)+scale(deer_den)+scale(inv.dc)+scale(hmod_ex) + scale(QUERCUS) + scale(FAGGRA) + scale(PINUS) + scale(STDSZCD) + scale(cancov), data = seeds2)

summary(seed.lm) #r2=0.0642
AIC(seed.lm) #8392
vif(seed.lm) # all are below 1.3

coordinates(seeds2)<-~X+Y # set X/Y coordinates as the coordinates for the dataset
coords<-coordinates(seeds2)

IDs<-row.names(as(seeds2,"data.frame"))
seeds_kd1<-dnearneigh(coords,d1=0, d2=10000, row.names=IDs)
seeds_kd1_w<-nb2listw(seeds_kd1,style='B', zero.policy=TRUE)
plot(seeds_kd1_w,coords)

car.mod<-spautolm(sqseeds~prec_ex+tmax_ex+inv.dc+hmod_ex + QUERCUS + FAGGRA + PINUS + STDSZCD + cancov +deer_den, 
                  data = seeds2, listw=seeds_kd1_w, family = "CAR", method="eigen", verbose = TRUE,zero.policy = TRUE)

summary(car.mod, Nagelkerke=T, correlation=F) 

seeds2$fit<-car.mod$fit$fitted.values
seeds2$resid<-car.mod$fit$residuals
plot(seeds2$resid~seeds2$fit)
abline(lm(seeds2$resid~seeds2$fit), col='red')

# neighborhood 20km
IDs<-row.names(as(seeds2,"data.frame"))
seeds_kd20<-dnearneigh(coords,d1=0, d2=20000, row.names=IDs)
seeds_kd20_w<-nb2listw(seeds_kd20,style='B', zero.policy=TRUE)
plot(seeds_kd20_w,coords)

car.mod20<-spautolm(sqseeds~prec_ex+tmax_ex+inv.dc+hmod_ex + QUERCUS + FAGGRA + PINUS + STDSZCD + cancov+deer_den, 
                    data = seeds2,listw=seeds_kd20_w, family = "CAR", method="eigen", verbose = TRUE,zero.policy = TRUE)

summary(car.mod20, Nagelkerke=T, correlation=F) 
seeds2$fit20<-car.mod20$fit$fitted.values
seeds2$resid20<-car.mod20$fit$residuals

plot(seeds2$resid20~seeds2$fit20)
abline(lm(seeds2$resid20~seeds2$fit20), col='red')

# neighborhood 20km with row standardized weights instead of Binary
seeds_kd20w<-dnearneigh(coords,d1=0, d2=20000, row.names=IDs)
seeds_kd20w_w<-nb2listw(seeds_kd20w,style='W', zero.policy=TRUE)


car.mod20w<-spautolm(sqseeds~scale(prec_ex)+scale(tmax_ex)+scale(inv.dc)+scale(hmod_ex) + scale(QUERCUS) + scale(FAGGRA) + scale(PINUS) + scale(STDSZCD) + scale(cancov)+ scale(deer_den), 
                    data = seeds2,listw=seeds_kd20w_w, family = "CAR", method="eigen", verbose = TRUE,zero.policy = TRUE)

summary(car.mod20w, Nagelkerke=T) #0.1325
(car.mod20w$fit$imat) #10316

seeds2$fit20w<-car.mod20w$fit$fitted.values
seeds2$resid20w<-car.mod20w$fit$residuals

plot(seeds2$resid20~seeds2$fit20)
abline(lm(seeds2$resid20~seeds2$fit20), col='red')

#------------------------------
# Sapling Densities 
#------------------------------
saps<-read.csv('Saplings_m2_with_pred_df.csv')[,-c(1)]
saps$sqsaps<-sqrt(saps$saps_m2)
saps$maple<-saps$ACERUB+saps$ACESAC
saps$oakhick<-saps$QUERCUS+saps$CARYA
saps$inv.dc<-saps$inv_sm_/100
saps$cancov<-saps$CANOPY_/100
saps2<-subset(saps,type=="train" ) # select points that are only in the training dataset
saps.test<-subset(saps,type=='test')

# Run global model first
sap.lm<-lm(sqsaps~prec_ex+inv.dc+hmod_ex+maple+QUERCUS+FAGGRA+PINUS+STDSZCD+cancov+deer_den, data=saps2)
summary(sap.lm) #r2=0.128
AIC(sap.lm) #-8839
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

car.mod.sap<-spautolm(sqsaps~scale(prec_ex)+scale(inv.dc)+scale(hmod_ex)+scale(QUERCUS)+scale(maple)+scale(FAGGRA)+
    scale(PINUS)+scale(STDSZCD)+scale(cancov)+scale(deer_den), data = saps2, 
    listw=saps_kd20_w, family = "CAR", method="eigen", verbose = TRUE,zero.policy = TRUE)

summary(car.mod.sap, Nagelkerke=T) #0.1622, AIC=-9297
# took about 15 minutes to run

saps2$fit<-car.mod.sap$fit$fitted.values
saps2$resid<-car.mod.sap$fit$residuals

plot(saps2$resid~saps2$fit)
abline(lm(saps2$resid~saps2$fit), col='red')

