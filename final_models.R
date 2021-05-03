##Load data files and packages
data<-read.csv("Data/compiled_data_0324.csv")
library(mgcv)
library(car)
library(qpcR)
library(tidyverse)
fil<-filter(data, Households>0)
#####LST Models

#Model for Building Density effect on LST
lst.model=lme(LST_mean~BD+X.canopy+log(river.distance..meters.)+Imp2+
                log(Income), data=data, random=~1|Borough,
            method="REML", na.action=na.exclude)
summary(lst.model)
anova(lst.model)

#Model for Pop. Density effect on LST
lm.lst<-lm(LST_mean~Households+BD, data=fil)
summary(lm.lst)

########NO2 Models

#Non-Spatial Model of NO2
no2.gam6<-gam(NO2_mean~s(BD)+s(X.canopy)+s(Income)+s(petrochem_distance)+
                s(road.distance..meters.), data=data, method="REML")
summary(no2.gam6)

#Spatial+ Model for Building density effect
BD_gam<- gam(BD~s(Lat*Long), data=data)
resid_BD<-residuals(BD_gam)  
income_gam<-gam(log(Income)~s(Lat*Long), data=data, na.action=na.exclude)
resid_income<-residuals(income_gam)
road_gam<-gam(log(road.distance..meters.)~s(Lat*Long), data=data)
resid_road<-residuals(road_gam)
canopy_gam<-gam(X.canopy~s(Lat*Long), data=data)
resid_canopy<-residuals(canopy_gam)

no2.gam11<-gam(NO2_mean~resid_BD+resid_income+resid_road+resid_canopy+s(Lat,Long), data=data,
               method="REML")
summary(no2.gam11)
anova(no2.gam11)

#Spatial+ model for Pop. density effect
House_gam<- gam(Households~s(Lat*Long), data=fil)
resid_House<-residuals(House_gam)
BD_gam<- gam(BD~s(Lat*Long), data=fil)
resid_BD<-residuals(BD_gam)

no2.gam.house<-gam(NO2_mean~s(resid_House)+s(resid_BD)+s(Lat,Long), data=fil,
                   method="REML")
summary(no2.gam.house)

##########GS Models

Boroughs<-as.factor(data$Borough)
#Building density effect on public GS access
model<-lme(Man_GS ~BD +log (Income),
           data=data, random=~1|Borough, method="REML", na.action=na.exclude)
summary(model)

#Building density effect on private GS access
yard.gam<-gam(yard.household~s(BD)+s(Income)+s(Boroughs, bs="re"), data=data, method="REML")
summary(yard.gam)

#Pop. density effect on public GS access
gam.gs<-gam(Man_GS~s(Households)+s(BD)+s(Boroughs, bs="re"), data=data, method="REML")
summary(gam.gs)

#Pop. density effect on private GS access
gam.gs3<-gam(yard.household~s(Households)+s(BD)+s(Boroughs, bs="re"), data=data, method="REML")
summary(gam.gs3)

#Models using buffer areas as a metric

#500m buffer for pop. density
gs.lm3<-lme(GS_500~Households, data=fil, random=~1|Borough, method="REML",
           na.action=na.exclude)
summary(gs.lm3)

#300m buffer for pop. density
gs.lm4<-lme(GS_300~Households, data=fil, random=~1|Borough, method="REML",
           na.action=na.exclude)
summary(gs.lm4)

#800m buffer for pop. density
gs.lm5<-lme(GS_800~Households, data=fil, random=~1|Borough, method="REML",
           na.action=na.exclude)
summary(gs.lm5)

#1000m buffer for pop. density
gs.lm6<-lme(GS_1000~Households, data=fil, random=~1|Borough, method="REML",
           na.action=na.exclude)
summary(gs.lm6)

#1000m buffer for building density
gs.lm7<-lme(GS_1000~BD+Income, data=data, random=~1|Borough, method="REML",
           na.action=na.exclude)
summary(gs.lm7)

#800m buffer for building density
gs.lm8<-lme(GS_800~BD+Income, data=data, random=~1|Borough, method="REML",
           na.action=na.exclude)
summary(gs.lm8)

#500m buffer for building density
gs.lm9<-lme(GS_500~BD+Income, data=data, random=~1|Borough, method="REML",
           na.action=na.exclude)
summary(gs.lm9)

#300m buffer for building density
gs.lm10<-lme(GS_300~BD+Income, data=data, random=~1|Borough, method="REML",
            na.action=na.exclude)
summary(gs.lm10)






