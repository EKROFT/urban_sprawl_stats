data<-read.csv("Data/shortlist_data_0813.csv")
View(data)

data2<-read.csv("Data/compiled_data_0813.csv")
#View(data2)

###################LST#######################
LST.model<-lm(data$LST_mean~data$X.canopy+data$Imp.+data$BD+data$Income+data$river.distance..meters.)
summary(LST.model)
plot(LST.model)

LST2.model<-lm(data2$LST_mean~data2$X.canopy+data2$Imp.+data2$BD+data2$Income+data2$river.distance..meters.)
summary(LST2.model)
plot(LST2.model)

##the diagnostic plots actually look good here, a linear model could work well
##worked with both shortlist and longlist data
##all variables included had significant p values, removing any weakened the model

##testing out some possible interactions:

LST3.model<-lm(data$LST_mean~(data$X.canopy*data$Imp.)+data$BD+data$Income+data$river.distance..meters.)
summary(LST3.model)
##the interaction is not significant, makes the model weaker

library(mgcv)
Lst.gam<-gam(LST_mean~X.canopy+Imp.+s(BD)+Income+
               s(river.distance..meters.), data=data,
             method="REML")
summary(Lst.gam)
gam.check(Lst.gam)
plot(Lst.gam)
##gam model isn't really better than linear model (adj. r2 were 0.01 different)
##according to gam, the terms all either linear or nearly linear

##############################NO2######################
NO2.model<-lm(data$NO2_mean~data$Road.+data$Income+data$petrochem_distance+data$BD)
summary(NO2.model)
plot(NO2.model)
##diagnostics don't look so great
##log transform didn't help, neither did sqrt
##including canopy cover weakened the model
##inclduing impervious cover made no difference to the model
##inclduing NDVI weakened the model

NO2.ranked<-rank(data$NO2_mean)
ranked.data<-data.frame(NO2_mean=data$NO2_mean, NO2_ranked=NO2.ranked, income=data$Income,
                        BD=data$BD, petrochem=data$petrochem_distance, road=data$Road.)
View(ranked.data)

lm.no2.ranked<-lm(NO2_ranked~BD+petrochem+road+income, data=ranked.data)
summary(lm.no2.ranked)
plot(lm.no2.ranked)
##rank transform helped but still not perfect

library(mgcv)
NO2.gam<-gam(data$NO2_mean~data$BD+s(data$petrochem_distance, k=9)+
               s(data$Income, k=27)+s(data$Imp., k=9)
               +data$X.canopy)
summary(NO2.gam)
plot(NO2.gam)

plot(NO2.gam, residuals=TRUE, pch=1, seWithMean = TRUE)

par(mfrow=c(2,2))
gam.check(NO2.gam)
concurvity(NO2.gam)
##diagnostic plots look pretty good
##including NDVI did not change the model
##removing Road% had no impact on the model

NO2.gam2<-gam(data2$NO2_mean~data2$BD+s(data2$petrochem_distance, k=9)+
                s(data2$Income, k=35)+s(data2$Imp., k=9)
              +data2$X.canopy)
summary(NO2.gam2)

gam.check(NO2.gam2)

plot(NO2.gam2, residuals=TRUE, pch=1, shade = TRUE, shade.col="blue")

##seems to work on full dataset too, but I had to increase k even more for income

##want to check what happens if I remove BD from the equation:

NO2.gam3<-gam(data2$NO2_mean~s(data2$petrochem_distance, k=9)+
                s(data2$Income, k=28))
summary(NO2.gam3)
gam.check(NO2.gam3)

##basically same result but I was able to make k lower, deviance explained increased by <1%

##########################GS#######################
GS.model<-lm(sqrt(data$nearest_M)~data$BD+data$Income)
summary(GS.model)
plot(GS.model)
##diagnostic plots iffy
##sqrt transform seems to help 
##log transform did not help
##rank transform did not help

GS.gam<-gam(data$nearest_M~s(data$BD)+s(data$Income))
summary(GS.gam)
plot(GS.gam, residuals=TRUE, pch=1)
gam.check(GS.gam)
##diagnostic plots not looking so great, maybe don't use this

GS.lm2<-lm(sqrt(data$nearest_M)~data$BD*data$Income)
summary(GS.lm2)
plot(GS.lm2)
##still not great but better than first linear model, qqplot doesn't look normal

GS.ranked<-rank(data$nearest_M)
GS.ranked.data<-data.frame(GS=GS.ranked, Income=data$Income, BD=data$BD)
View(GS.ranked.data)
GS.lm2.ranked<-lm(GS.ranked.data$GS~GS.ranked.data$Income*GS.ranked.data$BD)
plot(GS.lm2.ranked)
summary(GS.lm2.ranked)
##same issue as above, qqplot doesn't look normal

##Trying GS as continuous variable:
plot(data$Distance_M~data$BD)
abline(GS0)
GS<-lm(Distance_M~BD+Income, data=data)
GS0<-lm(Distance_M~BD, data=data)
summary(GS0)
plot(GS0)
summary(GS)
plot(GS)
##definitely not normally distributed 

GS.cont.gam<-gam(Distance_M~BD+s(Income, k=30), data=data)
plot(GS.cont.gam)
summary(GS.cont.gam)
gam.check(GS.cont.gam)
##doesn't look too good

##Testing new GS metric
GS_percent<-lm(data2$GS.~data2$BD)
plot(data2$GS.~data2$BD)
abline(GS_percent)
summary(GS_percent)
##no trends with either BD or Income

