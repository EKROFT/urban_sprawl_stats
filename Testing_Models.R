data<-read.csv("shortlist_data_0715.csv")
View(data)

data2<-read.csv(file.choose())

###################LST#######################
LST.model<-lm(data$LST_mean~data$X.canopy+data$Imp.+data$BD+data$Income+data$river.distance..meters.)
summary(LST.model)
plot(LST.model)

LST2.model<-lm(data2$LST_mean~data2$X.canopy+data2$Imp.+data2$BD+data2$Income+data2$river.distance..meters.)
summary(LST2.model)
plot(LST2.model)

##the diagnostic plots actually look good here, a linear model could work well
##worked with both shortlist and longlist data

##############################NO2######################
NO2.model<-lm(data$NO2_mean~data$Road.+data$Income+data$petrochem_distance+data$BD)
summary(NO2.model)
plot(NO2.model)
##diagnostics don't look so great
##log transform didn't help, neither did sqrt

NO2.ranked<-rank(data$NO2_mean)
ranked.data<-data.frame(NO2_mean=data$NO2_mean, NO2_ranked=NO2.ranked, income=data$Income,
                        BD=data$BD, petrochem=data$petrochem_distance, road=data$Road.)
View(ranked.data)

lm.no2.ranked<-lm(NO2_ranked~BD+petrochem+road+income, data=ranked.data)
summary(lm.no2.ranked)
plot(lm.no2.ranked)
##rank transform helped but still not perfect

NO2.gam<-gam(data$NO2_mean~data$BD+s(data$petrochem_distance)+
               s(data$Income), method="REML")
summary(NO2.gam)
coef(NO2.gam)
plot(NO2.gam)

plot(NO2.gam, residuals=TRUE, pch=1, seWithMean = TRUE)

gam.check(NO2.gam)
##not sure about some of these diagnostic plots

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
##not sure if this model is valid based on the gam.check report on convergence
##diagnostic plots not looking so great

GS.lm2<-lm(data$nearest_M~data$BD*data$Income)
summary(GS.lm2)
##still not great but better than first model
