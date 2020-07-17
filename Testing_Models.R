data<-read.csv("shortlist_data_0715.csv")
View(data)

###################LST#######################
LST.model<-lm(data$LST_mean~data$X.canopy+data$Imp.+data$BD+data$Income+data$river.distance..meters.)
summary(LST.model)
plot(LST.model)

##the diagnostic plots actually look good here, a linear model could work well

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

NO2.gam<-gam(data$NO2_mean~s(data$BD)+s(data$petrochem_distance)+
               s(data$Income), method="REML")
summary(NO2.gam)
coef(NO2.gam)
plot(NO2.gam)

plot(NO2.gam, residuals=TRUE, pch=1)

##rank transform helped but still not perfect

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


GS.lm2<-lm(data$nearest_M~data$BD*data$Income)
summary(GS.lm2)
##still not great but better than first model
