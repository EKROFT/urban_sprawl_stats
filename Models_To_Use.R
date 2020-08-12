data<-read.csv("Data/shortlist_data_0727.csv")
#View(data)

data2<-read.csv("Data/compiled_data_0727.csv")
#View(data2)

###################LST#######################
LST.model<-lm(data$LST_mean~data$X.canopy+data$BD+data$Income+data$river.distance..meters.)
summary(LST.model)
plot(LST.model)

gally.lst<-data%>%select(Income, BD, river.distance..meters., X.canopy, Imp.)
ggpairs(gally.lst)

#percent canopy and impervious cover are highly correlated, should remove one from model.
#I removed Impervious from the model and left canopy cover.

car::vif(LST.model)
#multicolinearity not an issue

##############################NO2######################
library(mgcv)
NO2.gam<-gam(NO2_mean~BD+s(petrochem_distance, k=9)+
               s(Income, k=27)+s(Imp., k=9)
             +X.canopy, data=data)
summary(NO2.gam)
gam.check(NO2.gam)
vis.gam(NO2.gam, view=c("petrochem_distance", "BD"), plot.type="persp", theta=150)

##THIS MODEL INCLUDES PETROCHEM_DISTANCE BUT THIS COULD CHANGE#######

##########################GS#######################
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

##Shows that multiple metrics all produced the same result, no trend

