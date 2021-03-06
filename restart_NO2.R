no2.data<-read.csv("Data/compiled_data_0203.csv")
library(GGally)
View(no2.data)
library(tidyverse)

##Initial exploration
data.selected<-no2.data%>%select(NO2_mean,BD, X.canopy,
                                 Income,Imp., petrochem_distance, road.distance..meters.,
                                 Road.)
GGally::ggpairs(data.selected)
#hard to tell patterns based off the matrix

##Testing GAM models
library(mgcv)
no2.gam<-gam(NO2_mean~s(BD)+s(X.canopy)+s(Income)+s(Imp.)+s(petrochem_distance)+
               s(road.distance..meters.)+s(Road.), data=no2.data, method="REML")
summary(no2.gam)
gam.check(no2.gam)

no2.gam2<-gam(NO2_mean~s(BD)+s(X.canopy)+s(Imp.)+s(petrochem_distance)+s(Income)+
                s(road.distance..meters.)+s(Road.)+ti(petrochem_distance, Income), data=no2.data, method="REML")
summary(no2.gam2)
#interaction term not very significant

no2.gam3<-gam(NO2_mean~s(BD)+s(X.canopy)+s (Imp.)+s(petrochem_distance)+s(Income)+
                s(road.distance..meters.)+s(Road.)+
                ti(X.canopy, Imp.), data=no2.data, method="REML")
summary(no2.gam3)
#interaction between imp. and canopy seems less important

no2.gam.x<-gam(NO2_mean~s(BD)+s(X.canopy)+s (Imp.)+s(petrochem_distance)+s(Income)+
                s(road.distance..meters.)+s(Road.)+
                ti(X.canopy, BD), data=no2.data, method="REML")
summary(no2.gam.x)
#interaction between BD and canopy seems less important

anova(no2.gam,no2.gam2,no2.gam3, test="Chisq")
anova(no2.gam, no2.gam2, test="Chisq")
anova(no2.gam, no2.gam3, test="Chisq")
#appears that model with no interactions is best so far.

##trying removing variables
no2.gam4<-gam(NO2_mean~s(BD)+s(X.canopy)+s(Income)+s(Imp.)+s(petrochem_distance)+
                s(Road.), data=no2.data, method="REML")
summary(no2.gam4)
anova(no2.gam, no2.gam4, test="Chisq")
AIC(no2.gam)
AIC(no2.gam4)
#more complex model is better

no2.gam5<-gam(NO2_mean~s(BD)+s(X.canopy)+s(Income)+s(Imp.)+s(petrochem_distance)+
               s(road.distance..meters.), data=no2.data, method="REML")
anova(no2.gam,no2.gam4, no2.gam5, test="Chisq")
anova(no2.gam, no2.gam4, test="Chisq")
anova(no2.gam, no2.gam5, test="Chisq")
AIC(no2.gam5)
#model no2.gam and no2.gam5 are the same AIC so I'll choose the simpler one.

no2.gam6<-gam(NO2_mean~s(BD)+s(X.canopy)+s(Income)+s(petrochem_distance)+
                s(road.distance..meters.), data=no2.data, method="REML")
anova(no2.gam,no2.gam4, no2.gam6, test="Chisq")
anova(no2.gam5, no2.gam6, test="Chisq")
AIC(no2.gam5)
AIC(no2.gam6)
#model that excludes imp. seems stronger

no2.gam7<-gam(NO2_mean~s(BD)+s(Income)+s(petrochem_distance)+
                s(road.distance..meters.), data=no2.data, method="REML")
anova(no2.gam, no2.gam6, no2.gam7, test="Chisq")
anova(no2.gam6, no2.gam7, test="Chisq")
AIC(no2.gam6)
AIC(no2.gam7)
#model excluding canopy cover is slightly stronger but based on the literature I
#think it's important to include

#no2.gam6 seems to be the winner so far

gam.check(no2.gam6)
concurvity(no2.gam6, full=TRUE)


vis.gam(no2.gam6, view=c("petrochem_distance", "Income"), color="heat", plot.type="persp",
        theta=140)


library(nlme)
library(gstat)
library(sp)
library(ape)

##checking for spatial autocorrelation
no2.modelspace<-gam(NO2_mean~s(BD)+s(petrochem_distance)+s(X.canopy)+
                      s(road.distance..meters.), data=no2.data, method="REML")
coordinates(no2.data)<-c('Long','Lat')
resids<-residuals(no2.modelspace)
no2.data$resids=resids
bubble(no2.data,zcol='resids')
V<-variogram(resids~1, data=no2.data)
plot(V, pch=16, col="black")
#looks like definite spatial autocorrelation

test.dists <- as.matrix(dist(cbind(no2.data$Long, no2.data$Lat)))

test.dists.inv <- 1/test.dists
diag(test.dists.inv) <- 0

test.dists.inv[1:5, 1:5]

test.dists.inv[is.infinite(test.dists.inv)] <- 0

Moran.I(no2.data$resids, test.dists.inv)
#Moran's I seems to confirm this

#will have to account for spatial autocorrelation in the model
coords<-coordinates(no2.data)

no2.gam8<-gam(NO2_mean~s(BD)+s(Income)+s(petrochem_distance)+s(X.canopy)+
                s(road.distance..meters.)+s(Lat,Long), data=no2.data, method="REML")
summary(no2.gam8)
gam.check(no2.gam8)
concurvity(no2.gam8)
#distance and coords have high concurvity

no2.gam9<-gam(NO2_mean~s(BD)+s(Income)+s(X.canopy)+
                s(road.distance..meters.)+s(Lat,Long), data=no2.data, method="REML")
summary(no2.gam9)
gam.check(no2.gam9)
concurvity(no2.gam9)
#this model gets rid of concurvity

anova(no2.gam9, no2.gam8, test="Chisq")
AIC(no2.gam9)
AIC(no2.gam8)
AIC(no2.gam7)
##model 8 has lowest AIC but it has concurvity close to 100% so I don't think it's appropriate
#to include both
no2.gam10<-gam(NO2_mean~s(BD)+s(Income)+s(road.distance..meters.)+
                 s(Lat,Long, k=65), data=no2.data, method="REML")
gam.check(no2.gam10)
summary(no2.gam10)
plot(no2.gam10, scheme=2, page=1)

##trying spatial plus approach
BD_gam<- gam(BD~s(Lat*Long), data=no2.data)
resid_BD<-residuals(BD_gam)  
income_gam<-gam(log(Income)~s(Lat*Long), data=no2.data, na.action=na.exclude)
resid_income<-residuals(income_gam)
road_gam<-gam(log(road.distance..meters.)~s(Lat*Long), data=no2.data)
resid_road<-residuals(road_gam)
canopy_gam<-gam(X.canopy~s(Lat*Long), data=no2.data)
resid_canopy<-residuals(canopy_gam)

no2.gam11<-gam(NO2_mean~resid_BD+resid_income+resid_road+resid_canopy+s(Lat,Long), data=no2.data,
               method="REML")
summary(no2.gam11)
gam.check(no2.gam11)
plot(no2.gam11, scheme=2, page=1)
concurvity(no2.gam11)

vis.gam(no2.gam10, view=c("Income","road.distance..meters."), 
        plot.type="persp", theta=1, xlab="Income",
        ylab="Distance to Major Road", zlab="NO2 Concentration")


Boroughs<-as.factor(no2.data$Borough)
plot(NO2_mean~Income, data=no2.data)
abline(lm)
lm<-lm(NO2_mean~log(Income), data=no2.data)

##trying borough as random effect
no2.gam12<-gam(NO2_mean~s(BD)+s(Income)+s(road.distance..meters.)+s(X.canopy)+
                  s(Boroughs, bs="re"), data=no2.data, method="REML", na.action=na.exclude)
summary(no2.gam12)


##trying both approaches together
no2.gam13<-gam(NO2_mean~resid_BD+resid_income+resid_road+resid_canopy
               +s(Lat,Long)+s(Boroughs, bs="re"), data=no2.data,
               method="REML")
summary(no2.gam13)

library(qpcR)
values1<-AIC(no2.gam11, no2.gam12, no2.gam13)
akaike.weights(values1)
values1

##spatial+ model without borough random effect is best

fil<-filter(no2.data, Households>0)
plot(NO2_mean~Households, data=fil)
lm1<-lm(NO2_mean~Households, data=fil)
abline(lm1)
summary(lm1)


##Looking at alternative dates
no2.alt<-read.csv("Data/NO2_alternativedates.csv")

BD_gam<- gam(BD~s(Lat*Long), data=no2.alt)
resid_BD<-residuals(BD_gam)  
income_gam<-gam(log(Income)~s(Lat*Long), data=no2.alt, na.action=na.exclude)
resid_income<-residuals(income_gam)
road_gam<-gam(log(road.distance..meters.)~s(Lat*Long), data=no2.alt)
resid_road<-residuals(road_gam)
canopy_gam<-gam(X.canopy~s(Lat*Long), data=no2.alt)
resid_canopy<-residuals(canopy_gam)

no2.gamalt<-gam(MayNO2_mean~resid_BD+resid_income+resid_road+resid_canopy+s(Lat,Long), data=no2.alt,
               method="REML")
summary(no2.gamalt)
summary(no2.gam11)

plot(NO2_mean~BD, data=no2.alt, xlab="% Building Density", ylab="July NO2 Concentration", pch=16,
     xlim=c(0,100), ylim=c(0.00002, 0.00008))
ab<-(lm(OctNO2_mean~BD, data=no2.alt))
summary(ab)
plot(MayNO2_mean~BD, data=no2.alt,  xlab="% Building Density", ylab="May NO2 Concentration",
     pch=16, xlim=c(0,100), ylim=c(0.00002, 0.00008))
plot(OctNO2_mean~BD, data=no2.alt,  xlab="% Building Density", ylab=" October NO2 Concentration",
     pch=16, xlim=c(0,100), ylim=c(0.00002, 0.00008))

vis.gam(no2.gam11, view=c("resid_road", "resid_income"), plot.type="persp")

plot(NO2_mean~Households, data=no2.data)
lm1<-lm(NO2_mean~Households, data=no2.data)
abline(lm1)
summary(lm1)

##households as GAM
House_gam<- gam(Households~s(Lat*Long), data=fil)
resid_House<-residuals(House_gam)
BD_gam<- gam(BD~s(Lat*Long), data=fil)
resid_BD<-residuals(BD_gam)

no2.gam.house<-gam(NO2_mean~s(resid_House)+s(resid_BD)+s(Lat,Long), data=fil,
               method="REML")
summary(no2.gam.house)
gam.check(no2.gam.house)


#test vs. training data
library(gghighlight)
theme_set(theme_bw())

plot.train<-ggplot(no2.data, aes(x=BD, y=NO2_mean, color=Testing)) +
        labs(x="% Building Density", y="NO2 Concentration (mol/m2)")+
        geom_point(size=3)+
        theme_classic()+
        scale_colour_manual(values=c("red", "black"))
plot.train

