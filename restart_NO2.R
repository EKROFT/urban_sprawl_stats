no2.data<-read.csv("Data/shortlist_data_0813.csv")
library(GGally)
library(tidyverse)

##Initial exploration
data.selected<-no2.data%>%select(NO2_mean,BD, X.canopy,
                                 Income,Imp., petrochem_distance, road.distance..meters.,
                                 Road.)
GGally::ggpairs(data.selected)

##Testing GAM models
library(mgcv)
no2.gam<-gam(NO2_mean~s(BD)+s(X.canopy)+s(Income)+s(Imp.)+s(petrochem_distance)+
               s(road.distance..meters.)+s(Road.), data=no2.data, method="REML")
summary(no2.gam)
gam.check(no2.gam)

no2.gam2<-gam(NO2_mean~s(BD)+s(X.canopy)+s(Imp.)+s(petrochem_distance, Income)+
                s(road.distance..meters.)+s(Road.), data=no2.data, method="REML")
summary(no2.gam2)
#looks like there is an important interaction between income and petrochem_distance

no2.gam3<-gam(NO2_mean~s(BD)+s(X.canopy, Imp.)+s(petrochem_distance, Income)+
                s(road.distance..meters.)+s(Road.), data=no2.data, method="REML")
summary(no2.gam3)
#interaction between imp. and canopy seems less important

anova(no2.gam,no2.gam2,no2.gam3, test="Chisq")
#appears that model with no interactions is best so far.

##trying removing variables
no2.gam4<-gam(NO2_mean~s(BD)+s(X.canopy)+s(Income)+s(Imp.)+s(petrochem_distance)+
                s(Road.), data=no2.data, method="REML")
summary(no2.gam4)
anova(no2.gam, no2.gam4, test="Chisq")
#simpler model is better

no2.gam5<-gam(NO2_mean~s(BD)+s(X.canopy)+s(Income)+s(Imp.)+s(petrochem_distance)+
               s(road.distance..meters.), data=no2.data, method="REML")
anova(no2.gam,no2.gam4, no2.gam5, test="Chisq")
#model no2.gam4 is better than model 0 or model 5

no2.gam6<-gam(NO2_mean~s(BD)+s(X.canopy)+s(Income)+s(petrochem_distance)+
                s(Road.), data=no2.data, method="REML")
anova(no2.gam,no2.gam4, no2.gam6, test="Chisq")
#model that includes imp. seems stronger

no2.gam7<-gam(NO2_mean~s(BD)+s(Income)+s(Imp.)+s(petrochem_distance)+
                s(Road.), data=no2.data, method="REML")
anova(no2.gam, no2.gam4, no2.gam7, test="Chisq")
#model including canopy cover seems stronger

#no2.gam4 seems to be the winner so far

gam.check(no2.gam4)
concurvity(no2.gam4, full=TRUE)
#Impervious cover, BD and canopy cover may have concurvity
concurvity(no2.gam4, full=FALSE)
#Impervious seems concurvious with other variables.

gam.check(no2.gam6)
concurvity(no2.gam6, full=TRUE)
#this model gets rid of the concurvity
anova(no2.gam4, no2.gam6)
#these models seem to be almost the same so the one without concurvity is probably better (6)

library(nlme)
library(gstat)
library(sp)
library(ape)

##checking for spatial autocorrelation
no2.modelspace<-gam(NO2_mean~s(BD)+s(X.canopy)+s(petrochem_distance)+
                      s(Road.), data=no2.data, method="REML")
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