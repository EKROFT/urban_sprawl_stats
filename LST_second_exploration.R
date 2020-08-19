lst.data<-read.csv("Data/shortlist_data_0813.csv")
library(GGally)
library(tidyverse)


##Initial exploration
data.selected<-lst.data%>%select(LST_mean,BD, X.canopy,
                                 Income,river.distance..meters.,Imp.)
GGally::ggpairs(data.selected)

##Testing linear models

#First attempt model
lst.model<-lm(LST_mean~BD+X.canopy+Income+river.distance..meters.+Imp., data=lst.data)
summary(lst.model)

#canopy cover and impervious seem quite correlated, same with BD and impervious
#I'm testing a model that removes impervious cover for that reason
lst.model2<-lm(LST_mean~BD+X.canopy+Income+river.distance..meters., data=lst.data)
summary(lst.model2)

#I want to test if the simpler model was better
anova(lst.model,lst.model2)
#looks like last model is better

##checking model assumptions
plot(lst.model2, which=2)
plot(lst.model2, which=3)

library(nlme)
library(gstat)
library(sp)

lst.model4<-lm(LST_mean~BD+X.canopy+river.distance..meters., data=lst.data)
coordinates(lst.data)<-c('Long','Lat')
resids<-residuals(lst.model4)
lst.data$resids=resids
bubble(lst.data,zcol='resids')
V<-variogram(resids~1, data=lst.data)
plot(V, pch=16, col="black")
#variogram doesn't show much spatial sutocorrelation, but still a little

test.dists <- as.matrix(dist(cbind(lst.data$Long, lst.data$Lat)))

test.dists.inv <- 1/test.dists
diag(test.dists.inv) <- 0

test.dists.inv[1:5, 1:5]

library(ape)

test.dists.inv[is.infinite(test.dists.inv)] <- 0

Moran.I(lst.data$resids, test.dists.inv)
#Moran's I seems to indicate that there is spatial autocorrelation in the model residuals

#Model assumptions of independent observations not being met, need to account for this