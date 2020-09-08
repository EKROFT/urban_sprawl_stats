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
library(car)
vif(lst.model)

#canopy cover and impervious seem a little correlated, same with BD and impervious
#I'm testing a model that removes impervious cover for that reason
lst.model2<-lm(LST_mean~BD+X.canopy+Income+river.distance..meters., data=lst.data)
summary(lst.model2)
vif(lst.model2)

#I want to test if the simpler model was better
anova(lst.model,lst.model2)
#looks like more complex model is better

#since the vif showed nothing above 5 for first model, I will keep impervious

##checking model assumptions
plot(lst.model, which=2)
plot(lst.model, which=3)
#diagnostics don't look awesome

##now testing models with interactions

lst.model5<-lm(LST_mean~BD+(X.canopy*Imp.)+Income+river.distance..meters., data=lst.data)
summary(lst.model5)
#canopy:imp. interaction has p values close to 1

lst.model6<-lm(LST_mean~(BD*X.canopy)+Imp.+Income+river.distance..meters., data=lst.data)
summary(lst.model6)
#this BD-canopy cover interaction has a very high p value
anova(lst.model, lst.model6)
#model without interaction term is stronger


library(nlme)
library(gstat)
library(sp)

lst.model4<-lm(LST_mean~BD+X.canopy+river.distance..meters.+Imp., data=lst.data)
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

coords<-coordinates(lst.data)

lst.model7<-lm(LST_mean~BD+X.canopy+Income+river.distance..meters.+Imp.+coords, data=lst.data)
summary(lst.model7)

anova(lst.model, lst.model7, test="Chisq")
AIC(lst.model)
AIC(lst.model7)
#model with spatial term is stronger

plot(lst.model7, which=2)
plot(lst.model7, which=3)
#not looking normally distributed