##I clipped the raster image to exclude the NE of the island to see what the pattern would be
##in the absence of the pterochem facility's influence

datas<-read.csv("Data/Reduced_NO2_Data.csv")
View(datas)
air.lm<-lm(datas$REDNO2_mean~datas$BD)
plot(datas$REDNO2_mean~datas$BD)
abline(air.lm)
summary(air.lm)
#clearly no pattern

air.lm2<-lm(datas$REDNO2_mean~datas$BD+datas$Imp.)
summary(air.lm2)
#clearly no pattern

air.lm3<-lm(datas$REDNO2_mean~datas$petrochem_distance)
plot(datas$REDNO2_mean~datas$petrochem_distance)
summary(air.lm3)
abline(air.lm3)
##Even in the SW of the island, the distance to the petrochemical facility in the NE
##is the main determinant of NO2 concentrations 

air.lm4<-lm(datas$REDNO2_mean~datas$petrochem_distance+datas$Road.)
summary(air.lm4)
##After adding and deleting lots of possible variables, this seems to be strongest linear model

##Now I tried applying weights to the full shortlist dataset by mutliplying the NO2 values
##by their distance to the petrochemical facility. 

##Testing weighted values
weights.no2<-read.csv("Data/weighted_NO2_data.csv")
air.lm.weights<-lm(weights.no2$weightNO2~weights.no2$BD+weights.no2$Road.+weights.no2$Income)
summary(air.lm.weights)
plot(air.lm.weights, which=3)
##variance of residuals looks sketchy

library(mgcv)
gam.no2.weights<-gam(weightNO2~BD+s(Income, k=45)+
                       s(Imp., k=10)+NDVImean, data=weights.no2)
summary(gam.no2.weights)
plot(gam.no2.weights)
gam.check(gam.no2.weights)
concurvity(gam.no2.weights, full=FALSE)
##this model looks pretty good
vis.gam(gam.no2.weights, view=c("Income", "Imp."), plot.type="persp", theta=200,
        color="topo", ticktype="detailed")

##IMPORTANT NOTE##
#In the models above I tried including canopy cover and GS% but neither one
#was significant to the model. When I tried NDVI instead it worked better.
#I also tried including Road density but it hurt the model.
##I also tried including the interaction between Impervious and NDVI but this hurt the model.

##checking little scatterplots

plot(weightNO2~Income, data=weights.no2)
l<-lm(weightNO2~Income, data=weights.no2)
summary(l)
abline(l)

plot(data$NO2_mean~data$Income)
n<-lm(NO2_mean~Income, data=data)
summary(n)
abline(n)

b<-lm(NDVImean~Income, data=data)
plot(NDVImean~Income, data=data)
abline(b)

s<-lm(log(weightNO2)~log(X.canopy), data=weights.no2)
plot(log(weightNO2)~log(X.canopy), data=weights.no2)
abline(s)
#I logged the data here to help me see the dots better, not for real statistical purposes

v<-lm(Income~petrochem_distance, data=data)
summary(v)
plot(Income~petrochem_distance, data=data)
abline(v)

library(GGally)
library(tidyverse)

gally.data<-data%>%select(Income, petrochem_distance, NDVImean, X.canopy, Imp.)
ggpairs(gally.data)
##Distance to petrochem facility is significantly correlated with all these variables.
##Could this colinearity be an explanation for weird patterns I'm seeing?

####IMPORTANT NOTES################
#The way the reality of the city is, distance to the petrochemical facility
#is the only real controlling factor over NO2 pollution. This effect is so strong that
#by default, living further from the facility means lower NO2 levels. 

#Because of this, all the neighbourhoods with highest NO2 are right by the facility.
#These neighbourhoods also display the traits that normally cause high NO2 (low vegetation,
#high road densities, low income etc.). But when I apply weights, all these areas end up
#not counting because I'm multiplying them by 0 or almost 0 (ie: 0.003). This might be why
#I'm seeing reversed trends when I look at the weighted version of the data (I'm deleting
#any possible patterns by multiplying the low vegetation, low income areas by 0). 