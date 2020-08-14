##read in data and packages
dataset<-read.csv("Data/shortlist_data_0813.csv")
library(gstat)
library(sp)

######LST#####################

#set up variogram
breaks = seq(0, 1.5, l = 2)
coordinates(dataset)= ~ Lat+Long
bubble(dataset, zcol='LST_mean', fill=TRUE, do.sqrt=FALSE, maxsize=3)

#plot variogram
TheVariogram=variogram(LST_mean~1, data=dataset)
plot(TheVariogram, pch=16, col="navy")

#make variogram model
TheVariogramModel <- fit.variogram(TheVariogram, vgm("Exp"))
plot(TheVariogram, model=TheVariogramModel) 

####NO2###################

#set up variogram
breaks = seq(0, 1.5, l = 2)



bubble(dataset, zcol='NO2_mean', fill=TRUE, do.sqrt=FALSE, maxsize=3)

#plot variogram
TheVariogram=variogram(NO2_mean~1, data=dataset)
plot(TheVariogram, pch=16, col="forestgreen")

#make variogram model
TheVariogramModel <- fit.variogram(TheVariogram, vgm("Exp"))
plot(TheVariogram, model=TheVariogramModel)


##Moran's I
test.dists <- as.matrix(dist(cbind(dataset$Long, dataset$Lat)))

test.dists.inv <- 1/test.dists
diag(test.dists.inv) <- 0

test.dists.inv[1:5, 1:5]

#install.packages("ape")
library(ape)

test.dists.inv[is.infinite(test.dists.inv)] <- 0

Moran.I(dataset$NO2_mean, test.dists.inv)


#modelling residuals

#LST model
LST.model<-lm(data$LST_mean~data$X.canopy+data$Imp.+data$BD+data$river.distance..meters.)
summary(LST.model)
View(dataset)
library(nlme)

coordinates(data)<-c('Long','Lat')
bubble(data,zcol='resids')
data$resids=resid
V<-variogram(resid~1, data=data)
plot(V, pch=16)
VariogramModel <- fit.variogram(V, vgm("Sph"))
plot(V, model=VariogramModel, pch=16, col="black")

#NO2 model
library(mgcv)
NO2.gam25<-gam(data$NO2_mean~data$BD+s(data$petrochem_distance, k=9)+
                s(data$Imp., k=9)
              +data$X.canopy)
summary(NO2.gam25)
resdi.no2<-residuals(NO2.gam25)
data$resid.no2=resdi.no2
bubble(data, zcol='resid.no2')

V2<-variogram(resid.no2~1, data=data)
plot(V2, pch=16, col="black")

