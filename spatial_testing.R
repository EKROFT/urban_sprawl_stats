##read in data and packages
data<-read.csv("Data/Study_Site_coords.csv")
library(gstat)
library(sp)

######LST#####################

#set up variogram
breaks = seq(0, 1.5, l = 2)
coordinates(data)= ~ Lat+Long

bubble(data, zcol='LST_mean', fill=TRUE, do.sqrt=FALSE, maxsize=3)

#plot variogram
TheVariogram=variogram(LST_mean~1, data=data)
plot(TheVariogram, pch=16, col="navy")

#make variogram model
TheVariogramModel <- fit.variogram(TheVariogram, vgm("Exp"))
plot(TheVariogram, model=TheVariogramModel) 

####NO2###################

#set up variogram
breaks = seq(0, 1.5, l = 2)

bubble(data, zcol='NO2_mean', fill=TRUE, do.sqrt=FALSE, maxsize=3)

#plot variogram
TheVariogram=variogram(NO2_mean~1, data=data)
plot(TheVariogram, pch=16, col="forestgreen")
TheVariogram

#make variogram model
TheVariogramModel <- fit.variogram(TheVariogram, vgm("Exp"))
plot(TheVariogram, model=TheVariogramModel)

