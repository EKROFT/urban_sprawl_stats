##read in data and packages
data<-read.csv("Data/Study_Site_coords.csv")
library(gstat)

######LST#####################

#set up variogram
breaks = seq(0, 1.5, l = 2)

bubble(data, zcol='LST_mean', fill=TRUE, do.sqrt=FALSE, maxsize=3)

#plot variogram
TheVariogram=variogram(LST_mean~1, data=data)
plot(TheVariogram)

#make variogram model
TheVariogramModel <- vgm(psill=7, model="Exp", nugget=4.2, range=10000)
plot(TheVariogram, model=TheVariogramModel) 

####NO2###################

#set up variogram
breaks = seq(0, 1.5, l = 2)

bubble(data, zcol='NO2_mean', fill=TRUE, do.sqrt=FALSE, maxsize=3)

#plot variogram
TheVariogram=variogram(NO2_mean~1, data=data)
plot(TheVariogram)
TheVariogram

#make variogram model
TheVariogramModel <- vgm(psill=3e-11, model="Exp",
                         nugget=5e-12, range=15000)
plot(TheVariogram, model=TheVariogramModel)
