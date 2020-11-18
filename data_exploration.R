data<-read.csv("Data/shortlist_data_0813.csv")
View(data)


#install.packages("GGally")
library(GGally)
#install.packages("tidyverse")
library(tidyverse)

##Look at correlation matrices 

data.selected.LST<-
  data%>%select(BD,X.canopy,LST_mean, Income, river.distance..meters., Imp.,nearest_L)
GGally::ggpairs(data.selected.LST)

data.selected.NO2<-
  data%>%select(BD,NO2_mean,Income,petrochem_distance, Imp., X.canopy)
GGally::ggpairs(data.selected.NO2)


data.selected.GS<-
  data%>%select(GS.,BD, Income)
GGally::ggpairs(data.selected.GS)



##Looking at distributions
hist(data$LST_mean)
hist(data$NO2_mean)
hist(data$nearest_M)
hist(data$nearest_L)
hist(data$Imp.)
hist(data$BD)

##Looking at specific plots
plot(data$BD, data$X.canopy, pch=16, main="BD vs. %Canopy Cover", xlab="BD", ylab="%Canopy Cover")
##clear trend

plot(data$BD,data$LST_mean, pch=16, main="BD vs. LST", xlab="BD", ylab="LST")
##clear trend, linear

plot(data$X.canopy, data$LST_mean,pch=16, main="%Canopy Cover vs. LST", ylab="LST")
##clear trend, pretty linear

plot(data$nearest_L, data$LST_mean, pch=16, main="Green Space Access vs. LST",
     xlab="Distance to the nearest large green space", ylab="LST")
##doesn't look so clear

plot(data$Income, data$X.canopy, pch=16, main="Income vs. Canopy Cover", xlab="Income", ylab="Canopy Cover")
##doesn't look so clear

plot(data$Income, data$LST_mean, pch=16, main="Income vs. LST", xlab="Income", ylab="LST")
##doesn't look so clear, but slight negative trend?

plot(data$river.distance..meters.,data$LST_mean, main="Distance to River vs. LST",
     xlab="Distance to river", ylab="LST")
##not super clear, slight positive trend?

plot(data$Imp.,data$LST_mean, main="Impervious vs. LST",
     xlab="Impervious Cover", ylab="LST")
##clear positive trend, linear

plot(data$petrochem_distance, data$NO2_mean, pch=16, main="Distance to Factory vs. NO2", xlab="Distance
     to Factory", ylab= "NO2")
##negative trend

plot(data$Income, data$NO2_mean, pch=16, main="Income vs. NO2", xlab="Income", ylab= "NO2")
##trend unclear

plot(data$Income, data$BD, pch=16, main="Income vs. BD", xlab="Income", ylab="BD")
##not super clear, slight negative trend?

plot(data$BD, data$nearest_M, pch=16, main="BD vs. Green Space Access", xlab="Building Density", ylab= "Green Space Access")
##no clear trend

plot(data$Income, data$nearest_M, pch=16, main="Income vs. Green Space Access", xlab="Income", ylab= "Green Space Access")
##trend not strong

plot(data$Imp., data$NO2_mean, pch=16, main="Impervious Cover vs. NO2", xlab="Impervious Cover", ylab= "NO2")
##trend not clear

plot(data$BD, data$NO2_mean, pch=16, main="Building Density vs. NO2", xlab="Building Density", ylab= "NO2")
##trend not clear

plot(data$Income, data$Imp., pch=16, main="Impervious Cover vs. Income", xlab="Income", ylab= "Impervious Cover")
##negative trend

plot(data$Income, data$petrochem_distance, pch=16, main="Income vs. Distance to Factory", xlab="Income", ylab= "Distance to Factory")
##positive trend

plot(data$Road.,data$NO2_mean, pch=16, main="Road% vs. NO2", xlab="Road%", ylab= "NO2")
##not clear

plot(data$road.distance..meters.,data$NO2_mean, pch=16, main="Road Distance vs. NO2", xlab="Road Distance", ylab= "NO2")
##not clear

plot(data$NDVImean, data$NO2_mean, pch=16, main="NDVI vs. NO2", xlab="NDVI", ylab= "NO2")
##not clear

plot(data$X.canopy, data$NO2_mean, pch=16, main="Canopy% vs. NO2", xlab="%Canopy", ylab="NO2")
##not clear

plot(data$BD, data$Imp., pch=16, main="BD vs. Impervious", xlab="BD", ylab="Impervious")
##clear trend

plot(data$Imp., data$X.canopy, pch=16, main="%Canopy vs. Impervious", xlab="Impervious", ylab="%Canopy")
##clear trend
