data<-read.csv("compiled_data_0710.csv")
View(data)

###################LST#######################
LST.model<-lm(data$LST_mean~data$X.canopy+data$Imp.+data$BD+data$Income+data$river.distance..meters.)
summary(LST.model)
plot(LST.model)
##the diagnostic plots actually look good here, a linear model could work well

##############################NO2######################
NO2.model<-lm(data$NO2_mean~data$Road.+data$Income+data$petrochem_distance+data$BD)
summary(NO2.model)
plot(NO2.model)
##diagnostics don't look so great

##########################GS#######################
GS.model<-lm(data$nearest_M~data$BD+data$Income)
summary(GS.model)
plot(GS.model)
##diagnostic plots iffy