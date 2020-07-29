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