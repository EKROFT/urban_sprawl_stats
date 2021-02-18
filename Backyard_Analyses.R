#Backyard Analyses

yard.data<-read.csv("Data/compiled_data_0813.csv")

library(tidyverse)
library(ggplot2)

fil<-filter(yard.data, "Households">0)
plot(BY_Ratio~BD, data=yard.data)
lm1<-lm(BY_Ratio~BD, data=yard.data)
abline(lm1)
summary(lm1)
##BY Ratio is highly correlated with BD (p=0.0004)

plot(LST_mean~BY_Ratio, data=yard.data)
lm2<-lm(LST_mean~BY_Ratio, data=yard.data)
abline(lm2)
summary(lm2)

lst.plot<-ggplot(fil, aes(x=BY_Ratio, y=LST_mean)) +
  labs(x="Backyard ratio", y="Land Surface Temperature (C)")+
  geom_point(size=2, color="black")+
  geom_smooth(method=lm, color="black")+
  theme_classic()
lst.plot
#looks like there may be a significant pattern here

plot(NO2_mean~BY_Ratio, data=yard.data)
lm3<-lm(NO2_mean~BY_Ratio, data=yard.data)
abline(lm3)
summary(lm3)
#nothing here

plot(Man_GS~BY_Ratio, data=yard.data)
lm4<-lm(Man_GS~BY_Ratio, data=yard.data)
abline(lm4)
summary(lm4)
#nothing here

plot(X.canopy~BY_Ratio, data=yard.data)
lm5<-lm(X.canopy~BY_Ratio, data=yard.data)
abline(lm5)
summary(lm5)
#higher BY_Ratio have higher canopy cover