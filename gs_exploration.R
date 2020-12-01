gs.data<-read.csv("Data/compiled_data_0813.csv")
library(ggplot2)
library(stringr)
library(tidyverse)
library(viridis)
library(tidyverse)

#Manhattan's Distance GS vs. BD
gs.lm<-lm(Man_GS~BD+Income, data=gs.data)
gs.lm
summary(gs.lm)
plot(Man_GS~BD, data=gs.data)
abline(gs.lm)

plot(gs.lm)

plot1<-ggplot(gs.data, aes(x=BD, y=Man_GS)) +
  geom_point(size=3)+
  labs(x="% Building Density", y="Road Network Distance to the \nNearest Green Space (m)")+
  theme_classic()+
  scale_color_continuous(low="yellow", high="darkgreen")+
  geom_smooth(method=lm, color="black")
plot1

#Manhattan's Distance vs. Households
fil<-filter(gs.data, Households>0)
gs.lm2<-lm(Man_GS~Households+Income, data=fil)
summary(gs.lm2)
plot(Man_GS~Households, data=fil)
abline(gs.lm2)

plot(gs.lm2)

#buffer_500 vs. Households
gs.lm3<-lm(GS_500~Households, data=fil)
summary(gs.lm3)
plot(GS_500~Households, data=fil)
abline(gs.lm3)

fil2<-filter(gs.data, Man_GS<3000)
gs.lm4<-lm(Man_GS~BD+Income, data=fil2)
plot(Man_GS~BD, data=fil2)
abline(gs.lm4)
summary(gs.lm4)

library(mgcv)
##Trying household relationship as a GAM
gam.gs<-gam(GS_500~s(Households), data=fil)
summary(gam.gs)
plot(gam.gs)

#not statistically significant