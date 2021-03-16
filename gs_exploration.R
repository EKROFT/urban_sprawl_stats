gs.data<-read.csv("Data/compiled_data_0203.csv")
library(ggplot2)
library(stringr)
library(tidyverse)
library(viridis)
library(tidyverse)
library(nlme)

#Manhattan's Distance GS vs. BD
gs.lm<-lm(Man_GS~BD+Income, data=gs.data)
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
par(mfrow=c(2,2))
plot(GS_300~Households, data=fil)
plot(GS_500~Households, data=fil)
plot(GS_800~Households, data=fil)
plot(GS_1000~Households, data=fil)

gam_500<-gam(GS_500~s(Households)+s(Boroughs, bs="re"), data=fil, method="REML")
summary(gam_500)
#not significant

fil2<-filter(gs.data, Man_GS<3000)
gs.lm4<-lm(Man_GS~BD+Income, data=fil, xlab="Households", ylab="Walking distance to green space")
plot(Man_GS~BD, data=fil)
abline(gs.lm4)
summary(gs.lm4)

gs.lm5<-lm(GS_300~Households, data=fil)
summary(gs.lm5)

library(mgcv)
##Trying household relationship as a GAM
Boroughs<-as.factor(fil$Borough)
gam.gs<-gam(Man_GS~s(Households)+s(Boroughs, bs="re"), data=fil, method="REML")
summary(gam.gs)
plot(gam.gs)
gam.check(gam.gs)
plot(Man_GS~Households, data=fil, pch=16)
gam.gs2<-gam(Man_GS~s(BD)+s(Income), data=gs.data)
summary(gam.gs2)

##backyard stuff
by.model<-lm(BY_Ratio~BD, data=gs.data)
summary(by.model)
plot(BY_Ratio~BD, data=gs.data, xlab="Building Density", ylab="Backyard Ratio",
     pch=16)
abline(by.model)


#Manhattan's Distance GS vs. BD
library(nlme)

model = lme(Man_GS ~ BD, random=~1|Borough,
            data=gs.data,
            method="REML")

library(car)

Anova(model)

model.fixed = gls(Man_GS~BD, data=gs.data, method="REML")
anova(model,model.fixed)
summary(model)
summary(model.fixed)

##Training vs. test data
#install.packages("gghighlight")
library(gghighlight)
theme_set(theme_bw())

plot.train<-ggplot(gs.data, aes(x=BD, y=Man_GS, color=Testing)) +
  labs(x="% Building Density", y="Road Network Distance to the \nNearest Green Space (m)")+
  geom_point(size=3)+
  scale_colour_manual(values=c("red", "black"))+
  geom_smooth(method=lm, color="black")
plot.train

