data<-read.csv("Data/compiled_data_0813.csv")
library(ggplot2)
library(stringr)
library(tidyverse)
library(viridis)

my.theme<-theme(axis.title = element_text(size=12),
                legend.title=element_text(size=12),
                legend.text = element_text(size=10))

       
#Plot 1: BD vs. LST colour coded for impervious cover

plot1<-ggplot(data, aes(x=BD, y=LST_mean, color=Imp2)) +
  geom_point(size=2)+
  labs(x="Building Density (%)", y="Land Surface Temperature (C)",color="Impervious \nCover (%)")+
  geom_smooth(method=lm, color="black")+
  theme_classic()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_color_viridis(option="A")

plot1+my.theme

plotx<-ggplot(data, aes(x=BD, y=LST_mean)) +
  geom_point(size=2)+
  labs(x="Building Density (%)", y="Land Surface Temperature (C)")+
  geom_smooth(method=lm, color="black")+
  theme_classic()

plotx+my.theme

#Plot 2: BD vs. LST colour coded for income

plot2<-ggplot(data, aes(x=BD, y=LST_mean, color=Income)) +
  geom_point(size=2)+
  labs(x="Building Density (%)", y="Land Surface Temperature (C)",color="Median \nHousehold \nIncome")+
  geom_smooth(method=lm, color="black")+
  theme_classic()+
  scale_color_gradient(low="yellow", high="red")+
  geom_vline(xintercept=c(50), linetype="dotted")
plot2+my.theme

#Plot 3: NO2 vs. distance to the petrochem facility colour coded for canopy cover
plot3<-ggplot(data, aes(x=petrochem_distance, y=NO2_mean, color=X.canopy)) +
  geom_point(size=2)+
  labs(x="Distance to Petrochemical Facility", y="NO2 Concentration (mg/m3)",color="% Canopy \nCover")+
  geom_smooth(method=lm, color="black")+
  theme_classic()+
  scale_color_viridis(option = "C")
plot3+my.theme

#Plot 4: LST vs. Income
plot4<-ggplot(data, aes(x=Income, y=LST_mean)) +
  geom_point(size=2)+
  labs(x="Median Household Income", y="Land Surface Temperature (C)")+
  theme_classic()
  
plot4+my.theme

#Plot 5: BD vs. LST colour coded for canopy cover

plot5<-ggplot(data, aes(x=BD, y=LST_mean, color=NDVImean)) +
  geom_point(size=2)+
  labs(x="Building Density (%)", y="Land Surface Temperature (C)",color="NDVI")+
  geom_smooth(method=lm, color="black")+
  theme_classic()+
  scale_color_viridis(option="D")

plot5+my.theme

#Plot 6: Income vs. LST colour coded for NDVI
plot6<-ggplot(data, aes(x=Income, y=LST_mean, color=NDVImean)) +
  geom_point(size=3)+
  labs(x="Income", y="Land Surface Temperature (C)",color="NDVI")+
  theme_classic()+
  scale_color_continuous(low="palegreen", high="darkgreen")+
  geom_smooth(method=lm, color="black")+
  geom_hline(yintercept=c(35), linetype="dotted")
plot6+my.theme

#Plot 7: Something about green space
plot7<-ggplot(data, aes(x=BD, y=Distance_M))+
  geom_point(size=3)+
  labs(x="Building Density (%)", y="Distance to the Nearest \nPublic Green Space (m)")+
  theme_classic()
plot7+my.theme

#Plot 8: Same thing but categorical
plot8<-ggplot(data, aes(x=BD, y=nearest_M))+
  geom_point(size=3)+
  labs(x="Building Density (%)", y="Distance to the Nearest \nPublic Green Space (minutes)")+
  theme_classic()
plot8+my.theme


#Plot 9: Manhattan's Distance GS vs. BD
gs.lm<-lm(Man_GS~BD+Income, data=data)
gs.lm
summary(gs.lm)
plot(Man_GS~BD, data=data)
abline(gs.lm)

plot(gs.lm)

plot9<-ggplot(data, aes(x=BD, y=Man_GS)) +
  geom_point(size=3)+
  labs(x="% Building Density", y="Road Network Distance to the \nNearest Green Space (m)")+
  theme_classic()+
  scale_color_continuous(low="yellow", high="darkgreen")+
  geom_smooth(method=lm, color="black")
plot9+my.theme

#Plot 10: Bringing in canopy cover to LST relationship
plot10<-ggplot(data, aes(x=BD, y=LST_mean, color=X.canopy)) +
  geom_point(size=2)+
  labs(x="Building Density (%)", y="Land Surface Temperature (C)",color="Canopy \nCover (%)")+
  geom_smooth(method=lm, color="black")+
  theme_classic()+
  scale_color_gradientn(colours=rainbow(5))
plot10+my.theme

lm<-lm(Man_GS~log(Income), data=data)
summary(lm)
plot(Man_GS~Income, data=data)
abline(lm)


## Plot 11: Housholds vs. Green Space in 500m Buffer
fil<-filter(data, Households>0)
plot11<-ggplot(fil, aes(x=Households, y=GS_500)) +
  geom_point(size=2)+
  labs(x="Number of Households", y="Green Space in 500m Buffer (m2)")+
  theme_classic()
plot11+my.theme

## Plot 12: How much does temperature decrease due to canopy?
fil.tree<-filter(data, BD>50, BD<51)
plot12<-ggplot(fil.tree, aes(x=X.canopy, y=LST_mean, color=BD))+
  geom_point(size=2)+
  labs(x="% Canopy Cover", y="LST (C)", color="BD")+
  theme_classic()+
  scale_color_gradientn(colours=rainbow(3)) +
  geom_smooth(method=lm, color="black")
  
plot12+my.theme
  
#at 1%, 50% increase led to 10C cooler = 0.2
#at 10%, 80% increase led to 5C cooler = 0.06
# at 20%, 10% increase led to 3C cooler = 0.3
# at 30%, 10% increase led to 3C cooler = 0.3
# at 40%, 30% increase led to 3C cooler = 0.1
# AT 50%, 2% increase led to 1C cooler = 0.5