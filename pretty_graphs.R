data<-read.csv("Data/shortlist_data_0813.csv")
library(ggplot2)
library(stringr)
library(tidyverse)
library(viridis)

my.theme<-theme(axis.title = element_text(size=12),
                legend.title=element_text(size=12),
                legend.text = element_text(size=10))

       
#Plot 1: BD vs. LST colour coded for impervious cover

plot1<-ggplot(data, aes(x=BD, y=LST_mean, color=Imp.)) +
  geom_point(size=2)+
  labs(x="Building Density (%)", y="Land Surface Temperature (C)",color="Impervious \nCover (%)")+
  geom_smooth(method=lm, color="black")+
  theme_classic()+
  scale_color_gradient(low="white", high="red")

plot1+my.theme

#Plot 2: BD vs. LST colour coded for income

plot2<-ggplot(data, aes(x=BD, y=LST_mean, color=Income)) +
  geom_point(size=2)+
  labs(x="Building Density (%)", y="Land Surface Temperature (C)",color="Median \nHousehold \nIncome")+
  geom_smooth(method=lm, color="black")+
  theme_classic()+
  scale_color_gradient(low="yellow", high="red")+
  geom_vline(xintercept=c(50), linetype="dotted")
plot2+my.theme

#Plot 3: NO2 vs. distance to the petrochem facility colour coded for income
plot3<-ggplot(data, aes(x=petrochem_distance, y=NO2_mean, color=Income)) +
  geom_point(size=2)+
  labs(x="Distance to Petrochemical Facility", y="NO2 Concentration (mg/m3)",color="Median \nHousehold \nIncome")+
  geom_smooth(method=lm, color="black")+
  theme_classic()+
  scale_color_viridis(option = "C")+
  geom_vline(xintercept=c(10000, 30000), linetype="dotted")
plot3+my.theme

#Plot 4: LST vs. Income
plot4<-ggplot(data, aes(x=Income, y=LST_mean)) +
  geom_point(size=2)+
  labs(x="Median Household Income", y="Land Surface Temperature (C)")+
  theme_classic()
  
plot4+my.theme

#Plot 5: BD vs. LST colour coded for canopy cover

plot5<-ggplot(data, aes(x=BD, y=LST_mean, color=X.canopy)) +
  geom_point(size=2)+
  labs(x="Building Density (%)", y="Land Surface Temperature (C)",color="Canopy \nCover (%)")+
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




