##This script is just me learning, nothing in it has worked so far



##read in data and packages
data<-read.csv("Data/Study_Site_coords.csv")
install.packages("spdep")
library(spdep)
install.packages("rgdal")
library(rgdal)
library(sf)
install.packages("tmap")
library(tmap)
library(tidyverse)
library(raster)
shapes<-st_read("site_coords.gpkg")

##select relevant columns
data<-shapes%>%select(id)

##get coordinates and plot them
xy<-st_coordinates(data)
plot(xy, cex=1, pch=20, col='green')


w<-poly2nb(data, row.names = data$id)
class(w)
summary(w)

ww<-nb2listw(w)

#map LST
tm_shape(shapes) + tm_polygons(style="quantile", col = "LST_mean") +
  tm_legend(outside = FALSE, text.size = .8) 



