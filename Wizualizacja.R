library(maptools)
library(rgeos)
library(ggplot2)
library(ggmap)
library(mapdata)
library(rgdal)
library(rgeos)
library(ggplot2)


shp1 <- readOGR("dem-rejurb-rejstat-shp/REJURB_20151231.shp")
shp1 <- spTransform(shp1,  CRS("+proj=longlat +datum=WGS84"))

ggplot() +
  geom_path(data=shp1, aes(x=long, y=lat, group=id), colour="black", size=0.25)+
  geom_point(aes(x= stops$stop_lon, y= stops$stop_lat))

x <- stops$stop_lat
y <- stops$stop_lon
p<-SpatialPoints(data.frame(y, x), 
                 proj4string=CRS("+proj=longlat +datum=WGS84"))
reg_p <- gCovers(shp1, p, byid = TRUE) 
