library(maptools)
library(rgeos)
library(ggplot2)
library(ggmap)
library(mapdata)
library(rgdal)
library(rgeos)
library(ggplot2)
library(plotly)
library(dplyr)
library(grid)
library(gridExtra)

shp1 <- readOGR("C:\\Users\\Karolina Kopycka\\Desktop\\DATATHON\\dem-rejurb-rejstat-shp\\REJURB_20151231.shp")
shp1 <- spTransform(shp1,  CRS("+proj=longlat +datum=WGS84"))

shp1@data$REJON<- as.character(shp1@data$VARNAME_1)
shp1f <- fortify(shp1, region = "NAZWA")
head(shp1f, 4)

shp2<-shp1@data %>% select(c(1,2,14,15,16,17)) %>% mutate( 
  pw0_24 = (shp1@data$W0_2+ shp1@data$W3_6+ shp1@data$W7_12+ shp1@data$W13_15+ shp1@data$W16_18+ shp1@data$W19_24)/shp1$SUMA,
  pw25_59 = (shp1@data$W25_34+ shp1@data$W35_44+ shp1@data$W45_59K64M)/shp1$SUMA,
  pw60_100 = (shp1@data$W60K65M_79+shp1@data$W80_I_W)/shp1$SUMA)


p1<-ggplot() +
  geom_map(data=shp2, aes( map_id=NAZWA, fill=pw0_24), map=shp1f)+
  geom_path(data=shp1f, aes(x=long, y=lat, group=as.factor(id)), colour="white", size=0.4)+
  geom_point(aes(x= stops$stop_lon, y= stops$stop_lat), color="#001f4d", shape=19)+
  scale_fill_gradient(low = "#8c8c8c", high = "black")+
  ggtitle("WROCŁAW")
p2<-ggplot() +
  geom_map(data=shp2, aes( map_id=NAZWA, fill=pw25_59), map=shp1f)+
  geom_path(data=shp1f, aes(x=long, y=lat, group=as.factor(id)), colour="white", size=0.4)+
  geom_point(aes(x= stops$stop_lon, y= stops$stop_lat), color="#001f4d", shape=19)+
  scale_fill_gradient(low = "#8c8c8c", high = "black")+
  ggtitle("WROCŁAW")
p3<-ggplot() +
  geom_map(data=shp2, aes( map_id=NAZWA, fill=pw60_100), map=shp1f)+
  geom_path(data=shp1f, aes(x=long, y=lat, group=as.factor(id)), colour="white", size=0.4)+
  geom_point(aes(x= stops$stop_lon, y= stops$stop_lat), color="#001f4d", shape=19)+
  scale_fill_gradient(low = "#8c8c8c", high = "black")+
  ggtitle("WROCŁAW")
  
grid.arrange(p1,p2,p3)
  
  


  



x <- stops$stop_lat
y <- stops$stop_lon
p<-SpatialPoints(list(x,y), proj4string=CRS("+proj=longlat +datum=WGS84"))

d<-data.frame(x,y)
coordinates(d) <- c("lon", "lat")


pp<-gContains(shp1,p, byid=T)













