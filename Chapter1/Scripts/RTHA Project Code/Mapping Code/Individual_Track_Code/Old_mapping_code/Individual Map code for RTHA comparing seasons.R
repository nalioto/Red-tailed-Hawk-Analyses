#Plotting a single Track for a RTHA 

library(rworldmap)
library(sp)
library(cartography)
library(rgdal)
library(adehabitatLT)
library(lubridate)
library(gdata)
library(bcpa)
library(ggplot2)
library(rgeos)
library(geosphere)
library(raster)
library(maptools)
library(dplyr)
library(stringr)
library(viridis)
library(ggmap)

## bring in raw movement data
## Use front slash to tell R the order of folders to go through to find CSV
Sa <- read.csv("Chapter.1/Data/2021 Red-tails/Sam.csv", header=TRUE) 
Ri <- read.csv("Chapter.1/Data/2021 Red-tails/Rip.csv", header=TRUE) 
Ra <- read.csv("Chapter.1/Data/2022 Red-tails/Rapini.csv", header=TRUE) 
Bu <- read.csv("Chapter.1/Data/2022 Red-tails/Bucatini.csv", header=TRUE) 


#filter to first initial deployment Spring and Fall tracks


#lets try plotting some maps of complete tracks range wide 

basemap<-map_data("world")
states<-map_data("state")
mexico<-subset(basemap, region %in% c("Mexico"))

# how to download Canadian Province boundaries

if (!file.exists("./src/ref/ne_50m_admin_1_states_provinces_lakes/ne_50m_admin_1_states_provinces_lakes.dbf")){
  download.file(file.path('http://www.naturalearthdata.com/http/',
                          'www.naturalearthdata.com/download/50m/cultural',
                          'ne_50m_admin_1_states_provinces_lakes.zip'), 
                f <- tempfile())
  unzip(f, exdir = "./src/ref/ne_50m_admin_1_states_provinces_lakes")
  rm(f)
}

## This adds the lakes

region <- readOGR("./src/ref/ne_50m_admin_1_states_provinces_lakes", 'ne_50m_admin_1_states_provinces_lakes', encoding='UTF-8')

regions <- subset(region, name %in% c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "QuÃ©bec", "New Brunswick", "Prince Edward Island", "Nova Scotia", "Newfoundland and Labrador", "Yukon", "Northwest Territories", "Nunavut")) # region is defined in the first part of the code (see above)

# code chunk for Mexican states
# Mexican states shapefile downloaded from: https://www.arcgis.com/home/item.html?id=ac9041c51b5c49c683fbfec61dc03ba8. Unzip by double clicking the downloaded file. Then place the folder on your desktop.
# Do not Run none of (Nick's) Birds are in Mexico 
mex <- readOGR(dsn ='/Users/Bryce/Dropbox/Research/RTHA project/Maps/Map R Code/mexstates/mexstates.shp')

#filter for this season. change if necessary 
filtered1 <- filter(Sa, timestamp >= "2021-04-01 00:01:00.000")
filtered2 <- filter(Ri, timestamp >= "2021-04-01 00:01:00.000")
filtered3 <- filter(Ra, timestamp >= "2022-04-01 00:01:00.000")
filtered4 <- filter(Bu, timestamp >= "2022-04-01 00:01:00.000")

filtered3$fall.22 <- ifelse(filtered3$timestamp >= "2022-11-15 00:01:00.000", 1, 2)

View(filtered1)
rm(filtered)
# plot each birds track on a map and color by individual local identifier 

ggplot(filtered4,aes(location.long, location.lat)) + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  geom_polygon(data=states, aes(x=long, y=lat, group=group), fill="grey",color="white", size=0.35) +
  geom_polygon(data=region, aes(x=long, y=lat, group=group), fill="grey", color="white", size=0.35) +
  geom_polygon(data=mexico, aes(x=long, y=lat, group=group), fill="grey", color="white", size=0.35) + 
  geom_path(aes(x=location.long,y=location.lat),size=1.5, color = "magenta1") + #c("yellow1","magenta1")[filtered3$fall.22]) +
  scale_fill_viridis(discrete=TRUE,option="c") + 
  xlab("longitude") +
  ylab("latitude") +
  theme(panel.grid.minor=element_blank(),
        legend.title= element_blank(),
        axis.title=element_text(size=12),
        axis.text=element_text(size=12)) +
  coord_fixed(xlim=c(-93,-75), ylim=c(40,52), ratio=1.5) 

# Coordinates for KY Birds (lat/long)
#coord_fixed(xlim=c(-93,-75), ylim=c(37,52), ratio=1.5) 

#Coordinates for MI Birds (lat/long)
#coord_fixed(xlim=c(-93,-75), ylim=c(42,52), ratio=1.5)
