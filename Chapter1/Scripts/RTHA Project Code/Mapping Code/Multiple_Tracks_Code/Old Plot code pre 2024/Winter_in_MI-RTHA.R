## Wintering Locations Patagium & Herald 
## Nick Alioto
## 02/23/2022

######################
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
library(gtools)
library(stringr)
library(viridis)
library(raster)
library(rgdal)
library(ggmap)
library(rasterVis)

## clear workspace
rm(list=ls())
rm()                 #Remove particular object from the environmnet 

# bring in raw movement data downloaded as a csv from Movebank.org
a <- read.csv("Chapter.1/Data/2021_Red-tails/Patagium.csv", header=TRUE) 
b <- read.csv("Chapter.1/Data/2021_Red-tails/Herald.csv", header=TRUE) 
c <- read.csv("Chapter.1/Data/2021_Red-tails/Jack.csv", header=TRUE) 

#Need to convert the timestamps
a$timestamps <- as.POSIXct(strptime(a$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
b$timestamps <- as.POSIXct(strptime(b$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
c$timestamps <- as.POSIXct(strptime(c$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")

## Subset the times we want to plot if we don't do it numerically
a <- subset(a, timestamp>="2021-04-03 00:00:01" & timestamp<="2022-02-05 23:59:46")
b <- subset(b, timestamp>="2021-04-03 00:00:01" & timestamp<="2022-02-26 23:58:54")
c <- subset(c, timestamp>="2021-04-03 00:00:01" & timestamp<="2022-02-08 20:03:49")

#
##Combine all CSV's into one single file
raw <- rbind(a,b,c)

## Sub Set to only relevant columns
raw <- subset(raw, select=c(timestamp,location.long,location.lat,individual.local.identifier)) 

##rename column names
raw <- raw %>%
  rename(
    long = location.long,
    lat = location.lat,
    tagid = individual.local.identifier
  )
str(raw) 
raw$tagid <- as.factor(raw$tagid) 
str(raw)

##order timestamps when including GPS and Argos data, and omit NAs
rtha <- raw[order(raw$tagid, raw$timestamp),]
rtha <- na.omit(rtha)
str(rtha)

##########################################
##########################################

# lets grab the world and U.S. states shapefiles from the maps package for plotting in ggplot2
basemap<-map_data("world") # this world map fills in Great Lakes, plot states/provinces/mexico individually
states<-map_data("state")
mexico<-subset(basemap, region %in% c("Mexico")) # subset out Mexico for plotting
summary()

###################################################################
############################################################################
# code hunk for downloading Canadian Province boundaries
library(rgdal)
if (!file.exists("./src/ref/ne_50m_admin_1_states_provinces_lakes/ne_50m_admin_1_states_provinces_lakes.dbf")){
  download.file(file.path('http://www.naturalearthdata.com/http/',
                          'www.naturalearthdata.com/download/50m/cultural',
                          'ne_50m_admin_1_states_provinces_lakes.zip'), 
                f <- tempfile())
  unzip(f, exdir = "./src/ref/ne_50m_admin_1_states_provinces_lakes")
  rm(f)
}
region$name
region <- readOGR("./src/ref/ne_50m_admin_1_states_provinces_lakes", 'ne_50m_admin_1_states_provinces_lakes', encoding='UTF-8')
regions <- subset(region, name %in% c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "QuÃ©bec", "New Brunswick", "Prince Edward Island", "Nova Scotia", "Newfoundland and Labrador", "Yukon", "Northwest Territories", "Nunavut","Alaska")) # region is defined in the first part of the code (see above)
################################################################################


# running this chunk of code will save a .png map to your working directory
# font size of axis titles


ggplot(raw,aes(location.long, location.lat)) + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  geom_polygon(data=states, aes(x=long, y=lat, group=group), fill="grey",color="white", size=0.35) +
  geom_polygon(data=region, aes(x=long, y=lat, group=group), fill="grey", color="white", size=0.35) +
  geom_polygon(data=mexico, aes(x=long, y=lat, group=group), fill="grey", color="white", size=0.35) + 
  geom_path(aes(x=location.long,y=location.lat, group= individual.local.identifier,color= individual.local.identifier), linewidth =1) + #color = ("orangered2")) +#color = c("yellow1","magenta1")[filtered$fall.22]) +
  scale_fill_viridis(discrete=TRUE,option="d") + 
  xlab("longitude") +
  ylab("latitude") +
  theme(panel.grid.minor=element_blank(),
        legend.title= element_blank(),
        axis.title=element_text(size=12), # font size of axis titles
        axis.text=element_text(size=14))+ # font size of lat/long ticks
  coord_fixed(xlim=c(-93,-75), ylim=c(42,52), ratio=1.5) # specifying the limits of the plotting area in lat/long and the mapping ratio (will distort projection)

#Coordinates for MI Birds (lat/long)
#coord_fixed(xlim=c(-93,-75), ylim=c(42,52), ratio=1.5)

dev.off() # turn off the device
################################################################################################
??scale_color_viridis