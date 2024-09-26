#
# Neil Paprocki
# Apr 2021
# Map of Gunsight Mountain migrants
#
###############################################################################################
######################################################################

setwd("~/Desktop/U of I/Data Analysis/ggplot2 Mapping/data")
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

# bring in raw movement data downloaded as a csv from Movebank.org
a <- read.csv("Gunsight_HAUS04.csv", header=TRUE) 
b <- read.csv("Gunsight_133179.csv", header=TRUE) 
c <- read.csv("Gunsight_92359.csv", header=TRUE)
d <- read.csv("Gunsight_92361.csv", header=TRUE)

str(b)

# subset each for timeframe we want to plot
a$timestamp <- ymd_hms(a$timestamp,tz='GMT')
b$timestamp <- ymd_hms(b$timestamp,tz='GMT')
c$timestamp <- ymd_hms(c$timestamp,tz='GMT')
d$timestamp <- ymd_hms(d$timestamp,tz='GMT')

a <- subset(a, timestamp>="2016-01-01 00:00:01" & timestamp<="2016-07-01 00:00:01")
b <- subset(b, timestamp>="2019-01-01 00:00:01" & timestamp<="2019-07-01 00:00:01")
c <- subset(c, timestamp>="2020-01-01 00:00:01" & timestamp<="2020-07-01 00:00:01")
d <- subset(d, timestamp>="2020-01-01 00:00:01" & timestamp<="2020-07-01 00:00:01")

# subset to only relevant columns
a <- subset(a, select=c(timestamp,location.long,location.lat,individual.local.identifier)) 
b <- subset(b, select=c(timestamp,location.long,location.lat,individual.local.identifier)) 
c <- subset(c, select=c(timestamp,location.long,location.lat,individual.local.identifier)) 
d <- subset(d, select=c(timestamp,location.long,location.lat,individual.local.identifier)) 
e<-rbind(a,b)
f<-rbind(e,c)
raw <- rbind(f,d)
str(raw)

raw <- raw %>%
	rename(
		long = location.long,
		lat = location.lat,
		tagid = individual.local.identifier
		)
str(raw)
raw$tagid <- as.factor(raw$tagid) 
str(raw)

# order timestamps when including GPS and Argos data, and omit NAs
d <- raw[order(raw$tagid, raw$timestamp),]
d <- na.omit(d)
str(d)
##########################################
##########################################
# lets grab the world and U.S. states shapefiles from the maps package for plotting in ggplot2
basemap<-map_data("world") # this world map fills in Great Lakes, plot states/provinces/mexico individually
states<-map_data("state")
mexico<-subset(basemap, region %in% c("Mexico")) # subset out Mexico for plotting
summary(d)
###################################################################
str(d)
# running this chunk of code will save a .png map to your working directory
png("Gunsight_RLHA_migration.png", width=2000, height=1500, res=300) # saving map as a .png file with name, dimensions, and resolution
map.plot = ggplot(d, aes(long, lat)) + theme_bw() +
			geom_polygon(data=states, aes(x=long, y=lat, group=group), fill="grey",color="white", size=0.35,) +
			geom_polygon(data=regions, aes(x=long, y=lat, group=group), fill="grey", color="white", size=0.35) +
			geom_polygon(data=mexico, aes(x=long, y=lat, group=group), fill="grey", color="white", size=0.35) +
			geom_path(aes(x=long,y=lat,group=tagid,color=tagid),size=0.7) + # adding movement paths of birds, grouping and coloring by bird ID and specify width of the line
			scale_color_viridis(discrete=TRUE,option="D") + # adding a color palette for the lines
			xlab("longitude") + # x label
			ylab("latitude") +  # y label
			theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), # removing major and minor grid lines (lat/long lines)
			legend.position="none", # specifying we do not want a legend
			axis.title=element_blank(), # font size of axis titles
			axis.text=element_text(size=9)) + # font size of lat/long ticks
			coord_fixed(xlim=c(-160,-140), ylim=c(55,65), ratio=1.5) # specifying the limits of the plotting area in lat/long and the mapping ratio (will distort projection)
map.plot # plot the map
dev.off() # turn off the device
################################################################################################
??scale_color_viridis
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

region <- readOGR("./src/ref/ne_50m_admin_1_states_provinces_lakes", 'ne_50m_admin_1_states_provinces_lakes', encoding='UTF-8')
regions <- subset(region, name %in% c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Québec", "New Brunswick", "Prince Edward Island", "Nova Scotia", "Newfoundland and Labrador", "Yukon", "Northwest Territories", "Nunavut","Alaska")) # region is defined in the first part of the code (see above)
############################################################################
