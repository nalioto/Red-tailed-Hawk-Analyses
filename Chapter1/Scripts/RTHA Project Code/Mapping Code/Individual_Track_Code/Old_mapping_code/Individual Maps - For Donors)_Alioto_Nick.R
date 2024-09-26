############################################
#   One individual Map - for Donor Updates #
#               Nick Alioto                # 
#                07/18/2022                #
############################################


#Packages required
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
a <- read.csv("Data/2022 Red-tails/Heathcliff.csv", header=TRUE) 

str(a)

# subset each for timeframe we want to plot, tz= timezone
a$timestamp <- ymd_hms(a$timestamp,tz='GMT')



a <- subset(a, timestamp>="2022-04-01 00:00:01")


# subset to only relevant columns
a <- subset(a, select=c(timestamp,location.long,location.lat,individual.local.identifier)) 

str(raw)

a <- a %>%
  rename(
    long = location.long,
    lat = location.lat,
    tagid = individual.local.identifier
  )
str(a)
a$tagid <- as.factor(raw$tagid) 
str(a)

# order timestamps when including GPS and Argos data, and omit NAs    ## I don't have Argos units##
rtha <- raw[order(raw$tagid, raw$timestamp),]
rtha <- na.omit(rtha)
str(rtha)

##########################################
##########################################

# lets grab the world and U.S. states shapefiles from the maps package for plotting in ggplot2
basemap<-map_data("world") # this world map fills in Great Lakes, plot states/provinces/mexico individually
states<-map_data("state")
mexico<-subset(basemap, region %in% c("Mexico")) # subset out Mexico for plotting
summary(rtha)

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
regions <- subset(region, name %in% c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "QuÃ©bec", "New Brunswick", "Prince Edward Island", "Nova Scotia", "Newfoundland and Labrador", "Yukon", "Northwest Territories", "Nunavut","Alaska")) # region is defined in the first part of the code (see above)
############################################################################
region$name


###################################################################
str(rtha)

# running this chunk of code will save a .png map to your working directory
png("Kentucky_Winter_locations_RTHA_12/2021.png", width=2000, height=1500, res=300) # saving map as a .png file with name, dimensions, and resolution

map.1 <- ggplot(a, aes(long, lat)) + theme_bw() +
  geom_polygon(data=states, aes(x=long, y=lat, group=group), fill="grey",color="white", size=0.35,) +
  geom_polygon(data=regions, aes(x=long, y=lat, group=group), fill="grey", color="white", size=0.35) +
  geom_polygon(data=mexico, aes(x=long, y=lat, group=group), fill="grey", color="white", size=0.35) +
  geom_path(aes(x=long,y=lat,group=tagid,color=tagid),size=0.9) + # adding movement paths of birds, grouping and coloring by bird ID and specify width of the line
  scale_color_viridis(discrete=TRUE,option="C") + # adding a color palette for the lines
  xlab("longitude") + # x label
  ylab("latitude") +  # y label
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), # removing major and minor grid lines (lat/long lines)
        legend.position="none", # specifying we do not want a legend
        axis.title=element_blank(), # font size of axis titles
        axis.text=element_text(size=9)) + # font size of lat/long ticks
  coord_fixed(xlim=c(-86,-83), ylim=c(45,47.5), ratio=1.5) # specifying the limits of the plotting area in lat/long and the mapping ratio (will distort projection)
plot(map.1) # plot the map
dev.off() # turn off the device
################################################################################################
??scale_color_viridis
