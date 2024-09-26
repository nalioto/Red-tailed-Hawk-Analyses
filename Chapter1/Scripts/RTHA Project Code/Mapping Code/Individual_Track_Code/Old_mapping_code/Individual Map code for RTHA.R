##############################
#                            #
#  Individual Red-tail Track #
#        Nick Alioto         #
##############################

#Libraries
############################
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
##############################

## bring in raw movement data
## Use front slash to tell R the order of folders to go through to find CSV
H1 <- read.csv("Chapter.1/Data/2021_Red-tails/Patagium.csv", header=TRUE) 
H2 <- read.csv("Chapter.1/Data/2021_Red-tails/Trinity.csv", header=TRUE) 
H3 <- read.csv("Chapter.1/Data/2022_Red-tails/Mackinaw.csv", header=TRUE) 
H4 <- read.csv("Chapter.1/Data/2023_Red-tails/Lightfoot.csv", header=TRUE) 
H5 <- read.csv("Chapter.1/Data/2023_Red-tails/Cyprus.csv", header=TRUE) 



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
filtered <- filter(H3, timestamp >= "2021-04-01 00:01:00.000") # Depends on Deployment Date
filtered$spr.24 <- ifelse(H3$timestamp >= "2024-03-25 00:01:00.000", 1, 2) # For separating movements
rm(filtered)

View(filtered)

# plot each birds track on a map and color by individual local identifier 

ggplot(filtered, aes(location.long, location.lat)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  geom_polygon(data=states, aes(x=long, y=lat, group=group), fill="grey",colour="white", linewidth=0.35) +
  geom_polygon(data=region, aes(x=long, y=lat, group=group), fill="grey", colour="white", linewidth=0.35) +
  geom_polygon(data=mexico, aes(x=long, y=lat, group=group), fill="grey", colour="white", linewidth=0.35) + 
  geom_path(aes(x=location.long,y=location.lat), linewidth = 1, color = c("darkviolet","orange2")[filtered$spr.24]) +
  scale_fill_viridis(discrete=TRUE,option="d") + 
  xlab("") + 
  ylab("") + 
  theme(panel.grid.minor=element_blank(),
        legend.title= element_blank(),
        axis.title=element_text(size=12),
        axis.text=element_text(size=12))+
  coord_fixed(xlim=c(-92.25,-70), ylim=c(37,55), ratio=1.5) 


# Coordinates for KY Birds (lat/long)
# Coord_fixed(xlim=c(-93,-75), ylim=c(37,52), ratio=1.5) 

#Coordinates for MI Birds (lat/long)
#coord_fixed(xlim=c(-93,-75), ylim=c(42,52), ratio=1.5)

# Coordinates for Northern Manitoba (Cricket)
# coord_fixed(xlim=c(-95,-75), ylim=c(42,57), ratio=1.5) 

# Coordinates for showing tracks including all tracked birds
# coord_fixed(xlim=c(-95,-69), ylim=c(34,57), ratio=1.5) 


#####################
# Good color combos #
####################

# magenta and yellow 2, indianred2, olivedrab, (orange2, darkviolet)
