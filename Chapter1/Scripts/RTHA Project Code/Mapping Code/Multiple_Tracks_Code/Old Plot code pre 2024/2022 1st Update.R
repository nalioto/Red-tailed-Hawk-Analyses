######################################
##### 2022 Movement Update 2 #########
#####                        #########
#####      05/28/2022        #########
######################################

#Let's get those packages loaded
library(rworldmap)
library(sp)
library(cartography)
library(rgdal)
library(adehabitatLT)
library(lubridate)
library(gdata)
library(bcpa)
library(tidyverse)
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
library(plotly)
library(RColorBrewer)

## clear workspace
rm(list=ls())

# bring in raw movement data downloaded as a csv from Movebank.org
a <- read.csv("Chapter.1/Data/2022 Red-tails/Angell.csv", header=TRUE) 
b <- read.csv("Chapter.1/Data/2022 Red-tails/Bucatini.csv", header=TRUE) 
c <- read.csv("Chapter.1/Data/2022 Red-tails/Ginger.csv", header=TRUE) 
d <- read.csv("Chapter.1/Data/2022 Red-tails/Hallowee.csv", header=TRUE) 
e <- read.csv("Chapter.1/Data/2022 Red-tails/Mackinaw.csv", header=TRUE) 
f <- read.csv("Chapter.1/Data/2022 Red-tails/Petosegay.csv", header=TRUE) 
g <- read.csv("Chapter.1/Data/2022 Red-tails/SAASY.csv", header=TRUE) 
h <- read.csv("Chapter.1/Data/2022 Red-tails/Voyageur.csv", header=TRUE) 
str()

# subset each for timeframe we want to plot, tz= timezone
a$timestamp <- ymd_hms(a$timestamp,tz='EST')
b$timestamp <- ymd_hms(b$timestamp,tz='EST')
c$timestamp <- ymd_hms(c$timestamp,tz='EST')
d$timestamp <- ymd_hms(d$timestamp,tz='EST')
e$timestamp <- ymd_hms(e$timestamp, tz='EST')
f$timestamp <- ymd_hms(f$timestamp, tz='EST')
g$timestamp <- ymd_hms(g$timestamp, tz='EST')
h$timestamp <- ymd_hms(h$timestamp, tz='EST')


a <- subset(a, timestamp>="2022-04-01 00:00:01")
b <- subset(b, timestamp>="2022-04-01 00:00:01")
c <- subset(c, timestamp>="2022-04-01 00:00:01")
d <- subset(d, timestamp>="2022-04-01 00:00:01")
e <- subset(e, timestamp>="2022-04-01 00:00:01")
f <- subset(f, timestamp>="2022-04-01 00:00:01")
g <- subset(g, timestamp>="2022-04-01 00:00:01")
h <- subset(h, timestamp>="2022-04-01 00:00:01")
#i <- subset(i, timestamp>="2022-04-01 00:00:01")
#j <- subset(j, timestamp>="2022-04-01 00:00:01")
#k <- subset(k, timestamp>="2022-04-01 00:00:01")
#l <- subset(l, timestamp>="2022-04-01 00:00:01")
#m <- subset(m, timestamp>="2022-04-01 00:00:01")
#n <- subset(n, timestamp>="2022-04-01 00:00:01")
#o <- subset(o, timestamp>="2022-04-01 00:00:01")


# subset to only relevant columns
a <- subset(a, select=c(timestamp,location.long,location.lat,individual.local.identifier)) 
b <- subset(b, select=c(timestamp,location.long,location.lat,individual.local.identifier)) 
c <- subset(c, select=c(timestamp,location.long,location.lat,individual.local.identifier)) 
d <- subset(d, select=c(timestamp,location.long,location.lat,individual.local.identifier)) 
e <- subset(e, select=c(timestamp,location.long,location.lat,individual.local.identifier))
f <- subset(f, select=c(timestamp,location.long,location.lat,individual.local.identifier))
g <- subset(g, select=c(timestamp,location.long,location.lat,individual.local.identifier))
h <- subset(h, select=c(timestamp,location.long,location.lat,individual.local.identifier))
#i <- subset(i, select=c(timestamp,location.long,location.lat,individual.local.identifier))
#j <- subset(j, select=c(timestamp,location.long,location.lat,individual.local.identifier))
#k <- subset(k, select=c(timestamp,location.long,location.lat,individual.local.identifier))
#l <- subset(l, select=c(timestamp,location.long,location.lat,individual.local.identifier))
#m <- subset(m, select=c(timestamp,location.long,location.lat,individual.local.identifier))
#n <- subset(n, select=c(timestamp,location.long,location.lat,individual.local.identifier))
#o <- subset(o, select=c(timestamp,location.long,location.lat,individual.local.identifier))


full.dat<-rbind(a,b,c,d,e,f,g,h)

str(full.d)

full.dat <- full.dat %>%
  rename(
    long = location.long,
    lat = location.lat,
    tagid = individual.local.identifier
  )
str(raw)
full.dat$tagid <- as.factor(full.dat$tagid) 


# order timestamps when including GPS and Argos data, and omit NAs
#full.d.1 <- full.d[order(full.d$tagid, full.d$timestamp),]
#full.d.1 <- na.omit(full.d)



##########################################
##########################################

# lets grab the world and U.S. states shapefiles from the maps package for plotting in ggplot2
basemap<-map_data("world") # this world map fills in Great Lakes, plot states/provinces/mexico individually
states<-map_data("state")
mexico<-subset(basemap, region %in% c("Mexico")) # subset out Mexico for plotting
summary()

###################################################################
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


#####################
#     Plot Code     #
####################

plot1 <- ggplot(full.dat, aes(location.long, location.lat)) + 
  theme_bw() +
  geom_polygon(data=states, aes(x=long, y=lat, group=group), fill="grey",color="white", size=0.35,) +
  geom_polygon(data=regions, aes(x=long, y=lat, group=group), fill="grey", color="white", size=0.35) +
  geom_polygon(data=mexico, aes(x=long, y=lat, group=group), fill="grey", color="white", size=0.35) +
  geom_path(aes(x=location.long,y=location.lat,group=individual.local.identifier,color=individual.local.identifier),size=0.9) + # adding movement paths of birds, grouping and coloring by bird ID and specify width of the line
  scale_fill_brewer(palette = "Set2") + # adding a color palette for the lines
  xlab("longitude") + # x label
  ylab("latitude") +  # y label
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), # removing major and minor grid lines (lat/long lines)
        legend.position="none", # specifying we do not want a legend
        axis.title=element_blank(), # font size of axis titles
        axis.text=element_text(size=9)) + # font size of lat/long ticks
  coord_fixed(xlim=c(-93,-72), ylim=c(40,55), ratio=1.5) # specifying the limits of the plotting area in lat/long and the mapping ratio (will distort projection)

plot(plot1)

#Wrap the plot to make it intercative!
plotly::ggplotly(plot1)

#Save the plot to the working directory
png("2022.movement.update.png", width=2000, height=1500, res=300) # saving map as a .png file with name, dimensions, and resolution


################################################################################################
??scale_color_viridis
############################################################################
