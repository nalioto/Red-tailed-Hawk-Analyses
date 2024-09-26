#######################################
##  Updated Plot code for RTHA Tracks ##
##             Nick Alioto            ##
##           June,14,2024             ##
########################################

#---------------------- Libraries needed
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(tidyverse)
library(plotly)
library(bayesmove)
#------------------------

## clear workspace
rm(list=ls())

## bring in raw movement data
## Use front slash to tell R the order of folders to go through to find CSV
H1 <- read.csv("Chapter.1/Data/2022_Red-tails/Angell.csv", header=TRUE) 
H2 <- read.csv("Chapter.1/Data/2021_Red-tails/Trinity.csv", header=TRUE) 
H3 <- read.csv("Chapter.1/Data/2022_Red-tails/Mackinaw.csv", header=TRUE) 
H4 <- read.csv("Chapter.1/Data/2023_Red-tails/Lightfoot.csv", header=TRUE) 
H5 <- read.csv("Chapter.1/Data/2023_Red-tails/Cyprus.csv", header=TRUE) 

# subset each for timeframe we want to plot
H1$timestamp <- ymd_hms(H1$timestamp,tz='EST')

#filter for this season. change if necessary or just plot all the trACK as one color
filtered <- filter(H1, timestamp >= "2022-04-01 00:01:00.000") # Depends on Deployment Date
filtered$spr.24 <- ifelse(H3$timestamp >= "2024-03-25 00:01:00.000", 1, 2) # For separating movements
rm(filtered)


str(b)

# For Multiple BIRDS ----
#Subset times we want for each bird
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
hawk <- rbind(f,d)
str(hawk)

# Need shape files to plot ----

# Get United states and Canada shape file
US <- ne_states(country = "United States of America", returnclass = "sf")

CAN <- ne_states(country = "Canada", returnclass = "sf")

# Merge to form North America
NorAm <- rbind(US, CAN)


# Plot Code ----

# Wrap your ggplot with plottly function to make it interactive
plotly::ggplotly()


ggplot() +
  geom_sf(data = NorAm, fill = "grey", color = "white", size = 0.2) +
  geom_path(data = filtered, aes(x=location.long,y=location.lat), linewidth = 1, color = "magenta") +  #c("darkviolet","orange2")[filtered$spr.24]) +
  scale_color_viridis_d()+
  theme_bw() +
  xlab("") + 
  ylab("") + 
  theme(panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(), 
       panel.background = element_blank()) +
  theme(panel.grid.minor=element_blank(),
        legend.title= element_blank(),
        axis.title=element_text(size = 12),   # element_text(size=12) adjust size   
        axis.text=element_text(size = 10),
        axis.ticks=element_blank()) +          # element_text(size=12) adjust size   
  coord_sf(xlim=c(-93,-75), ylim=c(37.5,53), expand = FALSE)

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


# SHINY APP In progress ----

# Use shiny app from (Bayesmove) to explore further

p1 %>%
  rename(id = , date = timestamp, x = location.lon, y = location.lat) %>%
  bayesmove::shiny_tracks(espg =4326)



#  CODE Graveyard ----

# Rename columns not totally neccessary
hawk <- hawk %>%
  rename(
    long = location.long,
    lat = location.lat,
    tagid = individual.local.identifier
  )
str(hawk)
hawk$tagid <- as.factor(hawk$tagid) 
str(hawk)
