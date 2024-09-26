########################################
##                                   ##
##          Nick Alioto              ##
##      Net-Displacement models      ##
##   to define migratory movements   ##
##          June 30th 2022           ##
#######################################

################################################################################
library(adehabitatLT)
library(amt) # The main package for ND models
library(rgeos)
library(rgdal)
library(sf)
library(geosphere)#Create the displacement values
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
###############################################################################3
rm(list = ls())

??amt()
??adehabitatLT()

#Read in individual bird, one at a time (*add UTM coordinates when downloading the data)
H1 <- read.csv("Data/2021 Red-tails/Herald.csv", header = TRUE)


# We need to split the time stamp column in to two new columns one for day and one for time
H2 <- str_split_fixed(H1$timestamp, " ", 2) %>% # Tells where there is a space in column to split it
      data.frame() %>% 
      rename( day = X1, time = X2) %>%  #rename the split columns
      cbind(H1, .)


#Convert the timestamps into proper format see amt() for more information
H2$ts <- as.POSIXct(lubridate::ymd(H2$day) +
                      lubridate::hms(H2$time))

#Check structure make sure it looks right
str(H2)

# Remember to download the data with UTM's included from Movebank
#Filter out all the columns we don't need in the data frame; ts denotes the new time column in proper format POSIXct
H2 <- H2[, (colnames(H2) %in% c('utm.easting', 'utm.northing', 
                                     'ts', 'individual.local.identifier'))]


#Data should be clean so we can make a track of the movement using amt() package
track.1 <- make_track(H2,utm.easting, utm.northing, ts, id= individual.local.identifier)


#Calculate the net-squared displacemnt for each bird using nsd() function
# requires and argument that is a track (just created above)
# take sqrt of displacemnt values - convert to km = easier interpretation

nsd.1 <- sqrt(nsd(track.1))

#divide values by 1000 to get units to KM 
nsd.1 <- nsd.1/1000


#Plot the net-displacement model
plot(nsd.1)


# Turn the track into a data frame so we can plot in ggplot
data <- as.data.frame(track.1)

#added the net displacement values to my data frame
data$net.displacement <- nsd.1

# Rename the the timestamp column
names(data)[names(data) == 't_'] <- 'time.stamp'


# Let's clean up these Net-Displacement Plots

plot1 <- ggplot(data = data) +
  geom_line(mapping=aes(x=time.stamp, y=net.displacement),
            color = "black",
            linetype = 1,
            size = 0.5) +
  theme_bw() +
  labs(title = "Net Displacement Model",
       x = "Month/Year",
       y = "Net Displacement (km)") 

## extra chunk in case the x axis does not show up for a particular bird

#  + scale_x_continuous(breaks=pretty(data$time.stamp, n=6),
                     #labels = c( "April 2021", "July 2021",
                                # "October 2021", "Jan 2022",
                                # "April 2022",  "July 2022", "October 2022")) 
plot(plot1)

?scale_x_continuous


################################################################################
# Let's try to plot date against the latitude
plot2 <- ggplot(data = H1) +
  geom_line(mapping=aes(x=timestamp, y=location.lat),
            color = "black",
            linetype = 1,
            size = 0.5) +
  theme_bw() +
  labs(title = "Latitudinal movement",
       x = "Month/Year",
       y = "Latitude") 

plot(plot2)
