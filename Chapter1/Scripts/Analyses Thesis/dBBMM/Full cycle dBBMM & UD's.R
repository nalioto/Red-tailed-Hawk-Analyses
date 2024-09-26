#################################
##        Nick Alioto           ##
##   Dynamic Brownian Bridge    ##
##       Movement Models +      ##
##  Utilization Distributions   ##
##           Full Cycle         ##
##      June 15th 2022          ##
################################

################################################################################
library(raster) 
library(move) #function to run dBBMM 
library(sp)
library(adehabitatLT) #Analysis of animal movements
library(amt) #Animal movement tools
library (lubridate) #Transform time stamps from UTC(movebank) to appropriate time zone
library(tidyverse)
library(rgdal) # To save as an ESRI shapefile to bring into Arc later
################################################################################
rm(list = ls())
rm()

# STEP 1
# Read in data
H1 <- read.csv("Chapter.1/Data/2021 Red-tails/Rowan.csv", header = TRUE)

# Step 2
#Format time into POSIXct so it can be converted into a move object
H1$timestamps <- as.POSIXct(strptime(H1$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")


#Step 3
#covert to a move object - make sure to project the coordinates
Rowan <- move(x=H1$location.long, y=H1$location.lat,
              time= as.POSIXct(H1$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
              proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
              data = H1, animal = H1$individual.local.identifier, sensor= H1$sensor.type) 

#Check projection of coordinates
projection(Rowan)


#Step 4
# Thin the whole annual cycle to one loction per day

full.data.thin <- thinTrackTime(Rowan, interval= as.difftime(1, units='days'),
                                 tolerance = as.difftime(1,"days")) #criteria = c("closest","first", "all"))



plot(Rowan.full) # *Check to see your points show the migratory trajectory you expect* 

#Step 5
# We need to project the data on a flat surface for dBBMM, sptransform is needed 
# Center=T: the center of the coordinate system is the center of the track. Units are in meters
Rowan.full <- spTransform(full.data.thin, center=TRUE)                # #CRSobj = "+proj=utm") - if from movebank directly

#Step 6
#Check the time lag in between points
summary(timeLag(Rowan.full, "mins"))

#Create a time step object to put into  the model & and a Raster for it to 
ts <- median(timeLag(Rowan.full, "mins"))

# Create a Raster to project onto
ext <- 200000
Ra <- raster(extent(Rowan.spring)+c(-ext,ext,-ext,ext), resolution=1000, crs = crs(Rowan.fall), vals=NULL)
str(Ra)

# originally ran at resolution 2500 possibly to low? Now at 1000 = 1km

extent(Rowan.fall) #extent of the birds area used


#STEP 7
#Create a dBBMM object for each bird and then plot the model
dBB.Rowan <- brownian.bridge.dyn(Rowan.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                 margin = 9,
                                 location.error = 5,
                                 raster = Ra,
                                 time.step=ts/15) #60,180,240 other time steps to play with

#Look at the Brownian Variance values in the model
getMotionVariance(dBB.Rowan)


#Plot dBBMM if we want to look at it, probably won't see much   ####################
plot(dBB.Rowan, main = "dBBMM")                                 ?brownian.bridge.dyn
?raster
####################

# Step 8
# Get the Utilization Distribution
UDrowan <- getVolumeUD(dBB.Rowan)

# Step 9
# Plot the resulting UD's and add contour lines to define areas of usage

par(mfrow=c(1,2)) # One row with 2 columns for plotting 2 separate graphs

plot(UDrowan, main="UD") # Main UD
plot(UDrowan, main="UD with Contour lines")
contour(UDrowan, levels= c(0.5,0.75,0.99), add=TRUE, lwd=c(0.5,0.75,0.99), lty=c(3,2,1))



#Calculate a cumulative Utilization Distribution for all birds in a given season
# Argument = raster layer with a corresponding utilization distribution (one for each RTHA)

fall.mig.cud <- hr_cud

spr.mig.cud <- hr_cud

## Save the UD as a Raster to be imported into R
writeRaster(UDrowan, filename='rowan.f.tiff', overwrite=TRUE)

