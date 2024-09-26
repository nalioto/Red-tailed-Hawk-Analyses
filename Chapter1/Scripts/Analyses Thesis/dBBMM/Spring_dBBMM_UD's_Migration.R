#################################
##        Nick Alioto           ##
##   Dynamic Brownian Bridge    ##
##       Movement Models +      ##
##  Utilization Distributions   ##
##       Spring Migrations      ##
##      August 14th 2024        ##
################################

################################################################################
library(raster) 
library(move) #function to run dBBMM 
library(sp)
library(adehabitatLT) #Analysis of animal movements
library(amt) #Animal movement tools
library (lubridate) #Transform time stamps from UTC(movebank) to appropriate time zone
library(tidyverse)
library(sf) # new version of SP
################################################################################
rm(list = ls())
rm()

# STEP 1 Data Read in ----
# Read in data 2021 birds
Hawks.1 <- list.files(path="Chapter.1/Data/2021_Red-tails",pattern = ".csv", full.names = T) # The order should be alphabetical

Hawks.21 <- do.call("rbind",lapply(Hawks.1, read.csv))
# In console type in Hawks.l to make sure it is in the order you want

#2022 birds
Hawks.2 <- list.files(path="Chapter.1/Data/2022_Red-tails",pattern = ".csv", full.names = T) # The order should be alphabetical

Hawks.22 <- do.call("rbind",lapply(Hawks.2, read.csv))

#2023 birds
Hawks.3 <- list.files(path="Chapter.1/Data/2023_Red-tails",pattern = ".csv", full.names = T) # The order should be alphabetical

Hawks.23 <- do.call("rbind",lapply(Hawks.3, read.csv))

# 2021 birds
{
  H1 <- read.csv(Hawks.1[7])
  H2 <- read.csv(Hawks.1[1])
  H3 <- read.csv(Hawks.1[4])
  H4 <- read.csv(Hawks.1[5])
  H5 <- read.csv(Hawks.1[6])
  H6 <- read.csv(Hawks.1[8])
  H7 <- read.csv(Hawks.1[9])
  
  # 2022 birds
  H8 <- read.csv(Hawks.2[1])
  H9 <- read.csv(Hawks.2[2])
  H10 <- read.csv(Hawks.2[3])
  H11 <- read.csv(Hawks.2[4])
  H12 <- read.csv(Hawks.2[5])
  H13 <- read.csv(Hawks.2[6])
  H14 <- read.csv(Hawks.2[7])
  H15 <- read.csv(Hawks.2[8])
  
  # 2023 birds
  H16 <- read.csv(Hawks.3[1])
  H17 <- read.csv(Hawks.3[2])
  H18 <- read.csv(Hawks.3[3])
  H19 <- read.csv(Hawks.3[4])
  H20 <- read.csv(Hawks.3[5])
  H21 <- read.csv(Hawks.3[6])
  H22 <- read.csv(Hawks.1[2]) # added in Jack down here so order of code birds wouldn't have to change (in 2021 cohort)
  
}

rm()

# Step 2 Format time ----
# Format time into POSIXct so it can be converted into a move object
{
  H1$timestamps <- as.POSIXct(strptime(H1$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H2$timestamps <- as.POSIXct(strptime(H2$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H3$timestamps <- as.POSIXct(strptime(H3$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H4$timestamps <- as.POSIXct(strptime(H4$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H5$timestamps <- as.POSIXct(strptime(H5$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H6$timestamps <- as.POSIXct(strptime(H6$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H7$timestamps <- as.POSIXct(strptime(H7$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H8$timestamps <- as.POSIXct(strptime(H8$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H9$timestamps <- as.POSIXct(strptime(H9$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H10$timestamps <- as.POSIXct(strptime(H10$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H11$timestamps <- as.POSIXct(strptime(H11$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H12$timestamps <- as.POSIXct(strptime(H12$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H13$timestamps <- as.POSIXct(strptime(H13$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H14$timestamps <- as.POSIXct(strptime(H14$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H15$timestamps <- as.POSIXct(strptime(H15$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H22$timestamps <- as.POSIXct(strptime(H22$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  
}


# Step 3 Make MOVE objects ----
# covert to a move object - make sure to project the coordinates UTM, Northing and Easting
rowan <- move(x=H1$location.long, y=H1$location.lat,
              time= as.POSIXct(H1$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
              proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
              data = H1, animal = H1$individual.local.identifier, sensor= H1$sensor.type) 

herald <- move(x=H2$location.long, y=H2$location.lat,
               time= as.POSIXct(H2$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
               proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
               data = H2, animal = H2$individual.local.identifier, sensor= H2$sensor.type) 


morpheus <- move(x=H3$location.long, y=H3$location.lat,
                 time= as.POSIXct(H3$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
                 proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
                 data = H3, animal = H3$individual.local.identifier, sensor= H3$sensor.type) 


patagium <- move(x=H4$location.long, y=H4$location.lat,
                 time= as.POSIXct(H4$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
                 proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
                 data = H4, animal = H4$individual.local.identifier, sensor= H4$sensor.type) 

rip <- move(x=H5$location.long, y=H5$location.lat,
            time= as.POSIXct(H5$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
            proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
            data = H5, animal = H5$individual.local.identifier, sensor= H5$sensor.type) 

sam <- move(x=H6$location.long, y=H6$location.lat,
            time= as.POSIXct(H6$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
            proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
            data = H6, animal = H6$individual.local.identifier, sensor= H6$sensor.type) 

trinity <- move(x=H7$location.long, y=H7$location.lat,
                time= as.POSIXct(H7$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
                proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
                data = H7, animal = H7$individual.local.identifier, sensor= H7$sensor.type)

jack <- move(x=H22$location.long, y=H22$location.lat,
             time= as.POSIXct(H22$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
             proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
             data = H22, animal = H22$individual.local.identifier, sensor= H22$sensor.type)


angell  <- move(x=H8$location.long, y=H8$location.lat,
                time= as.POSIXct(H8$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
                proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
                data = H8, animal = H8$individual.local.identifier, sensor= H8$sensor.type)

mackinaw <- move(x=H12$location.long, y=H12$location.lat,
                 time= as.POSIXct(H12$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
                 proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
                 data = H12, animal = H12$individual.local.identifier, sensor= H12$sensor.type)

rapini <- move(x=H14$location.long, y=H14$location.lat,
               time= as.POSIXct(H14$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
               proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
               data = H14, animal = H14$individual.local.identifier, sensor= H14$sensor.type)


voyageur <- move(x=H15$location.long, y=H15$location.lat,
                 time= as.POSIXct(H15$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
                 proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
                 data = H15, animal = H15$individual.local.identifier, sensor= H15$sensor.type)


rm()
#Check projection of coordinates for individula bird if need be
projection()

# Step 4 Filter by start and end of migration ----
### selecting a range of days we want to look at i.e(Fall or Spring Migration Periods)

# Removed Red I need to figure if that bird can be included
rm()

# 2022 Spring Migrations
rowan.spr <- rowan[(as.Date(timestamps(rowan))>= as.Date("2022-03-27") & as.Date(timestamps(rowan)) <= as.Date("2022-04-18"))]
herald.spr <- herald[as.Date(timestamps(herald))>= as.Date("2022-04-04")& as.Date(timestamps(herald)) <= as.Date("2022-04-24")] #Adjusted to show whole route unitl death
morpheus.spr <- morpheus[as.Date(timestamps(morpheus)) >= as.Date("2022-04-05")& as.Date(timestamps(morpheus)) <= as.Date("2022-04-21")]
patagium.spr <- patagium[as.Date(timestamps(patagium)) >= as.Date("2022-03-29") & as.Date(timestamps(patagium)) <= as.Date(("2022-04-22"))]
rip.spr <- rip[as.Date(timestamps(rip)) >= as.Date("2022-03-05") & as.Date(timestamps(rip)) <= as.Date(("2022-04-27"))]
sam.spr <- sam[as.Date(timestamps(sam)) >= as.Date("2022-04-08") & as.Date(timestamps(sam)) <= as.Date(("2022-04-27"))]
trinity.spr <- trinity[as.Date(timestamps(trinity)) >= as.Date("2022-04-04")& as.Date(timestamps(trinity)) <= as.Date(("2022-04-27"))]
jack.spr <-  jack[as.Date(timestamps(jack)) >= as.Date("2022-04-10")& as.Date(timestamps(jack)) <= as.Date(("2022-04-22"))]

# 2023 Spring Migrations
morpheus.spr.23 <- morpheus[as.Date(timestamps(morpheus)) >= as.Date("2023-04-11")& as.Date(timestamps(morpheus)) <= as.Date("2023-05-03")]
patagium.spr.23 <- patagium[as.Date(timestamps(patagium)) >= as.Date("2023-03-28")& as.Date(timestamps(patagium)) <= as.Date(("2023-04-12"))]
rip.spr.23 <- rip[as.Date(timestamps(rip)) >= as.Date("2023-02-19") & as.Date(timestamps(rip)) <= as.Date("2023-05-02")]
sam.spr.23 <- sam[as.Date(timestamps(sam)) >= as.Date("2023-03-16") & as.Date(timestamps(sam)) <= as.Date("2023-04-15")]
trinity.spr.23 <- trinity[as.Date(timestamps(trinity)) >= as.Date("2023-03-16")& as.Date(timestamps(trinity)) <= as.Date("2023-05-09")]
jack.spr.23 <-  jack[as.Date(timestamps(jack)) >= as.Date("2023-04-06")& as.Date(timestamps(jack)) <= as.Date("2023-06-08")]
angell.spr <- angell[as.Date(timestamps(angell)) >= as.Date("2023-04-12")& as.Date(timestamps(angell)) <= as.Date(("2023-04-28"))]
mackinaw.spr <- mackinaw[as.Date(timestamps(mackinaw)) >= as.Date("2023-04-25")& as.Date(timestamps(mackinaw)) <= as.Date(("2023-05-25"))]
rapini.spr <- rapini[as.Date(timestamps(rapini)) >= as.Date("2023-03-30")& as.Date(timestamps(rapini)) <= as.Date(("2023-05-09"))]
voyageur.spr <- voyageur[as.Date(timestamps(voyageur)) >= as.Date("2023-03-30")& as.Date(timestamps(voyageur)) <= as.Date(("2023-04-09"))]

summary()
# Mystack <- moveStack(list(rowan.fall,herald.fall,morpheus.fall,patagium.fall,rip.fall,sam.fall,trinity.fall), forceTz = "UTC")

# *Check to see your points show the migratory trajectory you expect, I cross referenced this in ARCGIS Pro* 
plot(voyageur.spr)


# Step 5 Project move objects onto flat surface ----
# We need to project the data on a flat surface for dBBMM, sptransform is needed 
# Center=T: the center of the coordinate system is the center of the track. Units are in meters
# sam.fall.2 <- spTransform(sam.fall, center=TRUE) *Old

rowan.spr <- spTransform(rowan.spr, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs") #center = TRUE

herald.spr <- spTransform(herald.spr, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

morpheus.spr <- spTransform(morpheus.spr, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

patagium.spr <- spTransform(patagium.spr, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

rip.spr <- spTransform(rip.spr, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

sam.spr <- spTransform(sam.spr, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

trinity.spr <- spTransform(trinity.spr, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

jack.spr <- spTransform(jack.spr, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

morpheus.spr.23 <- spTransform(morpheus.spr.23, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

patagium.spr.23 <- spTransform(patagium.spr.23, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

rip.spr.23 <- spTransform(rip.spr.23, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

sam.spr.23 <- spTransform(sam.spr.23, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

trinity.spr.23 <- spTransform(trinity.spr.23, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

jack.spr.23 <- spTransform(jack.spr.23, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

angell.spr <- spTransform(angell.spr, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

mackinaw.spr <- spTransform(mackinaw.spr, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

rapini.spr <- spTransform(rapini.spr, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")

voyageur.spr <- spTransform(voyageur.spr, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")


plot()
# extent of the birds area used
summary()
# extent of the birds area used it has been converted in Step 5
extent(r)

# Step 6 Check time lag to help inform timestep in the DBBMM ----
# Check the time lag in between points for each bird if need be
summary(timeLag(patagium.spr.22, "mins"))

# Create a time step object to put into the model for each individual bird
{
  ts1 <- median(timeLag(rowan.spr, "mins"))
  ts2 <- median(timeLag(herald.spr, "mins"))
  ts3 <- median(timeLag(morpheus.spr, "mins"))
  ts4 <- median(timeLag(patagium.spr, "mins"))
  ts5 <- median(timeLag(rip.spr, "mins"))
  ts6 <- median(timeLag(sam.spr, "mins"))
  ts7 <- median(timeLag(trinity.spr, "mins"))
  ts8 <- median(timeLag(jack.spr, "mins"))
  ts9 <- median(timeLag(morpheus.spr.23, "mins"))
  ts10 <- median(timeLag(patagium.spr.23, "mins"))
  ts11 <- median(timeLag(rip.spr.23, "mins"))
  ts12 <- median(timeLag(sam.spr.23, "mins"))
  ts13 <- median(timeLag(trinity.spr.23, "mins"))
  ts14 <- median(timeLag(jack.spr.23, "mins"))
  ts15 <- median(timeLag(angell.spr, "mins"))
  ts16 <- median(timeLag(mackinaw.spr, "mins"))
  ts17 <- median(timeLag(rapini.spr, "mins"))
  ts18 <- median(timeLag(voyageur.spr, "mins"))
}

# Create a Raster to project all of our birds onto so that they all fit
ext <- 900000 # If Rip or Trinity doesn't fit, increase the extent the raster even more

Ra1 <- raster(extent(rapini.spr)+c(-ext,ext,-ext,ext), resolution=1000, crs = crs(rowan.spr), vals=NULL)

# ?raster
# originally ran at resolution 2500 possibly to low? Now at 1000 = 1km - not much of a difference

# Step 7 Run movement models ----
# Create a dBBMM object for each bird and then plot the model
# Note is the median time lag between points is 1 min, set the step to 15 to increase computation time
# No need to estimates spaces use if we know where the bird is!

dBB.rowan <- brownian.bridge.dyn(rowan.spr, window.size = 25, #translates to a sliding window ~ 1 day
                                 margin = 9,
                                 location.error = 5,
                                 raster = Ra1,
                                 time.step= 15)

dBB.herald <- brownian.bridge.dyn(herald.spr, window.size = 25, #translates to a sliding window ~ 1 day
                                  margin = 9,
                                  location.error = 5,
                                  raster = Ra1,
                                  time.step= ts2/15)

dBB.morpheus <- brownian.bridge.dyn(morpheus.spr, window.size = 25, #translates to a sliding window ~ 1 day
                                    margin = 9,
                                    location.error = 5,
                                    raster = Ra1,
                                    time.step= 15)

dBB.patagium <- brownian.bridge.dyn(patagium.spr, window.size = 25, #translates to a sliding window ~ 1 day
                                    margin = 9,
                                    location.error = 5,
                                    raster = Ra1,
                                    time.step= 15) 


dBB.rip <- brownian.bridge.dyn(rip.spr, window.size = 25, #translates to a sliding window ~ 1 day
                               margin = 9,
                               location.error = 5,
                               raster = Ra1,
                               time.step= 15) 

dBB.sam <- brownian.bridge.dyn(sam.spr, window.size = 25, #translates to a sliding window ~ 1 day
                               margin = 9,
                               location.error = 5,
                               raster = Ra1,
                               time.step= ts6/15)

dBB.trinity <- brownian.bridge.dyn(trinity.spr, window.size = 25, #translates to a sliding window ~ 1 day
                                   margin = 9,
                                   location.error = 5,
                                   raster = Ra1,
                                   time.step= 15)

dBB.jack <- brownian.bridge.dyn(jack.spr, window.size = 25, #translates to a sliding window ~ 1 day
                                margin = 9,
                                location.error = 5,
                                raster = Ra1,
                                time.step=15)

dBB.morpheus2 <- brownian.bridge.dyn(morpheus.spr.23, window.size = 25, #translates to a sliding window ~ 1 day
                                     margin = 9,
                                     location.error = 5,
                                     raster = Ra1,
                                     time.step= 15) 

dBB.patagium2 <- brownian.bridge.dyn(patagium.spr.23, window.size = 25, #translates to a sliding window ~ 1 day
                                     margin = 9,
                                     location.error = 5,
                                     raster = Ra1,
                                     time.step= 15) 

dBB.rip2 <- brownian.bridge.dyn(rip.spr.23, window.size = 25, #translates to a sliding window ~ 1 day
                                margin = 9,
                                location.error = 5,
                                raster = Ra1,
                                time.step= 15) 

dBB.sam2 <- brownian.bridge.dyn(sam.spr.23, window.size = 25, #translates to a sliding window ~ 1 day
                                margin = 9,
                                location.error = 5,
                                raster = Ra1,
                                time.step= 15) 

dBB.trinity2 <- brownian.bridge.dyn(trinity.spr.23, window.size = 25, #translates to a sliding window ~ 1 day
                                    margin = 9,
                                    location.error = 5,
                                    raster = Ra1,
                                    time.step= ts13/15) 

dBB.jack2 <- brownian.bridge.dyn(jack.spr.23, window.size = 25, #translates to a sliding window ~ 1 day
                                 margin = 9,
                                 location.error = 5,
                                 raster = Ra1,
                                 time.step= 15) 


dBB.angell <- brownian.bridge.dyn(angell.spr, window.size = 25, #translates to a sliding window ~ 1 day
                                  margin = 9,
                                  location.error = 5,
                                  raster = Ra1,
                                  time.step= 15) 

dBB.mackinaw <- brownian.bridge.dyn(mackinaw.spr, window.size = 25, #translates to a sliding window ~ 1 day
                                    margin = 9,
                                    location.error = 5,
                                    raster = Ra1,
                                    time.step= 15) 

dBB.rapini <- brownian.bridge.dyn(rapini.spr, window.size = 25, #translates to a sliding window ~ 1 day
                                  margin = 9,
                                  location.error = 5,
                                  raster = Ra1,
                                  time.step= 15) 

dBB.voyageur <- brownian.bridge.dyn(voyageur.spr, window.size = 25, #translates to a sliding window ~ 1 day
                                    margin = 9,
                                    location.error = 5,
                                    raster = Ra1,
                                    time.step= 15) 

rm()
#Look at the Brownian Variance values in the model
getMotionVariance()#dBBMM output - Brownian motion variance

#Plot dBBMM if we want to look at it, probably won't see much, take sqrt to better visualize
plot(sqrt(dBB.)) 


# Step 8 Get the Utilization Distributions ----
# Get the Utilization Distribution for each bird
{
  # 2022 Spring Migrations
  UDrowan <- getVolumeUD(dBB.rowan)
  UDherald <- getVolumeUD(dBB.herald)
  UDmorpheus <- getVolumeUD(dBB.morpheus)
  UDpatagium <- getVolumeUD(dBB.patagium)
  UDrip <- getVolumeUD(dBB.rip)
  UDsam <- getVolumeUD(dBB.sam)
  UDtrinity <- getVolumeUD(dBB.trinity)
  UDjack <- getVolumeUD(dBB.jack)
  
  
  # 2023 Spring Migrations
  UDmorpheus2 <- getVolumeUD(dBB.morpheus2)
  UDpatagium2 <- getVolumeUD(dBB.patagium2)
  UDrip2 <- getVolumeUD(dBB.rip2)
  UDsam2 <- getVolumeUD(dBB.sam2)
  UDtrinity2 <- getVolumeUD(dBB.trinity2)
  UDjack2 <- getVolumeUD(dBB.jack2)
  UDangell <- getVolumeUD(dBB.angell)
  UDmackinaw <- getVolumeUD(dBB.mackinaw)
  UDrapini <- getVolumeUD(dBB.rapini)
  UDvoyageur <- getVolumeUD(dBB.voyageur)
}

# Step 9 Visually check the UD's ----
# Plot the resulting UD's and add contour lines to define areas of usage. Check each bird, make sure full route is captured

par(mfrow=c(1,2)) # One row with 2 columns for plotting 2 separate graphs

plot(UDrowan, main="UD") # Main UD
plot(UDrowan, main="UD with Contour lines")
contour(UDrowan, levels= c(0.5,0.75,0.99), add=TRUE, lwd=c(0.5,0.75,0.99), lty=c(3,2,1))


# Step 10 Individual Check----
# Save the UD as a Raster to be imported into ArcGIS to inspect individual birds before
# combining them
# 2022
writeRaster(UDrowan, filename='Chapter.1/Rasters/rowan.spr.tiff', overwrite=TRUE)
writeRaster(UDherald, filename='Chapter.1/Rasters/herald.spr.tiff', overwrite=TRUE)
writeRaster(UDmorpheus, filename='Chapter.1/Rasters/morpheus.spr.tiff', overwrite=TRUE)
writeRaster(UDpatagium, filename='Chapter.1/Rasters/patagium.spr.tiff', overwrite=TRUE)
writeRaster(UDrip, filename='Chapter.1/Rasters/rip.spr.tiff', overwrite=TRUE)
writeRaster(UDsam, filename='Chapter.1/Rasters/sam.spr.tiff', overwrite=TRUE)
writeRaster(UDtrinity, filename='Chapter.1/Rasters/trinity.spr.tiff', overwrite=TRUE)
writeRaster(UDjack, filename='Chapter.1/Rasters/jack.spr.tiff', overwrite=TRUE)


# 2023
writeRaster(UDmorpheus2, filename='Chapter.1/Rasters/morpheus.spr23.tiff', overwrite=TRUE)
writeRaster(UDpatagium2, filename='Chapter.1/Rasters/patagium.spr23.tiff', overwrite=TRUE)
writeRaster(UDrip2, filename='Chapter.1/Rasters/rip.spr23.tiff', overwrite=TRUE)
writeRaster(UDsam2, filename='Chapter.1/Rasters/sam.spr23.tiff', overwrite=TRUE)
writeRaster(UDtrinity2, filename='Chapter.1/Rasters/trinity.spr23.tiff', overwrite=TRUE)
writeRaster(UDjack2, filename='Chapter.1/Rasters/jack.spr23.tiff', overwrite=TRUE)
writeRaster(UDangell, filename='Chapter.1/Rasters/angell.spr.tiff', overwrite=TRUE)
writeRaster(UDmackinaw, filename='Chapter.1/Rasters/mackinaw.spr.tiff', overwrite=TRUE)
writeRaster(UDrapini, filename='Chapter.1/Rasters/rapini.spr.tiff', overwrite=TRUE)
writeRaster(UDvoyageur, filename='Chapter.1/Rasters/voyageur.spr.tiff', overwrite=TRUE)


# Step 11 Raster weighting & summing ----
# Multiply each birds UD by the total migration duration - should be in days(calculated in different script)

# Step 11 multiply the raster by migration duration ----
# 2022 Spring Migrations
{ 
  rowan.s.dur <-    (UDrowan * 18)
  herald.s.dur <-   (UDherald * 11 )
  morpheus.s.dur <- (UDmorpheus * 13)
  patagium.s.dur <- (UDpatagium * 9)
  rip.s.dur <-      (UDrip * 52)
  sam.s.dur <-      (UDsam * 11)
  trinity.s.dur <-  (UDtrinity * 19)
  jack.s.dur <- (UDjack * 11)
  
  
  # Spring 2023 Migrations
  morpheus.s.dur.2 <- (UDmorpheus2 * 21)
  patagium.s.dur.2 <-(UDpatagium2 * 9)
  rip.s.dur.2 <- (UDrip2 * 72)
  sam.s.dur.2 <- (UDsam2 * 30)
  trinity.s.dur.2 <- (UDtrinity2 * 6)
  jack.s.dur.2 <- (UDjack2 * 62)
  angell.s.dur <-(UDangell * 16)
  mackinaw.s.dur <-(UDmackinaw * 30)
  rapini.s.dur <-(UDrapini * 40)
  voyageur.s.dur <-(UDvoyageur * 7)
  
}

# Step 12 Cumulative UD ----
# Calculate a cumulative Utilization Distribution for all birds in a given season
# Argument = raster layer with a corresponding utilization distribution (one for each RTHA)

# stack all UDs together into a universal UD
spr.mig.stk <- stack(rowan.s.dur, herald.s.dur, morpheus.s.dur,
                     patagium.s.dur, rip.s.dur, sam.s.dur,
                     trinity.s.dur, jack.s.dur, morpheus.s.dur.2,
                     patagium.s.dur.2, rip.s.dur.2 , sam.s.dur.2,
                     trinity.s.dur.2, jack.s.dur.2, angell.s.dur, mackinaw.s.dur,
                     rapini.s.dur, voyageur.s.dur) 

# Sum total values for all UDs
spr.mig.sum <- sum(spr.mig.stk)

# Normalize - divide the whole stack by the sum of all the values in the combined raster so it sums to 1
spr.mig.cud <- spr.mig.sum/sum(values(spr.mig.sum)) 
plot(spr.mig.cud)

#Check that all values sum to 1 in Raster (we will know it everything has been weighted proportionally)
sum(values(spr.mig.cud))

# Step 13 Save the Raster to bring into ARCGIS ----
# Save the UD as a Raster for all Fall migration routes
writeRaster(spr.mig.cud, filename='Chapter.1/Rasters/spr.all.24.tiff', overwrite=TRUE)




# spr.all.23 - Raster I used at the AOS 2023 conference #

################################################################################
