#################################
##        Nick Alioto           ##
##   Dynamic Brownian Bridge    ##
##       Movement Models +      ##
##  Utilization Distributions   ##
##       Fall Migrations        ##
##       Aug 6th 2024           ##
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
library(terra)
################################################################################
rm(list = ls())
rm()

# STEP 1 Read in data by each years cohort ----
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


# Step 2 convert timestamps to POSIXct ----

# Format time into POSIXct so it can be converted into a move object
# MAKE SURE IN EXCEL TIMESTAMP IS IN FORMAT  !!! # yyyy-mm-dd h:mm:ss # !!!
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
  H16$timestamps <- as.POSIXct(strptime(H16$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H17$timestamps <- as.POSIXct(strptime(H17$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H18$timestamps <- as.POSIXct(strptime(H18$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H19$timestamps <- as.POSIXct(strptime(H19$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H20$timestamps <- as.POSIXct(strptime(H20$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H21$timestamps <- as.POSIXct(strptime(H21$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H22$timestamps <- as.POSIXct(strptime(H22$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
}

# Need to check for NA's
sum(is.na(H16))



# Step 3 MOVE Objects ----
# covert to a move object - make sure to project the coordinates UTM, Northing and Easting -- * No Jack for Spring *
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

angell <- move(x=H8$location.long, y=H8$location.lat,
               time= as.POSIXct(H8$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
               proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
               data = H8, animal = H8$individual.local.identifier, sensor= H8$sensor.type) 

bucatini <- move(x=H9$location.long, y=H9$location.lat,
                 time= as.POSIXct(H9$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
                 proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
                 data = H9, animal = H9$individual.local.identifier, sensor= H9$sensor.type) 

ginger <- move(x=H10$location.long, y=H10$location.lat,
               time= as.POSIXct(H10$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
               proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
               data = H10, animal = H10$individual.local.identifier, sensor= H10$sensor.type) 

hallowee <- move(x=H11$location.long, y=H11$location.lat,
                 time= as.POSIXct(H11$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
                 proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
                 data = H11, animal = H11$individual.local.identifier, sensor= H11$sensor.type) 

mackinaw <- move(x=H12$location.long, y=H12$location.lat,
                 time= as.POSIXct(H12$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
                 proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
                 data = H12, animal = H12$individual.local.identifier, sensor= H12$sensor.type) 

petosegay <- move(x=H13$location.long, y=H13$location.lat,
                  time= as.POSIXct(H13$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
                  proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
                  data = H13, animal = H13$individual.local.identifier, sensor= H13$sensor.type) 

rapini <- move(x=H14$location.long, y=H14$location.lat,
               time= as.POSIXct(H14$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
               proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
               data = H14, animal = H14$individual.local.identifier, sensor= H14$sensor.type) 

voyageur  <- move(x=H15$location.long, y=H15$location.lat,
                  time= as.POSIXct(H15$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
                  proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
                  data = H15, animal = H15$individual.local.identifier, sensor= H15$sensor.type) 

carlile  <- move(x=H16$location.long, y=H16$location.lat,
                 time= as.POSIXct(H16$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
                 proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
                 data = H16, animal = H16$individual.local.identifier, sensor= H16$sensor.type)

cricket  <- move(x=H17$location.long, y=H17$location.lat,
                 time= as.POSIXct(H17$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
                 proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
                 data = H17, animal = H17$individual.local.identifier, sensor= H17$sensor.type) 

cypress  <- move(x=H18$location.long, y=H18$location.lat,
                 time= as.POSIXct(H18$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
                 proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
                 data = H18, animal = H18$individual.local.identifier, sensor= H18$sensor.type) 

hc2  <- move(x=H19$location.long, y=H19$location.lat,
             time= as.POSIXct(H19$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
             proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
             data = H19, animal = H19$individual.local.identifier, sensor= H19$sensor.type) 

lightfoot  <- move(x=H20$location.long, y=H20$location.lat,
                   time= as.POSIXct(H20$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
                   proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
                   data = H20, animal = H20$individual.local.identifier, sensor= H20$sensor.type) 

mh1   <- move(x=H21$location.long, y=H21$location.lat,
              time= as.POSIXct(H21$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
              proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
              data = H21, animal = H21$individual.local.identifier, sensor= H21$sensor.type) 

jack   <- move(x=H22$location.long, y=H22$location.lat,
               time= as.POSIXct(H22$timestamps, format= "Y%-%m-d% %H:%M:%S", tz= "UTC"),
               proj= CRS("+proj=longlat +datum=WGS84 +datum=WGS84"),
               data = H22, animal = H22$individual.local.identifier, sensor= H22$sensor.type) 

# Remove df's for memory in R, only need the move objects moving forward
rm(H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12,H13,H14,H15,H16,H17,H18,H19,H20,H21,H22)

# Check projection of coordinates for individula bird if need be
projection()



# Step 4 Separate 2021/2022/2023 migration routes per individual ----

# 2021
### selecting a range of days we want to look at i.e(Fall or Spring Migration Periods)
{
  rowan.fall <- rowan[as.Date(timestamps(rowan)) >= as.Date("2021-09-15")& as.Date(timestamps(rowan)) <= as.Date("2021-11-05")]
  herald.fall <- herald[as.Date(timestamps(herald)) >= as.Date("2021-10-15")& as.Date(timestamps(herald)) <= as.Date("2022-02-14")] #Adjusted to show whole route
  morpheus.fall <- morpheus[as.Date(timestamps(morpheus)) >= as.Date("2021-10-23")& as.Date(timestamps(morpheus)) <= as.Date("2021-12-20")]
  patagium.fall <- patagium[as.Date(timestamps(patagium)) >= as.Date("2021-10-14")& as.Date(timestamps(patagium)) <= as.Date("2022-02-05")]
  rip.fall <- rip[as.Date(timestamps(rip)) >= as.Date("2021-10-05") & as.Date(timestamps(rip)) <= as.Date("2021-11-10")]
  sam.fall <- sam[as.Date(timestamps(sam)) >= as.Date("2021-09-21")& as.Date(timestamps(sam)) <= as.Date("2022-02-08")]
  trinity.fall <- trinity[as.Date(timestamps(trinity)) >= as.Date("2021-10-12")& as.Date(timestamps(trinity)) <= as.Date("2021-10-27")] 
  
  # 2022
  patagium.fall.22 <- patagium[as.Date(timestamps(patagium)) >= as.Date("2022-10-15")& as.Date(timestamps(patagium)) <= as.Date("2022-11-25")]
  morpheus.fall.22 <- morpheus[as.Date(timestamps(morpheus)) >= as.Date("2022-10-06")& as.Date(timestamps(morpheus)) <= as.Date("2022-11-13")]
  rip.fall.22 <- rip[as.Date(timestamps(rip)) >= as.Date("2022-09-14")& as.Date(timestamps(rip)) <= as.Date("2022-11-06")]
  trinity.fall.22 <- trinity[as.Date(timestamps(trinity)) >= as.Date("2022-10-06")& as.Date(timestamps(trinity)) <= as.Date("2022-11-29")]
  sam.fall.22 <- sam[as.Date(timestamps(sam)) >= as.Date("2022-09-19")& as.Date(timestamps(sam)) <= as.Date("2022-10-26")] #26, 10-06, 09-01 works, 09-16 works
  angell.fall <- angell[as.Date(timestamps(angell)) >= as.Date("2022-09-13")& as.Date(timestamps(angell)) <= as.Date("2022-11-03")]
  bucatini.fall <- bucatini[as.Date(timestamps(bucatini)) >= as.Date("2022-09-22")& as.Date(timestamps(bucatini)) <= as.Date("2023-03-01")] 
  ginger.fall <- ginger[as.Date(timestamps(ginger)) >= as.Date("2022-10-03")& as.Date(timestamps(ginger)) <= as.Date("2022-10-31")]
  hallowee.fall <- hallowee[as.Date(timestamps(hallowee)) >= as.Date("2022-10-07") & as.Date(timestamps(hallowee)) <= as.Date("2022-10-20")]
  mackinaw.fall <- mackinaw[as.Date(timestamps(mackinaw)) >= as.Date("2022-09-23")& as.Date(timestamps(mackinaw)) <= as.Date("2022-11-15")]
  rapini.fall <- rapini[as.Date(timestamps(rapini)) >=  as.Date("2022-10-21")& as.Date(timestamps(rapini)) <= as.Date("2022-11-21")]
  petosegay.fall <- petosegay[as.Date(timestamps(petosegay)) >= as.Date("2022-10-17")& as.Date(timestamps(petosegay)) <= as.Date("2022-11-25")]
  voyageur.fall <- voyageur[as.Date(timestamps(voyageur)) >= as.Date("2022-10-14")& as.Date(timestamps(voyageur)) <= as.Date("2022-11-02")]
  
  # 2023 
  patagium.fall.23 <- patagium[as.Date(timestamps(patagium)) >= as.Date("2023-10-07")& as.Date(timestamps(patagium)) <= as.Date("2023-11-16")]
  morpheus.fall.23 <- morpheus[as.Date(timestamps(morpheus)) >= as.Date("2023-10-15")& as.Date(timestamps(morpheus)) <= as.Date("2023-11-8")]
  rip.fall.23 <-  rip[as.Date(timestamps(rip)) >= as.Date("2023-09-08")& as.Date(timestamps(rip)) <= as.Date("2023-11-11")]
  trinity.fall.23 <- trinity[as.Date(timestamps(trinity)) >= as.Date("2023-10-21")& as.Date(timestamps(trinity)) <= as.Date("2023-11-10")]
  sam.fall.23 <- sam[as.Date(timestamps(sam)) >= as.Date("2023-10-13")& as.Date(timestamps(sam)) <= as.Date("2023-10-23")]
  jack.fall.23 <-  jack[as.Date(timestamps(jack)) >= as.Date("2023-10-14")& as.Date(timestamps(jack)) <= as.Date("2023-11-21")]
  angell.fall.23 <- angell[as.Date(timestamps(angell)) >= as.Date("2023-10-07")& as.Date(timestamps(angell)) <= as.Date("2023-11-05")]
  mackinaw.fall.23 <- mackinaw[as.Date(timestamps(mackinaw)) >= as.Date("2023-10-11")& as.Date(timestamps(mackinaw)) <= as.Date("2023-12-09")]
  carlile.fall <- carlile[as.Date(timestamps(carlile)) >= as.Date("2023-11-04")& as.Date(timestamps(carlile)) <= as.Date("2023-11-25")]
  cricket.fall <- cricket[as.Date(timestamps(cricket)) >= as.Date("2023-10-25")& as.Date(timestamps(cricket)) <= as.Date("2023-11-15")]
  cypress.fall <- cypress[as.Date(timestamps(cypress)) >= as.Date("2023-09-11")& as.Date(timestamps(cypress)) <= as.Date("2023-10-31")]
  hc2.fall <- hc2[as.Date(timestamps(hc2)) >= as.Date("2023-10-06")& as.Date(timestamps(hc2)) <= as.Date("2023-10-23")] # heathcliff II
  lightfoot.fall <- lightfoot[as.Date(timestamps(lightfoot)) >= as.Date("2023-10-20")& as.Date(timestamps(lightfoot)) <= as.Date("2023-11-10")]
  mh1.fall <- mh1[as.Date(timestamps(mh1)) >= as.Date("2023-10-12")& as.Date(timestamps(mh1)) <= as.Date("2023-11-10")]
}

# Check the tracks to make sure they are correct migration path
plot(mackinaw.fall.23,
     xlab = "longitude", 
     ylab = "latitude", 
     main = "migration trajectory") # *Check to see your points show the migratory trajectory you expect, I cross referenced this in ARCGIS Pro* 

summary(sam.fall.22)




#Step 5 Projection----
# We need to project the data on a flat surface for dBBMM, sptransform is needed 
# Center=T: the center of the coordinate system is the center of the track. Units are in meters
{
  rowan.fall <- spTransform(rowan.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs") #center = TRUE
  
  herald.fall <- spTransform(herald.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  morpheus.fall <- spTransform(morpheus.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  patagium.fall <- spTransform(patagium.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  rip.fall <- spTransform(rip.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  sam.fall <- spTransform(sam.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  trinity.fall <- spTransform(trinity.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  patagium.fall.22 <- spTransform(patagium.fall.22, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  morpheus.fall.22 <- spTransform(morpheus.fall.22, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  rip.fall.22 <- spTransform(rip.fall.22, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  trinity.fall.22 <- spTransform(trinity.fall.22, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  sam.fall.22 <- spTransform(sam.fall.22, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  angell.fall <- spTransform(angell.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  bucatini.fall <- spTransform(bucatini.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  ginger.fall <- spTransform(ginger.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  hallowee.fall <- spTransform(hallowee.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  mackinaw.fall <- spTransform(mackinaw.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  petosegay.fall <- spTransform(petosegay.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  rapini.fall <- spTransform(rapini.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  voyageur.fall <- spTransform(voyageur.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  patagium.fall.23 <- spTransform(patagium.fall.23, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  morpheus.fall.23 <- spTransform(morpheus.fall.23, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  rip.fall.23 <- spTransform(rip.fall.23, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  trinity.fall.23 <- spTransform(trinity.fall.23, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  sam.fall.23 <- spTransform(sam.fall.23, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  jack.fall.23 <- spTransform(jack.fall.23, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  angell.fall.23 <- spTransform(angell.fall.23, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  mackinaw.fall.23 <- spTransform(mackinaw.fall.23, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  carlile.fall <- spTransform(carlile.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  cricket.fall <- spTransform(cricket.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  cypress.fall <- spTransform(cypress.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  hc2.fall <- spTransform(hc2.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  lightfoot.fall <- spTransform(lightfoot.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
  
  mh1.fall <- spTransform(mh1.fall, CRSobj = "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
}

# summary of the track
summary(rapini.fall)


#extent of the birds area used it has been converted in Step 5
extent(cricket.fall)



# Step 6 Time Lag Check ----

# Step 7 check time lag between points to inform DBBMM ----
# Check the time lag in between points for each bird if need be will reduce dbbmm run
# time
summary(timeLag(rapini.fall, "mins"))

#Create a time step object to put into the model for each individual bird
{
  ts1 <- median(timeLag(rowan.fall, "mins"))
  ts2 <- median(timeLag(herald.fall, "mins"))
  ts3 <- median(timeLag(morpheus.fall, "mins"))
  ts4 <- median(timeLag(patagium.fall, "mins"))
  ts5 <- median(timeLag(rip.fall, "mins"))
  ts6 <- median(timeLag(sam.fall, "mins"))
  ts7 <- median(timeLag(trinity.fall, "mins"))
  ts8 <- median(timeLag(rip.fall.22, "mins"))
  ts9 <- median(timeLag(trinity.fall.22, "mins"))
  ts10 <- median(timeLag(sam.fall.22, "mins"))
  ts11 <- median(timeLag(angell.fall, "mins"))
  ts12 <- median(timeLag(bucatini.fall, "mins"))
  ts13 <- median(timeLag(ginger.fall, "mins"))
  ts14 <- median(timeLag(hallowee.fall, "mins"))
  ts15 <- median(timeLag(mackinaw.fall, "mins"))
  ts16 <- median(timeLag(petosegay.fall, "mins"))
  ts17 <- median(timeLag(rapini.fall, "mins"))
  ts18 <- median(timeLag(voyageur.fall, "mins"))
  ts19 <- median(timeLag(morpheus.fall.22, "mins"))
  ts20 <- median(timeLag(patagium.fall.22, "mins"))
  ts21<- median(timeLag(patagium.fall.23, "mins"))
  ts22 <- median(timeLag(morpheus.fall.23, "mins"))
  ts23 <- median(timeLag(rip.fall.23, "mins"))
  ts24 <- median(timeLag(trinity.fall.23, "mins"))
  ts25 <- median(timeLag(sam.fall.23, "mins"))
  ts26 <- median(timeLag(jack.fall.23, "mins"))
  ts27 <- median(timeLag(angell.fall.23, "mins"))
  ts28 <- median(timeLag(mackinaw.fall.23, "mins"))
  ts29 <- median(timeLag(carlile.fall, "mins"))
  ts30 <- median(timeLag(cricket.fall, "mins"))
  ts31 <- median(timeLag(cypress.fall, "mins"))
  ts32 <- median(timeLag(hc2.fall, "mins"))
  ts33 <- median(timeLag(lightfoot.fall, "mins"))
  ts34 <- median(timeLag(mh1.fall, "mins"))
  
}

# Create a Raster to project all of our birds onto so that they all fit
ext <- 830000 # If Rip or Trinity doesn't fit, increase the extent the raster even more

Ra1 <- raster(extent(cricket.fall)+c(-ext,ext,-ext,ext), resolution=1000, crs = crs(cricket.fall), vals=NULL)
str(Ra1)
# ?raster
# originally ran at resolution 2500 possibly to low? Now at 1000 = 1km - not much of a difference



#STEP 8 RUn dBBMM's ----
#Create a dBBMM object for each bird and then plot the model

dBB.rowan <- brownian.bridge.dyn(rowan.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                 margin = 9,
                                 location.error = 5,
                                 raster = Ra1,
                                 time.step=ts1/15)

dBB.herald <- brownian.bridge.dyn(herald.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                  margin = 9,
                                  location.error = 5,
                                  raster = Ra1,
                                  time.step=ts2/15)

dBB.morpheus <- brownian.bridge.dyn(morpheus.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                    margin = 9,
                                    location.error = 5,
                                    raster = Ra1,
                                    time.step=ts3/15)

dBB.patagium <- brownian.bridge.dyn(patagium.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                    margin = 9,
                                    location.error = 5,
                                    raster = Ra1,
                                    time.step=ts4/15)


dBB.rip <- brownian.bridge.dyn(rip.fall, window.size = 25, #translates to a sliding window ~ 1 day
                               margin = 9,
                               location.error = 5,
                               raster = Ra1,
                               time.step= 15) ### COMMENT OUT FOR 'RIP' ###

dBB.sam <- brownian.bridge.dyn(sam.fall, window.size = 25, #translates to a sliding window ~ 1 day
                               margin = 9,
                               location.error = 5,
                               raster = Ra1,
                               time.step=ts6/15)


dBB.trinity <- brownian.bridge.dyn(trinity.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                   margin = 9,
                                   location.error = 5,
                                   raster = Ra1,
                                   time.step= ts7)

dBB.patagium2 <- brownian.bridge.dyn(patagium.fall.22, window.size = 25, #translates to a sliding window ~ 1 day
                                     margin = 9,
                                     location.error = 5,
                                     raster = Ra1,
                                     time.step=ts20/15)


dBB.morpheus2 <- brownian.bridge.dyn(morpheus.fall.22, window.size = 25, #translates to a sliding window ~ 1 day
                                     margin = 9,
                                     location.error = 5,
                                     raster = Ra1,
                                     time.step=ts19/15)

dBB.rip2 <- brownian.bridge.dyn(rip.fall.22, window.size = 25, #translates to a sliding window ~ 1 day
                                margin = 9,
                                location.error = 5,
                                raster = Ra1,
                                time.step= ts8/15)

dBB.trinity2 <- brownian.bridge.dyn(trinity.fall.22, window.size = 25, #translates to a sliding window ~ 1 day
                                    margin = 9,
                                    location.error = 5,
                                    raster = Ra1,
                                    time.step= ts9/15)

dBB.sam2 <- brownian.bridge.dyn(sam.fall.22, window.size = 25, #translates to a sliding window ~ 1 day
                                margin = 9,
                                location.error = 5,
                                raster = Ra1,
                                time.step= ts10/15)

dBB.angell <- brownian.bridge.dyn(angell.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                  margin = 9,
                                  location.error = 5,
                                  raster = Ra1,
                                  time.step= 15)

dBB.bucatini <- brownian.bridge.dyn(bucatini.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                    margin = 9,
                                    location.error = 5,
                                    raster = Ra1,
                                    time.step= ts12/15)

dBB.ginger <- brownian.bridge.dyn(ginger.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                  margin = 9,
                                  location.error = 5,
                                  raster = Ra1,
                                  time.step= 15)

dBB.hallowee <- brownian.bridge.dyn(hallowee.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                    margin = 9,
                                    location.error = 5,
                                    raster = Ra1,
                                    time.step= 15)

dBB.mackinaw <- brownian.bridge.dyn(mackinaw.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                    margin = 9,
                                    location.error = 5,
                                    raster = Ra1,
                                    time.step= 15)

dBB.petosegay <- brownian.bridge.dyn(petosegay.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                     margin = 9,
                                     location.error = 5,
                                     raster = Ra1,
                                     time.step= 15)

dBB.rapini <- brownian.bridge.dyn(rapini.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                  margin = 9,
                                  location.error = 5,
                                  raster = Ra1,
                                  time.step= 15)

dBB.voyageur <- brownian.bridge.dyn(voyageur.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                    margin = 9,
                                    location.error = 5,
                                    raster = Ra1,
                                    time.step= ts18/15)

dBB.patagium3 <- brownian.bridge.dyn(patagium.fall.23, window.size = 25, #translates to a sliding window ~ 1 day
                                     margin = 9,
                                     location.error = 5,
                                     raster = Ra1,
                                     time.step= ts21/15)

dBB.morpheus3 <- brownian.bridge.dyn(morpheus.fall.23, window.size = 25, #translates to a sliding window ~ 1 day
                                     margin = 9,
                                     location.error = 5,
                                     raster = Ra1,
                                     time.step= ts22/15)

dBB.rip3 <- brownian.bridge.dyn(rip.fall.23, window.size = 25, #translates to a sliding window ~ 1 day
                                margin = 9,
                                location.error = 5,
                                raster = Ra1,
                                time.step= 15)

dBB.trinity3 <- brownian.bridge.dyn(trinity.fall.23, window.size = 25, #translates to a sliding window ~ 1 day
                                    margin = 9,
                                    location.error = 5,
                                    raster = Ra1,
                                    time.step= ts24/15)

dBB.sam3 <- brownian.bridge.dyn(sam.fall.23, window.size = 25, #translates to a sliding window ~ 1 day
                                margin = 9,
                                location.error = 5,
                                raster = Ra1,
                                time.step= ts25/15)

dBB.jack <- brownian.bridge.dyn(jack.fall.23, window.size = 25, #translates to a sliding window ~ 1 day
                                margin = 9,
                                location.error = 5,
                                raster = Ra1,
                                time.step= ts26/15)

dBB.angell2 <- brownian.bridge.dyn(angell.fall.23, window.size = 25, #translates to a sliding window ~ 1 day
                                   margin = 9,
                                   location.error = 5,
                                   raster = Ra1,
                                   time.step= ts27/15) ##### COME BACK

dBB.mackinaw2 <- brownian.bridge.dyn(mackinaw.fall.23, window.size = 25, #translates to a sliding window ~ 1 day
                                     margin = 9,
                                     location.error = 5,
                                     raster = Ra1,
                                     time.step= ts28/15)

dBB.carlile <- brownian.bridge.dyn(carlile.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                   margin = 9,
                                   location.error = 5,
                                   raster = Ra1,
                                   time.step= 15)

dBB.cricket <- brownian.bridge.dyn(cricket.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                   margin = 9,
                                   location.error = 5,
                                   raster = Ra1,
                                   time.step= 15)

dBB.cypress <- brownian.bridge.dyn(cypress.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                   margin = 9,
                                   location.error = 5,
                                   raster = Ra1,
                                   time.step= 15)

dBB.hc2 <- brownian.bridge.dyn(hc2.fall, window.size = 25, #translates to a sliding window ~ 1 day
                               margin = 9,
                               location.error = 5,
                               raster = Ra1,
                               time.step= 15)

dBB.lightfoot <- brownian.bridge.dyn(lightfoot.fall, window.size = 25, #translates to a sliding window ~ 1 day
                                     margin = 9,
                                     location.error = 5,
                                     raster = Ra1,
                                     time.step= 15)

dBB.mh1 <- brownian.bridge.dyn(mh1.fall, window.size = 25, #translates to a sliding window ~ 1 day
                               margin = 9,
                               location.error = 5,
                               raster = Ra1,
                               time.step= 15)

# Look at the Brownian Variance values in the model
getMotionVariance()#dBBMM output - Brownian motion variance

#Plot dBBMM if we want to look at it, probably won't see much, take sqrt to better visualize
plot(sqrt(dBB.trinity)) 


# Step 9 UDs ----
# Get the Utilization Distribution for each bird

# 2021 fall migrations
{
  UDrowan <- getVolumeUD(dBB.rowan)
  UDherald <- getVolumeUD(dBB.herald)
  UDmorpheus <- getVolumeUD(dBB.morpheus)
  UDpatagium <- getVolumeUD(dBB.patagium)
  UDrip <- getVolumeUD(dBB.rip)
  UDsam <- getVolumeUD(dBB.sam)
  UDtrinity <- getVolumeUD(dBB.trinity)
  
  # 2022 fall migrations
  UDpatagium2 <- getVolumeUD(dBB.patagium2)
  UDmorpheus2 <- getVolumeUD(dBB.morpheus2)
  UDrip2 <- getVolumeUD(dBB.rip2)
  UDtrinity2 <- getVolumeUD(dBB.trinity2)
  UDsam2 <- getVolumeUD(dBB.sam2)
  UDangell <- getVolumeUD(dBB.angell)
  UDbucatini <- getVolumeUD(dBB.bucatini)
  UDginger <- getVolumeUD(dBB.ginger)
  UDhallowee <- getVolumeUD(dBB.hallowee)
  UDmackinaw <- getVolumeUD(dBB.mackinaw)
  UDpetosegay <- getVolumeUD(dBB.petosegay)
  UDrapini <- getVolumeUD(dBB.rapini)
  UDvoyageur <- getVolumeUD(dBB.voyageur)
  
  # 2023 fall migrations
  UDpatagium3 <- getVolumeUD(dBB.patagium3)
  UDmorpheus3 <- getVolumeUD(dBB.morpheus3)
  UDrip3 <- getVolumeUD(dBB.rip3)
  UDtrinity3 <- getVolumeUD(dBB.trinity3)
  UDsam3 <- getVolumeUD(dBB.sam3)
  UDjack <- getVolumeUD(dBB.jack)
  UDangell2 <- getVolumeUD(dBB.angell2)
  UDmackinaw2 <- getVolumeUD(dBB.mackinaw2)
  UDcarlile <- getVolumeUD(dBB.carlile)
  UDcricket <- getVolumeUD(dBB.cricket)
  UDcypress <- getVolumeUD(dBB.cypress)
  UDhc2 <- getVolumeUD(dBB.hc2)
  UDlightfoot <- getVolumeUD(dBB.lightfoot)
  UDmh1 <- getVolumeUD(dBB.mh1)
  
}


# Step 11 Bringing it all together ----
# Calculate a cumulative Utilization Distribution for all birds in a given season
# Argument = raster layer with a corresponding utilization distribution (one for each RTHA)

# stack all UD's together
fall.mig.stk <-  stack(UDrowan,UDherald,UDmorpheus,UDpatagium,UDrip,UDsam, UDtrinity,
                       UDpatagium2,UDmorpheus2,UDrip2,UDtrinity2,UDsam2,UDangell,UDbucatini,
                       UDginger, UDhallowee,UDmackinaw,UDpetosegay, UDrapini,UDvoyageur,
                       UDpatagium3,UDmorpheus3,UDrip3,UDtrinity3,UDsam3,UDjack,UDangell2,
                       UDmackinaw2, UDcarlile,UDcricket,UDcypress, UDhc2,UDlightfoot,UDmh1)  

sum.fallmig.days <- (41+25+42+37+36+15+15+41+38+51+54+17+51+132+23+13+53+38+31+17+39+24+
               64+20+10+38+29+58+20+20+50+16+21+28)

# Sum total values for all UDs - # divide number of migrations (34) * # days add 
fall.mig.sum <- sum(fall.mig.stk)/(34 * sum.fallmig.days)


plot(fall.mig.sum)

fall.cud <- fall.mig.sum

fa


# Normalize - divide the whole stack by the sum of all the values in the combined ratser so they sum to 1
fall.mig.cud <- fall.mig.sum/sum(values(fall.mig.sum)) 
plot(fall.mig.cud)

# Check that all values sum to 1 in Raster
sum(values(fall.mig.cud))


# Step 12 Save the Space use Raster for import into ARC GIS ----

# Save the UD as a Raster for all Fall migration routes
writeRaster(fall.mig.cud, filename='Chapter.1/Rasters/fall.all.24.tiff', overwrite=TRUE)










# Step 10 Plot UDs *not neccessary only to check space use of new mig events ----
# Plot the resulting UD's and add contour lines to define areas of usage. CHheck each bird, make sure full route is captured

par(mfrow=c(1,2)) # One row with 2 columns for plotting 2 separate graphs

plot(UDrapini, main="UD") # Main UD
plot(UDrapini, main="UD with Contour lines")
contour(UDrapini, levels= c(0.5,0.75,0.99), add=TRUE, lwd=c(0.5,0.75,0.99), lty=c(3,2,1))

# Save the UD as a Raster to be imported into ArcGIS to inspect individual birds
writeRaster(UDrapini, filename= 'rapini.tiff', overwrite=TRUE)


# Multiply each birds UD by the total migration duration - should be in days (calculated in different script)

# 2021 Durations # RUN EACH LINE INDIVIDUALLY

rowan.f.dur <-    (UDrowan * 41)               # (Add all days of migration)
herald.f.dur <-   (UDherald * 25)
morpheus.f.dur <- (UDmorpheus * 42)
patagium.f.dur <- (UDpatagium * 37)
rip.f.dur <-      (UDrip * 36)
sam.f.dur <-      (UDsam * 15)
trinity.f.dur <-  (UDtrinity * 15)

# 2022 Durations
patagium.f.dur2 <- (UDpatagium2 *41)
morpheus.f.dur2 <- (UDmorpheus2 * 38)
rip.f.dur2 <-  (UDrip2 * 51)
trinity.f.dur2 <-  (UDtrinity2 * 54)
sam.f.dur2 <-  (UDsam2 * 17)
angell.f.dur <-  (UDangell * 51)
bucatini.f.dur <-  (UDbucatini * 132)
ginger.f.dur <-  (UDginger * 23)
hallowee.f.dur <-  (UDhallowee * 13)
mackinaw.f.dur <-  (UDmackinaw * 53)
petosegay.f.dur <-  (UDpetosegay * 38)
rapini.f.dur <- (UDrapini * 31)
voyageur.f.dur <- (UDvoyageur * 17)

# 2023 Durations
patagium.f.dur3 <- (UDpatagium3*39)
morpheus.f.dur3 <- (UDmorpheus3 * 24)
rip.f.dur3 <- (UDrip3 * 64)
trinity.f.dur3 <- (UDtrinity3 * 20)
sam.f.dur3 <- (UDsam3 * 10)
jack.f.dur <- (UDjack * 38)
angell.f.dur2 <- (UDangell2 * 29)
mackinaw.f.dur2 <- (UDmackinaw2 * 58)
carilie.f.dur <- (UDcarlile * 20)
cricket.f.dur <- (UDcricket * 20)
cypress.f.dur <- (UDcypress * 50)
hc2.f.dur <- (UDhc2 * 16)
lightfoot.f.dur <- (UDlightfoot * 21)
mh1.f.dur <- (UDmh1 * 28)


# Step 11 Bringing it all together ----
# Calculate a cumulative Utilization Distribution for all birds in a given season
# Argument = raster layer with a corresponding utilization distribution (one for each RTHA)

# stack all UD's together
fall.mig.stk <-  stack(rowan.f.dur,herald.f.dur,morpheus.f.dur,patagium.f.dur,rip.f.dur,sam.f.dur,trinity.f.dur,patagium.f.dur2,
                       morpheus.f.dur2, rip.f.dur2,trinity.f.dur2,sam.f.dur2,angell.f.dur,bucatini.f.dur,ginger.f.dur,hallowee.f.dur,
                       mackinaw.f.dur,petosegay.f.dur, rapini.f.dur, voyageur.f.dur,patagium.f.dur3,morpheus.f.dur3,rip.f.dur3, 
                       trinity.f.dur3,sam.f.dur3,jack.f.dur,angell.f.dur2,mackinaw.f.dur2,carilie.f.dur,cricket.f.dur,
                       cypress.f.dur,hc2.f.dur,lightfoot.f.dur,mh1.f.dur)  # divide number of migrations * # days add to line 760


# Sum total values for all UDs
fall.mig.sum <- sum(fall.mig.stk)/(nmigs *ndays)

# Normalize - divide the whole stack by the sum of all the values in the combined ratser so they sum to 1
fall.mig.cud <- fall.mig.sum/sum(values(fall.mig.sum)) 
plot(fall.mig.cud)

# Check that all values sum to 1 in Raster
sum(values(fall.mig.cud))

# Let's get the contours from this raster need the Volume of the UD

raster2contour(fall.mig.cud)

# Step 12 Save the Space use Raster for import into ARC GIS ----

# Save the UD as a Raster for all Fall migration routes
writeRaster(fall.mig.cud, filename='Chapter.1/Rasters/fall.all.24.tiff', overwrite=TRUE)



plot(UDpatagium)
> foo <- UDpatagium
> foo[foo >0.95] = NA
> plot(foo)
> foo[foo >0.99] = NA
> plot(foo)
> foo[UDpatagium > 0.99] = NA
> plot(foo)
> foo <- UDpatagium
> foo[UDpatagium > 0.99] = NA
> plot(foo)








######## CODE GRAVEYARD ############### ----

# Save the UD as a Raster for all Fall migration routes
writeRaster(fall.perc, filename='fall.mig.perc1.tiff', overwrite=TRUE)


plot(UDrowan *(UDrowan <= 0.95))





### ------------------------------- ###
### Function for Raster Percentiles ###
### By: Nick Jaffe, For: N. Alioto  ###
### ------------------------------- ###

library(raster)

## Inputs 
# r: your raster (should sum to 1) in my case fall.mig.cum
# prc: a vector of percentiles (between 0:1) e.g. 0.99,0.75,0.5

UD.Perc <- function(r, prc){
  
  r.stk = stack()
  
  for(i in prc){
    
    thrsh = sort(r[])[max(which(cumsum(sort(r[])) <= 1-i))]
    r.stk = stack(r.stk,(r > thrsh))
    
  }
  
  names(r.stk) <- paste("P",prc*100,sep="")
  return(r.stk)
  
}

# Conver Fall Migs to Percentiles

fall.perc <- UD.Perc(r= fall.mig.cud, prc = c(0.99,0.75,0.5))

#Plot check
plot(fall.perc) # should have contours I defined
plot(sum(fall.perc)) #combining all contours into 1 plot

sum(values(fall.perc$P50 * fall.mig.cud))




# (Bad) Example 
r1 = raster(volcano); r1 = r1/sum(r1[])
op = UD.Perc(r=r1, prc = c(0.99,0.75,0.5))

plot(op) # Note the raster names in the plot titles
plot(sum(op)) # Can combine all three (note: you will prob. have to edit the labels)

sum(values(op$P50 * r1))


# Save the UD as a Raster for all Fall migration routes
writeRaster(fall.perc, filename='fall.mig.perc1.tiff', overwrite=TRUE)





# Extra Information Backup ----
# Migration dates for each specific bird to run fall 2021 dBBMM

# rowan.fall <- Rowan[as.Date(timestamps(Rowan))%in%c(as.Date("2021-09-15"):as.Date("2021-11-05"))]
# herald.fall <- Herald[as.Date(timestamps(Herald))%in%c(as.Date("2021-10-15"):as.Date("2022-02-14"))] #Adjusted to show whole route
# morpheus.fall <- morpheus[as.Date(timestamps(morpheus))%in%c(as.Date("2021-10-23"):as.Date("2021-12-20"))]
# patagium.fall <- patagium[as.Date(timestamps(patagium))%in%c(as.Date("2021-10-14"):as.Date("2022-02-05"))]
# rip.fall <- rip[as.Date(timestamps(rip))%in%c(as.Date("2021-10-05"):as.Date("2021-11-10"))]
# sam.fall <- sam[as.Date(timestamps(sam))%in%c(as.Date("2021-09-21"):as.Date("2022-02-08"))]
# trinity.fall <- trinity[as.Date(timestamps(trinity))%in%c(as.Date("2021-09-28"):as.Date("2021-12-05"))]

# 2022
#rip.fall.22 <- rip[as.Date(timestamps(rip))%in%c(as.Date("2022-09-14"):as.Date("2022-11-06"))]
#trinity.fall.22 <- trinity[as.Date(timestamps(trinity))%in%c(as.Date("2022-10-06"):as.Date("2022-11-29"))]
#sam.fall.22 <- sam[as.Date(timestamps(sam))%in%c(as.Date("2022-09-19"):as.Date("2022-10-26"))] #26, 10-06, 09-01 works, 09-16 works
#angell.fall <- angell[as.Date(timestamps(angell))%in%c(as.Date("2022-09-13"):as.Date("2022-11-03"))]
#bucatini.fall <- bucatini[as.Date(timestamps(bucatini))%in%c(as.Date("2022-09-22"):as.Date("2023-01-31"))] #last day of data in this CSV
#ginger.fall <- ginger[as.Date(timestamps(ginger))%in%c(as.Date("2022-10-03"):as.Date("2022-10-31"))]
#hallowee.fall <- hallowee[as.Date(timestamps(hallowee))%in%c(as.Date("2022-10-07"):as.Date("2022-10-20"))]
#mackinaw.fall <- mackinaw[as.Date(timestamps(mackinaw))%in%c(as.Date("2022-09-23"):as.Date("2022-11-15"))]
#petosegay.fall <- petosegay[as.Date(timestamps(petosegay))%in%c(as.Date("2022-10-17"):as.Date("2022-11-25"))]
#voyageur.fall <- voyageur[as.Date(timestamps(voyageur))%in%c(as.Date("2022-10-14"):as.Date("2022-11-02"))]



)

















