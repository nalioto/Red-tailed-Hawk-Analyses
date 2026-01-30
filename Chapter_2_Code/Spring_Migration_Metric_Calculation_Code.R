#######################################
##  Spring Migration Parameter Code  ##
##        Nick Alioto                ##
##       August 13 2024              ##
#######################################

################################################################################
library(amt)
library(adehabitatLT)
library(tidyverse)
################################################################################
rm(list = ls())

##### Separating Spring and Fall ###########

# use amt package for this
# We want to define start/end dates, duration, Distance, cumulative distance, migration
# straightness and speed
# Need to subset data into migration periods for individual metrics
################################################################################
rm()

# STEP 1
# Read in data 2021 birds
Hawks.1 <- list.files(path="Chapter.1/Data/2021_Red-tails",pattern = ".csv", full.names = T) # The order should be alphabetical

Hawks.21 <- do.call("rbind",lapply(Hawks.1, read.csv))
# In console type in Hawks.l to make sure it is in the order you want

# 2022 birds
Hawks.2 <- list.files(path="Chapter.1/Data/2022_Red-tails",pattern = ".csv", full.names = T) # The order should be alphabetical

Hawks.22 <- do.call("rbind",lapply(Hawks.2, read.csv))

# 2023 birds
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
  H23 <- read.csv(Hawks.3[7]) # Added in Roger That- checked in 2 years after initial deployment
}

# Step 2
#Format time into POSIXct so it can be converted into a move object
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
  H23$timestamps <- as.POSIXct(strptime(H23$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  
}

# Convert the times tamps from a 'character' to 'numeric' class for filtering purposes
{
  H1$ts.numeric <- as.numeric(H1$timestamps)
  H2$ts.numeric <- as.numeric(H2$timestamps)
  H3$ts.numeric <- as.numeric(H3$timestamps)
  H4$ts.numeric <- as.numeric(H4$timestamps)
  H5$ts.numeric <- as.numeric(H5$timestamps)
  H6$ts.numeric <- as.numeric(H6$timestamps)
  H7$ts.numeric <- as.numeric(H7$timestamps)
  H8$ts.numeric <- as.numeric(H8$timestamps)
  H9$ts.numeric <- as.numeric(H9$timestamps)
  H10$ts.numeric <- as.numeric(H10$timestamps)
  H11$ts.numeric <- as.numeric(H11$timestamps)
  H12$ts.numeric <- as.numeric(H12$timestamps)
  H13$ts.numeric <- as.numeric(H13$timestamps)
  H14$ts.numeric <- as.numeric(H14$timestamps)
  H15$ts.numeric <- as.numeric(H15$timestamps)
  H16$ts.numeric <- as.numeric(H16$timestamps)
  H17$ts.numeric <- as.numeric(H17$timestamps)
  H18$ts.numeric <- as.numeric(H18$timestamps)
  H19$ts.numeric <- as.numeric(H19$timestamps)
  H20$ts.numeric <- as.numeric(H20$timestamps)
  H21$ts.numeric <- as.numeric(H21$timestamps)
  H22$ts.numeric <- as.numeric(H22$timestamps)
  H23$ts.numeric <- as.numeric(H23$timestamps)
  
  
}

# Convert our timestamps to Julian Days for each hawk
{
  
  H1$julian.day <- format(H1$timestamps, "%j") 
  H2$julian.day <- format(H2$timestamps, "%j") 
  H3$julian.day <- format(H3$timestamps, "%j") 
  H4$julian.day <- format(H4$timestamps, "%j") 
  H5$julian.day <- format(H5$timestamps, "%j") 
  H6$julian.day <- format(H6$timestamps, "%j") 
  H7$julian.day <- format(H7$timestamps, "%j") 
  H8$julian.day <- format(H8$timestamps, "%j") 
  H9$julian.day <- format(H9$timestamps, "%j") 
  H10$julian.day <- format(H10$timestamps, "%j") 
  H11$julian.day <- format(H11$timestamps, "%j") 
  H12$julian.day <- format(H12$timestamps, "%j") 
  H13$julian.day <- format(H13$timestamps, "%j") 
  H14$julian.day <- format(H14$timestamps, "%j") 
  H15$julian.day <- format(H15$timestamps, "%j")
  H16$julian.day <- format(H16$timestamps, "%j")
  H17$julian.day <- format(H17$timestamps, "%j")
  H18$julian.day <- format(H18$timestamps, "%j")
  H19$julian.day <- format(H19$timestamps, "%j")
  H20$julian.day <- format(H20$timestamps, "%j")
  H21$julian.day <- format(H21$timestamps, "%j")
  H22$julian.day <- format(H22$timestamps, "%j")
  H23$julian.day <- format(H23$timestamps, "%j")
  
  
}

## Now let's create objects to store the fall and spring migratory movements 
# Inspect tracks for start and end points of migration
# Can search within the dat frame for specific points, then retrieve the numeric values associated with that time stamp
# * Use dates from Excel where I defined migratory movement start and end dates *

{
  # Spring 2022 Migrations
  spring.rowan <- H1 %>% filter(between(ts.numeric,(1648627157),(1650217705)))
  # Mortality during migration April 23rd (include for dBBMM not for migration characteristics)
  # spring.herald <- H2 %>% filter(between(ts.numeric,(1649764801),(1650743934))) 
  spring.morpheus <- H3 %>% filter(between(ts.numeric,(1649332609),(1650524760)))
  spring.patagium <- H4 %>% filter(between(ts.numeric,(1649606301),(1650455935)))
  spring.rip <- H5 %>% filter(between(ts.numeric,(1646481450),(1651060644)))
  spring.sam <- H6 %>% filter(between(ts.numeric,(1649445714),(1650455844)))
  spring.trinity <- H7 %>% filter(between(ts.numeric,(1649347451),(1651061040)))
  spring.jack <- H22 %>% filter(between(ts.numeric,(1649620974),(1650629048)))
  
  # Spring 2023 Migrations
  spring.morpheus2 <- H3 %>% filter(between(ts.numeric,(1681243021),(1683141300)))
  spring.patagium2 <- H4 %>% filter(between(ts.numeric,(1680465521),(1681315128)))
  spring.rip2 <- H5 %>% filter(between(ts.numeric,(1676836644),(1683062639)))
  spring.sam2 <- H6 %>% filter(between(ts.numeric,(1678996999),(1681591441)))
  spring.trinity2 <-H7 %>% filter(between(ts.numeric,(1680465521),(1681315128)))
  spring.jack2 <- H22 %>% filter(between(ts.numeric,(1680811501),(1686244108)))
  spring.angell <- H8 %>% filter(between(ts.numeric, (1681322947),(1682699881)))
  spring.mackinaw <- H12 %>% filter(between(ts.numeric,(1682445540),(1685050267)))
  spring.rapini <- H14 %>% filter(between(ts.numeric,(1680199141),(1683650976)))
  spring.voyageur <- H15 %>% filter(between(ts.numeric, (1680192045),(1680879669)))
  
  # Spring 2024 Migrations
  spring.morpheus3 <- H3 %>% filter(between(ts.numeric,(1711972591),(1713124601)))
  spring.patagium3 <- H4 %>% filter(between(ts.numeric,(1712505541),(1713371137)))
  spring.trinity3 <-H7 %>% filter(between(ts.numeric,(1712419410),(1713645959)))
  spring.mackinaw2 <- H12 %>% filter(between(ts.numeric,(1713279571),(1714682959)))
  spring.carlile <- H16 %>% filter(between(ts.numeric,(1708095613),(1712522344)))
  spring.cricket <- H17 %>% filter(between(ts.numeric,(1708365633),(1714593606)))
  spring.cypress <- H18 %>% filter(between(ts.numeric,(1712588433),(1714942209)))
  spring.lightfoot <- H20 %>% filter(between(ts.numeric,(1710439241),(1713196825)))
  spring.rogerthat <- H23 %>% filter(between(ts.numeric,(1711994825),(1712857050)))
  
  # Spring 2025 Migrations
  spring.patagium4 <- H4 %>% filter(between(ts.numeric,(1745524705),(1746460728)))
}
#check for duplicated timestamps
any(duplicated())

#Check structure make sure it looks right ts should be in POSIXct format
str()

# Remember to download the data with UTM's included from Movebank
#Filter out all the columns we don't need in the data frame; 
# ts denotes the new time column in proper format POSIXct
{
  spring.rowan <- spring.rowan [, (colnames(spring.rowan ) %in% c('utm.easting', 'utm.northing',
                                                                  'timestamps', 'individual.local.identifier','julian.day'))]                           #Add ground.speed?
  
  spring.morpheus <- spring.morpheus [, (colnames(spring.morpheus ) %in% c('utm.easting', 'utm.northing', 
                                                                           'timestamps', 'individual.local.identifier','julian.day'))]
  
  spring.patagium <- spring.patagium [, (colnames(spring.patagium) %in% c('utm.easting', 'utm.northing', 
                                                                          'timestamps', 'individual.local.identifier','julian.day'))]
  
  spring.rip <- spring.rip [, (colnames(spring.rip ) %in% c('utm.easting', 'utm.northing', 
                                                            'timestamps', 'individual.local.identifier','julian.day'))]
  
  spring.trinity <- spring.trinity [, (colnames(spring.trinity) %in% c('utm.easting', 'utm.northing', 
                                                                       'timestamps', 'individual.local.identifier','julian.day'))]
  
  spring.sam <- spring.sam [, (colnames(spring.sam ) %in% c('utm.easting', 'utm.northing', 
                                                            'timestamps', 'individual.local.identifier','julian.day'))]
  
  spring.jack <- spring.jack [, (colnames(spring.jack ) %in% c('utm.easting', 'utm.northing', 
                                                               'timestamps', 'individual.local.identifier','julian.day'))]
  
  spring.morpheus2 <- spring.morpheus2 [, (colnames(spring.morpheus2) %in% c('utm.easting', 'utm.northing', 
                                                                             'timestamps', 'individual.local.identifier','julian.day'))]
  
  spring.patagium2 <- spring.patagium2 [, (colnames(spring.patagium2) %in% c('utm.easting', 'utm.northing', 
                                                                             'timestamps', 'individual.local.identifier','julian.day'))]
  
  spring.rip2 <- spring.rip2 [, (colnames(spring.rip2) %in% c('utm.easting', 'utm.northing', 
                                                              'timestamps', 'individual.local.identifier','julian.day'))]
  
  spring.sam2 <- spring.sam2 [, (colnames(spring.sam2) %in% c('utm.easting', 'utm.northing', 
                                                              'timestamps', 'individual.local.identifier','julian.day'))]
  
  spring.trinity2 <- spring.trinity2 [, (colnames(spring.trinity2) %in% c('utm.easting', 'utm.northing', 
                                                                          'timestamps', 'individual.local.identifier','julian.day'))]
  
  spring.jack2 <- spring.jack2 [, (colnames(spring.jack2) %in% c('utm.easting', 'utm.northing', 
                                                                 'timestamps', 'individual.local.identifier','julian.day'))]
  
  spring.angell <- spring.angell [, (colnames(spring.angell) %in% c('utm.easting', 'utm.northing',
                                                                    'timestamps', 'individual.local.identifier', 'julian.day'))]
  
  spring.mackinaw <- spring.mackinaw [, (colnames(spring.mackinaw) %in% c('utm.easting', 'utm.northing',
                                                                          'timestamps', 'individual.local.identifier', 'julian.day'))]
  
  spring.rapini <- spring.rapini [, (colnames(spring.rapini) %in% c('utm.easting', 'utm.northing',
                                                                    'timestamps', 'individual.local.identifier', 'julian.day'))]
  
  spring.voyageur <- spring.voyageur [, (colnames(spring.voyageur) %in% c('utm.easting', 'utm.northing',
                                                                          'timestamps', 'individual.local.identifier','julian.day'))] 
  
  spring.morpheus3 <- spring.morpheus3 [, (colnames(spring.morpheus3) %in% c('utm.easting', 'utm.northing',
                                                                             'timestamps', 'individual.local.identifier','julian.day'))] 
  
  spring.patagium3 <- spring.patagium3 [, (colnames(spring.patagium3) %in% c('utm.easting', 'utm.northing',
                                                                             'timestamps', 'individual.local.identifier','julian.day'))] 
  
  spring.trinity3 <- spring.trinity3 [, (colnames(spring.trinity3) %in% c('utm.easting', 'utm.northing',
                                                                          'timestamps', 'individual.local.identifier','julian.day'))] 
  
  spring.mackinaw2 <- spring.mackinaw2 [, (colnames(spring.mackinaw2) %in% c('utm.easting', 'utm.northing',
                                                                             'timestamps', 'individual.local.identifier','julian.day'))] 
  
  spring.carlile <- spring.carlile [, (colnames(spring.carlile) %in% c('utm.easting', 'utm.northing',
                                                                       'timestamps', 'individual.local.identifier','julian.day'))]
  
  spring.cricket <- spring.cricket [, (colnames(spring.cricket) %in% c('utm.easting', 'utm.northing',
                                                                       'timestamps', 'individual.local.identifier','julian.day'))] 
  
  spring.cypress <- spring.cypress [, (colnames(spring.cypress) %in% c('utm.easting', 'utm.northing',
                                                                       'timestamps', 'individual.local.identifier','julian.day'))]
  
  spring.lightfoot <- spring.lightfoot [, (colnames(spring.lightfoot) %in% c('utm.easting', 'utm.northing',
                                                                             'timestamps', 'individual.local.identifier','julian.day'))] 
  
  spring.rogerthat <- spring.rogerthat [, (colnames(spring.rogerthat) %in% c('utm.easting', 'utm.northing',
                                                                             'timestamps', 'individual.local.identifier','julian.day'))] 
  
  spring.patagium4 <- spring.patagium4 [, (colnames(spring.patagium4) %in% c('utm.easting', 'utm.northing',
                                                                             'timestamps', 'individual.local.identifier','julian.day'))] 
} 

# Data should be clean now we have a track for all spring migrations
{
  # 2022 Spring Migrations
  rowan.track.s <- make_track(spring.rowan ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  # herald.track.S <- make_track(spring.herald ,utm.easting, utm.northing, timestamps, id= individual.local.identifier)
  morpheus.track.s <- make_track(spring.morpheus ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  patagium.track.s <- make_track(spring.patagium ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  rip.track.s <- make_track(spring.rip ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  trinity.track.s <- make_track(spring.trinity ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  sam.track.s <- make_track(spring.sam ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  jack.track.s <- make_track(spring.jack, utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  
  # 2023 Spring Migrations
  morpheus.track.s2 <- make_track(spring.morpheus2 ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  patagium.track.s2 <- make_track(spring.patagium2 ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  rip.track.s2 <- make_track(spring.rip2 ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  trinity.track.s2 <- make_track(spring.trinity2 ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  sam.track.s2 <- make_track(spring.sam2 ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  jack.track.s2 <- make_track(spring.jack2 ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  angell.track.s <-    make_track(spring.angell ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  mackinaw.track.s <-    make_track(spring.mackinaw ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  rapini.track.s <-    make_track(spring.rapini ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  voyageuer.track.s <- make_track(spring.voyageur ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  
  # 2024 Spring Migrations
  morpheus.track.s3 <- make_track(spring.morpheus3 ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  patagium.track.s3 <- make_track(spring.patagium3 ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  trinity.track.s3 <- make_track(spring.trinity3 ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  mackinaw.track.s2 <-    make_track(spring.mackinaw2 ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  carlile.track.s <-    make_track(spring.carlile ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  cricket.track.s <-    make_track(spring.cricket ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  cypress.track.s <-    make_track(spring.cypress ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  lightfoot.track.s <-    make_track(spring.lightfoot ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  rogerthat.track.s <-    make_track(spring.rogerthat ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  
  # 2025 Spring Migrations
  patagium.track.s4 <- make_track(spring.patagium4 ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
}

# Check tracks - make sure they look right
plot()

# WATCH THE CAPITAL S on the Tracks you make above!
# Create a list of all the tracks in order - this way we can run a For loop through it MAKE sure birds are in correct order as above
spring.tracks <- list(rowan.track.s,morpheus.track.s,patagium.track.s,rip.track.s,
                      trinity.track.s,sam.track.s, jack.track.s, morpheus.track.s2, patagium.track.s2,
                      rip.track.s2, trinity.track.s2, sam.track.s2, jack.track.s2, angell.track.s, mackinaw.track.s,
                      rapini.track.s, voyageuer.track.s,morpheus.track.s3, patagium.track.s3, trinity.track.s3,
                      mackinaw.track.s2, carlile.track.s,cricket.track.s, cypress.track.s, lightfoot.track.s,
                      rogerthat.track.s,patagium.track.s4)

# Assign each list a name, in this case each track corresponds to the individual bird. Keep the order the same!
bird.id <- list("Rowan","Morpheus","Patagium","Rip","Trinity","Sam","Jack", "Morpheus2", "Patagium2","Rip2", "Trinity2",
                "Sam2","Jack2","Angell", "Mackinaw","Rapini", "Voyageur", "Morpheus3", "Patagium3","Trinity3","Mackinaw2",
                "Carlile", "Cricket","Cypress","Lightfoot","RogerThat","Patagium4")


##############################################################################
#                MIGRATORY CHARACTERSITIC CODE & FUNCTIONS                   #
##############################################################################

#-------------------------------------------------------------------------------
# Duration Function
## A function to calculate the time increment between consecutive points ##
## Default output in seconds, other options are "auto", "mins", "hours","days", or "weeks" ##
duration.fun <- function(datetime, output.units = "secs") {
  duration <- c(difftime(datetime[2:length(datetime)], datetime[1:(length(datetime) - 1)], units = output.units), "NA")
  return(duration)
}

#-------------------------------------------------------------------------------
spring.migration <- vector('list', 27)

#To test for one individual before we run the full loop
# i = 1 This will test it works on 1 bird
for (i in 1:27) {
  bird <- paste(bird.id[i]) #list of named birds
  spr.dur <- from_to(spring.tracks[[i]])
  duration <- duration.fun(spr.dur, output.units = "days")
  td <- tot_dist(spring.tracks[[i]])/1000   ##Total distance travelled = m divide by 1000 to get KM
  cd <- cum_dist(spring.tracks[[i]])/1000   #Cumlative distance travelled = m divide by 1000 to get KM
  s <- straightness(spring.tracks[[i]])     ##straightness of route (0-1 0= crooked, 1 = straightline)
  jd <-  min(as.numeric(as.data.frame(spring.tracks[[i]])[ ,'julian.day'])) #start of migration for each bird based on the tracks - get an avergae spring start date
  ed <- max(as.numeric(as.data.frame(spring.tracks[[i]])[,'julian.day']))
  
  spring.migration[[i]] <- data.frame(
    bird.ID = bird,
    duration.days = duration[1],
    total.dist = td,
    cumulative.dist = cd,
    straightness = s,
    julian.day = jd,
    end.day = ed
  )
  #speed = speed)
}

spring.metrics <- do.call(rbind, spring.migration) #combine all birds into a clean data frame

# Calculate Migration Speed = Cumulative Distance/Duartion 
spring.metrics$speed <- spring.metrics$cumulative.dist/as.numeric(spring.metrics$duration.days)

# Migration Metric Mean and SD ----

# Let's get averages and SD for each metric so we can plot them down below

# Step 1: Calculate the mean for each column
mean_values <- sapply(spring.metrics[, c("duration.days", "total.dist", "cumulative.dist", 
                                         "straightness", "julian.day","end.day", "speed")], mean)

# Step 2: Calculate the SD for each metric

sd_values <- sapply(spring.metrics[, c("duration.days", "total.dist", "cumulative.dist", 
                                       "straightness","julian.day","end.day", "speed")], sd)


# Step 3: Combine the mean and SE into a new data frame
spring_metrics_summary <- data.frame(
  Metric = c("Duration (days)", "Total Distance (km)", "Cumulative Distance (km)",
             "Straightness", "Start Date (Julian Day)","End Date (Julian Day)","Speed (km/day)"),
  Mean = round(mean_values, 2),
  SD = round(sd_values, 2),
  Mean_SD = paste0(round(mean_values, 2), " ± ", round(sd_values, 2))
)

# Save this data frame as a CSV.
write.csv(spring_metrics_summary,"Chapter.1/Data/full_migrations/spring_migration_summary_25.csv", row.names = FALSE)
write.csv(spring.metrics,"Chapter.1/Data/full_migrations/spring_migration_individual_metrics_25.csv", row.names = FALSE)


