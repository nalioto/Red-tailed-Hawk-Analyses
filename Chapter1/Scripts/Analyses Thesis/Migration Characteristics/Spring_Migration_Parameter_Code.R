###################################
##  Spring Migration Parameters  ##
##        Nick Alioto            ##
##       August 13 2024         ##
###################################

################################################################################
library(amt)
library(adehabitatLT)
library(tidyverse)
################################################################################
rm(list = ls())

##### Separarting Spring and Fall ###########

# use amt package for this
# We want to define start/end dates, duration, Distance, cumulative distance, migration
# straightness and speed
# Need to subset data into migration periods for individual metrics

################################################################################
rm(Hawks.2)

# STEP 1
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
  H22$timestamps <- as.POSIXct(strptime(H22$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  
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
  H22$ts.numeric <- as.numeric(H22$timestamps)
  
}
str(H1)

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
  H22$julian.day <- format(H22$timestamps, "%j") 
  
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
  
}
#check for duplicated timestamps
any(duplicated(spring.angell))

#Check structure make sure it looks right ts should be in POSIXct format
str(H2)

# Remember to download the data with UTM's included from Movebank
#Filter out all the columns we don't need in the data frame; ts denotes the new time column in proper format POSIXct
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
} 


#Data should be clean no we have a track for all movement
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
  angell.track.S <-    make_track(spring.angell ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  mackinaw.track.s <-    make_track(spring.mackinaw ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  rapini.track.s <-    make_track(spring.rapini ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  voyageuer.track.s <- make_track(spring.voyageur ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  
  
}

# WATCH THE CAPITAL S on the Tracks you make above!
#Create a list of all the tracks in order - this way we can run a For loop through it MAKE sure birds are in correct order as above
spring.tracks <- list(rowan.track.s,morpheus.track.s,patagium.track.s,rip.track.s,
                      trinity.track.s,sam.track.s, jack.track.s, morpheus.track.s2, patagium.track.s2,
                      rip.track.s2, trinity.track.s2, sam.track.s2, jack.track.s2, angell.track.S, mackinaw.track.s,
                      rapini.track.s, voyageuer.track.S )

#Assign each list a name, in this case each track corresponds to the individual bird. Keep the order the same!
bird.id <- list("Rowan","Morpheus","Patagium","Rip","Trinity","Sam","Jack", "Morpheus2", "Patagium2","Rip2", "Trinity2",
                "Sam2","Jack2","Angell", "Mackinaw","Rapini", "Voyageur")


##############################################################################
#                MIGRATORY CHARACTERSITIC CODE & FUNCTIONS                   #
##############################################################################

#-------------------------------------------------------------------------------
#Duration Function
## A function to calculate the time increment between consecutive points ##
## Default output in seconds, other options are "auto", "mins", "hours","days", or "weeks" ##
duration.fun <- function(datetime, output.units = "secs") {
  duration <- c(difftime(datetime[2:length(datetime)], datetime[1:(length(datetime) - 1)], units = output.units), "NA")
  return(duration)
}

#-------------------------------------------------------------------------------
spring.migration <- vector('list', 17)

#To test for one individual before we run the full loop
# i = 1 This will test it works on 1 bird
for (i in 1:17) {
  bird <- paste(bird.id[i]) #list of named birds
  spr.dur <- from_to(spring.tracks[[i]])
  duration <- duration.fun(spr.dur, output.units = "days")
  td <- tot_dist(spring.tracks[[i]])/1000   ##Total distance travelled = m divide by 1000 to get KM
  cd <- cum_dist(spring.tracks[[i]])/1000   #Cumlative distance travelled = m divide by 1000 to get KM
  s <- straightness(spring.tracks[[i]])     ##straightness of route (0-1 0= crooked, 1 = straightline)
  jd <-  min(as.numeric(as.data.frame(spring.tracks[[i]])[ ,'julian.day'])) #start of migration for each bird based on the tracks - get an avergae spring start date
  
  spring.migration[[i]] <- data.frame(
    bird.ID = bird,
    duration.days = duration[1],
    total.dist = td,
    cumulative.dist = cd,
    straightness = s,
    julian.day = jd)
  #speed = speed)
  
}

spring.metrics <- do.call(rbind, spring.migration) #combine all birds into a clean data frame

# Calculate Migration Speed = Cumulative Distance/Duartion 
spring.metrics$speed <- spring.metrics$cumulative.dist/as.numeric(spring.metrics$duration.days)


# Migration Metric averages and Std errors ----

# Step 1: Calculate the mean for each column
mean_values <- sapply(spring.metrics[, c("duration.days", "total.dist", "cumulative.dist", "straightness", "julian.day", "speed")], mean)

# Step 2: Calculate the SE for each column using the std function
# std function 
std <- function(x) sd(x) / sqrt(length(x))

se_values <- sapply(spring.metrics[, c("duration.days", "total.dist", "cumulative.dist", "straightness","julian.day", "speed")], std)


# Step 3: Combine the mean and SE into a new data frame
spring_metrics_summary <- data.frame(
  Metric = c("Duration (days)", "Total Distance (km)", "Cumulative Distance (km)", "Straightness", "Start Date (Julian Day)", "Speed (km/day)"),
  Mean = round(mean_values, 2),
  SE = round(se_values, 2),
  Mean_SE = paste0(round(mean_values, 2), " ± ", round(se_values, 2))
)

# Save this data frame as a CSV.
write.csv(spring_metrics_summary,"Chapter.1/Data/full_migrations/spring_migration_summary.csv", row.names = FALSE)
write.csv(spring.metrics,"Chapter.1/Data/full_migrations/spring_migration_individual_metrics.csv", row.names = FALSE)



#### Plots ----

#Read in the saved csv to make plots with
spr.migration <- read.csv("Chapter.1/Data/full_migrations/spr.mig.full.csv", header = T)

#Averages to use for plot
duration.avg <- mean(spr.migration$duration.days)
tot.dist.avg <- mean(spr.migration$total.dist)
cum.dist.avg <- mean(spr.migration$cumulative.dist)
str.avg <- mean(spr.migration$straightness)
jul.avg <- mean(spr.migration$julian.day)
speed.avg <- mean(spr.migration$speed)

# Total Distance Plot----
total.distance.plot <-ggplot(data = fall.metrics, aes(x= as.factor(bird.ID), y= total.dist, fill = as.factor(bird.ID))) +
  geom_bar(stat='identity') +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2") +
  labs( x = "Individual hawks",
        y = " Total distance in Km") +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        legend.position = "none") +
  scale_y_continuous(breaks = seq(from=0, to=2000, by=500)) +
  geom_hline(yintercept = 1214, linetype= "dashed", color= "black") 

plot(total.distance.plot)  
##scale_y_continuous(breaks = seq(from=0, to=2000, by=100))  

# Cumulative Distance Plot----
cumulative.distance.plot <-ggplot(data = fall.metrics, aes(x= as.factor(bird.ID), y= cumulative.dist, fill = as.factor(bird.ID))) +
  geom_bar(stat='identity') +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2") +
  labs( x = "Individual hawks",
        y = " Cumulative distance in Km") +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        legend.position = "none") +
  scale_y_continuous(breaks = seq(from=0, to=2000, by=500)) +
  geom_hline(yintercept = 1822, linetype= "dashed", color= "black") 

plot(cumulative.distance.plot)  

# Straightness Plot----
straightness.plot <-ggplot(data = fall.metrics, aes(x= as.factor(bird.ID), y= straightness, fill = as.factor(bird.ID))) +
  geom_bar(stat='identity') +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2") +
  labs( x = "Individual hawks",
        y = " Straightness (0 - 1)") +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept = 0.68, linetype= "dashed", color= "black") 

plot(straightness.plot)

# Migratory Duration Plot ----
duration.plot <-ggplot(data = fall.metrics, aes(x= as.factor(bird.ID), y= duration.days, fill = as.factor(bird.ID))) +
  geom_bar(stat='identity') +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2") +
  labs( x = "Individual hawks",
        y = " Total days") +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept = 30, linetype= "dashed", color= "black") 

plot(duration.plot)





