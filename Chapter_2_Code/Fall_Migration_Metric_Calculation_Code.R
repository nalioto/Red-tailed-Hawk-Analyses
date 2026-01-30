####################################
##  Fall Migration Parameter code ##
##     Nick Alioto                ##
##    Auguest 6th 2024            ##
####################################

################################################################################
library(amt)
library(adehabitatLT)
library(tidyverse)
library(plotrix) # Calculate Standard Errors on metrics 
################################################################################
rm(list = ls())

##### Separating Spring and Fall ###########

# use amt package for this
# We want to define start/end dates, duration, Distance, cumulative distance, migration
# straightness and speed
# Need to subset data into migration periods for individual metrics

################################################################################
rm(hawk.list)

# STEP 1
# 2021 birds
Hawks.1 <- list.files(path="Chapter.1/Data/2021_Red-tails",pattern = ".csv", full.names = T) # The order should be alphabetical

Hawks.21 <- do.call("rbind",lapply(Hawks.1, read.csv))
# In console type in Hawks.l to make sure it is in the order you want

# 2022 birds
Hawks.2 <- list.files(path="Chapter.1/Data/2022_Red-tails",pattern = ".csv", full.names = T) # The order should be alphabetical

Hawks.22 <- do.call("rbind",lapply(Hawks.2, read.csv))

# 2023 birds
Hawks.3 <- list.files(path="Chapter.1/Data/2023_Red-tails",pattern = ".csv", full.names = T) # The order should be alphabetical

Hawks.23 <- do.call("rbind",lapply(Hawks.3, read.csv))

# 2024 birds
Hawks.4 <- list.files(path="Chapter.1/Data/2024_Red-tails",pattern = ".csv", full.names = T) # The order should be alphabetical

Hawks.24 <- do.call("rbind",lapply(Hawks.4, read.csv))

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
  H30 <- read.csv(Hawks.3[7]) # Roger That from 2023 - checked in late added here.
  
  # 2024 birds
  H23 <- read.csv(Hawks.4[1])
  H24 <- read.csv(Hawks.4[2])
  H25 <- read.csv(Hawks.4[3])
  H26 <- read.csv(Hawks.4[4])
  H27 <- read.csv(Hawks.4[5])
  H28 <- read.csv(Hawks.4[6])
  H29 <- read.csv(Hawks.4[7])
  
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
  H24$timestamps <- as.POSIXct(strptime(H24$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H25$timestamps <- as.POSIXct(strptime(H25$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H26$timestamps <- as.POSIXct(strptime(H26$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H27$timestamps <- as.POSIXct(strptime(H27$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H28$timestamps <- as.POSIXct(strptime(H28$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H29$timestamps <- as.POSIXct(strptime(H29$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H30$timestamps <- as.POSIXct(strptime(H30$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  
  
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
  H24$ts.numeric <- as.numeric(H24$timestamps)
  H25$ts.numeric <- as.numeric(H25$timestamps)
  H26$ts.numeric <- as.numeric(H26$timestamps)
  H27$ts.numeric <- as.numeric(H27$timestamps)
  H28$ts.numeric <- as.numeric(H28$timestamps)
  H29$ts.numeric <- as.numeric(H29$timestamps)
  H30$ts.numeric <- as.numeric(H30$timestamps)
  
  
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
  H24$julian.day <- format(H24$timestamps, "%j")
  H25$julian.day <- format(H25$timestamps, "%j")
  H26$julian.day <- format(H26$timestamps, "%j")
  H27$julian.day <- format(H27$timestamps, "%j")
  H28$julian.day <- format(H28$timestamps, "%j")
  H29$julian.day <- format(H29$timestamps, "%j")
  H30$julian.day <- format(H30$timestamps, "%j")
  
  
}

## Now let's create objects to store the fall and spring migratory movements 
# Inspect tracks for start and end points of migration in ARC
# Can search within the data frame for specific points, then retrieve the numeric values associated with that time stamp
# Use dates from Excel where I defined migratory movement start and end

# 2021 Fall Mig
{
  fall.rowan <- H1 %>% filter(between(ts.numeric,(1632427081),(1635998313)))
  fall.herald <- H2 %>% filter(between(ts.numeric,(1634385549),(1636588801)))
  fall.morpheus <- H3 %>% filter(between(ts.numeric,(1634957790),(1638640591)))
  fall.patagium <- H4 %>% filter(between(ts.numeric,(1634500741),(1637697511)))
  fall.rip <- H5 %>% filter(between(ts.numeric,(1633435050),(1636545489)))
  fall.sam <- H6 %>% filter(between(ts.numeric,(1634406960),(1635724652)))
  fall.trinity <- H7 %>% filter(between(ts.numeric,(1634061721),(1635357871)))
  
  # 2022 Fall Mig
  fall.morpheus2 <- H3 %>% filter(between(ts.numeric,(1665071790),(1668369390)))
  fall.patagium2 <- H4 %>% filter(between(ts.numeric,(1665849534),(1669406365)))
  fall.rip2 <- H5 %>% filter(between(ts.numeric,(1663175829),(1667505450)))
  fall.sam2 <- H6 %>% filter(between(ts.numeric,(1665086220),(1666555081)))
  fall.trinity2 <- H7 %>% filter(between(ts.numeric,(1665057895),(1669752235)))
  fall.angell <- H8 %>% filter(between(ts.numeric,(1663067366),(1667503466)))
  fall.bucatini <- H9 %>% filter(between(ts.numeric,(1663858759),(1672531166))) # Cut off at Dec 31st - continually kept moving every few weeks until March 1 2023
  fall.ginger <- H10 %>% filter(between(ts.numeric,(1665057798),(1667066599)))
  fall.hallowee <- H11 %>% filter(between(ts.numeric,(1665155041),(1666299841)))
  fall.mackinaw <- H12 %>% filter(between(ts.numeric,(1663934358),(1668535149)))
  fall.petosegay <- H13 %>% filter(between(ts.numeric,(1666023061),(1669389031)))
  fall.rapini <- H14 %>% filter(between(ts.numeric,(1666353540),(1669053559)))
  fall.voyageur <- H15 %>% filter(between(ts.numeric,(1665921678),(1667401299)))
  
  # 2023 Fall Mig
  fall.patagium3 <- H4 %>% filter(between(ts.numeric,(1696708741),(1700107141)))
  fall.morpheus3 <- H3 %>% filter(between(ts.numeric,(1697385384),(1699459015)))
  fall.rip3 <- H5 %>% filter(between(ts.numeric,(1694188650),(1699747075)))
  fall.trinity3 <- H7 %>% filter(between(ts.numeric,(1697918599),(1699646644)))
  fall.sam3 <- H6 %>% filter(between(ts.numeric,(1697226932),(1698105450)))
  fall.jack <- H22 %>% filter(between(ts.numeric,(1697313858),(1700597100)))
  fall.angell2 <- H8 %>% filter(between(ts.numeric,(1696701600),(1699217995)))
  fall.mackinaw2 <- H12 %>% filter(between(ts.numeric,(1697057995),(1702155565)))
  fall.carlile <- H16 %>% filter(between(ts.numeric,(1699120832),(1700929207)))
  fall.cricket <- H17 %>% filter(between(ts.numeric,(1698267609),(1700078446)))
  fall.cypress <- H18 %>% filter(between(ts.numeric,(1694447409),(1698792005)))
  fall.hc2 <- H19 %>% filter(between(ts.numeric,(1696604450),(1698007986)))
  fall.lightfoot <- H20 %>% filter(between(ts.numeric,(1697824832),(1699649444)))
  fall.mh1 <- H21 %>% filter(between(ts.numeric,(1697133620),(1699635005)))
  fall.rogerthat <- H30 %>% filter(between(ts.numeric,(1697802604),(1699120833)))
  
  # Fall 2024 Migrations
  
  fall.patagium4 <- H4 %>% filter(between(ts.numeric,(1728590332),(1731095936)))
  fall.trinity4 <- H7 %>% filter(between(ts.numeric,(1727971449),(1729253040)))
  fall.mackinaw3 <- H12 %>% filter(between(ts.numeric,(1728475159),(1732989481)))
  fall.cypress2 <- H18 %>% filter(between(ts.numeric,(1728064216),(1729977486)))
  fall.lightfoot2 <- H20 %>% filter(between(ts.numeric,(1729987269),(1732176039)))
  fall.rogerthat2 <- H30 %>% filter(between(ts.numeric,(1728579602),(1730235008)))
  fall.baker <- H23 %>% filter(between(ts.numeric,(1730041210),(1733335217)))
  fall.campa <- H24 %>% filter(between(ts.numeric,(1726416012),(1727380805)))
  fall.disco <- H25 %>% filter(between(ts.numeric,(1725559224),(1729022404)))
  fall.mercury <- H26 %>% filter(between(ts.numeric,(1726495222),(1729087256)))
  fall.owen <- H27 %>% filter(between(ts.numeric,(1728403287),(1729288833)))
  fall.paprika <- H28 %>% filter(between(ts.numeric,(1724774401),(1729195232)))
  fall.reno <- H29 %>% filter(between(ts.numeric,(1717945805),(1718391842)))
  
}

# check for duplicated timestamps - shouldn't be an issue with GSM transmitters
any(duplicated())#add bird here to check

# Check structure make sure it looks right ts should be in POSIXct format
str()

# Remember to download the data with UTM's included from Movebank
# Filter out all the columns we don't need in the data frame; 
# ts denotes the new time column in proper format POSIXct

{
  fall.rowan <- fall.rowan [, (colnames(fall.rowan ) %in% c('utm.easting', 'utm.northing', 
                                                            'timestamps', 'individual.local.identifier', 'julian.day'))]                           #Add ground.speed?
  
  fall.herald <- fall.herald [, (colnames(fall.herald ) %in% c('utm.easting', 'utm.northing', 
                                                               'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.morpheus <- fall.morpheus [, (colnames(fall.morpheus ) %in% c('utm.easting', 'utm.northing', 
                                                                     'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.patagium <- fall.patagium [, (colnames(fall.patagium) %in% c('utm.easting', 'utm.northing', 
                                                                    'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.rip <- fall.rip [, (colnames(fall.rip) %in% c('utm.easting', 'utm.northing', 
                                                     'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.trinity <- fall.trinity [, (colnames(fall.trinity) %in% c('utm.easting', 'utm.northing', 
                                                                 'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.sam <- fall.sam [, (colnames(fall.sam) %in% c('utm.easting', 'utm.northing', 
                                                     'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.rip2 <- fall.rip2 [, (colnames(fall.rip2 ) %in% c('utm.easting', 'utm.northing', 
                                                         'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.trinity2 <- fall.trinity2 [, (colnames(fall.trinity2) %in% c('utm.easting', 'utm.northing', 
                                                                    'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.sam2 <- fall.sam2 [, (colnames(fall.sam2 ) %in% c('utm.easting', 'utm.northing', 
                                                         'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.patagium2 <- fall.patagium2 [, (colnames(fall.patagium2) %in% c('utm.easting', 'utm.northing', 
                                                                       'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.morpheus2 <- fall.morpheus2 [, (colnames(fall.morpheus2) %in% c('utm.easting', 'utm.northing', 
                                                                       'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.angell <- fall.angell [, (colnames(fall.angell ) %in% c('utm.easting', 'utm.northing', 
                                                               'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.bucatini <- fall.bucatini [, (colnames(fall.bucatini ) %in% c('utm.easting', 'utm.northing', 
                                                                     'timestamps', 'individual.local.identifier', 'julian.day'))]
  
  fall.ginger <- fall.ginger [, (colnames(fall.ginger ) %in% c('utm.easting', 'utm.northing', 
                                                               'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.hallowee <- fall.hallowee [, (colnames(fall.hallowee ) %in% c('utm.easting', 'utm.northing', 
                                                                     'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.mackinaw <- fall.mackinaw [, (colnames(fall.mackinaw ) %in% c('utm.easting', 'utm.northing', 
                                                                     'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.petosegay <- fall.petosegay [, (colnames(fall.petosegay ) %in% c('utm.easting', 'utm.northing', 
                                                                        'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.rapini <- fall.rapini [, (colnames(fall.rapini ) %in% c('utm.easting', 'utm.northing', 
                                                               'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.voyageur <- fall.voyageur [, (colnames(fall.voyageur) %in% c('utm.easting', 'utm.northing', 
                                                                    'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.patagium3 <- fall.patagium3 [, (colnames(fall.patagium3) %in% c('utm.easting', 'utm.northing', 
                                                                       'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.morpheus3 <- fall.morpheus3 [, (colnames(fall.morpheus3) %in% c('utm.easting', 'utm.northing', 
                                                                       'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.rip3 <- fall.rip3 [, (colnames(fall.rip3) %in% c('utm.easting', 'utm.northing', 
                                                        'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.trinity3 <- fall.trinity3 [, (colnames(fall.trinity3) %in% c('utm.easting', 'utm.northing', 
                                                                    'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.sam3 <- fall.sam3 [, (colnames(fall.sam3) %in% c('utm.easting', 'utm.northing', 
                                                        'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.jack <- fall.jack [, (colnames(fall.jack) %in% c('utm.easting', 'utm.northing', 
                                                        'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.angell2 <- fall.angell2 [, (colnames(fall.angell2) %in% c('utm.easting', 'utm.northing', 
                                                                 'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.mackinaw2 <- fall.mackinaw2 [, (colnames(fall.mackinaw2) %in% c('utm.easting', 'utm.northing', 
                                                                       'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.carlile <- fall.carlile [, (colnames(fall.carlile) %in% c('utm.easting', 'utm.northing', 
                                                                 'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.cricket <- fall.cricket [, (colnames(fall.cricket) %in% c('utm.easting', 'utm.northing', 
                                                                 'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.cypress <- fall.cypress [, (colnames(fall.cypress) %in% c('utm.easting', 'utm.northing', 
                                                                 'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.hc2 <- fall.hc2 [, (colnames(fall.hc2) %in% c('utm.easting', 'utm.northing', 
                                                     'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.lightfoot <- fall.lightfoot[, (colnames(fall.lightfoot) %in% c('utm.easting', 'utm.northing', 
                                                                      'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.mh1 <- fall.mh1 [, (colnames(fall.mh1) %in% c('utm.easting', 'utm.northing', 
                                                     'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.rogerthat <- fall.rogerthat [, (colnames(fall.rogerthat) %in% c('utm.easting', 'utm.northing', 
                                                                       'timestamps', 'individual.local.identifier','julian.day'))]
  
  
  fall.patagium4 <- fall.patagium4 [, (colnames(fall.patagium4) %in% c('utm.easting', 'utm.northing', 
                                                                       'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.trinity4 <- fall.trinity4 [, (colnames(fall.trinity4) %in% c('utm.easting', 'utm.northing', 
                                                                    'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.mackinaw3 <- fall.mackinaw3 [, (colnames(fall.mackinaw3) %in% c('utm.easting', 'utm.northing', 
                                                                       'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.cypress2 <- fall.cypress2 [, (colnames(fall.cypress2) %in% c('utm.easting', 'utm.northing', 
                                                                    'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.lightfoot2 <- fall.lightfoot2[, (colnames(fall.lightfoot2) %in% c('utm.easting', 'utm.northing', 
                                                                         'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.rogerthat2 <- fall.rogerthat2 [, (colnames(fall.rogerthat2) %in% c('utm.easting', 'utm.northing', 
                                                                          'timestamps', 'individual.local.identifier','julian.day'))]
  
  
  fall.baker <- fall.baker[, (colnames(fall.baker) %in% c('utm.easting', 'utm.northing', 
                                                          'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.campa <- fall.campa[, (colnames(fall.campa) %in% c('utm.easting', 'utm.northing', 
                                                          'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.disco <- fall.disco[, (colnames(fall.disco) %in% c('utm.easting', 'utm.northing', 
                                                          'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.mercury <- fall.mercury [, (colnames(fall.mercury) %in% c('utm.easting', 'utm.northing', 
                                                                 'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.owen <- fall.owen[, (colnames(fall.owen) %in% c('utm.easting', 'utm.northing', 
                                                       'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.paprika <- fall.paprika[, (colnames(fall.paprika) %in% c('utm.easting', 'utm.northing', 
                                                                'timestamps', 'individual.local.identifier','julian.day'))]
  
  fall.reno <- fall.reno[, (colnames(fall.reno) %in% c('utm.easting', 'utm.northing', 
                                                       'timestamps', 'individual.local.identifier','julian.day'))]
  
}

# Turn into separate list of data frames to use in for loop later for

id_list <- list(fall.rowan, fall.herald, fall.morpheus, fall.patagium, fall.rip, fall.trinity, fall.sam,
                fall.rip2, fall.trinity2, fall.sam2, fall.patagium2, fall.morpheus2, fall.angell, fall.bucatini,
                fall.ginger, fall.hallowee, fall.mackinaw, fall.petosegay, fall.rapini, fall.voyageur,fall.patagium3,
                fall.morpheus3, fall.rip3, fall.trinity3, fall.sam3, fall.jack, fall.angell2, fall.carlile,
                fall.cricket, fall.cypress, fall.hc2, fall.lightfoot, fall.mh1, fall.rogerthat, fall.patagium4, fall.trinity4, fall.mackinaw3, 
                fall.cypress2, fall.lightfoot2, fall.rogerthat2, fall.baker, fall.campa, fall.disco, fall.mercury, fall.owen, 
                fall.paprika, fall.reno)


# Data should be clean now we have a tracks for all individual fall migrations by year and individual
# 2021 Fall Migrations
{
  rowan.track.F <- make_track(fall.rowan ,utm.easting, utm.northing, timestamps, julian.day, id= individual.local.identifier)
  herald.track.F <- make_track(fall.herald ,utm.easting, utm.northing, timestamps,julian.day, id= individual.local.identifier)
  morpheus.track.F <- make_track(fall.morpheus ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  patagium.track.F <- make_track(fall.patagium ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  rip.track.F <- make_track(fall.rip ,utm.easting, utm.northing, timestamps, julian.day, id= individual.local.identifier)
  trinity.track.F <- make_track(fall.trinity ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  sam.track.F <- make_track(fall.sam ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  
  # 2022 Fall Migrations
  rip.track.F2 <- make_track(fall.rip2 ,utm.easting, utm.northing, timestamps, julian.day,  id= individual.local.identifier)
  trinity.track.F2 <- make_track(fall.trinity2 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  sam.track.F2 <- make_track(fall.sam2 ,utm.easting, utm.northing, timestamps, julian.day,  id= individual.local.identifier)
  patagium.track.F2 <- make_track(fall.patagium2 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  morpheus.track.F2 <- make_track(fall.morpheus2 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  angell.track.F <- make_track(fall.angell ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  bucatini.track.F <- make_track(fall.bucatini ,utm.easting, utm.northing, timestamps, julian.day, id= individual.local.identifier)
  ginger.track.F <- make_track(fall.ginger ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  hallowee.track.F <- make_track(fall.hallowee ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  mackinaw.track.F <- make_track(fall.mackinaw ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  petosegay.track.F <- make_track(fall.petosegay ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  rapini.track.F <- make_track(fall.rapini ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  voyageur.track.F <- make_track(fall.voyageur ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  
  # 2023 Fall Migration Tracks
  patagium.track.F3 <- make_track(fall.patagium3 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  morpheus.track.F3 <- make_track(fall.morpheus3 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  rip.track.F3 <- make_track(fall.rip3 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  trinity.track.F3 <- make_track(fall.trinity3 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  sam.track.F3 <- make_track(fall.sam3 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  jack.track.F <- make_track(fall.jack ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  angell.track.F2 <- make_track(fall.angell2 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  mackinaw.track.F2 <- make_track(fall.mackinaw2 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  carlile.track.F <- make_track(fall.carlile ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  cricket.track.F <- make_track(fall.cricket ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  cypress.track.F <- make_track(fall.cypress ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  hc2.track.F <- make_track(fall.hc2 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  lightfoot.track.F <- make_track(fall.lightfoot ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  mh1.track.F <- make_track(fall.mh1 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  rogerthat.track.F <- make_track(fall.rogerthat ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  
  
  # 2024 Fall migration tracks
  patagium.track.F4 <- make_track(fall.patagium4 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  trinity.track.F4 <- make_track(fall.trinity4 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  mackinaw.track.F3 <- make_track(fall.mackinaw3 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  cypress.track.F2 <- make_track(fall.cypress2 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  lightfoot.track.F2 <- make_track(fall.lightfoot2 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  rogerthat.track.F2 <- make_track(fall.rogerthat2 ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  baker.track.F <- make_track(fall.baker ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  campa.track.F <- make_track(fall.campa ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  disco.track.F <- make_track(fall.disco ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  mercury.track.F <- make_track(fall.mercury ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  owen.track.F <- make_track(fall.owen ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  paprika.track.F <- make_track(fall.paprika ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  reno.track.F <- make_track(fall.reno ,utm.easting, utm.northing, timestamps,julian.day,  id= individual.local.identifier)
  
  
}

# Create a list of all the tracks in order - this way we can run a For loop through it MAKE sure birds are in correct order as above
fall.tracks <- list(rowan.track.F, herald.track.F, morpheus.track.F, patagium.track.F, rip.track.F,
                    trinity.track.F, sam.track.F, rip.track.F2, trinity.track.F2, sam.track.F2, patagium.track.F2,
                    morpheus.track.F2,angell.track.F, bucatini.track.F,ginger.track.F, hallowee.track.F, 
                    mackinaw.track.F, petosegay.track.F, rapini.track.F, voyageur.track.F, patagium.track.F3,
                    morpheus.track.F3, rip.track.F3, trinity.track.F3, sam.track.F3, jack.track.F, angell.track.F2,
                    mackinaw.track.F2, carlile.track.F, cricket.track.F, cypress.track.F,hc2.track.F, lightfoot.track.F,
                    mh1.track.F, rogerthat.track.F, patagium.track.F4, trinity.track.F4, mackinaw.track.F3, cypress.track.F2,
                    lightfoot.track.F2, rogerthat.track.F2, baker.track.F, campa.track.F, disco.track.F, mercury.track.F,
                    owen.track.F, paprika.track.F, reno.track.F)

# Assign each list a name, in this case each track corresponds to the individual bird. Keep the order the same
bird.id <- list("Rowan","Herald","Morpheus","Patagium","Rip","Trinity","Sam","Rip2", "Trinity2", "Sam2","Patagium2",
                "Morpheus2", "Angell","Bucatini","Ginger","Hallowee","Mackinaw","Petosegay","Rapini","Voyageur","Patagium3",
                "Morpheus3", "Rip3","Trinity3", "Sam3", "Jack","Angell2", "Mackinaw2","Carlile","Cricket","Cypress","Heathcliff II",
                "Lightfoot","MH1", "RogerThat", "Patagium4", "Trinity4", "Mackinaw3", "Cypress2", "Lightfoot2", "RogerThat2", 
                "Baker", "Campa", "Disco", "Mercury", "Owen", "Paprika", "Reno")

# Convert the list to a vector
bird.vector <- unlist(bird.id)

# Get the unique elements
unique.birds <- unique(bird.vector)

# Get the count of unique elements
num.unique.birds <- length(unique.birds)

num.unique.birds

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
fall.migration <- vector('list', 48)


# To test for one individual before we run the full loop
# i = 1 This will test it works on 1 bird
for (i in 1:48) {
  bird <- paste(bird.id[i]) #list of named birds
  fall.dur <- from_to(fall.tracks[[i]])
  duration <- duration.fun(fall.dur, output.units = "days")
  td <- tot_dist(fall.tracks[[i]])/1000   ##Total distance travelled = m divide by 1000 to get KM
  cd <- cum_dist(fall.tracks[[i]])/1000   #Cumlative distance travelled = m divide by 1000 to get KM
  s <- straightness(fall.tracks[[i]])     ##straightness of route (0-1 0= crooked, 1 = straightline)
  jd <-  min(as.numeric(as.data.frame(fall.tracks[[i]])[ ,'julian.day'])) #Get the start date for each birds migration remember (row,column)
  ed <- max(as.numeric(as.data.frame(fall.tracks[[i]])[,'julian.day']))
  # speed <- speed(fall.tracks[[i]])*86.4  # Speed of each migration period (m/s) need to convert to km/d
  # multiply by 86.4 to get KM/Day
  fall.migration[[i]] <- data.frame(
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

# combine all birds into a clean data frame
fall.metrics <- do.call(rbind, fall.migration) 

# Calculate Migration Speed per day = (Cumulative Distance/Duration)
fall.metrics$speed <- fall.metrics$cumulative.dist/ as.numeric(fall.metrics$duration.days)

# Migration Metric Mean and SD ----

# Let's get averages and SD for each metric so we can plot them down below

# Step 1: Calculate the mean for each column
mean_values <- sapply(fall.metrics[, c("duration.days", "total.dist", "cumulative.dist", 
                                       "straightness", "julian.day","end.day", "speed")], mean)

# Step 2: Calculate the SD for each metric

sd_values <- sapply(fall.metrics[, c("duration.days", "total.dist", "cumulative.dist", 
                                     "straightness","julian.day", "end.day", "speed")], sd)


# Step 3: Combine the mean and SE into a new data frame
fall_metrics_summary <- data.frame(
  Metric = c("Duration (days)", "Total Distance (km)", "Cumulative Distance (km)", 
             "Straightness", "Start Date (Julian Day)", "End Date (Julian Day)", "Speed (km/day)"),
  Mean = round(mean_values, 2),
  SD = round(sd_values, 2),
  Mean_SD = paste0(round(mean_values, 2), " ± ", round(sd_values, 2))
)

# Save this data frame as a CSV.
write.csv(fall_metrics_summary,"Chapter.1/Data/full_migrations/fall_migration_summary_25.csv", row.names = FALSE)

write.csv(fall.metrics,"Chapter.1/Data/full_migrations/fall_migration_individual_metrics_25.csv", row.names = FALSE)