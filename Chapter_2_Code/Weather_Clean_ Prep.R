###########################
#                         #
# Weather Data Cleaning   #
#     Nick Alioto         #
#  August 31st-2022       #
###########################

##################################
library(tidyverse)
library(lubridate)
library(naniar)
library(circular)
##################################

# Bring in data
# FOR 2022 - Subset out only days when Crossing Behavior occurred each Spring or birds where in vacinity to cross
# April 2021 = April 2nd - 22nd
# April 2022 = April 5th - 28th
# May 2022 = May 4th - 28th

# It's Raw---- Raw weather data
All.2021 <- read.csv("Chapter.2/Data/Weather.data.Mack.City.2021.csv", header = TRUE) #First data Frame
April.2022 <- read.csv("Chapter.2/Data/April.2022.csv", header = TRUE) # Second data Frame
May.2022 <- read.csv("Chapter.2/Data/May.2022.csv", header = TRUE) # Third data Frame
April.2023 <- read.csv("Chapter.2/Data/April.2023.csv", header = TRUE) # fourth data Frame
May.2023 <- read.csv("Chapter.2/Data/May.2023.csv", header = TRUE) # fifth data Frame


# May 2023 Cleaning ----
# Look at the data and its structure
glimpse(May.2023)


# Remove extra rows
May.2023 <- slice(May.2023, 2:7430) #Remove row with units of measure


# Combine Year,month,day,hour,sec into one column
May.2023 <- unite(May.2023, "Date", 1:3, sep ="-") #Bring together year,month,day, separate by dash (-)

May.2023 <- unite(May.2023, "time", 2:3, sep =":") #Add hour and minutes together, separate by colon (:)

May.2023 <- unite(May.2023, "timestamp",  1:2, sep = " ")

# Convert the time to proper format - data from NOAA is in UTC so we will keep it the same
May.2023$time2 <- as.POSIXct(strptime(May.2023$timestamp, format = "%Y-%m-%d %H:%M"), tz = "UTC")

glimpse(May.2023)

# Remove columns we don't want
glimpse(May.2023)

May.2023 <- select(May.2023,time2, WDIR, WSPD, GST, PRES, ATMP, WTMP )

# Need to rename Wind direction and Wind speed column to WDIR and WSP as in April 2022 data frame, for some reason it is different
# and they changed it again on NOAA

names(May.2023)[names(May.2023) == 'WSPD'] <- 'WSP'
names(May.2023)[names(May.2023) == 'GST'] <- 'D.GST'

# Need to convert all columns to numeric valuyes they are currently class chr
May.2023 <- May.2023 %>%
  mutate_at(c("WDIR", "WSP", "D.GST", "PRES", "ATMP", "WTMP"), as.numeric)

str(May.2023)
glimpse(May.2023)

# Need to create a column of the times so we can filter between optimal soaring time
May.2023$hms <- format(as.POSIXct(May.2023$time2), "%H:%M:%S") #Create a column of specific times to filter between

# Since optimal time for soaring is between 10am - 5pm we will filter between these time zones since our data is in UTC
May.2023.sub <- May.2023[May.2023$hms >= "15:00:00" & May.2023$hms <= "22:00:00",]

# Filter between specific days when crossing behavior was observed and or possible
May.2023.sub1 <-
  May.2023.sub %>%
  filter(between(May.2023.sub$time2, as.POSIXct('2023-05-01'), as.POSIXct('2023-05-31 23:54:00')))

# Create a new column "Date" so I can average all the values between 10-5 optimal crossing time for each individual day
May.2023.sub1$date <- format(as.Date(May.2023.sub1$time2), "%Y:%m:%d")

# Need to drop columns we no longer need, specifically (time2, hms)
May.2023.sub2 <- select(May.2023.sub1, WDIR, WSP, D.GST, PRES, ATMP, WTMP, date)

#Need to replace all 999.00 values with NA's for columns that did not collect data at specified time - need to remove crazy values 
# for Delta t as well
May.2023.sub3 <- May.2023.sub2 %>%
  replace_with_na(replace = list(WSP =  c(999.0),
                                 D.GST = c(999.0),
                                 PRES = c(999.0),
                                 ATMP = c(999.0),
                                 WTMP = c(999.0)))

# Let's create the Delta T variable: The difference between sea surface temp and air temperature (Nourani et al. 2021)
May.2023.sub3$delta.t  <- May.2023.sub3$WTMP - May.2023.sub3$ATMP


#### Get averages for all variables within our time we defined is ideal for
# Note we will take the means for each column and have it exclude the NA's
names(May.2023.sub3)

May.2023.averages<- May.2023.sub3 %>%
  
  dplyr::group_by(date) %>%
  
  dplyr::summarise(average.WDIR = mean(WDIR, na.rm = TRUE),
                   average.WSP = mean(WSP, na.rm = TRUE), average.D.GST = mean (D.GST, na.rm = TRUE), 
                   average.PRES = mean (PRES, na.rm = TRUE), average.ATMP = mean(ATMP, na.rm = TRUE), 
                   average.WTMP = mean(WTMP, na.rm = TRUE), average.delta.t = mean (delta.t, na.rm = TRUE))
rm()





# April 2023 Cleaning ----
# Look at the data and its structure
glimpse(April.2022)


# Remove extra rows
April.2023 <- slice(April.2023, 2:7122) #Remove row with units of measure


# Combine Year,month,day,hour,sec into one column
April.2023 <- unite(April.2023, "Date", 1:3, sep ="-") #Bring together year,month,day, separate by dash (-)

April.2023 <- unite(April.2023, "time", 2:3, sep =":") #Add hour and minutes together, separate by colon (:)

April.2023 <- unite(April.2023, "timestamp",  1:2, sep = " ")

# Convert the time to proper format - data from NOAA is in UTC so we will keep it the same
April.2023$time2 <- as.POSIXct(strptime(April.2023$timestamp, format = "%Y-%m-%d %H:%M"), tz = "UTC")

glimpse(April.2023)

# Remove columns we don't want
glimpse(April.2023)

April.2023 <- select(April.2023,time2, WDIR, WSPD, GST, PRES, ATMP, WTMP )

# Need to rename Wind direction and Wind speed column to WDIR and WSP as in April 2022 data frame, for some reason it is different
# and they changed it again on NOAA

names(April.2023)[names(April.2023) == 'WSPD'] <- 'WSP'
names(April.2023)[names(April.2023) == 'GST'] <- 'D.GST'

# Need to convert all columns to numeric valuyes they are currently class chr
April.2023 <- April.2023 %>%
  mutate_at(c("WDIR", "WSP", "D.GST", "PRES", "ATMP", "WTMP"), as.numeric)

str(April.2023)
glimpse(April.2023)

# Need to create a column of the times so we can filter between optimal soaring time
April.2023$hms <- format(as.POSIXct(April.2023$time2), "%H:%M:%S") #Create a column of specific times to filter between

# Since optimal time for soaring is between 10am - 5pm we will filter between these time zones since our data is in UTC
April.2023.sub <- April.2023[April.2023$hms >= "15:00:00" & April.2023$hms <= "22:00:00",]

# Filter between specific days when crossing behavior was observed and or possible
April.2023.sub1 <-
  April.2023.sub %>%
  filter(between(April.2023.sub$time2, as.POSIXct('2023-04-01'), as.POSIXct('2023-04-30 23:54:00')))

# Create a new column "Date" so I can average all the values between 10-5 optimal crossing time for each individual day
April.2023.sub1$date <- format(as.Date(April.2023.sub1$time2), "%Y:%m:%d")

# Need to drop columns we no longer need, specifically (time2, hms)
April.2023.sub2 <- select(April.2023.sub1, WDIR, WSP, D.GST, PRES, ATMP, WTMP, date)

# Need to replace all 999.00 values with NA's for columns that did not collect data at specified time - need to remove crazy values 
# for Delta t as well
April.2023.sub3 <- April.2023.sub2 %>%
  replace_with_na(replace = list(WSP =  c(999.0),
                                 D.GST = c(999.0),
                                 PRES = c(999.0),
                                 ATMP = c(999.0),
                                 WTMP = c(999.0)))

# Let's create the Delta T variable: The difference between sea surface temp and air temperature (Nourani et al. 2021)
April.2023.sub3$delta.t  <- April.2023.sub3$WTMP - April.2023.sub3$ATMP


#### Get averages for all variables within our time we defined is ideal for
# Note we will take the means for each column and have it exclude the NA's
names(April.2023.sub3)

April.2023.averages<- April.2023.sub3 %>%
  
  dplyr::group_by(date) %>%
  
  dplyr::summarise(average.WDIR = mean(WDIR, na.rm = TRUE),
                   average.WSP = mean(WSP, na.rm = TRUE), average.D.GST = mean (D.GST, na.rm = TRUE), 
                   average.PRES = mean (PRES, na.rm = TRUE), average.ATMP = mean(ATMP, na.rm = TRUE), 
                   average.WTMP = mean(WTMP, na.rm = TRUE), average.delta.t = mean (delta.t, na.rm = TRUE))
rm()



# May 2022 Cleaning ----

# Read in the May 2022 data
May.2022 <- read.csv("Chapter.2/Data/May.2022.csv", header = TRUE) # Third data Frame

# Look at the data and its structure
glimpse(May.2022)

# Remove extra rows
May.2022 <- slice(May.2022, 2:7419) #Remove row with units of measure


# Combine Year,month,day,hour,sec into one column
May.2022 <- unite(May.2022, "Date", 1:3, sep ="-") #Bring together year,month,day, separate by dash (-)

May.2022 <- unite(May.2022, "time", 2:3, sep =":") #Add hour and minutes together, separate by colon (:)

May.2022 <- unite(May.2022, "timestamp",  1:2, sep = " ")

# Convert the time to proper format - data from NOAA is in uTC so we will keep it the same
May.2022$time2 <- as.POSIXct(strptime(May.2022$timestamp, format = "%Y-%m-%d %H:%M"), tz = "UTC")

glimpse(May.2022)

# Need to rename Wind direction and Wind speed column to WDIR and WSP as in April 2022 data frame, for some reason it is different
names(May.2022)[names(May.2022) == 'WDI'] <- 'WDIR'
names(May.2022)[names(May.2022) == 'R.WSP'] <- 'WSP'

# Remove columns we don't want, first check column names again with head function
head(May.2022)
May.2022 <- select(May.2022,time2, WDIR, WSP, D.GST, PRES, ATMP, WTMP )


# Need to convert all columns to numeric valuyes they are currently class chr
May.2022 <- May.2022 %>%
  mutate_at(c("WDIR", "WSP", "D.GST", "PRES", "ATMP", "WTMP"), as.numeric)

str(May.2022)
glimpse(May.2022)

# Need to create a column of the times so we can filter between optimal soaring time
May.2022$hms <- format(as.POSIXct(May.2022$time2), "%H:%M:%S") #Create a column of specific times to filter between

# Since optimal time for soaring is between 10am - 5pm we will filter between these time zones since our data is in UTC
May.2022.sub <- May.2022[May.2022$hms >= "15:00:00" & May.2022$hms <= "22:00:00",]

# Filter between specific days when crossing behavior was observed and or possible i.e. bird was in Straits area
May.2022.sub1 <-
  May.2022.sub %>%
  filter(between(May.2022.sub$time2, as.POSIXct('2022-05-04'), as.POSIXct('2022-05-29')))

# Create a new column "Date" so I can average all the values between 10-5 optimal crossing time for each individual day
May.2022.sub1$date <- format(as.Date(May.2022.sub1$time2), "%Y:%m:%d")

# Need to drop columns we no longer need, specifically (time2, hms)
May.2022.sub2 <- select(May.2022.sub1, WDIR, WSP, D.GST, PRES, ATMP, WTMP, date)

# Need to replace all 999.00 values with NA's for columns that did not collect data at specified time - looks as though
# there are no NA's as we see the obs from May.sub.2 -> May.sub.3 did not change
May.2022.sub3 <- May.2022.sub2 %>%
  replace_with_na(replace = list(WSP =  c(999.0),
                                 D.GST = c(999.0),
                                 PRES = c(999.0),
                                 ATMP = c(999.0),
                                 WTMP = c(999.0)))

# Let's create the Delta T variable: The difference between sea surface temp and air temperature (Nourani et al. 2021)
May.2022.sub3$delta.t  <- May.2022.sub3$WTMP - May.2022.sub3$ATMP

#### Get averages for all variables within our time we defined is ideal for
# Note we will take the means for wach column and have it exclude the NA's
names(May.2022.sub3)

May.2022.averages<- May.2022.sub3 %>%
  
  dplyr::group_by(date) %>%
  
  dplyr::summarise(average.WDIR = mean(WDIR, na.rm = TRUE),
                   average.WSP = mean(WSP, na.rm = TRUE), average.D.GST = mean (D.GST, na.rm = TRUE), 
                   average.PRES = mean (PRES, na.rm = TRUE), average.ATMP = mean(ATMP, na.rm = TRUE), 
                   average.WTMP = mean(WTMP, na.rm = TRUE), average.delta.t = mean (delta.t, na.rm = TRUE))
rm()



# April 2022 Cleaning ----

# Read in the data
April.2022 <- read.csv("Chapter.2/Data/April.2022.csv", header = TRUE) # Second data Frame

# Look at the data and its structure
glimpse(April.2022)


# Remove extra rows
April.2022 <- slice(April.2022, 2:7191) #Remove row with units of measure


# Combine Year,month,day,hour,sec into one column
April.2022 <- unite(April.2022, "Date", 1:3, sep ="-") #Bring together year,month,day, separate by dash (-)

April.2022 <- unite(April.2022, "time", 2:3, sep =":") #Add hour and minutes together, separate by colon (:)

April.2022 <- unite(April.2022, "timestamp",  1:2, sep = " ")

# Convert the time to proper format - data from NOAA is in uTC so we will keep it the same
April.2022$time2 <- as.POSIXct(strptime(April.2022$timestamp, format = "%Y-%m-%d %H:%M"), tz = "UTC")

glimpse(April.2022)

# Remove columns we don't want
glimpse(April.2022)

April.2022 <- select(April.2022,time2, WDIR, WSP, D.GST, PRES, ATMP, WTMP )


# Need to convert all columns to numeric valuyes they are currently class chr
April.2022 <- April.2022 %>%
  mutate_at(c("WDIR", "WSP", "D.GST", "PRES", "ATMP", "WTMP"), as.numeric)

str(April.2022)
glimpse(April.2022)

# Need to create a column of the times so we can filter between optimal soaring time
April.2022$hms <- format(as.POSIXct(April.2022$time2), "%H:%M:%S") #Create a column of specific times to filter between

# Since optimal time for soaring is between 10am - 5pm we will filter between these time zones since our data is in UTC
April.2022.sub <- April.2022[April.2022$hms >= "15:00:00" & April.2022$hms <= "22:00:00",]

# Filter between specific days when crossing behavior was observed and or possible
April.2022.sub1 <-
  April.2022.sub %>%
  filter(between(April.2022.sub$time2, as.POSIXct('2022-04-05'), as.POSIXct('2022-04-29')))

# Create a new column "Date" so I can average all the values between 10-5 optimal crossing time for each individual day
April.2022.sub1$date <- format(as.Date(April.2022.sub1$time2), "%Y:%m:%d")

# Need to drop columns we no longer need, specifically (time2, hms)
April.2022.sub2 <- select(April.2022.sub1, WDIR, WSP, D.GST, PRES, ATMP, WTMP, date)

# Need to replace all 999.00 values with NA's for columns that did not collect data at specified time - need to remove crazy values 
# for Delta t as well
April.2022.sub3 <- April.2022.sub2 %>%
  replace_with_na(replace = list(WSP =  c(999.0),
                                 D.GST = c(999.0),
                                 PRES = c(999.0),
                                 ATMP = c(999.0),
                                 WTMP = c(999.0)))

# Let's create the Delta T variable: The difference between sea surface temp and air temperature (Nourani et al. 2021)
April.2022.sub3$delta.t  <- April.2022.sub3$WTMP - April.2022.sub3$ATMP


#### Get averages for all variables within our time we defined is ideal for
# Note we will take the means for wach column and have it exclude the NA's
names(April.2022.sub3)

April.2022.averages<- April.2022.sub3 %>%
  
  dplyr::group_by(date) %>%
  
  dplyr::summarise(average.WDIR = mean(WDIR, na.rm = TRUE),
                   average.WSP = mean(WSP, na.rm = TRUE), average.D.GST = mean (D.GST, na.rm = TRUE), 
                   average.PRES = mean (PRES, na.rm = TRUE), average.ATMP = mean(ATMP, na.rm = TRUE), 
                   average.WTMP = mean(WTMP, na.rm = TRUE), average.delta.t = mean (delta.t, na.rm = TRUE))
rm()




# April 2021 Cleaning----

# Read in the data
All.2021 <- read.csv("Chapter.2/Data/Weather.data.Mack.City.2021.csv", header = TRUE) #First data Frame

# Look at the data and its structure
glimpse(All.2021)

# Remove extra rows
All.2021 <- slice(All.2021, 2:85220) #Remove row with units of measure


# Combine Year,month,day,hour,sec into one column
All.2021 <- unite(All.2021, "Date", 1:3, sep ="-") #Bring together year,month,day, separate by dash (-)

All.2021 <- unite(All.2021, "time", 2:3, sep =":") #Add hour and minutes together, separate by colon (:)

All.2021 <- unite(All.2021, "timestamp",  1:2, sep = " ")

# Convert the time to proper format - data from NOAA is in uTC so we will keep it the same
All.2021$time2 <- as.POSIXct(strptime(All.2021$timestamp, format = "%Y-%m-%d %H:%M"), tz = "UTC")

glimpse(All.2021)

# Need to rename Wind direction and Wind speed column to WDIR and WSP as in April 2022 data frame, for some reason it is different
names(All.2021)[names(All.2021) == 'WSPD'] <- 'WSP'
names(All.2021)[names(All.2021) == 'GST'] <- 'D.GST'

# Remove columns we don't want, first check column names again with head function
head(All.2021)
All.2021 <- select(All.2021,time2, WDIR, WSP, D.GST, PRES, ATMP, WTMP )


# Need to convert all columns to numeric valuyes they are currently class chr
All.2021 <- All.2021 %>%
  mutate_at(c("WDIR", "WSP", "D.GST", "PRES", "ATMP", "WTMP"), as.numeric)

str(All.2021)
glimpse(All.2021)

# Need to create a column of the times so we can filter between optimal soaring time
All.2021$hms <- format(as.POSIXct(All.2021$time2), "%H:%M:%S") #Create a column of specific times to filter between

# Since optimal time for soaring is between 10am - 5pm we will filter between these time zones since our data is in UTC
All.2021.sub <- All.2021[All.2021$hms >= "15:00:00" & All.2021$hms <= "22:00:00",]

# Filter between specific days when crossing behavior was observed and or possible i.e. bird was in Straits area
All.2021.sub1 <-
  All.2021.sub %>%
  filter(between(All.2021.sub$time2, as.POSIXct('2021-04-02'), as.POSIXct('2021-04-23')))

# Create a new column "Date" so I can average all the values between 10-5 optimal crossing time for each individual day
All.2021.sub1$date <- format(as.Date(All.2021.sub1$time2), "%Y:%m:%d")

# Need to drop columns we no longer need, specifically (time2, hms)
All.2021.sub2 <- select(All.2021.sub1, WDIR, WSP, D.GST, PRES, ATMP, WTMP, date)

# Need to replace all 999.00 values with NA's for columns that did not collect data at specified time - looks as though
# there are no NA's as we see the obs from May.sub.2 -> May.sub.3 did not change
All.2021.sub3 <- All.2021.sub2 %>%
  replace_with_na(replace = list(WSP =  c(999.0),
                                 D.GST = c(999.0),
                                 PRES = c(999.0),
                                 ATMP = c(999.0),
                                 WTMP = c(999.0)))

# Let's create the Delta T variable: The difference between sea surface temp and air temperature (Nourani et al. 2021)
All.2021.sub3$delta.t  <- All.2021.sub3$WTMP - All.2021.sub3$ATMP

#### Get averages for all variables within our time we defined is ideal for
# Note we will take the means for wach column and have it exclude the NA's
names(All.2021.sub3)

April.2021.averages<- All.2021.sub3 %>%
  
  dplyr::group_by(date) %>%
  
  dplyr::summarise(average.WDIR = mean(WDIR, na.rm = TRUE),
                   average.WSP = mean(WSP, na.rm = TRUE), average.D.GST = mean (D.GST, na.rm = TRUE), 
                   average.PRES = mean (PRES, na.rm = TRUE), average.ATMP = mean(ATMP, na.rm = TRUE), 
                   average.WTMP = mean(WTMP, na.rm = TRUE), average.delta.t = mean (delta.t, na.rm = TRUE))
rm()

# Combining all cleaned weather data----

weather.dat <- rbind(April.2021.averages,April.2022.averages,May.2022.averages,April.2023.averages,May.2023.averages)

# Save this data frame as a CSV.
write.csv(weather.dat,"Chapter.2/Data/Cleaned Data/clean.weather.update.2023.csv", row.names = FALSE)
