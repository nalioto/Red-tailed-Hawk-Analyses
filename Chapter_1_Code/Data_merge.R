#####################################
# Merging Weather and Behavior Data #
#   Preparation for Data Analysis   #
#          Nick Alioto              #
#         April 27 2025             #
####################################

##################################
library(tidyverse)
library(lubridate)
library(ggplot2)
##################################


# Load in clean weather and behavior data
weather.dat <-read.csv("Chapter.2/Data/Cleaned Data/clean.weather.update.2023.csv", header = TRUE)
behavior.dat <- read.csv("Chapter.2/Data/Cleaned Data/clean.behavior.2024.corrected(3).csv", header = TRUE)
direct.dat <- read.csv("Chapter.2/Data/Cleaned Data/clean.behavior.2024.direct.csv", header = TRUE)
rm()
# Let's look at the data to make sure everyhting is in order
glimpse(weather.dat)
str(behavior.dat)

# Need to reformat the date to POSIXCt format
weather.dat$date <- as.POSIXct(strptime(weather.dat$date, format = "%Y:%m:%d"), tz = "UTC") # Note adding colons changed to hyphen
behavior.dat$date <- as.POSIXct(strptime(behavior.dat$date, format = "%Y-%m-%d"), tz = "UTC")

direct.dat$date <- as.POSIXct(strptime(direct.dat$date, format = "%Y-%m-%d"), tz = "UTC")

# Need to convert the dates to factor before we merge them - so they are the same
weather.dat$date <- as.factor(weather.dat$date)
behavior.dat$date <- as.factor(behavior.dat$date)

direct.dat$date <- as.factor(direct.dat$date)

str(weather.dat)
str(behavior.dat)

# Need to merge the data together by date - use merge in base R

all.dat <- merge(weather.dat,behavior.dat, by = 'date')

dir.dat <- merge(weather.dat,direct.dat, by = 'date')

# May need to go back and change bird.ID & sex to as.factor in case it doesn't work

str(all.dat)

#Save this data frame as a CSV.
write.csv(all.dat,"Chapter.2/Analyses/chp2_analysis.2024.updated.behaviors(1).csv", row.names = FALSE)

write.csv(dir.dat,"Chapter.2/Analyses/chp2_analysis.2024.direct.cross.csv", row.names = FALSE)

