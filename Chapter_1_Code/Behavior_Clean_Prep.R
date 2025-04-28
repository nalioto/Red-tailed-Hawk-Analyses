###########################
#                         #
# Behavior Data Cleaning  #
#     Nick Alioto         #
#   April 27 - 2025       #
###########################

##################################
library(tidyverse)
library(lubridate)
##################################

# Bring in the crossing behavior data
cross.b <- read.csv("Chapter.2/Data/Crossing_behavior_update.csv", header = TRUE)

# Let's just check the structure
glimpse(cross.b)

# Separate month from date so we can assign it to a number and format it properly
cross.b <- cross.b %>% separate(date, c("month","day","year"))

# Format month to a numerical value - if April assign a 4 else a 5 - only dealing with April and May
cross.b$month.2 <- ifelse(cross.b$month == "April", 4,5)

#Drop the old month column
cross.b <- select(cross.b, bird.ID, day, year, behavior, month.2)

# Reunite columns into correct format - date tells it the new column to make
# followed by the order of the columns and how I want them separated in this case by a hyphen
cross.b <- unite(cross.b, "Date", c(3,5,2), sep = "-")


# Need to format the date time to POSIXCT to match the weather data
cross.b$date <- as.POSIXct(strptime(cross.b$Date, format = "%Y-%m-%d"), tz = "UTC")

#Drop the old date column
cross.b <- select(cross.b, bird.ID, behavior, date)

glimpse(cross.b)


# Need to add in a column for sex of each bird - sex designation needs to be in quotes
cross.b$sex <- ifelse(cross.b$bird.ID == "Angell"|cross.b$bird.ID == "Voyageur"|
                        cross.b$bird.ID == "Ginger"|cross.b$bird.ID == "Orzo"|
                        cross.b$bird.ID == "Hallowee"|
                        cross.b$bird.ID == "Sam"| cross.b$bird.ID == "Trinity" |
                        cross.b$bird.ID == "Patagium"| cross.b$bird.ID == "Herald" |
                        cross.b$bird.ID == "MH1" | cross.b$bird.ID == "MH2", "female","male")

# Let's make the sex column binary-  Andy's advice for the model
# Now if the bird was a female = 1, if a male = 0

cross.b$sex <- ifelse(cross.b$sex == "female",1,0)

#########################################################################################
##### Remove all birds except those that DID not make a direct crosses and 2 year olds #
########################################################################################

direct.dat <- cross.b %>%
  filter(
    !(bird.ID == "Angell" & lubridate::year(date) == 2022),
    !(bird.ID == "Bucatini" & lubridate::year(date) == 2022),
    !(bird.ID == "Heathcliff" & lubridate::year(date) == 2022),
    !(bird.ID == "Herald" & lubridate::year(date) == 2022),
    !(bird.ID == "Rip" & lubridate::year(date) == 2022),
    !(bird.ID == "Sam" & lubridate::year(date) == 2022),
    !(bird.ID == "Uhtred" & lubridate::year(date) == 2022),
    !(bird.ID == "Patagium" & lubridate::year(date) == 2021),
    !(bird.ID == "Rip" & lubridate::year(date) == 2023),
    !(bird.ID == "Morpheus" & lubridate::year(date) == 2023),
    !(bird.ID == "Mackinaw" & lubridate::year(date) == 2023),
    !(bird.ID == "Jack" & lubridate::year(date) == 2023),
    !(bird.ID == "Heathcliif (2)" & lubridate::year(date) == 2023),
    !(bird.ID == "Bucatini" & lubridate::year(date) == 2023),
    !(bird.ID == "Americana" & lubridate::year(date) == 2023),
    !(bird.ID == "Mackinaw" & lubridate::year(date) == 2022),
    !(bird.ID == "MH1" & lubridate::year(date) == 2023),
  )
# Manually remove Bucatini 2022 data
direct.dat <- direct.dat[-c(4:23), ]


tibble <- direct.dat%>%
  group_by(bird.ID) %>%
  reframe(years_present = unique(year(date)))
################################################################################

str(cross.b)

# Need to add a new columns for all behaviors

# New columns to define crossing behaviors; if the behavior is == 1 assign
# a 1, ELSE assign it a zero etc per behavior

# Direct Cross LP to UP
cross.b$direct <- ifelse (cross.b$behavior == 1,1,0)
direct.dat$direct <- ifelse (direct.dat$behavior == 1,1,0)

# island stop-over, Crossing attempt that resulted in an island stop-over for more than a day
cross.b$indirect.so <- ifelse(cross.b$behavior == 2,1,0)

# indirect cross, successful crossing after a stop-over on an island 
cross.b$cross.after.stop <- ifelse(cross.b$behavior == 3,1,0)

# Indirect flight over the islands with no stop
cross.b$indirect.flyover <- ifelse(cross.b$behavior == 4,1,0)

# TRUE FAIL - out 100m over the water before returning in a "v" pattern
cross.b$fail <- ifelse(cross.b$behavior == 5,1,0)

# Crossing the Straits - Either behavior 1 = direct or 2 which is cross to the island w stopover
# or 4 whicb is a direct flyover the island with no stop.
cross.b$all.crosses <- ifelse(cross.b$behavior %in% c(1,2,4), 1, 0)

# No Cross - The bird did not attempt to cross the Straits on a given day
cross.b$no.cross <- ifelse(cross.b$behavior == 0,1,0)

#Save this data frame as a CSV.
write.csv(cross.b,"Chapter.2/Data/Cleaned Data/clean.behavior.2024.corrected(3).csv", row.names = FALSE)