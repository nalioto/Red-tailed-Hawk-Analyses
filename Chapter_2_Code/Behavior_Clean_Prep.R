###########################
#                         #
# Behavior Data Cleaning  #
#     Nick Alioto         #
#  January 9 - 2023       #
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

# Need to add a new columns for all behaviors

# New column to define an optimal cross; if the behavior is == 1 assign
# a 1, ELSE assign it a zero

cross.b$optimal <- ifelse (cross.b$behavior == 1,1,0)

# island stop-over, Crossing attempt that resulted in an island stop-over for more than a day
cross.b$stop.over <- ifelse(cross.b$behavior == 2,1,0)

# non-optimal cross, successfull cross after a stop-over on an island OR flew over the islands before reaching UP
cross.b$non.opt <- ifelse(cross.b$behavior == 3,1,0)

# TRUE FAIL - out 100m over the water before returning in a "v" patter
cross.b$fail <- ifelse(cross.b$behavior == 4,1,0)

#Save this data frame as a CSV.
write.csv(cross.b,"Chapter.2/Data/Cleaned Data/clean.behavior.2024.corrected(2).csv", row.names = FALSE)
