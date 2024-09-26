## MSRW Spring 2023 Report ##
## Figures Code ##
## Nick Alioto 06/26/2022 ##

library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(dplyr)
library(magrittr)
library(RColorBrewer)

#Read in the data (Remember working out of a project we don't need to set the WD)
# 2022 Data ----
rb_2020 <- read.csv("Chapter.1/Data/Banding Code/2024_banding_data/2020 Raptor Banding Data Spring.csv", header = T)
rb_2021 <- read.csv("Chapter.1/Data/Banding Code/2024_banding_data/2021 Raptor Banding Data Spring.csv", header = T)
rb_2022 <- read.csv("Chapter.1/Data/Banding Code/2024_banding_data/2022 Raptor Banding Data.csv", header = T)
rb_2023 <- read.csv("Chapter.1/Data/Banding Code/2024_banding_data/2023 Raptor Banding Data.csv", header = T)
rb_2024 <- read.csv("Chapter.1/Data/Banding Code/2024_banding_data/2024 Raptor Banding Data.csv", header = T)
cap_2024 <- read.csv("Chapter.1/Data/Banding Code/2024_banding_data/cap_cases_2024.csv", header = T)


# Plot 1 - Season totals (per day) ----

names(rb_2024)

# Clean out rows that contain the flicker and RTHA from evans site
rb_2024 <- rb_2024[-c(10,131), ] 


# Need to fix the dates structure
rb_2024 <- rb_2024 %>%
  mutate(Date = mdy(Date),   # convert date using lubridate
         Date = format(Date, "%m-%d-%Y")) # Format the date


# Need to make a df with the tally of all species per day in the season

daily_totals_24 <- rb_2024 %>%
  group_by(Date,Species.Code) %>%
  summarise(count = n()) %>%
  ungroup()

# daily average captures
mean(daily_totals_24$count)

# Let's make a stacked barplot to show the different ages captured daily
# Had to trouble shoot commented out lines I no longer wanted may need later.

ggplot(data = daily_totals_24, aes(fill= Species.Code, x=Date, y= count )) + 
  geom_bar(position= "stack", stat = "identity") +
  labs(title = "Species totals Each Day for 2024 Banding Season",
       x = "Date",
       y = " Count",
       fill = "Species Code") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.1)) +
  scale_y_continuous(breaks = seq(0, 12, by = 1), limits = c(0, 12)) +
  scale_fill_brewer(palette = "Set1")
  
  
# R Color brewer palletes - look at all the combos
display.brewer.all()

# Mean capture rate
# Calculate the daily average capture total across the whole season
daily_average_capture <- daily_totals_24 %>%
  group_by(Date) %>%
  summarise(daily_total = sum(count)) %>%
  summarise(daily_average = mean(daily_total))

# 2024 average was 3.6

################################################################################

# Capillaria Plot 2024 ----

### Data Cleaning for df2
View(cap_2024)

# Rearrange the order of age classes in the data so they are chronological when we plot

cap_2024$Age <- factor(cap_2024$Age, levels=c("SY", "TY", "ASY", "ATY"))

str(cap_2024)

# Need to coerce Lesion to be numeric 
cap_2024$Lesion.Cases <- as.numeric(cap_2024$Lesion.Cases)

str(cap_2024)


# Let's do pie graph  of Capillaria Presence this year
# Basic pie chart - 
ggplot(data = cap_2024, aes(x="", y=Lesion.Cases, fill= Age)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + # remove background, grid, numeric labels
  ggtitle("Number of Capillaria detections per Age Class 2024") +
  theme(plot.title = element_text(hjust = 0.5)) + #Center the plot title 
  scale_fill_brewer(palette = "Set1")

################################################################################

# Cumulative Trend Plot for 2020-2024 ----

# Need to standardize the dates for all bandinf df's
# Need to fix the dates structure so they are all the same

# 2020
rb_2020 <- rb_2020 %>%
  mutate(Date = mdy(Date),   # convert date using lubridate
         Date = format(Date, "%m-%d-%Y")) # Format the date
# 2021
# Need to fix the dates structure
rb_2021 <- rb_2021 %>%
  mutate(Date = mdy(Date),   # convert date using lubridate
         Date = format(Date, "%m-%d-%Y")) # Format the date

# 2022
rb_2022 <- rb_2022 %>%
  mutate(Date = mdy(Date),   # convert date using lubridate
         Date = format(Date, "%m-%d-%Y")) # Format the date

# 2023
rb_2023 <- rb_2023 %>%
  mutate(Date = mdy(Date),   # convert date using lubridate
         Date = format(Date, "%m-%d-%Y")) # Format the date

# 2024
rb_2024 <- rb_2024 %>%
  mutate(Date = mdy(Date),   # convert date using lubridate
         Date = format(Date, "%m-%d-%Y")) # Format the date

# Drop columns we don't need from each df, We only need date and species count
# Remove columns we don't want

rb_2020 <- select(rb_2020,Date, Species.Code)
rb_2021 <- select(rb_2021,Date, Species.Code) 
rb_2022 <- select(rb_2022,Date, Species.Code) 
rb_2023 <- select(rb_2023,Date, Species.Code) 
rb_2024 <- select(rb_2024,Date, Species.Code) 

# Comibine all Data frames

all_dat <- bind_rows(
  mutate(rb_2020, Year = 2020), # adding a year column to distinguish between years
  mutate(rb_2021, Year = 2021),
  mutate(rb_2022, Year = 2022),
  mutate(rb_2023, Year = 2023),
  mutate(rb_2024, Year = 2024),
)

# convert date column to Date object 
all_dat$Date <- as.Date(all_dat$Date, format = "%m-%d-%Y")

# Add a day/month column to ignore year for grouping
all_dat <- all_dat %>%
  mutate(DayMonth = format(Date, "%m-%d"))

# Group by Year and DayMonth to calculate daily capture totals
daily_totals <- all_dat %>%
  group_by(Year, DayMonth) %>%
  summarise(DailyTotal = n())

# Convert DayMonth back to Date type for plotting (using 2000 as a placeholder year)
daily_totals$DayMonth <- as.Date(paste("2000", daily_totals$DayMonth, sep = "-"), format = "%Y-%m-%d")


# Plot the data using ggplot2

# Color for each year

colors <- c("2020" = "gray70", "2021" = "blue", "2022" = "gray70", "2023" = "gray70", "2024" = "orange")

ggplot(daily_totals, aes(x = DayMonth, y = DailyTotal, color = as.factor(Year), group = Year)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = colors) +
  labs(title = "Daily Capture Trends 2020-2024",
       x = "Date (Day/Month)",
       y = "Daily Capture Total",
       color = "Year") +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme_bw()



