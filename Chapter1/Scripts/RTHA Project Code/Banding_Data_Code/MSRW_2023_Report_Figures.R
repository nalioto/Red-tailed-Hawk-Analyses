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
df1 <- read.csv("Chapter.1/Data/Banding Code/2023_banding_data/daily.captures.2023.csv", header = T)
df2 <- read.csv("Chapter.1/Data/Banding Code/2023_banding_data/cap.cases.csv", header = T)
df3 <- read.csv("Chapter.1/Data/Banding Code/2023_banding_data/rtha.captures.2023.csv", header = T)
  
######################################
##                                  ##
##           Plot 1                 ##
######################################


#Let's inspect the data
#Check the structure of the data before proceeding 
str(df1)
View(df1)

# Need to remove the 2nd row from the .csv
df1 <- slice(df1, 2:46)

# This will add a column to group each date to the number in the corresponding 
# column
df1$group<-"same"
names(df1) # see new column name

#Average Captures per day

mean(df1$Count.of.Species.Code)

# Average captures per day = 4.72 in 2022
# Average captures per day = 4.6 in 2023


#Plot for number or birds caught each trapping day during the season
p1 <- ggplot(data = df1, aes(x = Row.Labels, y = Count.of.Species.Code, group=group )) +
  geom_point(color = "black", size = 2) + 
  geom_line(aes(color =group)) +
  theme_bw()+
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  scale_y_continuous(breaks=1:15, limits = c(1, 15)) + 
  labs(title = "Daily Capture Totals Spring 2023",
       subtitle = "Mackinaw City, MI",
       x = "Date (Year-Month-Day)", 
       y = "Totals per Day")
plot(p1)



##############################
#   Plot 2 Capillaria Data   #
#############################

### Data Cleaning for df2
View(df2)

# Rearrange the order of age classes in the data so they are chronological when we plot

df2$Age <- factor(df2$Age, levels=c("SY", "TY", "ASY", "ATY"))

glimpse(df2)

# Need to make coerce Lesion to be numeric 
df2$Lesion.Cases <- as.numeric(df2$Lesion.Cases)

str(df2)

# Let's do pie graph  of Capillaria Presence this year
# Basic pie chart - 
ggplot(data = df2, aes(x="", y=Lesion.Cases, fill=Age)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + # remove background, grid, numeric labels
  ggtitle("Number of Capillaria detections per Age Class 2023") +
  theme(plot.title = element_text(hjust = 0.5)) #Center the plot title +

  

################################################################################
## Create a boxplot of SSHA with Capillarity  #### Used in 2022 not a great plot! In my opinion
p2 <- ggplot(df2, aes(x= Age, y=Lesion, fill= Age)) +
  geom_boxplot() +
  scale_fill_viridis( discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Lesion Scores by Age in Sharp-shinned Hawk") +
  scale_x_discrete(labels = c("1"," 2", "at least 2", 
                              "at least 3"))
plot(p2)



#####################################
##             Plot 3              ##
# Red-tailed Hawk captures by Age  #
#                                  #
######################################


# Clean out rows that have Northern Goshawk in Them
df3 <- df3[-c(33), ]


# Order the age classes
df3$Age <- factor(df3$Age, levels=c("SY", "TY", "ATY"))
df3$Date <- as.factor(df3$Date)

# dplyr- create a tally for all age classes from each day of the season
RTHA.df <- df3 %>%group_by(Age, Date) %>% tally()
names(RTHA.df)


# Let's make a stacked barplot to show the different ages captured daily
# Had to trouble shoot commented out lines I no longer wanted may need later.

rt.plot <- ggplot(data = RTHA.df, aes(fill= Age, x=Date, y=n )) + 
  geom_bar(position= "stack", stat = "identity") +
  scale_color_viridis(discrete = T, option = "D")+
  scale_fill_viridis(discrete = T, labels= c("1", "2", "> 3")) +
  theme_bw() +
  #scale_fill_discrete(labels= c("1", "2", "at least 3", "at least 4")) +
  scale_y_continuous(breaks=0:15, limits = c(0, 15)) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  labs(title = "Red-tailed Hawk daily captures 2023",
       x= "Date (Year/Month/Day",
       y= "Total Captures per Day")
#theme_ipsum()
plot(rt.plot)

# R Color brewer palletes - look at all the combos
display.brewer.all()
  

?scale_fill_viridis()
