## MSRW Spring 2022 Report ##
## Figures Code ##
## Nick Alioto 05/26/2022 ##

library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(dplyr)
library(magrittr)


#Read in the data (Remember working out of a project we don't need to set the WD)
df1 <- read.csv("Data/captures.csv", header = T)
df2 <- read.csv("Data/Capillaria Data 2022.csv", header = T)


######################################
##                                  ##
##           Plot 1                 ##
######################################


#Let's inspect the data
#Check the structure of the data before proceeding 
str(df1)
View(df1)

# This will add a column to group each date to the number in the corresponding 
# column
df1$group<-"same"
names(df1) # see new column name

#Average Captures per day

mean(df1$Count)

# Average captures per day = 4.72 in 2022


#Plot for number or birds caught each trapping day during the season
p1 <- ggplot(data = df1, aes(x = Date, y = Count, group=group )) +
  geom_point(color = "black", size = 2) + 
  geom_line(aes(color =group)) +
  theme_bw()+
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  scale_y_continuous(breaks=1:15, limits = c(1, 15)) + 
  labs(title = "Daily Raptor totals Spring 2022",
       subtitle = "Mackinaw City, MI",
       x = "Date (Month/Day/Year)", 
       y = "Totals per Day")
plot(p1)




# Capillaria Plot #

###############################################################################
###############################################################################

### Data Cleaning for df2
View(df2)

# Let's get rid of rows with COHA we don't want them for this plot
# tells r to take out specific rows  [row,columns] r syntax, -c deletes them

df2 <- df2[-c(2,7,57),]

View(df2)

# Rearrange the order of age classes in the data so they are chronological when we plot

df2$Age <- factor(df2$Age, levels=c("SY", "TY", "ASY", "ATY"))



## Create a boxplot of SSHA with Capillarity 
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
##                                 ##
######################################

#Read in the data
RTHA.1 <- read.csv("Data/RTHA 2022.CSV", header = T)
View(RTHA.1)
  

# Clean out rows that have Rough-Legged Hawk in Them
RTHA.1 <- RTHA.1[-c(15,16,85,91), ]


# Order the age classes
RTHA.1$Age <- factor(RTHA.1$Age, levels=c("SY", "TY", "ATY", "A4Y"))
RTHA.1$Date <- as.factor(RTHA.1$Date)

# dplyr- create a tally for all age classes from each day of the season
RTHA.2 <- RTHA.1 %>%group_by(Age, Date) %>% tally()
names(RTHA.2)


# Let's make a stacked barplot to show the different ages captured daily
# Had to trouble shoot commented out lines I no longer wanted may need later.

p3 <- ggplot(data = RTHA.2, aes(fill= Age, x=Date, y=n )) + 
  geom_bar(position= "stack", stat = "identity") +
  theme_bw() +
  scale_fill_viridis(discrete = T, labels= c("1", "2", "at least 3", "at least 4")) +
  #scale_fill_discrete(labels= c("1", "2", "at least 3", "at least 4")) +
  #scale_y_continuous(breaks=0:8, limits = c(0, 8)) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  labs(title = "Red-tailed hawk daily captures 2022",
       x= "Date (Month/Day/Year",
       y= "Total Captured per Day")
  #theme_ipsum()
plot(p3)
  
?scale_fill_viridis()
