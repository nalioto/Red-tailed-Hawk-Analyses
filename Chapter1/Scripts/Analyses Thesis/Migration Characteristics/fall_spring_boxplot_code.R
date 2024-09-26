##----------------------------##
#  Spring vs. Fall Box plots  #
#   Nick Alioto 02/20/2023    #
## --------------------------##

## Libraries ----
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(RColorBrewer)

## Load in the data ----
fall.mig.full <- read.csv("Chapter.1/Data/full_migrations/fall.migration.23.csv", header = T)
spr.mig.full <- read.csv("Chapter.1/Data/full_migrations/spr.mig.full.23.csv", header = T)

rm()

## Combine the data frames ----
# Need to combine the birds for Fall into one csv
names(spr.mig.full)

# I do not need line 23 now!
# fall.mig <- rbind(fall.mig.full.21, fall.mig.full.22) 

# Add a season column for each df ----
fall.mig <- fall.mig.full %>% mutate (season = "fall")
spr.mig <- spr.mig.full %>% mutate(season = "spring")

## Merge Spring and fall data to make plots (new DF called Metrics) ----
metrics <- rbind(fall.mig, spr.mig)

# Create a Color Palette ----
nicks.colors <- c("#FF7F00","#00FA9A") # A nice spring green combo with  dark orange

# Calculate Averages ----
#Averages to use for plot (FALL)
fall.duration.avg <- mean(fall.mig$duration.days)
fall.tot.dist.avg <- mean(fall.mig$total.dist)
fall.cum.dist.avg <- mean(fall.mig$cumulative.dist)
fall.str.avg <- mean(fall.mig$straightness)
fall.spd.avg <- mean(fall.mig$speed)

# (SPRING)
#Averages to use for plot
spr.duration.avg <- mean(spr.mig$duration.days)
spr.tot.dist.avg <- mean(spr.mig$total.dist)
spr.cum.dist.avg <- mean(spr.mig$cumulative.dist)
spr.str.avg <- mean(spr.mig$straightness)
spr.spd.avg <- mean(spr.mig$speed)

# Boxplot 1-duration ----

duration.box <- ggplot(data = metrics) +
  geom_boxplot(aes(x = season, y = duration.days, fill = season)) +
  scale_fill_manual(values = nicks.colors) +
  xlab("Season")+
  ylab("Duration (days)") +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 16, color = "black", hjust = 0.5),
        legend.position = "none") +
  ggtitle("Migration Duration by Season")
  
plot(duration.box)
  

# Boxplot 2- total distance ----
tot.dist.box <- ggplot(data = metrics) +
  geom_boxplot(aes(x = season, y = total.dist, fill = season)) +
  scale_fill_manual(values = nicks.colors) +
  xlab("Season") + 
  ylab("Distance (Km)") +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 16, color = "black", hjust = 0.5),
        legend.position = "none") +
  ggtitle("Total Distance by Season")

plot(tot.dist.box)

# Boxplot 3- cumulative distance ----
cum.dist.box <- ggplot(data = metrics) +
  geom_boxplot(aes(x = season, y = cumulative.dist, fill = season)) +
  scale_fill_manual(values = nicks.colors) +
  xlab("Season") + 
  ylab("Cumulative Distance (Km)") +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 16, color = "black", hjust = 0.5),
        legend.position = "none") +
  ggtitle("Cumulative Distance by Season")

plot(cum.dist.box)

# Boxplot 4- Straightness ----
tort.box <- ggplot(data = metrics) +
  geom_boxplot(aes(x = season, y = straightness, fill = season)) +
  scale_fill_manual(values = nicks.colors) +
  xlab("Season") + 
  ylab("Tortuosity") +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 16, color = "black", hjust = 0.5),
        legend.position = "none") +
  ggtitle("Route Efficiency by Season")

plot(tort.box)

# Boxplot 5 - Daily Distance KM/day ----

daily.dist.box <- ggplot(data = metrics) +
  geom_boxplot(aes(x = season, y = speed, fill = season)) +
  scale_fill_manual(values = nicks.colors) +
  xlab("Season") + 
  ylab("Daily Distance (Km/day)") +
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 16, color = "black", hjust = 0.5),
        legend.position = "none") +
  ggtitle("Daily Travel Distances")

plot(daily.dist.box)

