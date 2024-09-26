##-----------------------##
#   RTHA Full Migrations  #
#     Nick Alioto         #
#     May 27th  2024      #
##-----------------------##

## Libraries ----
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(tidyverse)
library(plotly)
library(bayesmove)

## clear workspace
rm(list=ls())
rm()                 #Remove particular object from the environmnet 

# bring in raw movement data downloaded as a csv from Movebank.org
# 2021 Red-tails #
a <- read.csv("Chapter.1/Data/2021_Red-tails/Herald.csv", header=TRUE) 
b <- read.csv("Chapter.1/Data/2021_Red-tails/Jack.csv", header=TRUE) 
c <- read.csv("Chapter.1/Data/2021_Red-tails/Kirby.csv", header=TRUE) 
d <- read.csv("Chapter.1/Data/2021_Red-tails/Morpheus.csv", header=TRUE) 
e <- read.csv("Chapter.1/Data/2021_Red-tails/Patagium.csv", header=TRUE) 
f <- read.csv("Chapter.1/Data/2021_Red-tails/Rip.csv", header=TRUE)
g <- read.csv("Chapter.1/Data/2021_Red-tails/Rowan.csv", header=TRUE)
h <- read.csv("Chapter.1/Data/2021_Red-tails/Sam.csv", header = TRUE)
i <- read.csv("Chapter.1/Data/2021_Red-tails/Trinity.csv", header = TRUE)

# 2022 Red-tails #
j <- read.csv("Chapter.1/Data/2022_Red-tails/Angell.csv", header = TRUE)
k <- read.csv("Chapter.1/Data/2022_Red-tails/Bucatini.csv", header = TRUE)
l <- read.csv("Chapter.1/Data/2022_Red-tails/Ginger.csv", header = TRUE)
m <- read.csv("Chapter.1/Data/2022_Red-tails/Hallowee.csv", header = TRUE)
n <- read.csv("Chapter.1/Data/2022_Red-tails/Mackinaw.csv", header = TRUE)
o <- read.csv("Chapter.1/Data/2022_Red-tails/Petosegay.csv", header = TRUE)
p <- read.csv("Chapter.1/Data/2022_Red-tails/Rapini.csv", header = TRUE)
q <- read.csv("Chapter.1/Data/2022_Red-tails/Voyageur.csv", header = TRUE)

# 2023 Red-tails #
r <- read.csv("Chapter.1/Data/2023_Red-tails/Carlile.csv", header = TRUE)
s <- read.csv("Chapter.1/Data/2023_Red-tails/Cricket.csv", header = TRUE)
t <- read.csv("Chapter.1/Data/2023_Red-tails/Cyprus.csv", header = TRUE)
u <- read.csv("Chapter.1/Data/2023_Red-tails/Heathcliff-2.csv", header = TRUE)
v <- read.csv("Chapter.1/Data/2023_Red-tails/Lightfoot.csv", header = TRUE)
w <- read.csv("Chapter.1/Data/2023_Red-tails/MH1.csv", header = TRUE)


#Need to convert the timestamps
a$timestamps <- as.POSIXct(strptime(a$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
b$timestamps <- as.POSIXct(strptime(b$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
c$timestamps <- as.POSIXct(strptime(c$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
d$timestamps <- as.POSIXct(strptime(d$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
e$timestamps <- as.POSIXct(strptime(e$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
f$timestamps <- as.POSIXct(strptime(f$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
g$timestamps <- as.POSIXct(strptime(g$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
h$timestamps <- as.POSIXct(strptime(h$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
i$timestamps <- as.POSIXct(strptime(i$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
j$timestamps <- as.POSIXct(strptime(j$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
k$timestamps <- as.POSIXct(strptime(k$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
l$timestamps <- as.POSIXct(strptime(l$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
m$timestamps <- as.POSIXct(strptime(m$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
n$timestamps <- as.POSIXct(strptime(n$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
o$timestamps <- as.POSIXct(strptime(o$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
p$timestamps <- as.POSIXct(strptime(p$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
q$timestamps <- as.POSIXct(strptime(q$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
r$timestamps <- as.POSIXct(strptime(r$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
s$timestamps <- as.POSIXct(strptime(s$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
t$timestamps <- as.POSIXct(strptime(t$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
u$timestamps <- as.POSIXct(strptime(u$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
v$timestamps <- as.POSIXct(strptime(v$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
w$timestamps <- as.POSIXct(strptime(w$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")




## Subset the times we want to plot if we don't do it numerically
{
a <- subset(a, timestamp>="2021-04-03 00:00:01" & timestamp<="2022-02-28 23:59:46")
b <- subset(b, timestamp>="2021-04-03 00:00:01" & timestamp<="2022-02-28 23:58:54")
c <- subset(c, timestamp>="2021-04-03 00:00:01" & timestamp<="2022-02-28 20:03:49")
d <- subset(d, timestamp>="2021-04-03 00:00:01" & timestamp<="2022-02-28 20:03:49")
e <- subset(e, timestamp>="2021-04-03 00:00:01" & timestamp<="2022-02-28 20:03:49")
f <- subset(f, timestamp>="2021-04-03 00:00:01" & timestamp<="2022-02-28 20:03:49")
g <- subset(g, timestamp>="2021-04-03 00:00:01" & timestamp<="2022-02-28 20:03:49")
h <- subset(h, timestamp>="2021-04-03 00:00:01" & timestamp<="2022-02-28 20:03:49")

i <- subset(i, timestamp>="2022-04-01 00:00:01" & timestamp<="2023-02-08 20:03:49")
j <- subset(j, timestamp>="2022-04-01 00:00:01" & timestamp<="2023-02-08 20:03:49")
k <- subset(k, timestamp>="2022-04-01 00:00:01" & timestamp<="2023-02-08 20:03:49")
l <- subset(l, timestamp>="2022-04-01 00:00:01" & timestamp<="2023-02-08 20:03:49")
m <- subset(m, timestamp>="2022-04-01 00:00:01" & timestamp<="2023-02-08 20:03:49")
n <- subset(n, timestamp>="2022-04-01 00:00:01" & timestamp<="2023-02-08 20:03:49")
o <- subset(o, timestamp>="2022-04-01 00:00:01" & timestamp<="2023-02-08 20:03:49")
p <- subset(p, timestamp>="2022-04-01 00:00:01" & timestamp<="2023-02-08 20:03:49")
    }


# Need to drop extra columns from a few DF's were I downloaded extra data (2023 birds specifically)
r <- r %>% select(-21, -22) # column 21 and 22 correspond to study.time.zone and study.local.time
s <- s %>% select(-21, -22)
t <- t %>% select(-21, -22)
u <- u %>% select(-21, -22)
v <- v %>% select(-21, -22)



##Combine all CSV's into one single file
raw <- rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w)

# let's remove the old data frames for saving stirage space in R
rm(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w)

## Sub Set to only relevant columns for plotting
raw <- subset(raw, select=c(timestamp,location.long,location.lat,individual.local.identifier)) 

##rename column names
raw <- raw %>%
  rename(
    long = location.long,
    lat = location.lat,
    tagid = individual.local.identifier
  )
str(raw) 
raw$tagid <- as.factor(raw$tagid) 
str(raw)

##order timestamps when including GPS and Argos data, and omit NAs #
rtha <- raw[order(raw$tagid, raw$timestamp),]
rtha <- na.omit(rtha)
str(rtha)

# Need shape files to plot ----

# Get United states and Canada shape file
US <- ne_states(country = "United States of America", returnclass = "sf")

CAN <- ne_states(country = "Canada", returnclass = "sf")

# Merge to form North America
NorAm <- rbind(US, CAN)

# Plot Code ----

# Wrap your ggplot with plottly function to make it interactive
plotly::ggplotly()

ggplot() +
  geom_sf(data = NorAm, fill = "grey", color = "white", size = 0.2) +
  geom_path(data = raw , aes(x=location.long,y=location.lat), linewidth = 1, color = "magenta") + # for multiple birds # group= tagid,color= tagid) #c("darkviolet","orange2")[filtered$spr.24]) +
  scale_color_viridis_d()+
  theme_bw() +
  xlab("") + 
  ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(panel.grid.minor=element_blank(),
        legend.title= element_blank(),
        axis.title=element_text(size = 12),   # element_text(size=12) adjust size   
        axis.text=element_text(size = 10),
        axis.ticks=element_blank()) +          # element_text(size=12) adjust size   
  coord_sf(xlim=c(-93,-75), ylim=c(37.5,53), expand = FALSE)
#Coordinates for MI Birds (lat/long)
#coord_fixed(xlim=c(-93,-75), ylim=c(42,52), ratio=1.5)

# Coordinates for KY Birds (lat/long)
#coord_fixed(xlim=c(-93,-75), ylim=c(37,52), ratio=1.5) 

# Coordinates for showing tracks including all tracked birds #
# coord_fixed(xlim=c(-95,-69), ylim=c(34,57), ratio=1.5) 

dev.off() # turn off the device
################################################################################################
??scale_color_viridis