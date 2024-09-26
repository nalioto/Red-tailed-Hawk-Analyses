#################################################
#####           Jonathan Trudeau            #####
#####    Code to animate movement data      #####
#####   (consecutive location data points)  #####
#####               15Aug19                 #####
#################################################

#### This code will create a GIF, displaying animated location data on a satellite image ####


#Packages needed for this
library(moveVis) #creates the animation and stiches together
library(move) #needed for the movement aspect of the code (Move class)
library(raster) #plotting
library(ggplot2) #plotting


#set your working Directory
setwd("C:/Users/jtrudeau/OneDrive - Michigan State University/Documents/MSU Deer Project/LOCATION Data/Downloaded Data/July 2019_All Deer/Locations")

#load location file
move_data <- read.csv("Deer_locations_Clean_Jul19.csv", header = TRUE)

#setwd("C:/Users/jtrudeau/OneDrive - Michigan State University/Documents/MSU Deer Project/LOCATION Data")

#load location file
#move_data_2 <- read.csv("Merging/All_locations/Deer_locations_cleaned_Sept20.csv", header = TRUE)


## Getting the data in the correct format

##Merging date and time into a single column
move_data$timestamp <- paste(move_data$LMT_Date,move_data$LMT_Time)
#converts time and date to Places date then time
move_data$timestamp_a <- strptime(move_data$timestamp, "%m/%d/%Y %H:%M:%S")
#ordering the locations (ensures they are in cronological order)
movedata<-move_data[with(move_data, order(move_data$DeerID, move_data$timestamp_a)),]

## Different format depending on the data
head(move_data_2)
##Merging date and time into a single column
move_data_2$timestamp <- paste(move_data_2$date,move_data_2$time)
#converts time and date to Places date then time
move_data_2$timestamp_a <- strptime(move_data_2$timestamp, "%Y-%m-%d %H:%M:%S")
#ordering the locations (ensures they are in cronological order)
movedata_2<-move_data_2[with(move_data_2, order(move_data_2$ID, move_data_2$timestamp_a)),]
movedata_2$DeerID <- movedata_2$ID



#giving the new dataframe time stamps
date_time <-as.POSIXct(strptime(as.character(movedata_2$timestamp_a), "%Y-%m-%d %H:%M:%S", tz="EST" ))
#defining day, month, year, and time incase you desire these for later analysis
day <- as.numeric(format(date_time, "%d"))
month <- as.numeric(format(date_time, "%m"))
year <- as.numeric(format(date_time, "%Y"))
time <- strftime(date_time, "%H:%M:%S")



#Create a new data.frame with only the attributes needed
#Do not use UTM locations, only use lat and long. I have UTM_E, but this is just saying the X-axis is Longitide
final_move <- data.frame(ID=as.factor(movedata$DeerID), Height=movedata$Height..m., UTM_E=movedata$Longitude...U.00B0.. ,
                         UTM_N=movedata$Latitude...U.00B0.., day, month, year, time, timestamp=date_time)

#Create a new data.frame with only the attributes needed
#Do not use UTM locations, only use lat and long. I have UTM_E, but this is just saying the X-axis is Longitide
#final_move <- data.frame(ID=as.factor(movedata_2$DeerID), UTM_E=movedata_2$Longitude,
#                         UTM_N=movedata_2$Latitude, day, month, year, time, timestamp=date_time)

head(move_data)

unique(final_move$ID) #getting the unique IDs of the deer
#Subset of a specific Deer
fmsu106 <- subset(final_move, final_move$ID == "MSU_46")
W_25 <- subset(final_move, final_move$ID == "W_25" | final_move$ID == "W_24")
W_02 <- subset(final_move, final_move$ID == "W_02" | final_move$ID == "W_20")

class(fmsu106$timestamp)

#Subset of a specific time frame
fm <- subset(fmsu106, fmsu106$timestamp >= "2018-05-28 00:00:01" & 
               fmsu106$timestamp <= "2018-08-04 23:59:59")


#Subset of a specific time frame
#fm <- subset(W_25, W_25$timestamp >= "2020-05-02 00:00:01" & 
#               W_25$timestamp <= "2020-07-04 23:59:59")

#Subset of a specific time frame
#fm <- subset(W_02, W_02$timestamp >= "2020-05-02 00:00:01" & 
 #              W_02$timestamp <= "2020-06-30 23:59:59")

#Convert data.frame into a move class object to be used later.
#actually in lat and long so ignore that it says UTM.
final_move2 <- df2move(fm,  CRS ("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), 
                       x= "UTM_E", y= "UTM_N", time= "timestamp", track_id = "ID", data = NULL)
getwd()
write.csv(fm, file = "W_02.csv")

#Setting unique frame times. Make all tracks share unique timestamps to be assigned to frames. No gaps in sampling rates.
#Sampling rate set to 3 hours. To make the animation move quicker, increase the # of minutes, or vice versa to slow animation down.
m <- align_move(final_move2, res=200, digit=0, unit = "mins")

m.list <- split(m) # split m into list by individual
m.list <- mapply(x = m.list, y = c("blue", "green"), function(x, y){
  x$colour <- y
  return(x)
}) # add colour per individual
m <- moveStack(m.list)

get_maptypes()#Shows you the maps available to use as a background.
#To use any of the "mapbox" maps, you first need to make a mapbox account and get a token. https://www.mapbox.com
#This gives you 50 free maps a month.

setwd("C:/Users/jtrudeau/OneDrive - Michigan State University/Documents/MSU Deer Project/Resource Selection/Model Output")
load("Breeding_mod_1.Rdata")
Breeding_mod_1
summary(Breeding_mod_1)
#Creates the individual frames to be used in animation
#m is the frame timing we created above.
#map_service is the group of maps and map_type is the actual map you choose
#map_token. You need to specify your token. Please create your own and insert after map_token
#alpha is the transparency. Currently set to 50%

frames_p <- frames_spatial(m, trace_show = TRUE,
                         map_service = "mapbox", map_type = "satellite",
                         map_token = "pk.eyJ1IjoianRydWRlYXU5MSIsImEiOiJjanplZGdsYnowMWFiM2ludzVnZ211cmhrIn0.mlMUnSEuV26Du22ZevRILg",
                         alpha = 0.75) %>%
  add_labels(x = "Longitude", y = "Latitude") %>%
  add_northarrow() %>% 
  add_scalebar(height = 0.015) %>%
  add_timestamps (type = "label") %>%
  add_progress()


## Street map
frames_p <- frames_spatial(m, trace_show = TRUE,
                           tail_colour= c("blue", "green"), 
                           path_colours = c("blue", "green"),
                           path_fade = FALSE,
                           trace_colour = c("blue", "green"),
                           map_service = "osm", map_type = "streets", map_res = 0.8, 
                           map_token = "pk.eyJ1IjoianRydWRlYXU5MSIsImEiOiJjanplZGdsYnowMWFiM2ludzVnZ211cmhrIn0.mlMUnSEuV26Du22ZevRILg",
                           alpha = 0.75) %>%
  add_labels(x = "Longitude", y = "Latitude") %>%
  add_northarrow() %>% 
  add_scalebar(height = 0.015) %>%
  add_timestamps (type = "label") %>%
  add_progress()

frames[[20]] # preview one of the frames
?frames_spatial

#Animate frames and save to desired folder
#At the end you are naming the GIF and saving it in the correct format
#animate_frames(frames, out_file = "C:/Users/jtrudeau/OneDrive - Michigan State University/Documents/MSU Deer Project/Conferences/National TWS/Graphics/MSU46_ex_B.gif")
#animate_frames(frames_p, out_file = "C:/Users/jtrudeau/OneDrive - Michigan State University/Documents/MSU Deer Project/Outreach Documents/2020 hunter to hunter webinar/W_02_dispersal_streets.mp4")
animate_frames(frames_p, out_file = "C:/Users/jtrudeau/OneDrive - Michigan State University/Documents/MSU Deer Project/Presentations/Gary's Landscape Ecology/M46_Exurcsion_streets.mp4")


