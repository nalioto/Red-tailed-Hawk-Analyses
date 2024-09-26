##########################################
#                                        #
#  Code to convert data to MOVE Object   #
#                                        #
##########################################

#Read in individual bird, one at a time, #I could bring in all animals from movebank (refer to move vignette for code)
Rowan.RTHA <- read.csv("Data/2021 Red-tails/Rowan.csv", header = TRUE)


#Convert data into a move object =( 1 animal), move stack = (multiple animals)
#Dynamin.brn.brg function needs a move object
Rowan <- move(x=Rowan.RTHA$location.long, y=Rowan.RTHA$location.lat,
              time = as.POSIXct(Rowan.RTHA$timestamp, format= "%Y- %m- %d %H:%M:%OS",tz="UTC"),
              proj = CRS("+proj=utm"),
              data= Rowan.RTHA, 
              animal=Rowan.RTHA$individual.local.identifier, 
              sensor=Rowan.RTHA$sensor.type)
