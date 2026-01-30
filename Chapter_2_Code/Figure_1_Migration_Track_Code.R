#################################################
##  Chapter 2 Migratory Routes Fig 1 Plot Code ##
##     Nick Alioto                             ##
##    Auguest 26th 2025                        ##
##################################################

################################################################################
library(amt)
library(adehabitatLT)
library(tidyverse)
library(plotrix) # Calculate Standard Errors on metrics 
library(tidyverse)
library(plotly)
library(sf)
library(RColorBrewer)
################################################################################
rm(list = ls())

######################################################
# 1) Need shape files to plot migration tracks on----

# Set your file path to the unzipped .shp file
shapefile_path <- "Chapter.1/Data/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp"
lakes_path <- "Chapter.1/Data/ne_10m_lakes/ne_10m_lakes.shp"

# Read shapefile
states <- st_read(shapefile_path)
lakes <- st_read(lakes_path)

# Filter for large lakes (includes Great Lakes)
great_lakes <- lakes %>% filter(name %in% c("Lake Superior", "Lake Michigan", "Lake Huron", "Lake Erie", "Lake Ontario"))

# Filter USA and Canada admin areas
us_can <- states %>% filter(admin %in% c("United States of America", "Canada"))

# IMPORTANT: Make sure CRS matches
great_lakes <- st_transform(great_lakes, st_crs(us_can))

# Subtract lakes from states (this creates actual lake holes)
us_can_clean <- st_difference(us_can, st_union(great_lakes))


#################################################
# 2) RTHA Data - Spring and Fall Routes 2021-2025 ----


##### Separarting Spring and Fall ###########

# use amt package for this
# We want to define start/end dates, duration, Distance, cumulative distance, migration
# straightness and speed
# Need to subset data into migration periods for individual metrics

################################################################################
rm(hawk.list)

# STEP 1
# 2021 birds
Hawks.1 <- list.files(path="Chapter.1/Data/2021_Red-tails",pattern = ".csv", full.names = T) # The order should be alphabetical

Hawks.21 <- do.call("rbind",lapply(Hawks.1, read.csv))
# In console type in Hawks.l to make sure it is in the order you want

# 2022 birds
Hawks.2 <- list.files(path="Chapter.1/Data/2022_Red-tails",pattern = ".csv", full.names = T) # The order should be alphabetical

Hawks.22 <- do.call("rbind",lapply(Hawks.2, read.csv))

# 2023 birds
Hawks.3 <- list.files(path="Chapter.1/Data/2023_Red-tails",pattern = ".csv", full.names = T) # The order should be alphabetical

Hawks.23 <- do.call("rbind",lapply(Hawks.3, read.csv))

# 2024 birds
Hawks.4 <- list.files(path="Chapter.1/Data/2024_Red-tails",pattern = ".csv", full.names = T) # The order should be alphabetical

Hawks.24 <- do.call("rbind",lapply(Hawks.4, read.csv))

# 2021 birds
{
  H1 <- read.csv(Hawks.1[7])
  H2 <- read.csv(Hawks.1[1])
  H3 <- read.csv(Hawks.1[4])
  H4 <- read.csv(Hawks.1[5])
  H5 <- read.csv(Hawks.1[6])
  H6 <- read.csv(Hawks.1[8])
  H7 <- read.csv(Hawks.1[9])
  
  # 2022 birds
  H8 <- read.csv(Hawks.2[1])
  H9 <- read.csv(Hawks.2[2])
  H10 <- read.csv(Hawks.2[3])
  H11 <- read.csv(Hawks.2[4])
  H12 <- read.csv(Hawks.2[5])
  H13 <- read.csv(Hawks.2[6])
  H14 <- read.csv(Hawks.2[7])
  H15 <- read.csv(Hawks.2[8])
  
  # 2023 birds
  H16 <- read.csv(Hawks.3[1])
  H17 <- read.csv(Hawks.3[2])
  H18 <- read.csv(Hawks.3[3])
  H19 <- read.csv(Hawks.3[4])
  H20 <- read.csv(Hawks.3[5])
  H21 <- read.csv(Hawks.3[6])
  H22 <- read.csv(Hawks.1[2]) # added in Jack down here so order of code birds wouldn't have to change (in 2021 cohort)
  H30 <- read.csv(Hawks.3[7]) # Roger That from 2023 - checked in late added here.
  
  # 2024 birds
  H23 <- read.csv(Hawks.4[1])
  H24 <- read.csv(Hawks.4[2])
  H25 <- read.csv(Hawks.4[3])
  H26 <- read.csv(Hawks.4[4])
  H27 <- read.csv(Hawks.4[5])
  H28 <- read.csv(Hawks.4[6])
  H29 <- read.csv(Hawks.4[7])
  
}

# Step 2
#Format time into POSIXct so it can be converted into a move object
{
  H1$timestamps <- as.POSIXct(strptime(H1$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H2$timestamps <- as.POSIXct(strptime(H2$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H3$timestamps <- as.POSIXct(strptime(H3$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H4$timestamps <- as.POSIXct(strptime(H4$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H5$timestamps <- as.POSIXct(strptime(H5$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H6$timestamps <- as.POSIXct(strptime(H6$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H7$timestamps <- as.POSIXct(strptime(H7$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H8$timestamps <- as.POSIXct(strptime(H8$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H9$timestamps <- as.POSIXct(strptime(H9$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H10$timestamps <- as.POSIXct(strptime(H10$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H11$timestamps <- as.POSIXct(strptime(H11$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H12$timestamps <- as.POSIXct(strptime(H12$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H13$timestamps <- as.POSIXct(strptime(H13$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H14$timestamps <- as.POSIXct(strptime(H14$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H15$timestamps <- as.POSIXct(strptime(H15$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H16$timestamps <- as.POSIXct(strptime(H16$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H17$timestamps <- as.POSIXct(strptime(H17$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H18$timestamps <- as.POSIXct(strptime(H18$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H19$timestamps <- as.POSIXct(strptime(H19$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H20$timestamps <- as.POSIXct(strptime(H20$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H21$timestamps <- as.POSIXct(strptime(H21$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H22$timestamps <- as.POSIXct(strptime(H22$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H23$timestamps <- as.POSIXct(strptime(H23$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H24$timestamps <- as.POSIXct(strptime(H24$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H25$timestamps <- as.POSIXct(strptime(H25$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H26$timestamps <- as.POSIXct(strptime(H26$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H27$timestamps <- as.POSIXct(strptime(H27$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H28$timestamps <- as.POSIXct(strptime(H28$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H29$timestamps <- as.POSIXct(strptime(H29$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  H30$timestamps <- as.POSIXct(strptime(H30$timestamp, format =  "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  
  
}

# Convert the times tamps from a 'character' to 'numeric' class for filtering purposes
{
  H1$ts.numeric <- as.numeric(H1$timestamps)
  H2$ts.numeric <- as.numeric(H2$timestamps)
  H3$ts.numeric <- as.numeric(H3$timestamps)
  H4$ts.numeric <- as.numeric(H4$timestamps)
  H5$ts.numeric <- as.numeric(H5$timestamps)
  H6$ts.numeric <- as.numeric(H6$timestamps)
  H7$ts.numeric <- as.numeric(H7$timestamps)
  H8$ts.numeric <- as.numeric(H8$timestamps)
  H9$ts.numeric <- as.numeric(H9$timestamps)
  H10$ts.numeric <- as.numeric(H10$timestamps)
  H11$ts.numeric <- as.numeric(H11$timestamps)
  H12$ts.numeric <- as.numeric(H12$timestamps)
  H13$ts.numeric <- as.numeric(H13$timestamps)
  H14$ts.numeric <- as.numeric(H14$timestamps)
  H15$ts.numeric <- as.numeric(H15$timestamps)
  H16$ts.numeric <- as.numeric(H16$timestamps)
  H17$ts.numeric <- as.numeric(H17$timestamps)
  H18$ts.numeric <- as.numeric(H18$timestamps)
  H19$ts.numeric <- as.numeric(H19$timestamps)
  H20$ts.numeric <- as.numeric(H20$timestamps)
  H21$ts.numeric <- as.numeric(H21$timestamps)
  H22$ts.numeric <- as.numeric(H22$timestamps)
  H23$ts.numeric <- as.numeric(H23$timestamps)
  H24$ts.numeric <- as.numeric(H24$timestamps)
  H25$ts.numeric <- as.numeric(H25$timestamps)
  H26$ts.numeric <- as.numeric(H26$timestamps)
  H27$ts.numeric <- as.numeric(H27$timestamps)
  H28$ts.numeric <- as.numeric(H28$timestamps)
  H29$ts.numeric <- as.numeric(H29$timestamps)
  H30$ts.numeric <- as.numeric(H30$timestamps)
  
  
}
##########################
# Fall Migrations N = 48 #
##########################
# 2021 Fall Mig
{
  fall.rowan <- H1 %>% filter(between(ts.numeric,(1632427081),(1635998313)))
  fall.herald <- H2 %>% filter(between(ts.numeric,(1634385549),(1636588801)))
  fall.morpheus <- H3 %>% filter(between(ts.numeric,(1634957790),(1638640591)))
  fall.patagium <- H4 %>% filter(between(ts.numeric,(1634500741),(1637697511)))
  fall.rip <- H5 %>% filter(between(ts.numeric,(1633435050),(1636545489)))
  fall.sam <- H6 %>% filter(between(ts.numeric,(1634406960),(1635724652)))
  fall.trinity <- H7 %>% filter(between(ts.numeric,(1634061721),(1635357871)))
  
  # 2022 Fall Mig
  fall.morpheus2 <- H3 %>% filter(between(ts.numeric,(1665071790),(1668369390)))
  fall.patagium2 <- H4 %>% filter(between(ts.numeric,(1665849534),(1669406365)))
  fall.rip2 <- H5 %>% filter(between(ts.numeric,(1663175829),(1667505450)))
  fall.sam2 <- H6 %>% filter(between(ts.numeric,(1665086220),(1666555081)))
  fall.trinity2 <- H7 %>% filter(between(ts.numeric,(1665057895),(1669752235)))
  fall.angell <- H8 %>% filter(between(ts.numeric,(1663067366),(1667503466)))
  fall.bucatini <- H9 %>% filter(between(ts.numeric,(1663847934),(1677671941))) 
  fall.ginger <- H10 %>% filter(between(ts.numeric,(1665057798),(1667066599)))
  fall.hallowee <- H11 %>% filter(between(ts.numeric,(1665155041),(1666299841)))
  fall.mackinaw <- H12 %>% filter(between(ts.numeric,(1663934358),(1668535149)))
  fall.petosegay <- H13 %>% filter(between(ts.numeric,(1666023061),(1669389031)))
  fall.rapini <- H14 %>% filter(between(ts.numeric,(1666353540),(1669053559)))
  fall.voyageur <- H15 %>% filter(between(ts.numeric,(1665921678),(1667401299)))
  
  # 2023 Fall Mig
  fall.patagium3 <- H4 %>% filter(between(ts.numeric,(1696708741),(1700107141)))
  fall.morpheus3 <- H3 %>% filter(between(ts.numeric,(1697385384),(1699459015)))
  fall.rip3 <- H5 %>% filter(between(ts.numeric,(1694188650),(1699747075)))
  fall.trinity3 <- H7 %>% filter(between(ts.numeric,(1697918599),(1699646644)))
  fall.sam3 <- H6 %>% filter(between(ts.numeric,(1697226932),(1698105450)))
  fall.jack <- H22 %>% filter(between(ts.numeric,(1697313858),(1700597100)))
  fall.angell2 <- H8 %>% filter(between(ts.numeric,(1696701600),(1699217995)))
  fall.mackinaw2 <- H12 %>% filter(between(ts.numeric,(1697057995),(1702155565)))
  fall.carlile <- H16 %>% filter(between(ts.numeric,(1699120832),(1700929207)))
  fall.cricket <- H17 %>% filter(between(ts.numeric,(1698267609),(1700078446)))
  fall.cypress <- H18 %>% filter(between(ts.numeric,(1694447409),(1698792005)))
  fall.hc2 <- H19 %>% filter(between(ts.numeric,(1696604450),(1698007986)))
  fall.lightfoot <- H20 %>% filter(between(ts.numeric,(1697824832),(1699649444)))
  fall.mh1 <- H21 %>% filter(between(ts.numeric,(1697133620),(1699635005)))
  fall.rogerthat <- H30 %>% filter(between(ts.numeric,(1697802604),(1699120833)))
  
  # Fall 2024 Migrations
  
  fall.patagium4 <- H4 %>% filter(between(ts.numeric,(1728590332),(1731095936)))
  fall.trinity4 <- H7 %>% filter(between(ts.numeric,(1727971449),(1729253040)))
  fall.mackinaw3 <- H12 %>% filter(between(ts.numeric,(1728475159),(1732989481)))
  fall.cypress2 <- H18 %>% filter(between(ts.numeric,(1728064216),(1729977486)))
  fall.lightfoot2 <- H20 %>% filter(between(ts.numeric,(1729987269),(1732176039)))
  fall.rogerthat2 <- H30 %>% filter(between(ts.numeric,(1728579602),(1730235008)))
  fall.baker <- H23 %>% filter(between(ts.numeric,(1730041210),(1733335217)))
  fall.campa <- H24 %>% filter(between(ts.numeric,(1726416012),(1727380805)))
  fall.disco <- H25 %>% filter(between(ts.numeric,(1725559224),(1729022404)))
  fall.mercury <- H26 %>% filter(between(ts.numeric,(1726495222),(1729087256)))
  fall.owen <- H27 %>% filter(between(ts.numeric,(1728403287),(1729288833)))
  fall.paprika <- H28 %>% filter(between(ts.numeric,(1724774401),(1729195232)))
  fall.reno <- H29 %>% filter(between(ts.numeric,(1717945805),(1718391842)))
  
}
#####################
# Spring Migrations #
####################
{
  # Spring 2022 Migrations
  spring.rowan <- H1 %>% filter(between(ts.numeric,(1648627157),(1650217705)))
  # Mortality during migration April 23rd (include for dBBMM not for migration characteristics)
  # spring.herald <- H2 %>% filter(between(ts.numeric,(1649764801),(1650743934))) 
  spring.morpheus <- H3 %>% filter(between(ts.numeric,(1649332609),(1650524760)))
  spring.patagium <- H4 %>% filter(between(ts.numeric,(1649606301),(1650455935)))
  spring.rip <- H5 %>% filter(between(ts.numeric,(1646481450),(1651060644)))
  spring.sam <- H6 %>% filter(between(ts.numeric,(1649445714),(1650455844)))
  spring.trinity <- H7 %>% filter(between(ts.numeric,(1649347451),(1651061040)))
  spring.jack <- H22 %>% filter(between(ts.numeric,(1649620974),(1650629048)))
  
  # Spring 2023 Migrations
  spring.morpheus2 <- H3 %>% filter(between(ts.numeric,(1681243021),(1683141300)))
  spring.patagium2 <- H4 %>% filter(between(ts.numeric,(1680465521),(1681315128)))
  spring.rip2 <- H5 %>% filter(between(ts.numeric,(1676836644),(1683062639)))
  spring.sam2 <- H6 %>% filter(between(ts.numeric,(1678996999),(1681591441)))
  spring.trinity2 <-H7 %>% filter(between(ts.numeric,(1680465521),(1681315128)))
  spring.jack2 <- H22 %>% filter(between(ts.numeric,(1680811501),(1686244108)))
  spring.angell <- H8 %>% filter(between(ts.numeric, (1681322947),(1682699881)))
  spring.mackinaw <- H12 %>% filter(between(ts.numeric,(1682445540),(1685050267)))
  spring.rapini <- H14 %>% filter(between(ts.numeric,(1680199141),(1683650976)))
  spring.voyageur <- H15 %>% filter(between(ts.numeric, (1680192045),(1680879669)))
  
  # Spring 2024 Migrations
  spring.morpheus3 <- H3 %>% filter(between(ts.numeric,(1711972591),(1713124601)))
  spring.patagium3 <- H4 %>% filter(between(ts.numeric,(1712505541),(1713371137)))
  spring.trinity3 <-H7 %>% filter(between(ts.numeric,(1712419410),(1713645959)))
  spring.mackinaw2 <- H12 %>% filter(between(ts.numeric,(1713279571),(1714682959)))
  spring.carlile <- H16 %>% filter(between(ts.numeric,(1708095613),(1712522344)))
  spring.cricket <- H17 %>% filter(between(ts.numeric,(1708365633),(1714593606)))
  spring.cypress <- H18 %>% filter(between(ts.numeric,(1712588433),(1714942209)))
  spring.lightfoot <- H20 %>% filter(between(ts.numeric,(1710439241),(1713196825)))
  spring.rogerthat <- H30 %>% filter(between(ts.numeric,(1711994825),(1712857050)))
  
  # Spring 2025 Migrations
  spring.patagium4 <- H4 %>% filter(between(ts.numeric,(1745524705),(1746460728)))
}

# Make sure we have the right number of migrations Fall = 48, Spring = 27, Total = 75
length(ls(pattern = "^spring\\."))
length(ls(pattern = "^fall\\."))
length(ls(pattern = "^(spring|fall)\\."))


# Identify which objects start with spring and fall, then add a column to denote
# their season so we can plot spring and fall tracks respectively.

# Get all object names
mig_names <- ls(pattern = "^(spring|fall)\\.")

## Combine all objects, preserving source and season info
all_migs <- purrr::map_dfr(mig_names, function(obj_name) {
  df <- get(obj_name)
  df$source <- obj_name   # original object name
  df$season <- ifelse(grepl("^spring", obj_name), "spring", "fall")  # extract season
  
  # Ensure tag.local.identifier is character for consistent binding
  if("tag.local.identifier" %in% names(df)) {
    df$tag.local.identifier <- as.character(df$tag.local.identifier)
  }
  
  return(df)
})

# Some formatting issues
all_migs$season <- as.factor(all_migs$season)
all_migs$tag.local.identifier <- as.factor(all_migs$tag.local.identifier)

# Compute start and end points per individual trajectory - easier to edit in Adobe
start_end_points <- all_migs %>%
  group_by(source, tag.local.identifier, season) %>%
  arrange(timestamps) %>%
  summarise(
    start_long = first(location.long),
    start_lat  = first(location.lat),
    end_long   = last(location.long),
    end_lat    = last(location.lat),
    .groups = "drop"
  )

# Plot Code ----
ggplot() +
  geom_sf(data = us_can_clean, fill = "grey", color = "white", size = 0.2) +
  
  # Migration paths
  geom_path(data = all_migs, aes(
    x = location.long,
    y = location.lat,
    color = season,
    group = interaction(source, tag.local.identifier)), 
    linewidth = 0.75) +
  
  # Start points (filled)
  geom_point(
    data = start_end_points,
    aes(x = start_long, y = start_lat, color = season),
    shape = 21, fill = "black", size = 2, stroke = 0.3
  ) +
  
  # End points (open)
  geom_point(
    data = start_end_points,
    aes(x = end_long, y = end_lat, color = season),
    shape = 21, fill = "white", size = 2.5, stroke = 0.6
  ) +
  
  scale_color_manual(values = c("spring" = "#31a354", "fall" = "darkorange")) +
  theme_bw() +
  xlab("") + 
  ylab("") + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(),
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.ticks = element_blank()
  ) +
  coord_sf(xlim = c(-95.5, -69), ylim = c(34, 57), expand = FALSE)