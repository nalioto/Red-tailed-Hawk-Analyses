setwd("C:/Users/jtrudeau/OneDrive - Michigan State University/Documents/MSU Deer Project/Dissertation/Deer Location Data/Filtered Location Data/Temporal Filter (!= first two weeks)")
# All the files need to be in the WD
## Getting all the individual deer files into a single master file to be managed later.

library(dplyr)
library(readr)

df <- list.files(full.names = TRUE) %>% #all
  lapply(read_csv) %>%
  bind_rows

View(df)

# List of Collar IDs
Deer <- unique(df$`Deer_ID`)
View(Deer)

#convert the df into a csv file and save to WD
write.csv(df, file = "All_Deer_locations_2wk_Filter.csv")