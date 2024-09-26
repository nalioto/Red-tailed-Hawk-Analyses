
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(dplyr)

## Code to creat a boxplot with Banding Data
## RTHA_2 is the data frame.
## Code at end allows you to order the x-axis labels
ggplot(RTHA_2, aes(Age.Code, Mass, fill= Age)) +
  geom_boxplot() +
  scale_fill_viridis( discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.2, alpha = 0.9) +
  theme (
    legend.position = "none",
    plot.title = element_text(size = 13)) +
  ggtitle("Age vs. Mass of Red-tailed Hawks") +
  xlab("") +
  scale_x_discrete(labels = c("Second Year","Third Year", "After 3rd Year", "After 4th Year", "After 5th Year"))
