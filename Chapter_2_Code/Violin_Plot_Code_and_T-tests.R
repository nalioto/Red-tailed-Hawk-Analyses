##----------------------------##
#  Spring vs.Fall Violin plots #
#   Figure 2 in Manuscript    #
#   Nick Alioto 02/20/2023    #
## --------------------------##

## Libraries ----
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(RColorBrewer)
library(ggforce)
library(patchwork) # Combine Plots

## Load in the data ----
fall <- read.csv("Chapter.1/Data/full_migrations/fall_migration_individual_metrics_25.csv", header = T)
spr <- read.csv("Chapter.1/Data/full_migrations/spring_migration_individual_metrics_25.csv", header = T)

rm()

## Combine the data frames ----

# Add a season column for each df ----
fall <- fall %>% mutate (season = "fall")
spr <- spr %>% mutate(season = "spring")


## Merge Spring and fall data to make plots (new DF called Metrics) ----
metrics <- rbind(fall, spr)

# Create a Color Palette ----
nicks.colors <- c("#31a354","darkorange") # A nice spring green combo with  dark orange for fall

#########################################
# Raw Values with just Mean as a boxplot #
###########################################

# Optional: pick only the metrics you care about
metrics_subset <- metrics %>%
  dplyr::select(bird.ID, season, julian.day, end.day, duration.days, total.dist, speed, #dplyr:: in case of a conflicting argument
                straightness) %>%  
  tidyr::pivot_longer(cols = -c(bird.ID, season),
                      names_to = "Metric",
                      values_to = "Value") %>%
  # Convert Metric to factor with desired order
  mutate(Metric = factor(Metric,
                         levels = c("julian.day",
                                    "end.day",
                                    "duration.days",
                                    "total.dist",
                                    "speed",
                                    "straightness")))


#########################################################################
# Violin plot side by side for Start and End Dates for fall and spring #
#########################################################################

# Keep only start and end date metrics
metrics_dates <- metrics_subset %>%
  filter(Metric %in% c("julian.day", "end.day"))

# Make month breaks on the 1st of each month (March–December)
month_breaks <- as.numeric(format(
  seq.Date(from = as.Date("2020-01-01"), 
           to   = as.Date("2020-12-31"), 
           by   = "1 month"), 
  "%j")
)

month_labels <- format(
  seq.Date(from = as.Date("2020-01-01"), 
           to   = as.Date("2020-12-31"), 
           by   = "1 month"), 
  "%b"
)

# Data frame with migration start and end dates side by side
metrics_dates2 <- metrics_subset %>%
  filter(Metric %in% c("julian.day", "end.day")) %>%
  mutate(
    # order panels Spring | Fall
    season = factor(season, levels = c("spring", "fall")),
    # nicer x-axis labels
    Metric = factor(Metric,
                    levels = c("julian.day", "end.day"),
                    labels = c("Start", "End"))
  )

#####################################################################
# Migration Start and End Date plot by 2-week increments on Y-axis #
######################################################################

# Two-week (14 day) sequence of actual dates
axis_dates <- seq.Date(
  from = as.Date("2020-01-01"),
  to   = as.Date("2020-12-31"),
  by   = "14 days" # Can adjust this as needed
)

# Convert to Julian positions for scale_y_continuous
week2_breaks <- as.numeric(format(axis_dates, "%j"))

# Pretty date labels shown on the plot
week2_labels <- format(axis_dates, "%b %d")


ggplot(metrics_dates2,
       aes(x = Metric, y = Value, fill = season)) +
  
  geom_violin(trim = TRUE, alpha = 0.7) +
  
  geom_sina(
    size  = 1.5,
    shape = 21,
    fill  = "darkgrey",
    color = "black"
  ) +
  
  stat_summary(
    fun   = median,
    geom  = "point",
    shape = 21,
    size  = 4,
    fill  = "white"
  ) +
  
  scale_fill_manual(values = nicks.colors) +
  
  scale_y_continuous(
    name   = "Date (every 2 weeks)",
    breaks = week2_breaks,
    labels = week2_labels,
    limits = c(1, 366),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  labs(
    x     = NULL,
    title = "Start and End Dates by season"
  ) +
  
  facet_wrap(~ season, ncol = 2, scales = "fixed") +
  
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 13),
    panel.grid.major.y = element_line(linewidth = 0.3),
    panel.grid.minor.y = element_blank()
  )

########################################
# Combined Facet Plot for all Metrics  #
# (Summary Files with median and SD)     #
########################################

# Optional: pick only the metrics you care about
metrics_subset2 <- metrics %>%
  dplyr::select(bird.ID, season, duration.days, total.dist, speed, #dplyr:: in case of a conflicting argument
                straightness) %>%  
  tidyr::pivot_longer(cols = -c(bird.ID, season),
                      names_to = "Metric",
                      values_to = "Value") %>%
  # Convert Metric to factor with desired order
  mutate(Metric = factor(Metric,
                         levels = c("duration.days",
                                    "total.dist",
                                    "speed",
                                    "straightness")))

###############
# Violin Plot #
###############
ggplot(metrics_subset2, aes(x = season, y = Value, fill = season)) +
  geom_violin(trim = TRUE, alpha = 0.7) +               # distribution
  geom_sina(
    size = 1.5,
    shape = 21,
    fill = "darkgrey",
    color = "black",
  ) +
  stat_summary(fun = median, 
               geom = "point", 
               shape = 21, 
               size = 4, 
               fill = "white") +
  
  facet_wrap(~ Metric, scales = "free_y") +
  scale_fill_manual(values = nicks.colors) +
  labs(x = "Season", y = "Value", title = "Migration Metrics by Season") +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 13))

##################################
# T-tests for Migration Metrics  #
##################################

# Non-Normality Test
shapiro.test(log(metrics$total.dist[metrics$season == "spring"])) # p =  6.898e-05
shapiro.test(log(metrics$total.dist[metrics$season == "fall"])) # p = 1.236e-07

shapiro.test(log(metrics$duration.days[metrics$season == "spring"])) # p =  0.1129
shapiro.test(log(metrics$duration.days[metrics$season == "fall"])) # p =  0.4395

shapiro.test(log(metrics$speed[metrics$season == "spring"])) # p = 0.8901
shapiro.test(log(metrics$speed[metrics$season == "fall"])) # p =  0.495

shapiro.test(metrics$straightness[metrics$season == "spring"]) # p = 0.1597
shapiro.test(metrics$straightness[metrics$season == "fall"]) # p = 6467

# Convert season to factor
metrics$season <- as.factor(metrics$season)
str(metrics)

# T-tests
# After using a shapiro test to test for normality and log transforming to try and
# normalize the data all metrics except for total.dist were normal so we will run
# t.tests on all metrics and a wilcox test on total distance

t.test(straightness ~ season, data = metrics)
t.test(log(duration.days) ~ season, data = metrics)
t.test(log(speed) ~ season, data = metrics)
wilcox.test(total.dist ~ season, data = metrics, exact = FALSE)


