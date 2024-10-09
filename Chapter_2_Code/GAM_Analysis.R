#####################
#     GAM Models    #
#   Nick Alioto     # 
#  October 8th 2024 #
#####################

# Library ----
library(mgcv)
library(gratia) #Visualizing GAMS in ggplot
library(tidyverse)
library(tidymv)
library(pracma)
library(gamm4)
library(mgcViz)
library(performance)
library(MuMIn)
library(itsadug)
library(plotfunctions) # For the legends 

# Data Set-UP ----
full.dat <-read.csv("Chapter.2/Analyses/chp2_analysis.2024.updated.behaviors.csv", header = TRUE)

str(full.dat)
rm()

# Need to splice out April 12th 2023 - the buoy was down and there are NA's present, 
# this should subtract those 7 rows

full.dat <- full.dat[-c(173:180) , ] # Corresponds to the data associated with April 12th

# multicollinearity Check
subset <- full.dat[ ,2:8]

# Wind speed and gust had a correlation of 0.90, dropped gust, wind speed was deemed more important given our question in the analysis
cor.matrix <- cor(subset)

# Let's add the average angle that hawks are selecting to cross at

full.dat$pref.angle <- (newcol = 10)

# Calculate wind direction relative to average crossing angle
# Examples (we just want to know the degree difference between the
# "optimal" wind direction and observed wind direction (both in degrees
# with zero assumed as north)

get.angle <- function(x, y){
  180-ifelse(y>(x+180), (x+180)-(y-(x+180)+x), y-x)
}


full.dat$diff.obt <- get.angle(full.dat$pref.angle, full.dat$average.WDIR)  

# Rearrange the columns to make sub-setting easier

full.dat <- full.dat %>% select("date","average.WDIR","average.WSP", "average.D.GST", "average.PRES",
                                "average.ATMP", "average.WTMP", "average.delta.t","diff.obt", "bird.ID", 
                                "behavior", "sex", "optimal", "stop.over", "non.opt", "fail")


# Need to add in a Random effect for Bird ID since we have same birds crossing over 
# multiple years

full.dat$bird.ID <- as.factor(full.dat$bird.ID)


# Reproducibility 
set.seed(2)

# Linear vs smooth covariate check ----

# Optimal #

# Let's check which terms are not linear
linear.model <- gam(optimal ~ average.WDIR, data = full.dat)
smooth.model <- gam(optimal ~ s(average.WDIR), data = full.dat)
AIC(linear.model, smooth.model)
# Wind Direction = AIC of the smooth GAM is lower 
plot(smooth.model, all.terms = TRUE)


linear.model.1 <- gam(optimal ~ average.WSP, data = full.dat)
smooth.model.1 <- gam(optimal ~ s(average.WSP), data = full.dat)
AIC(linear.model.1, smooth.model.1)
# Wind Speed = AIC is the same
plot(smooth.model.1, all.terms = TRUE)


linear.model.2 <- gam(optimal ~ average.delta.t , data = full.dat)
smooth.model.2 <- gam(optimal ~ s(average.delta.t), data = full.dat)
AIC(linear.model.2, smooth.model.2)
# Delta T  AIC is lower 
plot(smooth.model.2, all.terms = TRUE)


# Suboptimal #

# Let's check which terms are not linear
linear.model.3 <- gam(stop.over ~ average.WDIR, data = full.dat)
smooth.model.3 <- gam(stop.over~ s(average.WDIR), data = full.dat)
AIC(linear.model.3, smooth.model.3)
# Wind Direction = same
plot(smooth.model.3, all.terms = TRUE)


linear.model.4 <- gam(stop.over ~ average.WSP, data = full.dat)
smooth.model.4 <- gam(stop.over ~ s(average.WSP), data = full.dat)
AIC(linear.model.4, smooth.model.4)
# Wind Speed = AIC is almost the same linear = 276.7944, smooth = 276.7943
plot(smooth.model.4, all.terms = TRUE)


linear.model.5 <- gam(stop.over ~ average.delta.t , data = full.dat)
smooth.model.5 <- gam(stop.over ~ s(average.delta.t), data = full.dat)
AIC(linear.model.5, smooth.model.5)
# Delta T is lower
plot(smooth.model.5, all.terms = TRUE)





#-------------------------------------------------------------------------------

# Analysis Optimal and Suboptimal Cross ----

# Optimal Models/Plots

# Smoothing all terms - wind support term and delta T
out.1 <- gam(optimal ~ s(average.WSP , diff.obt, k = 4) + s(average.delta.t) + s(bird.ID, bs ='re'),
             family= binomial, data = full.dat)

summary(out.1)
plot.gam(out.1)
# Basis dimension check
k.check(out.1)

#  Model diagnostics / Accuracy of Model Predictions using K-fold Cross Validation 
performance_accuracy(model = out.1, method = "cv", k = 5 ) # K-cross fold validation

# Running Null Models vs our Choice model for "Optimal Cross"

null.out <- gam(optimal ~ s(bird.ID, bs ='re'),
                family= binomial, data = full.dat)

null.out1 <- gam(optimal ~ s(average.WSP , diff.obt, k = 4) + s(bird.ID, bs ='re'),
                 family= binomial, data = full.dat)

null.out2 <- gam(optimal ~ s(average.delta.t) + s(bird.ID, bs ='re'),
                 family= binomial, data = full.dat)

# Ranking Optimal Models using AIC

# AIC check of all 4 models

AIC(out.1,null.out,null.out1,null.out2)


# Optimal Model Plots #

# For manuscript interaction of wind direction with delta T
vis.gam(out.1, view = c("diff.obt", "average.delta.t"),
        type = 'response', plot.type = 'persp', theta = 48,n.grid = 75,
        zlim = c(0,0.30),
        color = "topo",
        ticktype = "detailed",
        xlab = "Wind",
        ylab = "Uplift",
        zlab = " ")

# For manuscript interaction of wind speed with Delta T
vis.gam(out.1, view = c("average.WSP", "average.delta.t"),
        type = 'response', plot.type = 'persp', theta = 48,n.grid = 75,
        zlim = c(0,0.30),
        color = "topo",
        ticktype = "detailed",
        xlab = "Wind Speed m/s",
        ylab = "Uplift",
        zlab = " ")

# Optimal Cross - Contour plots
# Interaction of wind direction and wind speed

# Set up the layout to include space for the legend
layout(matrix(c(1, 2), ncol = 2), widths = c(4, 1))

# Adjust the margin for the main contour plot
par(mar = c(5, 5, 5, 1))

# Plot the GAMM for specific wind term interactions
vis.gam(out.1, view = c("diff.obt", "average.WSP"),
        type = 'response', plot.type = 'contour', n.grid = 75,
        color = "topo",
        xlab = "Wind Direction",
        ylab = "Wind Speed",
        contour.col = "transparent")

# Move to the next plot (for the legend)
par(mar = c(4, 0.1, 4, 2), xpd = NA)

# Plot an empty plot to set up the space for the legend
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

# Add the gradient legend
gradientLegend(valRange = c(0, 0.3),
               color = "topo",
               length = 0.6,
               pos=c(0.5,0.1,.7,0.8), coords=TRUE,
               side = 4,
               n.seg = 1)


################ SUBOPTIMAL ################

# Suboptimal Models/Plots 

# Smoothing all terms - wind support term and delta T
out.subopt1 <- gam(stop.over ~ s(average.WSP , diff.obt, k = 4) + s(average.delta.t) + s(bird.ID, bs ='re'),
                   family= binomial, data = full.dat)

summary(out.subopt1)
plot(out.subopt1, pch = 1)

# Basis dimension check
k.check(out.subopt1)

# Model diagnostics / Accuracy of Model Predictions
performance_accuracy(model = out.subopt1, method = "cv", k = 5 ) # K-cross fold validation

# Running Null models for suboptimal
# Null model

null.outI <- gam(stop.over ~ s(bird.ID, bs ='re'),
                 family= binomial, data = full.dat)

null.outI1 <- gam(stop.over ~ s(average.WSP , diff.obt, k = 4) + s(bird.ID, bs ='re'),
                  family= binomial, data = full.dat)

null.outI2 <- gam(stop.over ~ s(average.delta.t) + s(bird.ID, bs ='re'),
                  family= binomial, data = full.dat)

# Rank the models with AIC

AIC(out.subopt1, null.outI, null.outI1, null.outI2)

# Suboptimal Model Plots #

# For manuscript interaction of wind direction with uplift
vis.gam(out.subopt1, view = c("average.delta.t","diff.obt"),
        type = 'response', plot.type = 'persp', theta = -48,n.grid = 75, 
        zlim = c(0,0.30),
        color = "topo",
        ticktype = "detailed",
        xlab = "Uplift",
        ylab = "Wind",
        zlab = " ")

# For manuscript interaction of wind speed with uplift
vis.gam(out.subopt1, view = c("average.WSP", "average.delta.t"),
        type = 'response', plot.type = 'persp', theta = 45,n.grid = 75,
        zlim = c(0,0.30),
        color = "topo",
        ticktype = "detailed",
        xlab = "Wind Speed",
        ylab = "Uplift",
        zlab = " ")

# Contour Plot for Wind direction and speed (sub-optimal)

# Set up the layout to include space for the legend
layout(matrix(c(1, 2), ncol = 2), widths = c(4, 1))

# Adjust the margin for the main contour plot
par(mar = c(5, 5, 5, 1))

# Plot the GAM
vis.gam(out.subopt1, view = c("diff.obt", "average.WSP"),
        type = 'response', plot.type = 'contour', n.grid = 75,
        color = "topo",
        xlab = "Wind Direction",
        ylab = "Wind Speed",
        contour.col = "transparent")

# Move to the next plot (for the legend)
par(mar = c(4, 0.1, 4, 2), xpd = NA)  # (5,1,5,2) - original, (4,1,4,2)

# Plot an empty plot to set up the space for the legend
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

# Add the gradient legend
gradientLegend(valRange = c(0, 0.3),
               color = "topo",
               length = 0.6,
               pos=c(0.5,0.1,.7,0.8), coords=TRUE,
               side = 4,
               n.seg = 1)



# Code for some summary results about uplift conditions over the Straits during study 2021-2023 ----

# Check Behavior totals (optimal and Island stop over)
full.dat %>% count() #add column name

# Step 1: Extract the year and count unique days per year
unique_days_per_year <- full.dat %>%
  mutate(year = format(as.Date(date), "%Y")) %>%  # Extract the year
  group_by(year) %>%
  summarise(unique_days = n_distinct(date))

print(unique_days_per_year)

# Step 2: Count unique days with positive delta T and wind speed < 5 m/s
days_with_conditions <- full.dat %>%
  mutate(year = format(as.Date(date), "%Y")) %>%  # Extract the year
  group_by(year) %>%
  filter(average.delta.t > 0, average.WSP < 5) %>%  # Apply both conditions
  summarise(days_with_conditions = n_distinct(date)) # Count unique dates

print(days_with_conditions)

# Step 3: Count days with "Optimal cross" behavior on days that met conditions
optimal_cross_days <- full.dat %>%
  mutate(year = format(as.Date(date), "%Y")) %>%
  group_by(year) %>%
  filter(average.delta.t > 0, average.WSP < 5, optimal == 1) %>%  # Add Optimal.cross == 1 condition
  summarise(optimal_cross_days = n_distinct(date)) # Count unique dates

print(optimal_cross_days)

# Step 4: Combine the results for easier comparison
combined_results <- unique_days_per_year %>%
  left_join(days_with_conditions, by = "year") %>%
  left_join(optimal_cross_days, by = "year") %>%
  mutate(
    positive_delta_t_days = coalesce(days_with_conditions, 0),
    optimal_cross_days = coalesce(optimal_cross_days, 0)
  ) %>%
  select(year, unique_days, positive_delta_t_days, optimal_cross_days)

print(combined_results)

