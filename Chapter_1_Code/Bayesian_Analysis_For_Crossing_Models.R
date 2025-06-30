####################################
# Crossing Analysis                #
# Nick Alioto 04/27/2025           #
# For Movement Ecology Manuscript  #
####################################

# Library ----
library(tidyverse)
library(brms) # Bayesian Analysis using STAN
library(rstan)
library(tidybayes)
library(RNCEP)
library(MASS)

# 1. Data Set-UP ----
full.dat <-read.csv("Chapter.2/Analyses/chp2_analysis.2024.updated.behaviors(1).csv", header = TRUE)

str(full.dat)
rm()

# Need to splice out April 12th 2023 - the buoy was down and there are NA's present, 
# this should subtract those 7 rows

full.dat <- full.dat[-c(173:180) , ] # Corresponds to the data associated with April 12th


# Convert Wind Direction and Speed into vertical and horizontal components
U.V.winds <- NCEP.uv.revert(spd = full.dat$average.WSP, dir = full.dat$average.WDIR,
                            radians = FALSE)

# U > 0 blowing toward the East
# V > 0 wind is blowing from S to N
# V < 0 wind is blowing from N to S

# Merge data frames

full.dat <- cbind(full.dat, U.V.winds)

# Need to convert date to a Julian date to account for when a cross was made
full.dat$julian.day <- yday(full.dat$date)

# Reordering columns
full.dat <- full.dat %>%
  dplyr::select(date, average.WDIR, average.WSP, average.D.GST, average.PRES, 
                average.ATMP, average.WTMP, average.delta.t, U, V, 
                bird.ID, behavior, sex, direct, indirect.so, cross.after.stop,
                indirect.flyover, fail, all.crosses, no.cross, julian.day)

# 2. multicollinearity Check
subset <- full.dat[ ,2:10]

# Wind speed and gust had a correlation of 0.90, dropped gust, wind speed was deemed more important given our question in the analysis
cor.matrix <- cor(subset)


# convert Bird ID to factor for Random effect structure

full.dat$bird.ID <- as.factor(full.dat$bird.ID)


str(full.dat)

##############################################################
# Removing Non-breeders that were 2 at the time of crossing ##
#############################################################

# Need to remove 2 year old birds to account for different behaviors among breeders and non-breeders
full.dat$date <- as.Date(full.dat$date) 

full.dat <- full.dat %>%
  filter(
    !(bird.ID == "Heathcliif (2)" & lubridate::year(date) == 2023),
    !(bird.ID == "Mackinaw" & lubridate::year(date) == 2022),
    !(bird.ID == "MH1" & lubridate::year(date) == 2023),
  )

# Manually remove Bucatini 2022 data
full.dat <- full.dat[-c(126:145), ]

# Make sure those birds and their respective years were removed
tibble <- full.dat%>%
  group_by(bird.ID) %>%
  reframe(years_present = unique(year(date)))

str(full.dat$date)

# Check the numbers and types of crosses in the data set
table(full.dat$all.crosses) # 51 crosses 
table(full.dat$direct) # 38 direct
table(full.dat$indirect.so) # 10 with stopover
table(full.dat$indirect.flyover) # 3 Flew over islands directly

# Birds used in the analysis Check
n_distinct(full.dat$bird.ID) # n = 33

##################
# Bayesian Flare #
##################

################################################################################
#                             # Crossing Models #                              #
################################################################################
# All predictors are scaled in all models

############
# Model 1 #
###########

# Cross Model with U and V wind as separate components + uplift over water
out.c1 <- brm(formula = all.crosses ~ scale(U) + scale(V) + scale(average.delta.t) + (1|bird.ID), 
              data = full.dat, 
              family = bernoulli(link = 'logit'),
              warmup = 1000, 
              iter = 5000, 
              chains = 3, 
              init = 'random', 
              cores = 1, 
              seed = 123)

out.c1 <- add_criterion(out.c1, "loo") # Need to add loo criterion to model
pp_check(out.c1, ndraws = 100)

summary(out.c1)
pairs(out.c1) # make sure posteriors were properly sampled
plot(out.c1)

# Conditional Effects Plots for coefficients 

# Delta T (uplift over Water)
plot(conditional_effects(out.c1, effects = "average.delta.t"), points = TRUE)

# Vertical Wind
plot(conditional_effects(out.c1, effects = "V"), points = TRUE)

# Horizontal Wind
plot(conditional_effects(out.c1, effects = "U"), points = TRUE)


# Plotting effects of the Coefficients

# Extract posterior summaries and convert to a dataframe
posterior_summary <- as.data.frame(fixef(out.c1))  # Replace 'your_model' with the actual model name
posterior_summary$Parameter <- rownames(posterior_summary)

# Rename columns for clarity
colnames(posterior_summary) <- c("Estimate", "Est.Error", "l-95 CI", "u-95 CI", "Parameter")

# Remove the intercept
posterior_summary <- posterior_summary %>% 
  filter(Parameter != "Intercept")

# Plot with ggplot2
ggplot(posterior_summary, aes(x = Estimate, y = reorder(Parameter, Estimate))) +
  geom_point(color = "blue") +
  geom_errorbarh(aes(xmin = `l-95 CI`, xmax = `u-95 CI`), height = 0.2, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Coefficient Estimate", y = "Predictor", title = "Bayesian Cross Model Coefficients") +
  theme_minimal()



############
# Model 2 #
###########

# Cross Model with Wind Speed and here we drop the U and V wind
out.c2 <- brm(formula = all.crosses ~  scale(average.WSP) + scale(average.delta.t)  + (1|bird.ID), 
              data = full.dat, 
              family = bernoulli(link = 'logit'),
              warmup = 1000, 
              iter = 5000, 
              chains = 3, 
              init = 'random', 
              cores = 1, 
              seed = 123)
out.c2 <- add_criterion(out.c2, "loo") # Need to add loo criterion to model
pp_check(out.c2, ndraws = 100)

summary(out.c2)
pairs(out.c2)

# Conditional ffects Plots for coefficients 

# Wind speed
plot(conditional_effects(out.c2, effects = "average.WSP"), points = TRUE)

# Delta T (uplift over Water)
plot(conditional_effects(out.c2, effects = "average.delta.t"), points = TRUE)


############
# Model 3 #
###########

# Same as model c1 but now we account for the day of crossing (Julian Date)
out.c3 <- brm(formula = all.crosses ~ scale(U) + scale(V) + scale(average.delta.t) + scale(julian.day) + (1|bird.ID), 
              data = full.dat, 
              family = bernoulli(link = 'logit'),
              warmup = 1000, 
              iter = 5000, 
              chains = 3, 
              init = 'random', 
              cores = 1, 
              seed = 123)

out.c3 <- add_criterion(out.c3, "loo") # Need to add loo criterion to model
pp_check(out.c3, ndraws = 100)

summary(out.c3)
pairs(out.c3) # make sure posteriors were properly sampled
plot(out.c3)

# Conditional Effects Plots for coefficients 

# Julian Day
plot(conditional_effects(out.c3, effects = "julian.day"), points = TRUE)

# Delta T (uplift over Water)
plot(conditional_effects(out.c3, effects = "average.delta.t"), points = TRUE)

# Vertical Wind
plot(conditional_effects(out.c3, effects = "V"), points = TRUE)

# Horizontal Wind
plot(conditional_effects(out.c3, effects = "U"), points = TRUE)


############
# Model 4 #
###########
# Same as model c2 but with Julian date added here as well
out.c4 <- brm(formula = all.crosses ~  scale(average.WSP) + scale(average.delta.t) + scale(julian.day)  + (1|bird.ID), 
              data = full.dat, 
              family = bernoulli(link = 'logit'),
              warmup = 1000, 
              iter = 5000, 
              chains = 3, 
              init = 'random', 
              cores = 1, 
              seed = 123)
out.c4 <- add_criterion(out.c4, "loo") # Need to add loo criterion to model
pp_check(out.c4, ndraws = 100)

summary(out.c4)
pairs(out.c4)

# Plotting effects of the Coefficients

# Extract posterior summaries and convert to a dataframe
posterior_summary4 <- as.data.frame(fixef(out.c4))  # Replace 'your_model' with the actual model name
posterior_summary4$Parameter <- rownames(posterior_summary4)

# Rename columns for clarity
colnames(posterior_summary4) <- c("Estimate", "Est.Error", "l-95 CI", "u-95 CI", "Parameter")

# Remove the intercept
posterior_summary4 <- posterior_summary4 %>% 
  filter(Parameter != "Intercept")

# Plot with ggplot2
ggplot(posterior_summary4, aes(x = Estimate, y = reorder(Parameter, Estimate))) +
  geom_point(color = "blue") +
  geom_errorbarh(aes(xmin = `l-95 CI`, xmax = `u-95 CI`), height = 0.2, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Coefficient Estimate", y = "Predictor", title = "Bayesian Cross Model Coefficients") +
  theme_minimal()

# Conditional Effects Plots for coefficients 

# Julian Day
plot(conditional_effects(out.c4, effects = "julian.day"), points = TRUE)

# Wind speed
plot(conditional_effects(out.c4, effects = "average.WSP"), points = TRUE)

# Delta T (uplift over Water)
plot(conditional_effects(out.c4, effects = "average.delta.t"), points = TRUE)



############
# Model 5 #
###########
out.c5 <- brm(formula = all.crosses ~  scale(average.WSP) + scale(V) + scale(average.delta.t) + scale(julian.day)  + (1|bird.ID), 
              data = full.dat, 
              family = bernoulli(link = 'logit'),
              warmup = 1000, 
              iter = 5000, 
              chains = 3, 
              init = 'random', 
              cores = 1, 
              seed = 123)
out.c5 <- add_criterion(out.c5, "loo") # Need to add loo criterion to model
pp_check(out.c5, ndraws = 100)

summary(out.c5)
pairs(out.c5)

# Conditional Effects Plots for coefficients 

# Julian Day
plot(conditional_effects(out.c5, effects = "julian.day"), points = TRUE)

# Wind speed
plot(conditional_effects(out.c5, effects = "average.WSP"), points = TRUE)

# Delta T (uplift over Water)
plot(conditional_effects(out.c5, effects = "average.delta.t"), points = TRUE)

# Wind Support
plot(conditional_effects(out.c5, effects = "V"), points = TRUE)

#################################
# Model Comparison & Validation # 
#################################

loo_compare(out.c1, out.c2, out.c3, out.c4, out.c5, criterion = c("loo"))

#           elpd_diff    se_diff
# out.c5         0.0       0.0   
# out.c4        -1.4       2.4   
# out.c2        -1.7       3.9   
# out.c1        -6.5       5.2   
# out.c3        -7.4       4.7   


#########################################################
##### Summary results about Delta T in the data set #####
##### For results section in the manuscript         #####
#########################################################

# Step 1: Extract the year and count unique days with positive uplift per year
positive_uplift_days_per_year <- full.dat %>%
  mutate(
    date = as.Date(date),
    year = format(date, "%Y")
  ) %>%
  filter(average.delta.t > 0) %>%           # Keep only rows with positive uplift
  group_by(year) %>%
  summarise(average.delta.t = n_distinct(date))  # Count unique dates

# Step 2: Join with the total unique days per year
summary_table <- full.dat %>%
  mutate(
    date = as.Date(date),
    year = format(date, "%Y")
  ) %>%
  group_by(year) %>%
  summarise(unique_days = n_distinct(date)) %>%
  left_join(positive_uplift_days_per_year, by = "year") %>%
  mutate(average.delta.t = replace_na(average.delta.t, 0))  # Fill NA with 0 if no positive uplift

print(summary_table)


