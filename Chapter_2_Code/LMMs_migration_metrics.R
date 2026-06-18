##----------------------------##
#     Spring vs.Fall LMMs     #
#     & Diagnostic checks     #
#   Nick Alioto 06/05/2026    #
## --------------------------##

## Libraries ----
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(RColorBrewer)
library(ggforce)
library(patchwork) # Combine Plots
library(ggplot2)
library(lme4)
library(lmerTest) # adds p-values to lmer output, otherwise you won't get them
library(DHARMa)  # package for LMM diagnostics 
library(stringr)

## Load in the data ----
fall <- read.csv("Chapter.1/Data/full_migrations/fall_migration_individual_metrics_25.csv", header = T)
spr <- read.csv("Chapter.1/Data/full_migrations/spring_migration_individual_metrics_25.csv", header = T)

rm()

## Combine the data frames/ clean the data ----

# Add a season column for each df ----
fall <- fall %>% mutate (season = "fall")
spr <- spr %>% mutate(season = "spring")


## Merge Spring and fall data to make plots (new DF called Metrics) ----
metrics <- rbind(fall, spr)

# Need to drop the number designation so individuals can group properly
metrics <- metrics %>%
  mutate(bird.name = str_remove(bird.ID, "[0-9]+$"))

# Verify it worked
unique(metrics$bird.name)
# Should now give you ~30 unique names


#############################
#### Linear Mixed models ####
#############################

# Migration speed
speed.mod <- lmer(speed ~ season + (1|bird.name), data = metrics)
# Migration duration
duration.mod <- lmer(duration.days ~ season + (1|bird.name), data = metrics)
# Migration Distance
tdist.mod <- lmer(total.dist ~ season + (1|bird.name), data = metrics)
# Migration tortuosity 
tort.mod <- lmer(straightness ~ season + (1|bird.name), data = metrics)

# Model Outputs
summary(speed.mod)
summary(duration.mod)
summary(tdist.mod)
summary(tort.mod)

# Model Diagnostics 

library(DHARMa)  # best package for LMM diagnostics

# Simulate residuals
sim_resid <- simulateResiduals(speed.mod, plot = T)

plot(speed.mod)

# Residula plots
qqnorm(resid(speed.mod))
qqline(resid(speed.mod))

# Check for influential individuals
library(influence.ME)

infl <- influence(speed.mod, group = "bird.name")
plot(infl, which = "cook")  # Cook's distance per individual


###########################
# Estimate and 95% CI plot #
############################

# Extract fixed effects estimates and CIs
speed_estimates <- as.data.frame(confint(speed.mod, parm = "beta_"))

# Build a clean dataframe for plotting
speed_plot_df <- data.frame(
  season = c("Fall", "Spring"),
  estimate = c(
    fixef(speed.mod)["(Intercept)"],
    fixef(speed.mod)["(Intercept)"] + fixef(speed.mod)["seasonspring"]
  ),
  lower = c(
    speed_estimates["(Intercept)", "2.5 %"],
    speed_estimates["(Intercept)", "2.5 %"] + speed_estimates["seasonspring", "2.5 %"]
  ),
  upper = c(
    speed_estimates["(Intercept)", "97.5 %"],
    speed_estimates["(Intercept)", "97.5 %"] + speed_estimates["seasonspring", "97.5 %"]
  )
)

# Plot
ggplot(speed_plot_df, aes(x = season, y = estimate, color = season)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, linewidth = 1) +
  scale_color_manual(values = c("Fall" = "darkorange", "Spring" = "#31a354")) +
  labs(
    x = "Season",
    y = "Estimated Daily Travel Speed (km/day)",
    title = "LMM Estimated Migration Speed by Season"
  ) +
  theme_classic() +
  theme(legend.position = "none")


