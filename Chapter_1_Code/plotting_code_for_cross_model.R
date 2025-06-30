######################################
# Conditional effects/Random Effects #
#  Plot Code For Cross Model         #
#####################################

library(ggplot2)
# Need model outputs from analysis scripts for this code

#########################
# Crossing Moodel Plots #
#########################

# 1.) Julian Date

# Generate conditional effects
jd <- conditional_effects(out.c5, effects = "julian.day", points = TRUE)

# Extract the first plot data (since conditional_effects returns a list of plots)
jd_data <- jd$julian.day

ggplot(jd_data, aes(x = julian.day, y = estimate__)) +
  geom_line(color = "black", linewidth = 1.3) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "gold", alpha = 0.3) +
  geom_point(data = full.dat, aes(x = julian.day, y = all.crosses),
             shape = 16, size = 2) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    limits = c(0, 1)
  ) +
  labs(x = "Julian Day", y = "Probability of Crossing") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

# 2.) Wind Speed

# Wind speed
ws_effects <- conditional_effects(out.c5, effects = "average.WSP", points = TRUE)

# Extract the first plot data (since conditional_effects returns a list of plots)
ws_data <- ws_effects$average.WSP

ggplot(ws_data, aes(x = average.WSP, y = estimate__)) +
  geom_line(color = "black", linewidth = 1.3) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "cyan", alpha = 0.3) +
  geom_point(data = full.dat, aes(x = average.WSP, y = all.crosses),
             shape = 16, size = 2) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    limits = c(0, 1)
  ) +
  labs(x = "Wind Speed m/s", y = "Probability of Crossing") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )


# 3.) Delta T plot

# Delta T (uplift over Water)
dt_effects <- conditional_effects(out.c5, effects = "average.delta.t", points = TRUE)

# Extract the first plot data (since conditional_effects returns a list of plots)
dt_data <- dt_effects$average.delta.t

ggplot(dt_data, aes(x = average.delta.t, y = estimate__)) +
  geom_line(color = "black", linewidth = 1.3) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "seagreen1", alpha = 0.3) +
  geom_point(data = full.dat, aes(x = average.delta.t, y = all.crosses),
             shape = 16, size = 2) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    limits = c(0, 1)
  ) +
  labs(x = "Uplift over water", y = "Probability of Crossing") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )


# 4.)  Random effects Plot for Bird ID

# Visualize the random effects
# Extract random effects (posterior means & CIs for bird.ID)
ranefs <- ranef(out.c5)$bird.ID[, , 1]  # Extract only intercepts

# Convert to a dataframe for plotting
ranef_df <- data.frame(
  bird.ID = rownames(ranefs),
  estimate = ranefs[, "Estimate"],
  lower = ranefs[, "Q2.5"],
  upper = ranefs[, "Q97.5"]
)

# Get dynamic limits
ymin <- min(-2, floor(min(ranef_df$lower, na.rm = TRUE)))
ymax <- max(3, ceiling(max(ranef_df$upper, na.rm = TRUE)))

# Ensure alphabetical order, A on top
ranef_df$bird.ID <- factor(ranef_df$bird.ID, levels = rev(sort(unique(ranef_df$bird.ID))))

# Plot
ggplot(ranef_df, aes(x = bird.ID, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.5) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-2, 3, 1), limits = c(ymin, ymax)) +
  labs(x = "Bird ID", y = "Estimated Random Effect", title = "Variation in Bird ID Intercepts Crossing") +
  theme_minimal()


############################
# Code that produces Fig 1 #
# in the Additional File for #
# Alioto et al. 2025 - Movement Ecology #
#########################################

library(tidyverse)

# You need the full.dat data frame that was used in the crossing analysis in 
# order to use the code below.

# Step 1: Make 'crossing' factor from binary indicator
full.dat <- full.dat %>%
  mutate(crossing = case_when(
    all.crosses == 1 ~ "Crossed",
    no.cross == 1 ~ "No Cross",
    TRUE ~ NA_character_
  ))

# Step 2: Pivot atmospheric variables into long format
long.dat <- full.dat %>%
  pivot_longer(cols = c(average.WSP, average.delta.t, V),  # Variables from analysis
               names_to = "variable",
               values_to = "value") %>%
  filter(!is.na(crossing))  # In case some rows don't match either category

# Step 3: Plot
ggplot(long.dat, aes(x = crossing, y = value, fill = crossing)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Crossing Behavior", y = "Value", title = "Atmospheric Conditions on Crossing vs Non-Crossing Days") +
  scale_fill_manual(values = c("No Cross" = "#D55E00", "Crossed" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

    
