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
jd <- conditional_effects(out.c4, effects = "julian.day", points = TRUE)

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
ws_effects <- conditional_effects(out.c4, effects = "average.WSP", points = TRUE)

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
dt_effects <- conditional_effects(out.c4, effects = "average.delta.t", points = TRUE)

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
ranefs <- ranef(out.c4)$bird.ID[, , 1]  # Extract only intercepts

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


######################################################################
# Plot for Additional File on the effect of wind support on crossing #
#####################################################################

# 5.) Wind support V wind

# Wind speed
v_effects <- conditional_effects(out.c1, effects = "V", points = TRUE)

# Extract the first plot data (since conditional_effects returns a list of plots)
V_data <- v_effects$V

ggplot(V_data, aes(x = V, y = estimate__)) +
  geom_line(color = "black", linewidth = 1.3) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "darkgreen", alpha = 0.3) +
  geom_point(data = full.dat, aes(x = V, y = all.crosses),
             shape = 16, size = 2) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    limits = c(0, 1)
  ) +
  labs(x = "Wind Support", y = "Probability of Crossing") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )