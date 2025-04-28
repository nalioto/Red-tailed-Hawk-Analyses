######################################
# Conditional effects/Random Effects #
#  Plot Code For No Cross Model      #
#####################################

library(ggplot2)
# Need model outputs from analysis scripts for this

#########################
# No Crossing Moodel Plots #
#########################


# 1.) Wind Speed

# Wind speed
ws_effects2 <- conditional_effects(out.nc2, effects = "average.WSP", points = TRUE)

# Extract the first plot data (since conditional_effects returns a list of plots)
ws_data2 <- ws_effects2$average.WSP

ggplot(ws_data2, aes(x = average.WSP, y = estimate__)) +
  geom_line(color = "black", linewidth = 1.3) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "cyan", alpha = 0.3) +
  geom_point(data = full.dat, aes(x = average.WSP, y = no.cross),
             shape = 16, size = 2) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    limits = c(0, 1)
  ) +
  labs(x = "Wind Speed m/s", y = "Probability of Not Crossing") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )


# 2.) Delta T plot

# Delta T (uplift over Water)
dt_effects2 <- conditional_effects(out.nc2, effects = "average.delta.t", points = TRUE)

# Extract the first plot data (since conditional_effects returns a list of plots)
dt_data2 <- dt_effects2$average.delta.t

ggplot(dt_data2, aes(x = average.delta.t, y = estimate__)) +
  geom_line(color = "black", linewidth = 1.3) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "seagreen1", alpha = 0.3) +
  geom_point(data = full.dat, aes(x = average.delta.t, y = no.cross),
             shape = 16, size = 2) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    limits = c(0, 1)
  ) +
  labs(x = "Uplift over water", y = "Probability of Not Crossing") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )


# 3.)  Random effects Plot

# Visualize the random effects
# Extract random effects (posterior means & CIs for bird.ID)
ranefs.nc <- ranef(out.nc2)$bird.ID[, , 1]  # Extract only intercepts

# Convert to a dataframe for plotting
ranef_nc_df <- data.frame(
  bird.ID = rownames(ranefs.nc),
  estimate = ranefs.nc[, "Estimate"],
  lower = ranefs.nc[, "Q2.5"],
  upper = ranefs.nc[, "Q97.5"]
)

# Get dynamic limits
ymin <- min(-2, floor(min(ranef_nc_df$lower, na.rm = TRUE)))
ymax <- max(3, ceiling(max(ranef_nc_df$upper, na.rm = TRUE)))

# Ensure alphabetical order, A on top
ranef_nc_df$bird.ID <- factor(ranef_nc_df$bird.ID, levels = rev(sort(unique(ranef_nc_df$bird.ID))))

# Plot
ggplot(ranef_nc_df, aes(x = bird.ID, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.5) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-2, 3, 1), limits = c(ymin, ymax)) +
  labs(x = "Bird ID", y = "Estimated Random Effect", title = "Variation in Bird ID Intercepts No Crossing") +
  theme_minimal()