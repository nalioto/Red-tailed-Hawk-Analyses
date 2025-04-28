################################################################################
#                             # NO cross Models #                              #
################################################################################


############
# Model 1 #
###########

# no.cross Model with U and V wind as separate components 
out.nc1 <- brm(formula = no.cross ~ scale(U) + scale(V) + scale(average.delta.t) + (1|bird.ID), 
               data = full.dat, 
               family = bernoulli(link = 'logit'),
               warmup = 1000, 
               iter = 5000, 
               chains = 3, 
               init = 'random', 
               cores = 1, 
               seed = 123)
out.nc1 <- add_criterion(out.nc1, "loo") # Need to add loo criterion to model
pp_check(out.nc1, ndraws = 100)

(Plots)
summary(out.nc1)
pairs(out.nc1)


# Conditional Effects Plots for coefficients 

# Wind speed
plot(conditional_effects(out.nc1, effects = "U"), points = TRUE)

# Delta T (uplift over Water)
plot(conditional_effects(out.nc1, effects = "average.delta.t"), points = TRUE)

# Vertical Wind
plot(conditional_effects(out.nc1, effects = "V"), points = TRUE)


############
# Model 2 #
###########

# no.cross Model with wind speed. U and V wind have been dropped
out.nc2 <- brm(formula = no.cross ~ scale(average.WSP) + scale(average.delta.t) + (1|bird.ID), 
               data = full.dat, 
               family = bernoulli(link = 'logit'),
               warmup = 1000, 
               iter = 5000, 
               chains = 3, 
               init = 'random', 
               cores = 1, 
               seed = 123)
out.nc2 <- add_criterion(out.nc2, "loo") # Need to add loo criterion to model
pp_check(out.nc2, ndraws = 100)

summary(out.nc2)
pairs(out.nc1)

# Plots of Coefficients

# Extract posterior summaries and convert to a dataframe
posterior_2 <- as.data.frame(fixef(out.nc2))  # Replace 'your_model' with the actual model name
posterior_2$Parameter <- rownames(posterior_2)

# Rename columns for clarity
colnames(posterior_2) <- c("Estimate", "Est.Error", "l-95 CI", "u-95 CI", "Parameter")

# Remove the intercept
posterior_2 <- posterior_2 %>% 
  filter(Parameter != "Intercept")


# Plot with ggplot2
ggplot(posterior_2, aes(x = Estimate, y = reorder(Parameter, Estimate))) +
  geom_point(color = "blue") +
  geom_errorbarh(aes(xmin = `l-95 CI`, xmax = `u-95 CI`), height = 0.2, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Coefficient Estimate", y = "Predictor", title = "Bayesian No Cross Model Coefficients") +
  theme_minimal()


# Conditional Effects Plots for coefficients 

# Wind speed
plot(conditional_effects(out.nc2, effects = "average.WSP"), points = TRUE)

# Delta T (uplift over Water)
plot(conditional_effects(out.nc2, effects = "average.delta.t"), points = TRUE)


############
# Model 3 #
###########

# no.cross Model same as nc1 model but with Julian Date added
out.nc3 <- brm(formula = no.cross ~  scale(U) + scale(V) + scale(average.delta.t) + scale(julian.day) + (1|bird.ID), 
               data = full.dat, 
               family = bernoulli(link = 'logit'),
               warmup = 1000, 
               iter = 5000, 
               chains = 3, 
               init = 'random', 
               cores = 1, 
               seed = 123)
out.nc3 <- add_criterion(out.nc3, "loo") # Need to add loo criterion to model
pp_check(out.nc3, ndraws = 100)

summary(out.nc3)
plot(out.nc3)

############
# Model 4 #
###########

# no.cross Model with wind speed and Julian date
out.nc4 <- brm(formula = no.cross ~ scale(average.WSP) + scale(average.delta.t) + scale(julian.day) + (1|bird.ID), 
               data = full.dat, 
               family = bernoulli(link = 'logit'),
               warmup = 1000, 
               iter = 5000, 
               chains = 3, 
               init = 'random', 
               cores = 1, 
               seed = 123)
out.nc4 <- add_criterion(out.nc4, "loo") # Need to add loo criterion to model
pp_check(out.nc4, ndraws = 100)

summary(out.nc4)
plot(out.nc4)

# Conditional Effects Plots

# Wind speed
plot(conditional_effects(out.nc4, effects = "average.WSP"), points = TRUE)

# Delta T (uplift over Water)
plot(conditional_effects(out.nc4, effects = "average.delta.t"), points = TRUE)

# Julian Date 
plot(conditional_effects(out.nc4, effects = "julian.day"), points = TRUE)


# Model Comparison

# Let's compare the models with leave one out validation (LOO)
loo_compare(out.nc1, out.nc2, out.nc3, out.nc4, criterion = c("loo"))

#            elpd_diff  se_diff
# out.nc2       0.0       0.0   # Model nc2 has the best predicitive power
# out.nc4      -1.2       0.8   
# out.nc1      -7.0       4.5   
# out.nc3     -8.1        4.6  