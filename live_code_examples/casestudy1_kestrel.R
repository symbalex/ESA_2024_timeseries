# Load packages
library(mvgam)           # Fit, interrogate and forecast DGAMs
library(tidyverse)       # Tidy and flexible data manipulation
library(ggplot2)         # Flexible plotting

# Set up plotting environment
theme_set(theme_classic(base_size = 15,
                        base_family = 'serif'))
myhist = function(...){
  geom_histogram(col = 'white',
                 fill = '#B97C7C', ...)
}
hist_theme = function(){
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
}

# Load the annual American kestrel, Falco sparverius, abundance time series in
# taken in British Columbia, Canada. These data have been collected
# annually, corrected for changes in observer coverage and detectability,
# and logged. They can be found in the MARSS package
load(url('https://github.com/atsa-es/MARSS/raw/master/data/kestrel.rda'))
head(kestrel)

# Arrange the data into a data.frame
regions <- c("BC",
             "Alb",
             "Sask")
model_data <- do.call(rbind,
                      lapply(seq_along(regions),
                             function(x){
                               data.frame(year = kestrel[, 1],
                                          # Reverse the logging so that we deal directly
                                          # with the detection-adjusted counts
                                          adj_count = exp(kestrel[, 1 + x]),
                                          region = regions[x])})) %>%
  # Add series and time indicators for mvgam modelling
  dplyr::mutate(yearfac = as.factor(year),
                region = as.factor(region),
                series = as.factor(region),
                time = year)

# Inspect modelling data structure
head(model_data)
dplyr::glimpse(model_data)
levels(model_data$series)

# Split the data into a training and a testing split
data_train <- model_data %>%
  dplyr::filter(year <= 2001)
data_test <- model_data %>%
  dplyr::filter(year > 2001)

# Plot the time series together
plot_mvgam_series(data = data_train,
                  y = 'adj_count',
                  series = 'all')

# Now plot features for just one series at a time
plot_mvgam_series(data = data_train,
                  newdata = data_test,
                  y = 'adj_count',
                  series = 1)
plot_mvgam_series(data = data_train,
                  newdata = data_test,
                  y = 'adj_count',
                  series = 2)
plot_mvgam_series(data = data_train,
                  newdata = data_test,
                  y = 'adj_count',
                  series = 3)

# Look at distribution of the outcome variable
ggplot(data_train,
       aes(x = adj_count)) +
  myhist() +
  labs(x = 'Adjusted count', y = '') +
  hist_theme()
summary(data_train$adj_count)

# Heavy-ish tail to the right; perhaps a Gamma distribution
# Inspect default priors for a simple model that only includes
# random intercepts for years, implying that all three series share the same
# year effects
?mvgam::mvgam_formulae
?mgcv::random.effects
?mgcv::gam.models
?mvgam::get_mvgam_priors

# 1. Inspect default priors for a GAM that includes region-specific 
#    intercepts and a random effect of yearfac

# 2. Update the prior for the fixed effect beta coefficients to 
#    Normal(0, 1), coded as std_normal() in Stan syntax, and fit the
#    model

# 3. Inspect model summaries, diagnostics and estimated parameters
?mvgam::`mvgam-class`
?mvgam::mcmc_plot.mvgam
?mvgam::as.matrix.mvgam
?mvgam::pairs.mvgam

# 4. Inspect estimated conditional effects
?mvgam::conditional_effects.mvgam
?marginaleffects::plot_predictions
?marginaleffects::avg_predictions

# 5. Plot some posterior predictive checks
?mvgam::pp_check

# 6. Plot randomized quantile (Dunn-Smyth) residuals
?mvgam::plot.mvgam

# 7. Expand to a State-Space model with more appropriate nonlinear 
#    temporal effects
?mvgam::mvgam_formulae
?brms::gp
?mvgam::AR

# 8. Compare the two models using Approximate Leave-One-Out 
#    Cross-Validation
?mvgam::loo_compare

# 9. Compare forecast distributions from the two models
?mvgam::forecast
?mvgam::score