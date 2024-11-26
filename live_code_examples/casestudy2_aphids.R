# Load packages
library(mvgam)           # Fit, interrogate and forecast DGAMs
library(mgcv)            # Fit GAMs
library(tidyverse)       # Tidy and flexible data manipulation
library(ggplot2)         # Flexible plotting
library(gratia)          # Graceful plotting of GAM components
library(marginaleffects) # Efficient computation of regression effects

# Set up plotting environment
theme_set(theme_bw(base_size = 15,
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

# Load the 'aphids' data from the ecostats package. In each of two fields 
# (one of oats, one of wheat) there were eight plots, four with plastic netting 
# to exclude birds, and four without. Aphid abundance was counted on seven 
# different occasions over the first 38 weeks following netting. The hypothesis
# was that aphid numbers would DECREASE when birds were excluded, because an important 
# food source to tree sparrows is aphid predators, hoverflies and ladybird beetles,
# so presence of birds may be limit the effectiveness of a biological control of aphids.
load(url('https://github.com/dwarton/ecostats/raw/main/data/aphids.RData'))

# Bind the two datasets (experimental observations of aphid abundances
# over time in oat and wheat crop plots, under two experimental treatments)
aphid_dat <- aphids$oat %>%
  dplyr::mutate(crop = 'oat',
                series = paste0('oat_plot_', Plot)) %>%
  dplyr::bind_rows(aphids$wheat %>%
                     dplyr::mutate(crop = 'wheat',
                                   series = paste0('wheat_plot_', Plot)))
# View the data structure
dplyr::glimpse(aphid_dat)

# Wrangle data to improve variable names and create a
# time_since_treat variable
aphid_dat %>%
  dplyr::mutate(series = as.factor(series),
                crop = as.factor(crop),
                birds_excluded = as.factor(ifelse(
                  Treatment == 'excluded', 'yes', 'no')),
                time_since_treat = Time) %>%
  janitor::clean_names() %>%
  dplyr::select(-plot, -logcount, -treatment) %>%
  dplyr::arrange(series, time) -> aphid_ts

# Plot the data
aphid_ts %>%
  ggplot(., aes(x = time_since_treat,
                y = counts,
                col = birds_excluded)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ series) +
  labs(x = 'Time since treatment',
       y = 'Counts of aphids')

# A hierarchical GAM mgcv model is very often a useful way to get started
# when modelling ecological time series
?mgcv::smooth.construct.sz.smooth.spec
mod0 <- gam(
  # Observation formula
  formula = counts ~
    
    # Hierarchical (random) intercepts per individual plot
    s(series, bs = 're') +
    
    # Parametric interaction between crop type and bird exclusion,
    # used to centre the hierarchical smooth effects below
    crop * birds_excluded +
    
    # 'Average' nonlinear effect of time since treatment
    s(time_since_treat,
      k = 7) +
    
    # 'Deviation' nonlinear effects of time since treatment, where
    # every level of the interaction between crop type and bird exclusion
    # has a different smooth function that can deviate from the 'Average' 
    # function above. We use the 'sz' basis to ensure the deviations sum 
    # to zero, which is good for inference but requires that we centre the
    # effects using the parametric interaction effect above
    s(time_since_treat,
      birds_excluded, 
      crop,
      bs = 'sz', 
      k = 7),
  
  # The Aphid data in 'long' format
  data = aphid_ts,
  
  # A Negative Binomial observation family to capture excess
  # overdispersion
  family = nb()
)

# View the model summary
summary(mod0)

# Draw partial effects (conditional on all other terms being zero) 
# of the smooths
gratia::draw(mod0)

# Inspect the model's residuals
gratia::appraise(mod0, method = 'simulate')

# Inspect fit against the observed data
marginaleffects::plot_predictions(mod0,
                                  by = c('time_since_treat',
                                         'birds_excluded',
                                         'series'),
                                  points = 0.5)

# Average predictions for each treatment
marginaleffects::plot_predictions(
  mod0,
  by = c('time_since_treat',
         'birds_excluded',
         'crop')
)

# Smoother predictions on a fine grid
marginaleffects::plot_predictions(
  mod0,
  by = c('time_since_treat',
         'birds_excluded'),
  newdata = marginaleffects::datagrid(model = mod0,
                                      time_since_treat = 3:40,
                                      crop = 'oat',
                                      birds_excluded = unique)
)

marginaleffects::plot_predictions(
  mod0,
  by = c('time_since_treat',
         'birds_excluded'),
  newdata = marginaleffects::datagrid(model = mod0,
                                      time_since_treat = 30:70,
                                      crop = 'wheat',
                                      birds_excluded = unique)
)

# It seems there might be some evidence to support the hypothesis that
# aphid numbers would DECREASE when birds were excluded (i.e. counts would be
# lower when birds_excluded == 'yes'). But this effect is not entirely clean. Is 
# our model accurately fitting the data well enough for us to make this conclusion?

# Inspect residuals for possible remaining autocorrelation
aphid_ts %>%
  dplyr::bind_cols(resids = residuals(mod0)) %>%
  ggplot(., aes(time_since_treat, 
                resids)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ series) +
  geom_hline(yintercept = 0, 
             linetype = 'dashed')

# There is clearly unmodelled residual autocorrelation for a lot of these plots.
# We will need an mvgam model, but how to deal with the non-discrete 'time' aspect?
head(aphid_ts, 12)
?mvgam::mvgam_trends
?mvgam::CAR

# 1. Inspect default priors for a DGAM that includes the same effects as 
#    above, but move the hierarchical smooths to the latent process model
#    and include a CAR(1) continuous time autoregressive process

# 2. Update the priors for the fixed effect beta coefficients to 
#    Normal(0, 1), coded as std_normal() in Stan syntax, and fit the
#    model; constrain the AR1 coefficient prior to [0, 1], and fit the
#    model

# 3. Inspect model summaries, diagnostics and estimated parameters
?mvgam::`mvgam-class`
?mvgam::mcmc_plot.mvgam
?mvgam::as.matrix.mvgam
?mvgam::pairs.mvgam

# 4. Repeat the plots and summaries from above. Have your conclusions changed
#    at all? What other model expansions might you consider here?