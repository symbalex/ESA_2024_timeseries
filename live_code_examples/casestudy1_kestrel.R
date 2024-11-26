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
par(family = "serif",
    las = 0,
    mar = c(4.2,
            4.4,
            2.2,
            2.2),
    mgp = c(2.2,
            0.5,
            0),
    bty = "l",
    cex.axis = 1.25,
    cex.lab = 1.4,
    cex.main = 1.5,
    xaxs = 'r',
    yaxs = 'r',
    pch = 16)

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
?mgcv::random.effects
?mgcv::gam.models
get_mvgam_priors(formula = adj_count ~ region +
                   s(yearfac, bs = 're'),
                 data = data_train,
                 family = Gamma())

# Fit the model
mod1 <- mvgam(
  # Observation formula
  formula = adj_count ~ region +
                s(yearfac, bs = 're'),

  # Updated prior distributions using brms::prior()
  priors = prior(std_normal(), class = b),

  # training data in mvgam's long format
  data = data_train,

  # testing data in mvgam's long format
  newdata = data_test,

  # Gamma observation model with shared shape parameter
  family = Gamma(),
  share_obs_params = TRUE,
  
  # Ensure all messages are reported for transparency
  silent = 1
  )

# Look at the structure of the object
str(mod1, max.level = 1)
?mvgam::`mvgam-class`
methods(class = "mvgam")

# Look at the Stan code to better understand the model
stancode(mod1)

# Diagnostics
summary(mod1)
summary(mod1,
        include_betas = FALSE,
        smooth_test = FALSE)
mcmc_plot(mod1,
          type = 'rhat_hist')
mcmc_plot(mod1,
          variable = 'obs_params',
          type = 'trace')
mcmc_plot(mod1,
          variable = c('mean', 'sd'),
          regex = TRUE,
          type = 'trace')
pairs(mod1,
      variable = c('mean', 'sd'),
      regex = TRUE)

# Inspect estimated effects
coef(mod1)
conditional_effects(mod1)
conditional_effects(mod1,
                    type = 'link')
marginaleffects::avg_predictions(mod1, variable = 'region')

# Unconditional posterior predictive checks
pp_check(mod1,
         type = "ecdf_overlay_grouped",
         group = "region",
         ndraws = 50)
pp_check(mod1,
         type = "dens_overlay_grouped",
         group = "region",
         ndraws = 50)

# Conditional posterior predictive checks
hcs <- hindcast(mod1)
class(hcs)
?mvgam::`mvgam_forecast-class`
methods(class = "mvgam_forecast")

layout(matrix(1:4, nrow = 2, byrow = TRUE))
plot(hcs, series = 1)
plot(hcs, series = 2)
plot(hcs, series = 3)
layout(1)

# Residual checks
plot(mod1, type = 'residuals', series = 1)
plot(mod1, type = 'residuals', series = 2)
plot(mod1, type = 'residuals', series = 3)

# Inspect forecasts (if newdata available)
fcs <- forecast(mod1, newdata = data_test)
class(fcs)
layout(matrix(1:4, nrow = 2, byrow = TRUE))
plot(fcs, series = 1)
plot(fcs, series = 2)
plot(fcs, series = 3)
layout(1)

plot_predictions(mod1, newdata = model_data,
                 by = c('yearfac', 'region', 'region'))

# As a quick aside, show a model with splines of year for each region
mod1.2 <- mvgam(
  # Observation formula containing region-level intercepts and
  # hierarchical splines of year for each region
  formula = adj_count ~ region +
    s(year, k = 30, bs = 'cr') +
    s(year, region, k = 10, bs = 'sz'),

  # Updated prior distributions using brms::prior()
  priors = prior(std_normal(),
                 class = b),

  # training and testing data in mvgam's long format
  data = data_train,
  newdata = data_test,

  # Gamma observation model
  family = Gamma(),
  share_obs_params = TRUE
)
stancode(mod1.2)
gratia::draw(mod1.2)
plot_mvgam_smooth(mod1.2, smooth = 1, realisations = TRUE)

# All in-sample, unconditional plots look excellent!
conditional_effects(mod1.2, points = 0.5)
plot_predictions(mod1.2,
                 by = c('year', 'region', 'region'),
                 points = 0.5)
pp_check(mod1.2,
         type = "dens_overlay_grouped",
         group = "region",
         ndraws = 50)
pp_check(mod1.2,
         type = "pit_ecdf_grouped",
         group = "region",
         ndraws = 50)

str(mod1.2$resids)

hist(mod1.2$resids$Alb)
hist(mod1.2$resids$BC)
hist(mod1.2$resids$Sask)

plot(mod1.2, type = 'residuals')
plot(mod1.2, type = 'residuals', series = 2)
plot(mod1.2, type = 'residuals', series = 3)

# But what about forecasts?!?
plot_predictions(mod1.2, newdata = model_data,
                 by = c('year', 'region', 'region'),
                 type = 'link') +
  geom_vline(xintercept = 2000,
             linetype = 'dashed')

fcs <- forecast(mod1.2, newdata = data_test)
layout(matrix(1:4, nrow = 2, byrow = TRUE))
plot(fcs, series = 1)
plot(fcs, series = 2)
plot(fcs, series = 3)
layout(1)

# Expand model using SS model with more appropriate nonlinear 
# temporal effects
?brms::gp
?mvgam::AR
get_mvgam_priors(formula = adj_count ~ series,
                 trend_formula = ~ 
                   gp(year, k = 32, scale = FALSE) + 
                   gp(year, by = trend,
                      k = 20, scale = FALSE) - 1,
                 trend_model = AR(cor = TRUE),
                 data = model_data,
                 family = Gamma())
mod2 <- mvgam(
  # Observation formula, only containing region-level intercepts
  formula = adj_count ~ region,
  
  # Process model formula, containing hierarchical GPs of time
  trend_formula = ~ 
    gp(year, k = 32, scale = FALSE) + 
    gp(year, by = trend,
       k = 20, scale = FALSE) - 1,
  
  # Additional autoregressive dynamics (using a correlated AR(1))
  trend_model = AR(cor = TRUE),
  
  # Updated prior distributions using brms::prior()
  priors = c(prior(beta(3, 10),
                   class = sigma,
                   lb = 0, ub = 1),
             prior(normal(20, 10),
                   class = shape),
             prior(std_normal(),
                   class = `alpha_gp_trend(year)`),
             prior(std_normal(),
                   class = `alpha_gp_trend(year):trendtrend1`),
             prior(std_normal(),
                   class = `alpha_gp_trend(year):trendtrend2`),
             prior(std_normal(),
                   class = `alpha_gp_trend(year):trendtrend3`),
             prior(normal(0.5, 0.2),
                   class = ar1),
             prior(std_normal(),
                   class = b)),
  
  # training and testing data in mvgam's long format
  data = data_train,
  newdata = data_test,
  
  # Gamma observation model with independent shape parameters
  family = Gamma(),
  share_obs_params = FALSE,
  
  # Stan MCMC control for slower but more precise sampling
  adapt_delta = 0.95
)

# Inspect the Stan code
stancode(mod2)

# Diagnostics
summary(mod2,
        include_betas = FALSE,
        smooth_test = FALSE)
mcmc_plot(mod2,
          type = 'rhat_hist')
mcmc_plot(mod2,
          variable = 'sigma',
          regex = TRUE,
          type = 'trace')
pairs(mod2,
      variable = c('sigma', 'alpha_gp'),
      regex = TRUE)
pairs(mod2,
      variable = c('shape', 'alpha_gp'),
      regex = TRUE)
pairs(mod2,
      variable = c('rho_gp', 'alpha_gp'),
      regex = TRUE)
mcmc_plot(mod2,
          variable = 'shape',
          regex = TRUE,
          type = 'trace')
mcmc_plot(mod2,
          variable = 'alpha_gp',
          regex = TRUE,
          type = 'trace')
mcmc_plot(mod2,
          variable = 'Sigma',
          regex = TRUE,
          type = 'hist')

# If you wanted to pull these out and do something with them
head(as.matrix(mod2,
               variable = 'ar1',
               regex = TRUE))

# Unconditional posterior check
pp_check(mod2,
         type = "dens_overlay_grouped",
         group = "series",
         ndraws = 50)

# Inferences and unconditional predictions
conditional_effects(mod2)
conditional_effects(mod2,
                    type = 'link')
plot_predictions(mod2,
                 condition = c('year', 'series', 'series'),
                 points = 0.5)
plot_predictions(mod2,
                 condition = c('year', 'series', 'series'),
                 type = 'link',
                 process_error = FALSE)
marginaleffects::avg_predictions(mod2,
                                 variable = 'series')

# Look at residuals again
plot(mod2,
     type = 'residuals',
     series = 1)
plot(mod2,
     type = 'residuals',
     series = 2)
plot(mod2,
     type = 'residuals',
     series = 3)

# Compare models using in-sample fit metrics
loo_compare(mod1,
            mod2)

# Look at forecasts from each model and compare
fcs1 <- forecast(mod1)
fcs2 <- forecast(mod2)

layout(matrix(1:2,
              nrow = 2,
              byrow = TRUE))
for(x in 1:3){
  plot(fcs1,
       series = x)
  title('Random effects of year')
  plot(fcs2,
       series = x)
  title('GPs of year with AR1 dynamics')
}

# Score forecasts
?mvgam::score
score(fcs1,
      score = 'energy')
score(fcs1,
      score = 'energy')$all_series$score
score(fcs2,
      score = 'energy')$all_series$score

score(fcs1,
      score = 'variogram')$all_series$score
score(fcs2,
      score = 'variogram')$all_series$score

# What about an evenly-weighted ensemble forecast?
ens <- ensemble(fcs1,
                fcs2)
score(ens,
      score = 'variogram')$all_series$score

# Now a completely different model that uses a State-Space Vector
# Autoregression of order 1, with only the region-level intercepts
# as regression parameters
?mvgam::mvgam_formulae
varmod <- mvgam(
  # Observation formula, empty to only consider the Gamma observation process
  formula = adj_count ~ -1,

  # Process model formula that includes regional intercepts
  trend_formula = ~ region,

  # A VAR(1) dynamic process with fully parameterized covariance matrix Sigma
  trend_model = VAR(cor = TRUE),

  # Modified prior distributions using brms::prior()
  priors = c(prior(std_normal(),
                   class = Intercept_trend),
             prior(std_normal(),
                   class = b),
             prior(beta(3, 10),
                   class = sigma,
                   lb = 0, ub = 1)),

  # The time series data in 'long' format
  data = data_train,
  newdata = data_test,

  # Gamma observation model with independent shape parameters
  family = Gamma(),
  share_obs_params = FALSE,

  # Stan MCMC control for slower but more precise sampling
  adapt_delta = 0.95
)
summary(varmod)
mcmc_plot(varmod,
          type = 'rhat_hist')

# Estimates of the autoregressive coefficients
A_pars <- matrix(NA,
                 nrow = 3,
                 ncol = 3)
for(i in 1:3){
  for(j in 1:3){
    A_pars[i, j] <- paste0('A[', i, ',', j, ']')
  }
}
mcmc_plot(varmod,
          variable = as.vector(t(A_pars)),
          type = 'hist') +
  geom_vline(xintercept = 0,
             col = 'white',
             linewidth = 2) +
  geom_vline(xintercept = 0,
             linewidth = 1)

# And of the variance-covariance parameters
Sigma_pars <- matrix(NA,
                     nrow = 3,
                     ncol = 3)
for(i in 1:3){
  for(j in 1:3){
    Sigma_pars[i, j] <- paste0('Sigma[', i, ',', j, ']')
  }
}
mcmc_plot(varmod,
          variable = as.vector(t(Sigma_pars)),
          type = 'hist') +
  geom_vline(xintercept = 0,
             col = 'white',
             linewidth = 2) +
  geom_vline(xintercept = 0,
             linewidth = 1)

# Impulse response functions
?mvgam::irf
irfs <- irf(varmod,
            h = 12,
            orthogonal = FALSE)
plot(irfs, series = 1)
plot(irfs, series = 2)
plot(irfs, series = 3)

# Forecast error variance decompositions
?mvgam::fevd
fevds <- fevd(varmod, h = 12)
plot(fevds) +
  scale_fill_manual(values = c("#DCBCBC",
                               "#A25050",
                               "#5C0000")) +
  labs(fill = 'Process')
?mvgam::stability

# In-sample comparisons don't suggest much difference with mod2
loo_compare(mod1,
            mod2,
            varmod)

# What about forecast comparisons?
fcsvar <- forecast(varmod)

layout(matrix(1:2,
              nrow = 2,
              byrow = TRUE))
for(x in 1:3){
  plot(fcs2,
       series = x)
  title('GPs of year with AR1 dynamics')
  plot(fcsvar,
       series = x)
  title('VAR1 dynamics')
}

# Compare energy scores
score(fcs2,
      score = 'energy')$all_series$score
score(fcsvar,
      score = 'energy')$all_series$score

# Compare variogram scores
score(fcs2,
      score = 'variogram')$all_series$score
score(fcsvar,
      score = 'variogram')$all_series$score

# What about an ensemble of these two?
ens <- ensemble(fcs2, fcsvar)
sum(score(fcs2,
          score = 'energy')$all_series$score)
sum(score(fcsvar,
          score = 'energy')$all_series$score)
sum(score(ens,
          score = 'energy')$all_series$score)

# Perhaps the VAR1 isn't capturing the nonlinear trends as well
# as the hierarchical GPs; but we could easily combine the two!