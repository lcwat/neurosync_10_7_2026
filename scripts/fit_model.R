# fit model to simulated data and save to a binary fit object

# load libraries ----------------------------------------------------------

library(tidyverse)
library(lme4) # glm
library(brms) # bayesian glm
library(broom.mixed)

# load data ---------------------------------------------------------------

sim_data <- read_csv('data/sim_data.csv')

# take a look
glimpse(sim_data)

# variables:
#' x_age is standardized age (18-65)
#' x_trial is standardized trial (1-40)
#' condition needs to be converted to factor and effect coded
#' subject id and stimulus id group encounters

# convert to factors
sim_data <- sim_data |> 
  mutate(
    across(c(subject_id, stimulus_id, condition), factor)
  )

# effect code
contrasts(sim_data$condition) <- contr.sum(length(unique(sim_data$condition)))

# set seed for reproducibility
set.seed(1072026)

# glmer -------------------------------------------------------------------

# fit a glmer model
glmer_model <- glmer(
  formula = correct ~ 1 + x_age * x_trial * condition + (1 | stimulus_id) + 
    (x_trial * condition | subject_id), 
  data = sim_data, 
  family = 'binomial'
)

tidy(glmer_model) |> filter(effect == 'fixed')

# save fit
# dir.create('fits')
write_rds(glmer_model, 'fits/glmer_model.rds')

# brm ---------------------------------------------------------------------

brm_model <- brm(
  formula = correct ~ 1 + x_age * x_trial * condition + (1 | stimulus_id) + 
    (x_trial * condition | subject_id), 
  data = sim_data, 
  family = bernoulli(),                     # bernoulli likelihood (equivalent to binomial for glmer)
  prior = c(
    prior(normal(0, 2), class = Intercept), # intercept prior is on logit scale, 0 = 50% prob
    prior(normal(0, 3), class = b),         # weakly informed priors for FEs
    prior(exponential(1), class = sd)       # weakly regularizing priors for RE variances
  ), 
  iter = 4000, warmup = 2000, chains = 4, cores = 4, # set sampling pars
  control = list(adapt_delta = .95),        # avoid divergence
  backend = 'cmdstanr',                     # use cmdstanr engine
  sample_prior = T,                         # add prior samples to fit object
  seed = 1072026, 
  file = 'fits/brm_model.rds'               # write to file 
)

# view model 
summary(brm_model)

# sampling looks good

# ppc
yrep <- posterior_predict(brm_model, ndraws = 500)

bayesplot::ppc_bars(y = sim_data$correct, yrep = yrep)