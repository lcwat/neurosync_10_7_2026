# script to simulate multilevel model data similar to the types of data we get
# from a typical psychological experiment

# load libraries ----------------------------------------------------------

library(tidyverse)

# model description -------------------------------------------------------

# data are meant to simulate a memory task, where subjects correctly identify 
# whether a presented stimulus was actually present during a learning period

# to simplify the example, I include two continuous predictors (age, trial) and 
# one categorical predictor (condition)

# we expect that age will reduce recall accuracy and experience will improve recall
# accuracy. our condition is within-subjects and represents whether a subject was
# under distress via a conditioned aversive stimulus (e.g., tone that was previously 
# paired with a little shock)

# model (likelihood of being correct on trial i given age, trial, and condition
# grouping on subject j and stimulus k)

# Likelihood: Correct[i] ~ Bernoulli(mu[i])

# Model:
# mu[i] = B0[i] + B0[i][j] + B0[i][k] + (B[age][i] + B[age][i][j]) * age[i] + 
#   (B[trial][i] + B[trial][i][j]) * trial[i] + 
#   (B[condition][i] + B[condition][i][j]) * condition[i] + 
#   (B[condition * age][i] + B[condition * age][i][j]) * condition[i] * age[i] + 
#   (B[condition * trial][i] + B[condition * trial][i][j]) * condition[i] * trial[i] + 
#   (B[trial * age][i] + B[trial * age][i][j]) * trial[i] * age[i] +
#   (B[condition * age * trial][i] + B[condition * age * trial][i][j]) * condition[i] * age[i] * trial[i]

# R formula syntax
# correct ~ age * trial * condition + (1 | stimulus) + 
#   (age * trial * condition | subject)

# Parameters to sample:
# 1. Intercepts and group offset variances
#   - B0 is grand mean of memory accuracy
#   - sd for B0[i][j] and B0[i][k] are variance offsets from GM for subjects [j] and stimuli [k]
# 2. Slopes and group offset variances
#   - B[age] is effect of age on recall accuracy (negative)
#   - B[trial] is effect of trial on recall accuracy (positive)
#   - B[condition] is effect of manipulation on recall accuracy (negative, e.g., threat of shock = worse accuracy)
#   - B[condition * age] is interaction between age and condition (positive, e.g., manipulation more effective on older people)
#   - B[condition * trial] is interaction between trial and condition (negative, e.g., manipulation less effective with more experience)
#   - B[trial * age] is interaction between trial and age (negative, e.g., older people learn at slower rate)
#   - B[trial * age * condition] is full interaction (negative, e.g., older people with fewer trials under threat perform the worst)
#   - sd B[trial][j] small variance
#   - sd B[condition][j] large variance
#   - sd B[condition * trial][j] small variance
# 3. correlations between REs
#   - rho B0[i][j], B[trial][j], B[condition][j], B[condition * trial][j]

# create sample -----------------------------------------------------------

# set pars for simulation
set.seed(1072026)

n_subjects <- 100
n_stimuli_per_cond <- 20 # per condition

condition_levels <- c('fear', 'neutral')

# parameter values in log odds scale, 0 = 50% prob 
b_intercept <- 0 # GM
b_age <- -0.8
b_trial <- 0.05
b_condition <- -1
b_age_trial <- 0.01
b_age_cond <- 0.5
b_trial_cond <- -0.01
b_age_trial_cond <- -0.2

# re variances
sd_stim_intercept <- 0.1
sd_subj_intercept <- 0.2
sd_subj_trial <- 0.02
sd_subj_cond <- 0.1
sd_subj_ixn <- 0.005

# re correlations
rho_int_trial <- 0.1
rho_int_cond <- 0.2
rho_int_ixn <- 0.1
rho_trial_cond <- 0.1
rho_trial_ixn <- 0.1
rho_cond_ixn <- 0.1

# simulate stimuli
stimuli <- tibble(
  stimulus_id = factor(seq(1, n_stimuli_per_cond * 2, 1)), 
  condition = rep(condition_levels, each = n_stimuli_per_cond), 
  x_condition = recode(condition, 'neutral' = -.5, 'fear' = .5), 
  O_01 = rnorm(length(stimulus_id), mean = 0, sd = sd_stim_intercept) # simulate sd stim
)

# glimpse(stimuli)

# simulate subjects 

# build vcov matrix for RE sampling with correlations, get nearest non-pos definite
# to avoid error in sampling with mvnorm
m_cov <- as.matrix(Matrix::nearPD(
  matrix(
    c(
      sd_subj_intercept^2, rho_int_trial, rho_int_cond, rho_int_ixn,
      rho_int_trial, sd_subj_trial^2, rho_trial_cond, rho_trial_ixn, 
      rho_int_cond, rho_trial_cond, sd_subj_cond^2, rho_cond_ixn,
      rho_int_ixn, rho_trial_ixn, rho_cond_ixn, sd_subj_ixn^2
    ), nrow = 4, ncol = 4, byrow = TRUE
  )
)$mat)

# generate by subject random effects
subject_rfx <- MASS::mvrnorm(
  n = n_subjects,
  mu = rep(0, 4), # offset from 0, mean = 0
  Sigma = m_cov
) |> 
  as_tibble() |> 
  rename(T_0s = V1, T_trial = V2, T_condition = V3, T_ixn = V4)

subjects <- tibble(
  subject_id = factor(seq(1, n_subjects, 1)), 
  age = sample(18:65, n_subjects, replace = TRUE), 
  x_age = (age - mean(age)) / sd(age)
) |> 
  bind_cols(subject_rfx)

glimpse(subjects)

# create simulated data
sim_data <- crossing(subjects, stimuli) |> 
  group_by(subject_id) |> 
  mutate(
    trial = sample(1:n(), n()), 
    x_trial = (trial - mean(trial)) / sd(trial)
  ) |> 
  ungroup() |> 
  mutate(
    log_odds = b_intercept + T_0s + O_01 + 
      b_age * x_age + 
      (b_trial + T_trial) * x_trial + 
      (b_condition + T_condition) * x_condition + 
      b_age_trial * x_age * x_trial + 
      b_age_cond * x_age * x_condition + 
      (b_trial_cond + T_ixn) * x_trial * x_condition + 
      b_age_trial_cond * x_age * x_trial * x_condition, 
    prob = plogis(log_odds), 
    correct = rbinom(n(), size = 1, prob = prob)
  )

sim_data |>
  group_by(condition) |>
  summarize(
    p = mean(correct)
  ) |>
  ggplot(aes(x = condition, y = p)) +
  geom_col()

sim_data |> 
  ggplot(aes(x = age, y = correct, color = condition)) + 
  geom_point() +
  geom_smooth()

sim_data |> 
  ggplot(aes(x = trial, y = correct, color = condition)) + 
  geom_point() +
  geom_smooth()

sim_data |> 
  mutate(
    age_fac = factor(
      case_when(
        age < 35 ~ 'young', 
        age >= 35 ~ 'old'
      )
    )
  ) |> 
  ggplot(aes(x = trial, y = correct, color = condition)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~age_fac)

# save
write_csv(sim_data, 'data/sim_data.csv')
