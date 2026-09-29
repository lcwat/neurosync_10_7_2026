# script to simulate multilevel model data similar to the types of data we get
# from a typical psychological experiment

# load libraries ----------------------------------------------------------

library(tidyverse)

# create data -------------------------------------------------------------

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
#   - sd B[age][j] medium variance
#   - sd B[trial][j] small variance
#   - sd B[condition][j] large variance
#   - sd B[condition * age][j] small variance
#   - sd B[condition * trial][j] small variance
#   - sd B[trial * age][j] small variance
#   - sd B[trial * age * condition][j] small variance



