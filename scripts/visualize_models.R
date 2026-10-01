# this is the main script for visualizing models 

# load libraries ----------------------------------------------------------

# for data wrangling and shaping and visualization
library(tidyverse)

# for model predictions
library(emmeans)
library(marginaleffects)
library(tidybayes)

# for national park colors and font
library(NatParksPalettes)
library(showtext)

font_add_google('Inter')
showtext_opts(dpi = 300)
showtext_auto()

# plot theme, instead of calling all this code for each plot, I opt to create a 
# function to standardize theme elements across everything I plot
neurosync_theme <- function() {
  theme_bw() + 
  theme(
    panel.grid = element_blank(), 
    text = element_text(family = 'Inter', size = 11), 
    title = element_text(face = 'bold', size = 12)
  )
}

# load data and fits ------------------------------------------------------

sim_data <- read_csv('data/sim_data.csv')
glmer_model <- read_rds('fits/glmer_model.rds')
brm_model <- read_rds('fits/brm_model.rds')

# colors
clrs <- natparks.pals('Redwood')

# 1. categorical contrast -------------------------------------------------

# look at the effect of turning on and off condition on P(correct) while holding
# all other predictors constant

# we can get an average prediction using average values of other predictors
# and varying condition 

# average predictors -> plug into model formula -> vary condition

# emmeans
(preds <- emmeans(
  glmer_model,                   # model
  ~ condition * x_age * x_trial, # this should match regression formula
  regrid = 'response'            # output predictions in probabilities instead of log-odds
))

# can also compute a contrast between levels of condition using pairwise
emmeans(
  glmer_model,                   
  pairwise ~ condition * x_age * x_trial, # add pairwise for contrast
  regrid = 'response'
)

# to plot this, have to turn emmeans object into data frame/tibble object to 
# plug into ggplot

# categorical contrast is a bar plot with error bars, can use SE or CIs
preds |> 
  as_tibble() |> 
  ggplot(aes(x = condition)) + 
  
  # model preds
  geom_col(
    aes(y = prob),  # prediction is stored in variable prob
    fill = clrs[3], # set color of bar to one of our saved colors
    width = .3
  ) + 
  
  # model pred error
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL), 
    color = clrs[1], 
    width = 0, 
    linewidth = 1.2
  ) + 
  
  # optional, add in subject data over bars
  geom_point(
    # get average P(correct) for each subj in each condition
    data = sim_data |> 
      group_by(subject_id, condition) |> 
      summarize(mean = mean(correct)), 
    aes(y = mean),
    color = clrs[1], 
    alpha = .2, 
    shape = 1, 
    position = position_jitterdodge(jitter.width = .15, jitter.height = .1)
  ) + 
  
  # adjust scales and labels
  ylim(0, 1) + 
  labs(y = 'P(Correct)') + 
  neurosync_theme()

# save
# dir.create('fig_output')
ggsave(
  'fig_output/condition_contrast_bars.png', # file name, just save rendered plot
  device = 'png',                           # png
  width = 3, height = 4, units = 'in'       # dimensions
)

# marginaleffects

# instead of averaging then plugging in, marginaleffects will plug in data to 
# model, generate predictions, then average predictions within groups to generate
# contrasts
(preds <- avg_predictions(
  glmer_model, 
  by = 'condition' # marginalize over condition
))

# can get contrasts with hypothesis
avg_predictions(
  glmer_model, 
  by = 'condition', 
  hypothesis = ~ pairwise
)

# neater to work with, easier to compute correct marginal contrasts and can 
# directly plot with this preds object
preds |> 
  ggplot(aes(x = condition)) + 
  
  # model preds
  geom_col(
    aes(y = estimate),  # prediction is stored in variable estimate
    fill = clrs[3], # set color of bar to one of our saved colors
    width = .3
  ) + 
  
  # model pred error
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high), 
    color = clrs[1], 
    width = 0, 
    linewidth = 1.2
  ) + 
  
  # optional, add in subject data over bars
  geom_point(
    # get average P(correct) for each subj in each condition
    data = sim_data |> 
      group_by(subject_id, condition) |> 
      summarize(mean = mean(correct)), 
    aes(y = mean),
    color = clrs[1], 
    alpha = .2, 
    shape = 1, 
    position = position_jitterdodge(jitter.width = .15, jitter.height = .1)
  ) + 
  # adjust scales and labels
  ylim(0, 1) + 
  labs(y = 'P(Correct)') + 
  neurosync_theme()

ggsave(
  'fig_output/me_condition_contrast_bars.png', # file name, just save rendered plot
  device = 'png',                           # png
  width = 3, height = 4, units = 'in'       # dimensions
)

# usually, error/CI for constrasts from marginaleffects is smaller b/c of how 
# they are computed (predict then average vs. average then predict), can be 
# especially helpful reducing type II error when computing contrasts! 

# 2. true marginal effects (slopes) ---------------------------------------

# true statistical marginal effects are concerned with the change in a response
# variable (like P(correct)) given a small change in a continuous predictor, 
# essentially a slope. when your model has just one predictor, this is incredibly 
# easy to find (its the regression weight)! But as we add more predictors, it 
# gets a lot harder to compute this (requiring gnarly partial derivative eqs), 
# so instead, we numerically estimate them by plugging in representative values
# for the predictor getting a prediction, then adding a little value to the 
# predictor and getting that prediction to compute slope at that point

# this is fine for linear relationships, but we need to keep in mind that our
# logistic model is non-linear, and slope will change along predictor usually

# emmeans

# to get slopes with emmeans methods, use emtrends
emtrends(
  glmer_model, 
  ~ x_age, 
  var = 'x_age',
  at = list(x_age = c(-1, 0, 1)), # requires list of ages, x_age is standardized so we get -/+1 sd and mean 0
  regrid = 'response'
)

# here is the same thing in marginaleffects
slopes(
  glmer_model, 
  variables = 'x_age', 
  newdata = datagrid(
    x_age = c(-1, 0, 1)
  )
)

# although this is a nice quantification, we usually prefer to visualize this 
# effect, for this we go back to using predictions or emmeans
preds <- avg_predictions(
  glmer_model, 
  by = 'x_age', 
  newdata = datagrid(
    x_age = seq(min(sim_data$x_age), max(sim_data$x_age), length.out = 10),
    grid_type = 'counterfactual'
  )  
)

preds |> 
  ggplot(aes(x = x_age)) + 
  
  # add observed data
  geom_point(
    data = sim_data |> 
      group_by(subject_id) |> 
      summarize(mean = mean(correct), x_age = unique(x_age)), 
    aes(y = mean), 
    shape = 1, 
    color = clrs[5], 
    alpha = .4
  ) + 
  
  # add predicted line
  geom_line(
    aes(y = estimate), 
    color = clrs[6] 
  ) + 
  
  # add error ribbon
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high), 
    fill = clrs[6], 
    alpha = .3
  ) + 
  
  # adjust scales and labs
  ylim(0, 1) + 
  labs(
    y = 'P(Correct)', 
    x = 'Age'
  ) + 
  scale_x_continuous(
    # replace standardized age with age labels
    breaks = seq(min(sim_data$x_age), max(sim_data$x_age), length.out = 7), 
    labels = round(seq(min(sim_data$age), max(sim_data$age), length.out = 7), 0)
  ) + 
  neurosync_theme()

# save
ggsave(
  'fig_output/me_age.png', device = 'png', width = 6, height = 5, units = 'in'
)

# 3. interactions (slopes by condition) -----------------------------------

# usually though, we are more interested in probing complex interactions between
# variables. from the raw regression coefficents, it can be extremely hard, even
# for a seasoned researcher to intuit what is happening. lets do a visualization
# instead

# look at model fit for info about where to look
broom.mixed::tidy(glmer_model) |> 
  filter(effect == 'fixed')

# lets start with the 2-way interaction between age and condition, what does
# this negative parameter mean? 

# emmeans contrasts
emmeans(
  glmer_model, 
  pairwise ~ x_age * condition * x_trial, 
  by = 'condition',                       # group estimates on condition
  at = list(x_age = c(-1, 0, 1)),         # use these values of std age
  regrid = 'response'
)

# appears that estimates differ at each value of age

# marginaleffects plot
preds <- avg_predictions(
  glmer_model, 
  by = c('condition', 'x_age'), 
  newdata = datagrid(
    condition = unique,                         # use unique levels of condition
    x_age = seq(
      min(sim_data$x_age), max(sim_data$x_age), # sample from min to max
      length.out = 10
    ), 
    grid_type = 'counterfactual'                # robust predictions
  )
)

preds |> 
  ggplot(aes(x = x_age, color = condition)) + 
  
  # raw data behind predictions again
  geom_point(
    data = sim_data |> 
      group_by(subject_id, condition) |> 
      summarize(mean = mean(correct), x_age = unique(x_age)), 
    aes(y = mean), 
    shape = 1, alpha = .4
  ) + 
  
  # prediction line
  geom_line(
    aes(y = estimate)
  ) + 
  
  # error ribbon 
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = condition),
    color = NA, 
    alpha = .3
  ) + 
  
  # adjust scales and labs
  ylim(0, 1) + 
  labs(
    y = 'P(Correct)', 
    x = 'Age'
  ) + 
  scale_x_continuous(
    # replace standardized age with age labels
    breaks = seq(min(sim_data$x_age), max(sim_data$x_age), length.out = 7), 
    labels = round(seq(min(sim_data$age), max(sim_data$age), length.out = 7), 0)
  ) + 
  scale_color_manual(values = c(clrs[1], clrs[5])) + 
  scale_fill_manual(values = c(clrs[1], clrs[5])) +
  neurosync_theme()

# save
ggsave(
  'fig_output/me_age_condition.png', device = 'png', 
  width = 6, height = 5, units = 'in'
)

# now we can look at the three way interaction

# emmeans
emtrends(
  glmer_model, 
  ~ x_age * condition * x_trial, 
  var = 'x_age',           
  by = 'condition', 
  at = list(x_age = c(-1, 0, 1), x_trial = c(-1, 0, 1)), 
  regrid = 'response'
)

# visualize with marginaleffects
preds <- avg_predictions(
  glmer_model, 
  by = c('x_age', 'condition', 'x_trial'), 
  newdata = datagrid(
    condition = unique,
    x_age = seq(min(sim_data$x_age), max(sim_data$x_age), length.out = 10), 
    x_trial = c(-1, 0, 1), 
    grid_type = 'counterfactual'
  )
)

preds |> 
  mutate(
    trial = factor(
      x_trial, levels = c(-1, 0, 1), 
      labels = c('early', 'middle', 'late') # add labels for trial
    )
  ) |> 
  ggplot(aes(x = x_age, color = condition)) + 
  
  # raw data behind predictions again
  # geom_point(
  #   data = sim_data |> 
  #     group_by(subject_id, condition) |> 
  #     summarize(mean = mean(correct), x_age = unique(x_age)), 
  #   aes(y = mean), 
  #   shape = 1, alpha = .4
  # ) + 
  
  # prediction line
  geom_line(
    aes(y = estimate)
  ) + 
  
  # error ribbon 
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = condition),
    color = NA, 
    alpha = .3
  ) + 
  
  # adjust scales and labs
  ylim(0, 1) + 
  labs(
    y = 'P(Correct)', 
    x = 'Age'
  ) + 
  scale_x_continuous(
    # replace standardized age with age labels
    breaks = seq(min(sim_data$x_age), max(sim_data$x_age), length.out = 7), 
    labels = round(seq(min(sim_data$age), max(sim_data$age), length.out = 7), 0)
  ) + 
  scale_color_manual(values = c(clrs[1], clrs[5])) + 
  scale_fill_manual(values = c(clrs[1], clrs[5])) +
  neurosync_theme() + 
  facet_wrap(~trial)

# save
ggsave(
  'fig_output/me_age_condition_trial.png', device = 'png', 
  width = 7, height = 4, units = 'in'
)

# 4. misc: random effects -------------------------------------------------

# one interesting aspect of these predictions is how they incorporate random
# effects into their predictions. you have a few options of how you want to 
# go about this: 
# 1. Specify a random effects formula to include specific offsets into predicitions
#   with a different library (e.g., fitted from stats)
# 2. Zero out random effects to focus on fixed effects (this is what emmeans does
#   by default, marginaleffects will do this with re.form = NA)
# 3. integrate out random effects, this is what marginaleffects does by default
#   to conform its predict then average approach to generating preds

# marginaleffects spaghetti
preds <- fitted(
  glmer_model, 
  re_formula = ~ (x_trial * condition | subject_id)
) |> 
  as_tibble() |> 
  bind_cols(sim_data)

# spaghetti
preds |> 
  ggplot(aes(x = x_trial, color = as.factor(subject_id))) + 
  
  # line
  geom_line(
    aes(y = value), # value is our prediction
    alpha = .5
  ) + 
  
  # scales and labs
  ylim(0, 1) + 
  labs(
    y = 'P(Correct)'
  ) + 
  scale_x_continuous(
    'Trial', 
    breaks = seq(min(preds$x_trial), max(preds$x_trial), length.out = 9), 
    labels = round(seq(min(preds$trial), max(preds$trial), length.out = 9))
  ) + 
  scale_color_viridis_d(guide = 'none', option = 'turbo') + 
  
  neurosync_theme() + 
  facet_wrap(~condition)
  
# save
ggsave(
  'fig_output/spaghetti_trial_cond.png', device = 'png', 
  width = 7, height = 4, units = 'in'
)

# 5. bayesian vis ---------------------------------------------------------

# bayesian models provide us with more data for visualization and computing 
# constrasts as distributions rather than point estimates
preds <- predictions(
  brm_model, 
  newdata = datagrid(condition = unique), # unique values of condition
  by = 'condition',                       # compute marginal effect of condition
  re_formula = NULL                       # integrate across groups (subj/stimulus)
) |> 
  posterior_draws()                       # get predictions as draws from model posterior

# plot halfeyes
preds |> 
  ggplot(aes(x = draw, fill = factor(condition))) +
  
  # halfeye dists
  stat_halfeye() + 
  
  # scales and labs
  xlim(0, 1) + 
  scale_fill_manual(
    'Condition',
    values = c(clrs[2], clrs[5])
  ) + 
  labs(
    x = 'P(Correct)', 
    y = 'Density'
  ) + 
  
  neurosync_theme()

# save
ggsave(
  'fig_output/brm_halfeyes_cond.png', device = 'png', 
  width = 5, height = 4, units = 'in'
)

# constrast distribution
ate <- comparisons(
  brm_model, 
  variables = 'condition', 
  re_formula = NULL
) |> 
  posterior_draws()

# compute expectation from posterior then plot distribution
ate |> 
  group_by(drawid) |> 
  summarize(draw = mean(draw)) |> 
  ggplot(aes(x = draw)) +
  
  # halfeye dists
  stat_halfeye(fill = clrs[3]) + 
  
  # scales and labs
  labs(
    x = ggplot2:::parse_safe('P(Correct)[Neutral] - P(Correct)[Fear]'), 
    y = 'Density'
  ) + 
  
  neurosync_theme()

# save
ggsave(
  'fig_output/brm_constrast_cond.png', device = 'png', 
  width = 5, height = 4, units = 'in'
)

# takeaways ---------------------------------------------------------------

# emmeans better for probing relationships numerically, it is quick but less 
# robust because of how it goes about generating group level estimates for 
# both constrasts and marginal effects (slopes). emmeans will even tell you this
# in their documentation that the main purpose is not for generating predictions
# for plots. marginaleffects on the other hand is more robust and features many
# functions and amazing documentation for computing marginal effects and constrasts
# but also generating predictions that are designed for creating useful 
# visualizations. it is not free though, the approach consumes a lot more memory
# and will often take a bit more time to run. 
