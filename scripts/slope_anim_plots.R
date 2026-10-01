# animations for presentation

# load libraries ----------------------------------------------------------

library(tidyverse)
library(ggtext)

# create data -------------------------------------------------------------

glmer_model <- read_rds('fits/glmer_model.rds')

d <- tibble(
  x = sample(0:100, 1000, replace = T), 
  y = 1 / (1 + exp(-0.08*(x - 50)))
)

plot_tangent <- function(x) {
  y = 1 / (1 + exp(-0.08 * (x - 50)))
  yh = 1 / (1 + exp(-0.08 * (x + .001 - 50)))
  slope = (yh - y) / .001
  
  tangent = tibble(
    x = x, 
    y = y,
    slope = slope, 
    intercept = slope * -x + y, 
    nice_label = glue::glue(
      'x = {round(x, 0)}<br>', 
      'y = {round(y, 2)}<br>', 
      'slope: **{round(slope, 2)}**'
    )
  )
  
  d |> 
    ggplot(aes(x = x)) + 
    geom_abline(
      data = tangent,
      aes(slope = slope, intercept = intercept), 
      linewidth = .5, color = clrs[4], linetype = 3
    ) + 
    geom_point(
      data = tangent, 
      aes(y = y), color = clrs[4]
    ) + 
    geom_line(aes(y = y), color = clrs[1], linewidth = 1.2) + 
    geom_richtext(
      data = tangent,
      aes(y = y, label = nice_label), 
      nudge_y = -.1, 
      size = 3
    ) + 
    neurosync_theme()
}

dir.create('slope_anim_frames')

counter = 1

for (i in seq(20, 80, 1)) {
  p <- plot_tangent(i)
  
  # save
  ggsave(
    plot = p, filename = glue::glue('slope_anim_frames/x_{counter}.png'), 
    device = 'png', width = 4, height = 4, units = 'in'
  )
  
  counter = counter + 1
  
  print(counter)
}
