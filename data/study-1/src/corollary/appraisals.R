library(here)
library(tidyverse)
library(papaja)
library(see)
library(RColorBrewer)  
library(grid)
library(gridExtra)

theme_set(theme_apa(box = TRUE))

options(scipen = 999)
options(digits = 5)

source(here("data/study-1/src/01-wrangle.R"))

# Evolution over time
s1.appraisals_long_format <- s1.dew_emotions |> 
  mutate(
    id = row_number()
  ) |>
  select(
    user, x, y, elapsedMinutes, id
  ) |> 
  pivot_longer(
    -c(user, elapsedMinutes, id),
    names_to = c("dimension"),
  ) |> 
  mutate(
    dimension = if_else(dimension == "x", "Valence", "Control/Power"),
    group = case_when(
      str_detect(user, "s") ~ "Self",
      str_detect(user, "o") ~ "Partner",
      TRUE ~ "Mutual"
    ),
    group = factor(group, levels = c("Self", "Partner", "Mutual")),
    dimension = factor(dimension, levels = c("Valence", "Control/Power"))
  )

s1.appraisal_over_time.graph <- s1.appraisals_long_format |> 
  ggplot(aes(x = elapsedMinutes, y = value, group = user)) +
    geom_line(size = 0.5, alpha = 0.1) +
    geom_point(size = 0.5, alpha = 0.1) +
    geom_smooth(aes(group = group, color = group), data = s1.appraisals_long_format) +
    facet_grid(group ~ dimension) +
    scale_colour_brewer(palette = "Dark2") +
    theme(
      legend.position = "none"
    ) +
    labs(
      x = "Elapsed time",
      y = "Appraisal"
    ) +
  lims(y = c(-100, 100)) 
  
