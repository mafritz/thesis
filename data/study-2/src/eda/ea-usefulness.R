#Exploratory data analysis 
library(here)
library(tidyverse)
library(papaja)
library(skimr)

theme_set(theme_apa(box = TRUE))


source(here("data/study-2/src/01-wrangle.R"))

# Descriptive
s2.ea_usefulness.descriptive <- s2.ea_usefulness |> 
  group_by(dimension) |> 
  summarise(
    mean = mean(value),
    sd = sd(value)
  )

# Scatterplot and dispersion divided by dimensions
s2.eda.dimensions_dispersion <- s2.ea_usefulness |> 
  ggplot(aes(x = fct_rev(dimension), y = value, color = dimension)) +
  geom_jitter(size = 2, alpha = 0.2, na.rm = TRUE) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 15) +
  labs(
    x = NULL,
    y = "Self-Reported Perceived Usefulness"
  ) +
  scale_color_viridis_d() +
  scale_y_continuous(limits = c(1,10), breaks = c(1,4,7,10)) +
  theme(legend.position = "none") +
  coord_flip() +
  NULL

# Line plot of the averaged dimensions over time
s2.eda.dimensions_over_surveys <- s2.ea_usefulness |>
  ggplot(aes(x = survey, y = value, group = dimension)) +
  stat_summary(fun.data = mean_cl_normal, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_normal, geom = "point", size = 4) +
  facet_wrap(~dimension) +
  scale_y_continuous(limits = c(1,10), breaks = c(1,4,7,10)) +
  scale_color_viridis_d() +
  theme(
    text = element_text(size = 10)
  ) +
  labs(
    x = NULL,
    y = "Self-Reported Perceived Usefulness"
  ) +
  NULL

# Descriptive stratified
s2.ea_usefulness.descriptive_stratified <- s2.ea_usefulness |> 
  group_by(group, dimension, survey) |> 
  summarise(
    mean = mean(value),
    sd = sd(value)
  ) |> 
  ungroup() |>
  mutate(
    combined = paste0(printnum(mean), " (", printnum(sd), ")")
  ) |>
  select(group, dimension, survey, combined) |> 
  pivot_wider(names_from = survey, values_from = combined) |> 
  arrange(dimension, group)

s2.ea_usefulness.descriptive_dimension <- s2.ea_usefulness |> 
  group_by(dimension, survey) |> 
  summarise(
    mean = mean(value),
    sd = sd(value)
  ) |> 
  ungroup() |>
  mutate(
    group = "Total",
    combined = paste0(printnum(mean), " (", printnum(sd), ")")
  ) |>
  select(group, dimension, survey, combined) |> 
  pivot_wider(names_from = survey, values_from = combined) |> 
  arrange(dimension, group)

s2.ea_usefulness.descriptive_stratified  <- s2.ea_usefulness.descriptive_stratified |> 
  bind_rows(s2.ea_usefulness.descriptive_dimension) |> 
  arrange(dimension, group)
