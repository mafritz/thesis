# Exploratory data analysis
library(here)
library(tidyverse)
library(papaja)
library(skimr)

theme_set(theme_apa(box = TRUE))

source(here("data/study-2/src/01-wrangle.R"))

s2.sus_score$item <- factor(s2.sus_score$item, levels = c(
  "SUS1",
  "SUS2",
  "SUS3",
  "SUS4",
  "SUS5",
  "SUS6",
  "SUS7",
  "SUS8",
  "SUS9",
  "SUS10"
))

(s2.sus_by_item.graph <- s2.sus_score |>
  ggplot(aes(x = as_factor(item), y = item_score, color = group)) +
  geom_jitter(size = 2, alpha = 0.2) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 15) +
  facet_wrap(~group, nrow = 2) + 
  theme(legend.position = "none") +
  labs(
    x = NULL,
    y = "Score on the SUS item (reversed for even items)"
  ) +
  scale_color_viridis_d() +
  NULL
)
