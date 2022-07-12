library(here)
library(tidyverse)
library(papaja)
library(ggrepel)
library(jsonlite)
library(ggforce)
library(patchwork)
library(afex)
library(emmeans)

theme_set(theme_apa(box = TRUE))

source(here("data/study-3/src/01-wrangle.R"))
source(here("data/utils/dew-utils.R"))

# SUS comparison ----
s3.sus_overall <- s3.sus_scores %>% 
  group_by(participant) %>% 
  summarise(
    sus_score = sum(item_score) * 10 / 6
  ) %>% 
  ungroup() %>%
  summarise(
    source = "Total",
    n = n(),
    mean = mean(sus_score),
    sd = sd(sus_score)
  )

s3.sus_comparison <- s3.sus_scores %>% 
  group_by(source, participant) %>% 
  summarise(
    sus_score = sum(item_score) * 10 / 6
  ) %>% 
  ungroup() %>% 
  group_by(source) %>% 
  summarise(
    n = n(),
    mean = mean(sus_score),
    sd = sd(sus_score)
  ) %>% 
  bind_rows(s3.sus_overall)

s3.sus_items_benchmark <- tibble(
  item = paste0("SUS", 1:10),
  value = c(3.80, 5 - 1.85, 4.24, 5 - 1.51, 3.96, 5 - 1.77, 4.19, 5 - 1.66, 4.25, 5 - 1.64) * 1.4
)

s3.sus_items.graph <- s3.sus_scores %>% ggplot(aes(x = source, y = item_score, color = source)) +
    geom_hline(data = s3.sus_items_benchmark, aes(yintercept = value), size = 2, alpha = 0.4) +
    geom_jitter(alpha = 0.2, size = 2) +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.3, position = position_dodge(width = 0.1)) +
    stat_summary(fun = mean, geom = "point", size = 3, shape = 15, position = position_dodge(width = 0.6)) +
    facet_wrap(.~as_factor(item), nrow = 2, labeller = labeller(s3.sus_scores$item)) +
    labs(
      x = NULL,
      y = "System Usability Scale computed score"
    ) +
    scale_color_viridis_d() +
    scale_x_discrete(label = abbreviate) +
    scale_y_continuous(limits = c(1, 7), breaks = c(1, 3, 5, 7)) +
    theme(
      legend.position = "none"
    ) +
    NULL