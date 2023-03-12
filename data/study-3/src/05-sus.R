library(here)
library(tidyverse)
library(papaja)
library(see)
library(RColorBrewer)  
library(ggrepel)
library(jsonlite)
library(ggforce)
library(patchwork)
library(afex)
library(emmeans)

theme_set(theme_apa(box = TRUE))

source(here("data/study-3/src/01-wrangle.R"))
source(here("data/utils/dew-utils.R"))


# SUS items ---------------------------------------------------------------

# SUS comparison ----
s3.sus_overall <- s3.sus_scores |> 
  group_by(participant) |> 
  summarise(
    sus_score = sum(item_score) * 10 / 6
  ) |> 
  ungroup() |>
  summarise(
    source = "Total",
    n = n(),
    mean = mean(sus_score),
    sd = sd(sus_score)
  )

s3.sus_scores_by_item <- s3.sus_scores |>
  mutate(
    item_score = item_score + 1
  ) |> 
  group_by(item, source) |> 
  summarise(
    n = n(),
    mean = mean(item_score),
    sd = sd(item_score)
  )

s3.sus_comparison <- s3.sus_scores |> 
  group_by(source, participant) |> 
  summarise(
    sus_score = sum(item_score) * 10 / 6
  ) |> 
  ungroup() |> 
  group_by(source) |> 
  summarise(
    n = n(),
    mean = mean(sus_score),
    sd = sd(sus_score)
  ) |> 
  bind_rows(s3.sus_overall)

s3.sus_items_benchmark <- tibble(
  item = paste0("SUS", 1:10),
  value = c(3.80, 5 - 1.85, 4.24, 5 - 1.51, 3.96, 5 - 1.77, 4.19, 5 - 1.66, 4.25, 5 - 1.64) * 1.4
)

s3.sus_table <- tribble(
  ~item, ~label, ~ai, ~sc, ~bm,
  "SUS1", "I think that I would like to use this system frequently", "2.58 (1.27)", "3.86 (2.07)", "5.32",
  "SUS2", "I found the system unnecessarily complex", "5.77 (1.45)", "5.43 (1.55)", "4.41",
  "SUS3", "I thought the system was easy to use", "5.88 (1.66)", "5.43 (1.55)", "5.94",
  "SUS4", "I think that I would need the support of a technical person to be able to use this system", "6.35 (1.41)", "5.93 (1.54)", "4.89",
  "SUS5", "I found the various functions in this system were well integrated", "4.65 (1.52)", "5.43 (1.02)", "5.54",
  "SUS6", "I thought there was too much inconsistency in this system", "6.27 (0.96)", "6.14 (1.51)", "4.52",
  "SUS7", "I would imagine that most people would learn to use this system very quickly", "5.50 (1.84)", "5.57 (1.40)", "5.87",
  "SUS8", "I found the system very cumbersome to use", "5.58 (1.58)", "6.07 (1.73)", "4.68",
  "SUS9", "I felt very confident using the system", "4.38 (1.68)", "4.36 (1.15)", "5.95",
  "SUS10", "I needed to learn a lot of things before I could get going with this system", "6.73 (0.45)", "6.71 (0.61)", "4.70"
)

s3.sus_items.graph <- s3.sus_scores |> 
  mutate(
    item_score = item_score + 1
  ) |> 
  ggplot(aes(x = source, y = item_score, color = source)) +
    geom_hline(data = s3.sus_items_benchmark, aes(yintercept = value), size = 2, alpha = 0.4) +
    geom_jitter(alpha = 0.2, size = 2) +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.3, position = position_dodge(width = 0.1)) +
    stat_summary(fun = mean, geom = "point", size = 3, shape = 15, position = position_dodge(width = 0.6)) +
    facet_wrap(.~as_factor(item), nrow = 2, labeller = labeller(s3.sus_scores$item)) +
    labs(
      x = NULL,
      y = "System Usability Scale computed score"
    ) +
    scale_colour_brewer(palette = "Dark2") +
    scale_x_discrete(label = abbreviate) +
    scale_y_continuous(limits = c(1, 7), breaks = c(1, 3, 5, 7)) +
    theme(
      legend.position = "none"
    ) +
    NULL
