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

# Appraisal dimensions ----

s3.appraisal.descriptive.absolute <- s3.dew_combined_emotions %>% 
  summarise(
    Participants = n_distinct(user),
    Observations = n(),
    Valence = paste0(printnum(mean(x)), " (", printnum(sd(x)), ")"),
    `Control/Power` = paste0(printnum(mean(y)), " (", printnum(sd(y)), ")")
  ) %>% 
  ungroup() %>% 
  mutate(
    source = "Total"
  ) %>% 
  rename(
    Setting = source
  ) %>% 
  relocate(Setting, .before = Participants)

s3.appraisal.descripitve.comparison <- s3.dew_combined_emotions %>% 
  group_by(condition) %>% 
  summarise(
    Participants = n_distinct(user),
    Observations = n(),
    Valence = paste0(printnum(mean(x)), " (", printnum(sd(x)), ")"),
    `Control/Power` = paste0(printnum(mean(y)), " (", printnum(sd(y)), ")")
  ) %>% 
  ungroup() %>% 
  rename(
    Setting = condition
  ) %>% 
  arrange(Setting)

s3.appraisal.descriptive <- bind_rows(s3.appraisal.descripitve.comparison, s3.appraisal.descriptive.absolute)

s3.appraisal_sign_comparison <- s3.dew_combined_emotions %>% 
  mutate(
    Combination = case_when(
      ((x > 0 & y > 0) | (x < 0 & y < 0)) ~ "Same sign",
      ((x > 0 & y < 0) | (x < 0 & y > 0)) ~ "Opposite sign",
      TRUE ~ "Either or both neutral"
    )
  ) %>% 
  group_by(condition, Combination) %>% 
  summarise(
    Participants = n_distinct(user),
    Observations = n(),
  ) %>% 
  mutate(
    Proportion = Observations / sum(Observations)
  ) %>% 
  rename(
    Setting = condition
  ) %>% 
  arrange(
    Setting, desc(Combination)
  ) %>% 
  ungroup()

s3.appraisal_density.graph <- s3.dew_combined_emotions %>% 
    pivot_longer(cols = c(x,y), names_to = "dimension") %>%
    mutate(
      dimension = if_else(dimension == "x", "Valence", "Control/Power"),
      dimension = factor(dimension, levels = c("Valence", "Control/Power"))
    ) %>% 
    ggplot(aes(x = value, fill = condition)) +
    geom_density() +
    geom_vline(xintercept = 0, size = 2, color = "red", linetype = 2) +
    facet_grid(condition~dimension) +
    scale_fill_viridis_d() +
    theme(
      legend.position = "none"
    ) +
    labs(
      x = "Appraisal evaluation on sliders",
      y = "Density"
    ) +
    NULL

# Appraisal disposition ----
s3.appraisal_evaluation.graph <- s3.dew_combined_emotions %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "loess", formula = y ~ x, size = 2) +
    facet_wrap(~condition) +
    scale_color_viridis_d() +
    theme(
      legend.position = "right"
    ) +
    labs(
      x = "Valence",
      y = "Control/Power"
    ) +
    coord_fixed() +
    NULL

# Appraisal correlation ---

s3.appraisal_correlation.overall <- s3.dew_combined_emotions |> 
  group_by(user, condition) |> 
  summarise(
    N = n(),
    cor = cor(x, y)
  ) |> filter(
    N >= 2
  )

s3.appraisal_correlation.conditions <- s3.appraisal_correlation.overall |>
  group_by(condition) |> 
  summarise(
    N = n(),
    M = mean(cor, na.rm = TRUE),
    SD = sd(cor, na.rm = TRUE)
  )
