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

source(here("data/study-comparison/src/01-wrangle.R"))
source(here("data/utils/dew-utils.R"))

# Button vs. Other ----
sc.button_vs_other <- sc.dew_combined_emotions %>%
  filter(listed) %>% 
  group_by(condition, user, click) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  filter(n >= 5) %>%
  pivot_wider(names_from = "click", values_from = "n", values_fill = 0) %>%
  mutate(
    n = button + other,
    ratio = button / (button + other),
  )

# T-test ----
sc.button_vs_other.model <- t.test(
  x = sc.button_vs_other$button,
  y = sc.button_vs_other$other,
  paired = TRUE
)

sc.button_vs_other.descriptive <- sc.button_vs_other %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    mean = mean(ratio),
    sd = sd(ratio)
  ) %>% bind_rows(
    tibble(
      condition = "Total",
      n = nrow(sc.button_vs_other),
      mean = mean(sc.button_vs_other$ratio),
      sd = sd(sc.button_vs_other$ratio)
    )
  )

sc.button_vs_other.graph <- ggplot(sc.button_vs_other, aes(x = condition, y = ratio, color = condition)) +
  geom_jitter(alpha = 0.2, size = 2) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.3, position = position_dodge(width = 0.1)) +
  stat_summary(fun = mean, geom = "point", size = 6, shape = 15, position = position_dodge(width = 0.6)) +
  labs(
    x = NULL,
    y = "Frequency of clicks on one of the suggested feelings"
  ) +
  scale_color_viridis_d() +
  theme(
    legend.position = "none"
  ) +
  NULL

# Empirical observed slope ----
sc.empirical_feelings_disposition <- sc.dew_combined_emotions %>% 
  filter(listed, !is.na(observedSlope)) %>% 
  group_by(feeling) %>% 
  summarise(
    n = n(),
    valence = paste0(mean(x) %>% printnum(), " (", sd(x) %>% printnum(), ")"),
    control = paste0(mean(y) %>% printnum(), " (", sd(y) %>% printnum(), ")"),
    computed_slope = map2_dbl(mean(x), mean(y), dew.calculateSlope),
    expected_slope = first(expectedSlope),
    `|slope|` = abs(computed_slope - expected_slope)
  ) %>% 
  arrange(expected_slope) %>% 
  ungroup()

sc.empirical_feelings_disposition_circumplex <- sc.empirical_feelings_disposition %>% 
  mutate(
    pos_x = 0 - dew.getRadialY(computed_slope),
    pos_y = dew.getRadialX(computed_slope)
  )

sc.theoretical_feelings_disposition_circumplex <- sc.empirical_feelings_disposition %>% 
  mutate(
    pos_x = 0 - dew.getRadialY(expected_slope),
    pos_y = dew.getRadialX(expected_slope)
  )

sc.empirical_feelings_disposition_circumplex.graph <- ggplot(sc.empirical_feelings_disposition_circumplex, aes(x = pos_x, y = pos_y, label = feeling)) +
    geom_point(size = 7) +
    geom_text_repel(
      seed = 238769, box.padding = 0.9, 
      max.overlaps = Inf,
      position = position_nudge_repel(x = -20, y = -50),
      max.iter = 40000
    ) +
    labs(
      x = "Valence",
      y = "Control/Power"
    ) +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    ) +
    coord_fixed() +
    scale_x_reverse(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    ggtitle("Empirical/Observed Affective Space")

sc.theoretical_feelings_disposition_circumplex.graph <- ggplot(sc.theoretical_feelings_disposition_circumplex, aes(x = pos_x, y = pos_y, label = feeling)) +
    geom_point(size = 7) +
    geom_text_repel(
      seed = 238769, box.padding = 0.9, 
      max.overlaps = Inf,
      position = position_nudge_repel(x = -20, y = -50),
      max.iter = 40000
    ) +
    labs(
      x = "Valence",
      y = "Control/Power"
    ) +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    ) +
    coord_fixed() +
    scale_x_reverse(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    ggtitle("Theoretical/Expected Affective Space")

# Empirical disposition scater plot ----
sc.empirical_space_comparison.graph <- sc.dew_combined_emotions %>%
    filter(listed) |> 
    group_by(condition, feeling) %>% 
    summarise(
      mean_x = mean(x),
      mean_y = mean(y)
    ) %>% 
    ggplot(aes(x = mean_x, y = mean_y, label = feeling, shape = condition, color = condition)) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    geom_point(size = 5) +
    geom_text_repel(min.segment.length = 0, seed = 250, box.padding = 0.5, max.overlaps = Inf) +
    scale_x_continuous(limits = c(-100, 100)) +
    scale_y_continuous(limits = c(-100, 100)) +
    labs(x = "Observed mean of Valence", y = "Observed mean of Control/Power") +
    coord_fixed() +
    scale_colour_viridis_d() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    NULL