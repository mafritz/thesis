library(here)
library(tidyverse)
library(papaja)
library(ggrepel)
library(jsonlite)
library(ggforce)
library(patchwork)

theme_set(theme_apa(box = TRUE))

source(here("data/study-comparison/src/01-wrangle.R"))
source(here("data/utils/dew-utils.R"))

# Allocation between sources
sc.emotions_allocation <- sc.dew_combined_emotions |>
  mutate(
    Dataset = factor(source, levels = c("Usability Test", "Chapter 7", "Chapter 8"))
  ) |> 
  group_by(Dataset) |> 
  summarise(
    Setting = first(condition),
    Participants = n_distinct(user),
    Observations = n()
  )

# Appraisal dimensions

sc.appraisal.descriptive.absolute <- sc.dew_combined_emotions %>% 
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

sc.appraisal.descripitve.comparison <- sc.dew_combined_emotions %>% 
  group_by(source) %>% 
  summarise(
    Participants = n_distinct(user),
    Observations = n(),
    Valence = paste0(printnum(mean(x)), " (", printnum(sd(x)), ")"),
    `Control/Power` = paste0(printnum(mean(y)), " (", printnum(sd(y)), ")")
  ) %>% 
  ungroup() %>% 
  mutate(
    source = if_else(source == "Experiment", "Synch./Collab.", "Asynch./Indiv.")
  ) %>% 
  rename(
    Setting = source
  ) %>% 
  arrange(Setting)

sc.appraisal.descriptive <- bind_rows(sc.appraisal.descripitve.comparison, sc.appraisal.descriptive.absolute)

sc.appraisal_sign_comparison <- sc.dew_combined_emotions %>% 
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

(sc.appraisal_density.graph <- sc.dew_combined_emotions %>% 
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
    NULL)

# Appraisal disposition
(sc.appraisal_evaluation.graph <- sc.dew_combined_emotions %>% 
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
)

# Listed vs. not listed
sc.listed_vs_not_listed.absolute <- sc.dew_combined_emotions %>% 
  group_by(listed) %>% 
  summarise(
    n = n()
  ) %>% 
  ungroup() %>% 
  mutate(
    Setting = "Total",
    listed = if_else(listed, "Yes", "No")
  ) %>% 
  pivot_wider(names_from = listed, values_from = n) %>%
  mutate(Observations = Yes + No) %>% 
  mutate(`% Listed` = Yes / (Yes + No)) %>% 
  relocate(Observations, .before = No) %>% 
  rename(
    Listed = Yes,
    `Not Listed` = No
  )

sc.listed_vs_not_listed.comparison <- sc.dew_combined_emotions %>% 
  group_by(condition, listed) %>% 
  summarise(
    n = n()
  ) %>% 
  ungroup() %>% 
  mutate(
    listed = if_else(listed, "Yes", "No")
  ) %>% 
  pivot_wider(names_from = listed, values_from = n) %>%
  mutate(Observations = Yes + No) %>% 
  mutate(`% Listed` = Yes / (Yes + No)) %>% 
  relocate(Observations, .before = No) %>% 
  rename(
    Setting = condition,
    Listed = Yes,
    `Not Listed` = No
  )

sc.listed_vs_not_listed <- bind_rows(sc.listed_vs_not_listed.comparison, sc.listed_vs_not_listed.absolute)

sc.not_listed_labels <- sc.dew_combined_emotions %>% 
  filter(!listed) %>%
  mutate(
    original_feeling = str_replace(original_feeling, "ée", "é")
  ) %>% 
  group_by(condition) %>% 
  distinct(original_feeling) %>% 
  ungroup()

sc.not_listed_cumulated <- sc.not_listed_labels %>%
  group_by(condition) %>% 
  summarise(
    n = n()
  )

# Compared listed feelings frequency
sc.listed_feelings_frequency.absolute <- sc.dew_combined_emotions %>% 
  filter(listed) %>%
  left_join(sc.eatmint_circumplex, by = c("feeling" = "label_en")) %>% 
  group_by(feeling) %>%
  summarise(
    n = n(),
    quadrant = ceiling(first(angle) / 90) %>% as.roman(),
    angle = first(angle)
  ) %>%
  mutate(Total = n / sum(n) * 100) %>%
  ungroup() %>% 
  arrange(angle)


sc.listed_feelings_frequency.comparison <- sc.dew_combined_emotions %>% 
  filter(listed) %>%
  left_join(sc.eatmint_circumplex, by = c("feeling" = "label_en")) %>% 
  group_by(condition, feeling) %>%
  summarise(
    n = n(),
    quadrant = ceiling(first(angle) / 90) %>% as.roman()
  ) %>%
  mutate(freq = n / sum(n) * 100) %>%
  ungroup() %>% 
  select(-n) %>% 
  pivot_wider(names_from = condition, values_from = freq, values_fill = 0) %>% 
  mutate(`|Difference|` = abs(.[[3]] - .[[4]])) %>% 
  arrange(desc(`|Difference|`))

sc.listed_feelings_frequency <- left_join(
  sc.listed_feelings_frequency.absolute, 
  sc.listed_feelings_frequency.comparison, by = c("feeling", "quadrant")
) %>% 
  select(-c(n, angle)) %>% 
  rename(Feeling = feeling, Quadrant = quadrant)

# Button vs. Other
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

# Empirical observed slope
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

(sc.empirical_feelings_disposition_circumplex.graph <- ggplot(sc.empirical_feelings_disposition_circumplex, aes(x = pos_x, y = pos_y, label = feeling)) +
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
  ggtitle("Empirical/Observed Affective Space"))

(sc.theoretical_feelings_disposition_circumplex.graph <- ggplot(sc.theoretical_feelings_disposition_circumplex, aes(x = pos_x, y = pos_y, label = feeling)) +
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
  ggtitle("Theoretical/Expected Affective Space"))

# Empirical disposition scater plot
(sc.empirical_space_comparison.graph <- sc.dew_combined_emotions %>%
  mutate(
    condition = if_else(source == "Experiment", "Synch./Collab.", "Asynch./Indiv.")
  ) %>% 
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
  NULL)


# SUS comparison
sc.sus_overall <- sc.sus_scores %>% 
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

sc.sus_comparison <- sc.sus_scores %>% 
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
  bind_rows(sc.sus_overall)

sc.sus_items_benchmark <- tibble(
  item = paste0("SUS", 1:10),
  value = c(3.80, 5 - 1.85, 4.24, 5 - 1.51, 3.96, 5 - 1.77, 4.19, 5 - 1.66, 4.25, 5 - 1.64) * 1.4
)

(sc.sus_items.graph <- sc.sus_scores %>% ggplot(aes(x = source, y = item_score, color = source)) +
  geom_hline(data = sc.sus_items_benchmark, aes(yintercept = value), size = 2, alpha = 0.4) +
  geom_jitter(alpha = 0.2, size = 2) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.3, position = position_dodge(width = 0.1)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 15, position = position_dodge(width = 0.6)) +
  facet_wrap(.~as_factor(item), nrow = 2, labeller = labeller(sc.sus_scores$item)) +
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
  NULL)

