#Exploratory data analysis 
library(tidyverse)
library(papaja)
library(see)
library(here)
library(ggrepel)

source(here("data/study-2/src/01-wrangle.R"))

theme_set(theme_apa(box = TRUE))


# Compare frequency of subjective feelings
s2.expressed_emotions |>
  left_join(s2.feelings_translation, by = "label") |> 
  filter(listed) |>
  ggplot(aes(x = fct_rev(label_en), fill = group)) +
  geom_bar() +
  scale_fill_oi(palette = "black_first") +
  coord_flip() +
  labs(
    x = "Subjective feeling",
    y = "Count",
    fill = NULL
  )

# Cumulative emotions over time
s2.graph_expressed_emotions_over_time <- ggplot(s2.expressed_emotions, aes(x = abstime, y = cum_n, color = group)) +
  geom_line(size = 2) +
  labs(
    x = NULL,
    y = "Cumulative number of emotions expressed",
    color = NULL
  ) +
  scale_color_okabeito(palette = "black_first") +
  theme(
    legend.position = "right"
  ) +
  NULL

# Scatterplot representation
s2.expressed_emotions |> 
  filter(listed) |> 
  group_by(group, label) |> 
  summarise(
    mean_valence = mean(x),
    mean_control = mean(y),
    n = n()
  ) |> 
  inner_join(s2.feelings_translation, by = "label") |> 
  ggplot(aes(x = mean_valence, y = mean_control, label = label_en)) +
    geom_point() +
    geom_label_repel(
      force = 10,
      max.iter = 5000
    ) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    labs(
      x = "Average value of Valence",
      y = "Average value of Control/Power",
      size = "Observations",
      color = "Class"
    ) +
    scale_x_continuous(limits = c(-100, 100)) +
    scale_y_continuous(limits = c(-100, 100)) +
    scale_color_okabeito(palette = "black_first") +
    facet_wrap(~group) +
    theme(
      legend.position = "bottom"
    ) +
    NULL

# Compare number of emotions expressed
s2.graph_expressed_emotions_boxplot <- s2.expressed_emotions |> 
  group_by(participant, group) |> 
  summarise(
    count = n()
  ) |> 
  ggplot(aes(x = group, y = count, fill = group)) +
  geom_boxplot() +
  scale_fill_oi(palette = "black_first") +
  labs(
    x = NULL,
    y = "Emotions expressed"
  ) +
  theme(
    legend.position = "none"
  ) +
  NULL
