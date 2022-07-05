library(here)
library(tidyverse)
library(lmerTest)
library(papaja)

theme_set(theme_apa(box = TRUE))

options(scipen = 999)
options(digits = 5)

source(here("data/study-1/src/01-wrangle.R"))

s1.emotion_per_task_activity <- s1.dew_emotions |> 
  group_by(user, enigma_situation) |> 
  summarise(
    N = n(),
    group = first(group)
  ) |> 
  ungroup() |> 
  group_by(group, enigma_situation) |> 
  summarise(
    M = mean(N),
    SD = sd(N)
  ) |>
  mutate(
    Projection = case_when(
      enigma_situation == "Reading" ~ (M / 40) * 300,
      enigma_situation == "Solving" ~ (M / 200) * 300,
      TRUE ~ (M / 60) * 300
    )
  ) |> 
  arrange(enigma_situation)

s1.emotion_per_task_activity.graph <- ggplot(s1.emotion_per_task_activity, aes(x = group, y = Projection, fill = group)) +
  geom_bar(stat = "identity") +
  facet_wrap(~enigma_situation) +
  scale_fill_viridis_d() +
  theme(legend.position = "none") +
  labs(x = NULL, y = "Projected number of expressed emotions") +
  geom_hline(yintercept = 13.8, color = "gray", size = 2, alpha = 0.7) +
  NULL