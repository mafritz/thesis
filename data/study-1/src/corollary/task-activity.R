library(here)
library(tidyverse)
library(lmerTest)
library(papaja)
library(see)
library(RColorBrewer)  

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
    observed = mean(N)
  ) |>
  mutate(
    projected = case_when(
      enigma_situation == "Reading" ~ (observed / 40) * 300,
      enigma_situation == "Solving" ~ (observed / 200) * 300,
      TRUE ~ (observed / 60) * 300
    )
  ) |> 
  arrange(enigma_situation) |> 
  pivot_longer(
    -c(group, enigma_situation)
  ) |> 
  mutate(
    name = str_to_sentence(name)
  )

s1.emotion_per_task_activity.graph <- ggplot(s1.emotion_per_task_activity, aes(x = group, y = value, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.5, preserve = "single", padding = 0.5)) +
  facet_grid(name~enigma_situation) +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none") +
  labs(x = NULL, y = "Number of expressed emotions") +
  geom_hline(yintercept = 13.8, color = "gray", size = 2, alpha = 0.7) +
  NULL
