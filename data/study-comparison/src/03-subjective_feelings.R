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

# Listed vs. not listed ----
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

# Compared listed feelings frequency ----
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