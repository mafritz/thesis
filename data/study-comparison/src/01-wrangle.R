# Import all DEW files
library(here)
library(tidyverse)
library(skimr)

source(here("data/fritz2015/fritz2015_reanalysis.R"))
source(here("data/study-1/src/01-wrangle.R"))
source(here("data/study-2/src/01-wrangle.R"))
source(here("data/utils/dew-utils.R"))

dew_fritz2015 <- fritz2015_emotions %>% 
  select(
    user, x, y, click, feeling, listed
  ) %>%
  mutate(label = "Already modified") %>% 
  rename(label_en = feeling) %>% 
  mutate(
    listed = as.logical(listed),
    source = "Usability Test",
    condition = "Synch/Collaborative"
  )

dew_s1 <- s1.dew_emotions %>% 
  select(
    user, x, y, click, label, listed
  ) %>%
  mutate(
    label = str_to_title(label) %>% str_replace("ée", "é")
  ) %>% 
  left_join(s1.feelings_translation, by = "label") %>% 
  mutate(
    source = "Experiment",
    condition = "Synch./Collab.",
    listed = if_else(!is.na(label_en), TRUE, FALSE)
  )

dew_s2 <- s2.expressed_emotions %>% 
  select(
    participant, x, y, click, label, listed  
  ) %>% 
  mutate(
    label = str_to_title(label) %>% str_replace("ée", "é")
  ) %>%
  rename(user = participant) %>% 
  left_join(s1.feelings_translation, by = "label") %>% 
  mutate(
    source = "Longitudinal",
    condition = "Asynch./Indiv.",
    listed = if_else(!is.na(label_en), TRUE, FALSE)
  )

sc.eatmint_circumplex <- s1.dew_configuration$circumplex$feelings %>% 
  left_join(s1.feelings_translation)

# Combine all expressed emotions
sc.dew_combined_emotions = bind_rows(dew_s1, dew_s2) %>% 
  rename(feeling = label_en) %>% 
  mutate(
    click = as_factor(click),
    observedSlope = map2_dbl(x, y, dew.calculateSlope),
    observedQuadrant = as_factor(ceiling(observedSlope / 90))
  ) %>% 
  left_join(sc.eatmint_circumplex, by = c("feeling" = "label_en")) %>% 
  select(-label.y) %>% 
  rename(
    expectedSlope = angle,
    original_feeling = label.x
  ) %>% 
  mutate(
    expectedQuadrant = as_factor(ceiling(expectedSlope/90)),
    slopeDifference = expectedSlope - observedSlope,
    slopeDifference = if_else(slopeDifference >= 180, 180 - slopeDifference, slopeDifference),
    absSlopeDifference = abs(slopeDifference)
  )

rm(dew_fritz2015, dew_s1, dew_s2)

# SUS Fritz 2015

synch_usability <- ux.sus_scale_long %>% 
  rename(participant = user) %>% 
  transmute(
    participant = as_factor(participant),
    source = "Synch./Collab.",
    item = as_factor(item),
    item_num = order,
    item_score = score
  )

asynch_usability <- s2.sus_score %>% 
  select(participant, item, value, item_num, item_score) %>% 
  mutate(
    item = as_factor(item),
    source = "Asynch./Indiv."
  )

sc.sus_scores <- bind_rows(synch_usability, asynch_usability)
sc.sus_scores$item <- factor(sc.sus_scores$item, levels = c(
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

rm(synch_usability, asynch_usability)
