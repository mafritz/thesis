# Wrangle transformed data

library(tidyverse)
library(here)

# Import DEW expressed emotions
s2.expressed_emotions <- read_csv(here("data/study-2/data/transformed/dew-emotions.csv"))

# Add factors
s2.expressed_emotions <- s2.expressed_emotions %>% 
  mutate(
    participant = as_factor(participant),
    group = as_factor(group),
    label = as_factor(label)
  )

# Import EAU ratings
s2.ea_usefulness <- read_csv(here("data/study-2/data/transformed/ea_usefulness_complete.csv"))

# Add factors
s2.ea_usefulness <- s2.ea_usefulness %>% 
  mutate(
    participant = as_factor(participant),
    group = as_factor(group),
    survey = factor(survey, levels = c("Expectancy", "Demo", "Halfway", "Final"))
  )

# Rename dimensions
s2.ea_dimension_labels <- c(
  "Frequency", 
  "Affordance",
  "Social Presence",
  "Self-Understanding",
  "Understanding Others",
  "Self-Other Comparison",
  "Self-Regulation"
)

levels(s2.ea_usefulness$dimension) <- s2.ea_dimension_labels

s2.ea_usefulness$dimension <- factor(s2.ea_usefulness$dimension, s2.ea_dimension_labels)

# Import perceived emotions frequency
s2.perceived_emotions_frequency <- read_csv(here("data/study-2/data/transformed/perceived_emotions_frequency.csv"))

# Add factors
s2.perceived_emotions_frequency <- s2.perceived_emotions_frequency %>% 
  mutate(
    label_fr = as_factor(label_fr),
    label_en = as_factor(label_en),
    agent = factor(agent, c("Self", "Class", "Observed"))
  )

# Import SUS score
s2.sus_score <- read_csv(here("data/study-2/data/transformed/sus_score.csv"))

# Add factors
s2.sus_score <- s2.sus_score %>% 
  mutate(
    participant = as_factor(participant),
    group = as_factor(group)
  )

# Import GECo score
s2.geco_score <- read_csv(here("data/study-2/data/transformed/geco_score.csv"))

# Add factor
s2.geco_score$sub_competence <- factor(s2.geco_score$sub_competence, c("recognition", "understanding", "regulation", "management"))

# Import feelings translation
s2.feelings_translation <- read_csv(here("data/study-1/data/feelings_translation.csv"))

# Import participants
s2.participants_gender_age <- read_csv(here("data/study-2/data/transformed/participants.csv"))

# Aggregate participants relevant data
s2.participants_aggregated_all <- s2.ea_usefulness %>% 
  group_by(
    participant, group, survey
  ) %>% 
  summarise(
    avg_eau = mean(value)
  ) %>%
  ungroup() %>% 
  pivot_wider(
    names_from = survey,
    values_from = avg_eau
  ) %>% 
  mutate(
    participant = as_factor(participant)
  )

# Excluded participants
s2.excluded_participants <- s2.participants_aggregated_all %>% 
  filter(is.na(Halfway), is.na(Final)) %>% pull(participant)

# Filter out the excluded participants from aggregated
s2.participants_aggregated <- s2.participants_aggregated_all %>% 
  filter(!participant %in% s2.excluded_participants)

# Filter out from ea_usefulness
s2.ea_usefulness <- s2.ea_usefulness %>%
  filter(!participant %in% s2.excluded_participants)

# Filter out from GECo score
s2.geco_score <- s2.geco_score %>% 
  filter(!participant %in% s2.excluded_participants)

# Count expressed emotions per participant
s2.participants_expressed_emotions <- s2.expressed_emotions %>% 
  group_by(participant) %>% 
  summarise(
    num_emotions = n()
  )

# Integrate count with aggregated participants
s2.participants_aggregated <- s2.participants_aggregated %>% 
  left_join(s2.participants_expressed_emotions, by = "participant") %>% 
  mutate(
    num_emotions = replace_na(num_emotions, 0)
  )

# Filter our from expressed emotions
s2.expressed_emotions <- s2.expressed_emotions %>% 
  filter(!participant %in% s2.excluded_participants)

# Add cumulative count to expressed emotions based on group
s2.expressed_emotions <- s2.expressed_emotions %>% 
  group_by(group) %>% 
  mutate(
    cum_n = rank(time)
  ) %>% 
  ungroup()

# Calculate GECo score and add it to aggregated participants
s2.geco_total <- s2.geco_score %>% 
  group_by(participant) %>% 
  summarise(
    geco_total = mean(score)
  ) %>% 
  ungroup()

# Add GECo total to aggregated participant
s2.participants_aggregated <- s2.participants_aggregated %>% 
  left_join(s2.geco_total, by = "participant")

# Filter out from SUS score
s2.sus_score <- s2.sus_score %>% 
  filter(!participant %in% s2.excluded_participants)

# Calculate SUS score
s2.sus_total <- s2.sus_score %>% 
  group_by(participant) %>%
  summarise(
    sus_score = sum(item_score) * 10/6
  ) %>% 
  ungroup()

# Add SUS total to aggregated participant
s2.participants_aggregated <- s2.participants_aggregated %>% 
  left_join(s2.sus_total, by = "participant")
