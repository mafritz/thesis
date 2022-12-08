# Import and wrangle data from the DEW
library(here)
library(tidyverse)
library(jsonlite)
library(papaja)
library(see)
library(RColorBrewer)  
library(afex)
library(emmeans)

# Import data
s1.dew_emotions <- read_csv(here("data/study-1/data/dew-emotions.csv"))

# Load DEW configuration
s1.dew_configuration <- fromJSON(here("data/study-1/data/dew-configuration.json"))

# Import english translation of feelings in the circumplex
s1.feelings_translation <- read_csv(here("data/study-1/data/feelings_translation.csv"))
# Load ET data for cross-referncing
source(here("data/study-1/src/wrangle/wrangle-et.R"))

# Modify the listed to logical
s1.dew_emotions <- s1.dew_emotions |>
  mutate(
    listed = if_else(listed == "VERO", TRUE, FALSE)
  )


# Add the experimental condition
s1.dew_emotions <- s1.dew_emotions |> 
  mutate(
    group = case_when(
      str_detect(user, "s") ~ "Self",
      str_detect(user, "o") ~ "Partner",
      str_detect(user, "b") ~ "Mutual",
      TRUE ~ NA_character_
    )
  )
# Factor group in aggregated data
s1.dew_emotions$group <- factor(s1.dew_emotions$group, levels = c("Self", "Partner", "Mutual"))

# Add the enigma number and set to 1199999 the few cases registered some milliseconds after the 20 minutes end
s1.dew_emotions <- s1.dew_emotions |>
  mutate(
    elapsedTime = if_else(elapsedTime > 1199999, 1199999, elapsedTime),
    enigma = ceiling(elapsedTime / (60 * 1000 * 5)),
    elapsedMinutes = elapsedTime / (60 * 1000)
  )

# Modify french ée to é to uniform gender
s1.dew_emotions <- s1.dew_emotions |>
  mutate(
    label = str_replace(label, "ée", "é")
  )

# Recode some labels for consistency (e.g. french accents, -ée to -é, etc.)
s1.dew_emotions$label <- str_replace(
  s1.dew_emotions$label,
  pattern = "Énervé",
  replacement = "Enervé"
)

# Check for switch from not listed to listed in the circumplex after recoding
s1.dew_emotions <- s1.dew_emotions |>
  mutate(
    listed = if_else(label %in% s1.dew_configuration$circumplex$feelings$label, TRUE, FALSE)
  )

# Add english translation
s1.dew_emotions <- s1.dew_emotions |> 
  left_join(s1.feelings_translation, by = "label")

# # Makes all labels same format
# s1.dew_emotions <- s1.dew_emotions |>
#   mutate(
#     label = str_to_sentence(label),
#     label_en = str_to_sentence(label_en)
#   )

# Filter outlier with 62 expressed emotions and cross-reference with ET data
s1.dew_emotions <- s1.dew_emotions |>
  filter(
    !user == "24o",
    user %in% s1.et_data$user
  )

# Define the time according to the enigma and the corresponding situation (reading, solving, solution)
s1.dew_emotions <- s1.dew_emotions |>
  mutate(
    enigma_time = elapsedTime - ((enigma - 1) * 1000 * 60 * 5),
    enigma_situation = case_when(
      enigma_time < 1000 * 40 ~ "Reading",
      enigma_time < 1000 * 60 * 4 ~ "Solving",
      TRUE ~ "Solution"
    )
  )

# Order factors for the situation in the enigma
s1.dew_emotions$enigma_situation <- factor(s1.dew_emotions$enigma_situation, levels = c("Reading", "Solving", "Solution"))

# Aggregate data per user
s1.aggregated_emotions <- s1.dew_emotions |>
  group_by(user) |>
  summarise(
    n = n(),
  ) |>
  mutate(
    group = case_when(
      str_detect(user, "s") ~ "Self",
      str_detect(user, "o") ~ "Partner",
      str_detect(user, "b") ~ "Mutual",
      TRUE ~ NA_character_
    )
  )

# Filter by users that have ET data
s1.aggregated_emotions <- s1.aggregated_emotions |>
  filter(user %in% s1.et_data$user)

# Factor group in aggregated data
s1.aggregated_emotions$group <- factor(s1.aggregated_emotions$group, levels = c("Self", "Partner", "Mutual"))
