# Wrangle raw et
library(tidyverse)
library(here)

# Import data
s1.raw_eyetracking_data <- read_delim(here("data/study-1/data/all-eyetracking-data.tsv"), 
                                  "\t", escape_double = FALSE, trim_ws = TRUE)

# Replace ParticipantName with RecordingName 
s1.raw_eyetracking_data <- s1.raw_eyetracking_data %>% 
  mutate(
    ParticipantName = RecordingName
  )

# Filter out participant with too many emotions or transitions
s1.raw_eyetracking_data <- s1.raw_eyetracking_data %>% 
  filter(
    !ParticipantName == "24o",
    !ParticipantName == "39o",
  )

names(s1.raw_eyetracking_data) <- str_replace_all(names(s1.raw_eyetracking_data), " ", "_")
names(s1.raw_eyetracking_data) <- str_replace_all(names(s1.raw_eyetracking_data), "\\[", "")
names(s1.raw_eyetracking_data) <- str_replace_all(names(s1.raw_eyetracking_data), "\\]", "")
names(s1.raw_eyetracking_data) <- str_replace_all(names(s1.raw_eyetracking_data), "AOI", "")
names(s1.raw_eyetracking_data) <- str_replace_all(names(s1.raw_eyetracking_data), "Hit", "")

s1.raw_eyetracking_data <- s1.raw_eyetracking_data %>%
  mutate_at(
    c("displaying", "displaying_1", "displaying_2", "monitoring", "monitoring_1", "monitoring_2", "task", "task_1", "task_2"),
    as.logical
  )

s1.raw_eyetracking_data <- s1.raw_eyetracking_data %>% 
  mutate(
    TrackLoss = FALSE,
    StudioTestName = str_replace(StudioTestName, "modalite_", "") %>% str_to_title(),
    displaying_aoi = case_when(
      !is.na(displaying) ~ displaying,
      !is.na(displaying_1) ~ displaying_1,
      TRUE ~ displaying_2
    ),
    monitoring_aoi = case_when(
      !is.na(monitoring) ~ monitoring,
      !is.na(monitoring_1) ~ monitoring_1,
      TRUE ~ monitoring_2
    ),
    task_aoi = case_when(
      !is.na(task) ~ task,
      !is.na(task_1) ~ task_1,
      TRUE ~ task_2
    )
  )
