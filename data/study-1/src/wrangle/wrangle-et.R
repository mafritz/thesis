# Import and wrangle Eye-tracking data
library(here)
library(tidyverse)
library(papaja)
library(see)
library(RColorBrewer)  

s1.et_data <- read_csv(here("data/study-1/data/et-data.csv"))

# Filter outlier with 62 emotions
s1.et_data <- s1.et_data |>
  filter(user != "24o")

# Change group to factor and order them
s1.et_data$group <- factor(
  s1.et_data$group,
  levels = c(0, 1, 2),
  labels = c("Self", "Partner", "Mutual")
)

s1.et_data_tidy <- s1.et_data |>
  gather(
    key = "measure",
    value = "value",
    Fixation_Duration_DISPLAY_N:Visit_Count_TASK_Mean,
    na.rm = TRUE,
    factor_key = FALSE
  )

# Retrieve AOI and Stat type
s1.et_data_tidy <- s1.et_data_tidy |>
  mutate(measure = str_replace(measure, "Total_", "Total ")) |>
  separate(measure, c("mouvement", "supplement", "aoi", "stat"), sep = "_") |>
  unite("measure", mouvement, supplement, sep = " ")

# Change the name of the AOI
s1.et_data_tidy <- s1.et_data_tidy |>
  mutate(
    aoi = str_to_sentence(aoi)
  )
