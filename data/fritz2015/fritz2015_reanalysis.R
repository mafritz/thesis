library(readr)
library(here)
library(tidyverse)
library(papaja)
library(afex)
library(performance)
library(report)

fritz2015_survey <- read_delim(here::here("data/fritz2015/fritz2015_survey.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE, col_types = cols(Code = col_character())) |> 
  mutate(
    user = paste0("P", Code)
  )
fritz2015_emotions <- read_csv(here("data/study-1/data/fritz2015_emotions.csv"))
fritz2015_eyetracking <- read_csv(here("data/study-1/data/fritz2015_eyetracking.csv")) |> 
  left_join(fritz2015_survey, by = c("user" = "Code"))

fritz2015_excluded <- fritz2015_eyetracking |> 
  filter(
    Total_Visit_Duration_TASK_Sum < 3 * 60
  )

fritz2015_eyetracking_filtered <- fritz2015_eyetracking |> 
  filter(!user %in% fritz2015_excluded$user)

fritz2015_emotions_filtered <- fritz2015_emotions |> 
  filter(!user %in% fritz2015_excluded$user)

fritz2015_survey_filtered <- fritz2015_survey |> 
  filter(!user %in% fritz2015_excluded$user)


ux.expressing <- fritz2015_emotions_filtered |> 
  group_by(user, click) |>
  summarise(
    n = n()
  ) |> pivot_wider(names_from = click, values_from = n, values_fill = 0) |> 
  ungroup() |> 
  mutate(
    tot = button + other + none
  )

ux.click_type.test <- t.test(
  ux.expressing$button,
  ux.expressing$other,
  alternative = "greater",
  paired = "TRUE",
)

ux.displaying <- fritz2015_eyetracking_filtered |> 
  select(user, Total_Visit_Duration_Displaying_Sum, Mouse_Click_Count_Displaying_Sum, et_sample) |> 
  left_join(fritz2015_survey, by = "user") |> 
  transmute(
    user = user,
    et_sample = et_sample,
    TotEmo = TotEmo,
    duration = Total_Visit_Duration_Displaying_Sum,
    time_per_emotion = duration / TotEmo,
    numer_click = Mouse_Click_Count_Displaying_Sum / TotEmo
  )

ux.expressing_not_listed <- fritz2015_emotions_filtered |> filter(listed == 0, feeling != "None")
