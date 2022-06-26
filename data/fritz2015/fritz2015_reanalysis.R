library(readr)
library(here)
library(tidyverse)
library(papaja)
library(afex)
library(performance)
library(report)
library(corrr)

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

fritz2015_eyetracking_filtered <- fritz2015_eyetracking_filtered |> 
  mutate(across(-c(X1, user), ~as.numeric(gsub("-", "", .))))

fritz2015_emotions_filtered <- fritz2015_emotions |> 
  filter(!user %in% fritz2015_excluded$user)

fritz2015_survey_filtered <- fritz2015_survey |> 
  filter(!user %in% fritz2015_excluded$user)

ux.appraisals <- fritz2015_emotions_filtered |> 
  group_by(user) |> 
  summarise(
    n = n(),
    cor = cor(x, y)
  )

ux.expressing <- fritz2015_emotions_filtered |> 
  group_by(user, click) |>
  summarise(
    n = n()
  ) |> pivot_wider(names_from = click, values_from = n, values_fill = 0) |> 
  ungroup() |> 
  mutate(
    tot = button + other + none
  )

ux.perceiving <- fritz2015_eyetracking_filtered |> 
  transmute(
    user = user,
    timeline_visits = Visit_Count_Timeline_Sum,
    timeline_duration = Total_Visit_Duration_Timeline_Sum,
    linechart_visits = Visit_Count_Graphique_1_Sum + Visit_Count_Graphique_2_Sum,
    linechart_duration = Total_Visit_Duration_Graphique_1_Sum + Total_Visit_Duration_Graphique_2_Sum
  )

ux.sus_scale_long <- fritz2015_survey_filtered |> 
  select(user, SUS1:SUS10) |> 
  pivot_longer(-user, names_to = "item", values_to = "evaluation") |> 
  mutate(
    item = factor(item, levels = c(
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
    )),
    order = as.numeric(str_remove(item, "SUS")),
    score = if_else(
      order %% 2 != 0,
      evaluation - 1,
      7 - evaluation
    )
  )

ux.sus_total <- ux.sus_scale_long |> 
  group_by(user) |> 
  summarise(
    score = sum(score) * 10/6
  )
