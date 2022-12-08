#Exploratory data analysis for aggregated participant
library(here)
library(tidyverse)
library(papaja)
library(see)
library(RColorBrewer)  

source(here("data/study-2/src/01-wrangle.R"))

theme_set(theme_apa(box = TRUE))

s2.participants_no_expression <- s2.participants_aggregated |> 
  filter(
    num_emotions == 0
  ) |> 
  group_by(group) |>
  summarise(
    n = n()
  )

s2.valid_results_description <- s2.participants_aggregated |> 
  group_by(group) |> 
  summarise(
    `Number of students retained per class` = n(),
    `Number of students filling the expectancy survey` = sum(!is.na(Expectancy)),
    `Number of students filling the demo survey` = sum(!is.na(Demo)),
    `Number of students filling the halfway survey` = sum(!is.na(Halfway)),
    `Number of students filling the final survey` = sum(!is.na(Final))
  ) |> 
  ungroup() |>
  select(-group) |> 
  t()
