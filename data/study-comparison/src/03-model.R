library(here)
library(tidyverse)
library(papaja)
library(ggrepel)
library(jsonlite)
library(ggforce)
library(patchwork)
library(afex)
library(emmeans)
library(easystats)
library(rstanarm)

theme_set(theme_apa(box = TRUE))

source(here("data/study-comparison/src/01-wrangle.R"))
source(here("data/utils/dew-utils.R"))

# Appraisal correlation ---

sc.appraisal_correlation.overall <- sc.dew_combined_emotions |> 
  group_by(user, condition) |> 
  summarise(
    N = n(),
    cor = cor(x, y)
  ) |> filter(
    N >= 2
  )

sc.appraisal_correlation.conditions <- sc.appraisal_correlation.overall |>
  group_by(condition) |> 
  summarise(
    N = n(),
    M = mean(cor, na.rm = TRUE),
    SD = sd(cor, na.rm = TRUE)
  )
