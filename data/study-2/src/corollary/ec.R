# Inference EC
library(here)
library(tidyverse)
library(papaja)
library(easystats)
library(rstanarm)

options(scipen = 999)
options(digits = 5)

theme_set(theme_apa(box = TRUE))

source(here("data/study-2/src/01-wrangle.R"))


s2.ec_vs_eau <- s2.participants_aggregated %>% 
  pivot_longer(
    cols = Expectancy:Final,
    names_to = "survey",
    values_to = "EAU"
  )

s2.ec_vs_eau$survey <- factor(s2.ec_vs_eau$survey, c("Expectancy", "Demo", "Halfway", "Final"))

s2.geco_score_to_eau.wider <- s2.geco_score %>%
  mutate(
    sub_competence = str_to_sentence(sub_competence),
  ) |> 
  pivot_wider(names_from = "sub_competence", values_from = "score") %>% 
  inner_join(s2.participants_aggregated)

s2.geco_score_means = s2.geco_score_to_eau.wider |> summarise(
  across(Recognition:Management, ~ mean(.x))
)
s2.geco_score_sd = s2.geco_score_to_eau.wider |> summarise(
  across(Recognition:Management, ~ sd(.x))
)


s2.bayesian.model.expectancy <- stan_glm(
  Expectancy ~ Recognition + Understanding + Regulation + Management,
  data = s2.geco_score_to_eau.wider,
  prior = normal(
    location = c(0.66, 0.7, 1.12, .45), 
    scale = c(0.13, 0.16, 0.22, 0.18), 
    autoscale = TRUE
  )
)

s2.bayesian.result.expectancy <- describe_posterior(
  s2.bayesian.model.expectancy, 
  test = c("p_direction", "rope", "bayesfactor"),
  centrality = "median",
  dispersion = FALSE,
  ci = 0.95
)
