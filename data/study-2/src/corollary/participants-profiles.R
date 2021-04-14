library(here)
library(tidyverse)
library(directlabels)
library(papaja)

options(scipen = 999)
options(digits = 5)

theme_set(theme_apa(box = TRUE))

source(here("data/study-2/src/01-wrangle.R"))

s2.participant_split_expression <- s2.participants_aggregated %>% 
  filter(!is.na(Expectancy), !is.na(Final), !is.na(Demo), !is.na(Halfway)) %>% 
  arrange(num_emotions) %>% 
  top_n(2, Expectancy) %>% 
  mutate(
    type = "High overall Expectancy"
  ) %>% bind_rows(
    s2.participant_bottom_expression <- s2.participants_aggregated %>% 
      filter(!is.na(Final), !is.na(Demo), !is.na(Halfway)) %>% 
      top_n(-2, Expectancy) %>% 
      mutate(
        type = "Low overall Expectancy"
      )
  )
  

(s2.ea_usefulness.topexpress.graph <- s2.ea_usefulness %>% 
    filter(participant %in% s2.participant_split_expression$participant) %>%
    left_join(s2.participant_split_expression) %>% 
    ggplot(aes(x = as_factor(abbreviate(survey)), y = value, color = type, group = participant, label = num_emotions)) + 
    geom_point(size = 4) +
    geom_path(size = 2) +
    #geom_dl(aes(label = num_emotions), method = list("last.qp", cex = 1)) +
    facet_wrap(~dimension) +
    theme(legend.position="bottom", legend.box = "horizontal") +
    scale_color_viridis_d() +
    labs(
      x = "Survey",
      y = "Rating"
    ) +
    NULL)
