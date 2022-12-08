#Exploratory data analysis for EC
library(here)
library(tidyverse)
library(papaja)
library(see)
library(RColorBrewer)  

theme_set(theme_apa(box = TRUE))

source(here("data/study-2/src/01-wrangle.R"))

ggplot(s2.geco_score, aes(x = sub_competence, y = score, color = sub_competence)) +
  geom_jitter() +
  theme(
    legend.position = "none"
  ) +
  scale_colour_brewer(palette = "Dark2") +
  NULL

ggplot(s2.participants_aggregated, aes(x = geco_total, y = num_emotions)) +
  geom_point()

ggplot(s2.participants_aggregated, aes(x = geco_total, y = Expectancy)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  labs(
    x = "GECo total score",
    y = "EAU expectancy"
  ) +
  NULL
