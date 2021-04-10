library(here)
library(tidyverse)
library(lmerTest)
library(papaja)

theme_set(theme_apa(box = TRUE))

options(scipen = 999)
options(digits = 5)

source(here("data/study-1/src/01-wrangle.R"))

s1.feelings_over_time_graph <- s1.dew_emotions %>%
  filter(label %in% s1.dew_configuration$circumplex$feelings$label, !is.na(label_en)) %>%
  mutate(minutes = elapsedTime / 1000 / 60) %>%
  ggplot(aes(x = minutes, y = 0, color = group)) +
    geom_jitter(width = .0, height = 5) +
    facet_wrap(~label_en) +
    theme(axis.text.y = element_blank()) +
    ylab(NULL) +
    xlab("Elapsed time in minutes") +
    scale_y_discrete(breaks = NULL) +
    theme(
      legend.position = "none"
    ) +
  scale_color_viridis_d() +
  NULL
