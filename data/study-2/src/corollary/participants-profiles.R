library(here)
library(tidyverse)
library(directlabels)

options(scipen = 999)
options(digits = 5)

s2.participant_plus_20_emotions <- s2.participants_aggregated %>% 
  filter(num_emotions >= 20, !is.na(Expectancy), !is.na(Final))

(s2.ea_usefulness.topexpress.graph <- s2.ea_usefulness %>% 
    filter(participant %in% s2.participant_plus_20_emotions$participant) %>%
    left_join(s2.participant_plus_20_emotions) %>% 
    ggplot(aes(x = as_factor(abbreviate(survey)), y = value, color = dimension, group = dimension, label = num_emotions)) + 
    geom_line(size = 2) +
    geom_point(size = 4) +
    geom_dl(aes(label = num_emotions), method = list(dl.combine("last.points")), cex = 20) +
    facet_wrap(~participant) +
    theme(legend.position = "none") +
    scale_color_viridis_d() +
    labs(
      x = "Survey",
      y = "Rating"
    ) +
    NULL)