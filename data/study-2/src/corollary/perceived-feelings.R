# Exploratory data analysis
library(here)
library(tidyverse)
library(papaja)
library(scales)

source(here("data/study-2/src/01-wrangle.R"))

theme_set(theme_apa(box = TRUE))

# Plot perceived individual vs. class feelings
s2.perceived_emotions_frequency.graph <- s2.perceived_emotions_frequency %>%
  filter(agent != "Observed") |> 
  ggplot(aes(x = agent, y = avg_frequency, color = agent)) +
  geom_hline(aes(yintercept = avg_frequency), color = "gray", alpha = 0.6, size = 1, data = s2.perceived_emotions_frequency |> filter(agent == "Observed")) + 
  stat_summary(fun = mean_cl_normal, geom = "errorbar", aes(group = agent, color = agent), width = 0.4) +
  stat_summary(fun = mean, geom = "point", aes(group = agent, color = agent), size = 3, shape = 15) +
  facet_wrap(~label_en) +
  expand_limits(y = c(1, 5)) +
  scale_y_continuous(breaks = 1:5, labels = c("Never", "Seldom", "Sometimes", "Often", "Very often")) +
  # scale_x_discrete(label = abbreviate) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(legend.position = "none") +
  scale_color_viridis_d() +
  NULL
