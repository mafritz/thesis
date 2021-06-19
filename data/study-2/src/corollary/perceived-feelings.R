# Exploratory data analysis
library(here)
library(tidyverse)
library(papaja)
library(scales)

source(here("data/study-2/src/01-wrangle.R"))

theme_set(theme_apa(box = TRUE))

# Plot perceived individual vs. class feelings
s2.perceived_emotions_frequency.graph <- s2.perceived_emotions_frequency %>%
  mutate(
    avg_frequency = if_else((agent == "Observed"), rescale(avg_frequency, to = c(1,5)), avg_frequency),
    sd_frequency = if_else((agent == "Observed"), rescale(sd_frequency, to = c(1,5)), sd_frequency),
    lower.ci = if_else((agent == "Observed"), rescale(lower.ci, to = c(1,5)), lower.ci),
    upper.ci = if_else((agent == "Observed"), rescale(upper.ci, to = c(1,5)), upper.ci),
  ) %>%
  ggplot(aes(x = agent, y = avg_frequency, color = agent)) +
  geom_point(size = 1, shape = 15) +
  geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci), width = 0.2) +
  facet_wrap(~label_en) +
  expand_limits(y = c(1, 5)) +
  scale_y_continuous(breaks = 1:5, labels = c("Never", "Seldom", "Sometimes", "Often", "Very often")) +
  scale_x_discrete(label=abbreviate) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(legend.position = "none") +
  scale_color_viridis_d() +
  NULL
