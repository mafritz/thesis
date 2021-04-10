# Inference on EAU
library(here)
library(tidyverse)
library(papaja)
library(emmeans)
library(afex)

options(scipen = 999)
options(digits = 5)

theme_set(theme_apa(box = TRUE))

source(here("data/study-2/src/01-wrangle.R"))

s2.ea_usefulness_expectancy <- s2.ea_usefulness %>% filter(survey == "Expectancy")
s2.anova.ea_usefulness_expectancy <- aov_car(value ~ group * dimension + Error(participant/dimension), data = s2.ea_usefulness_expectancy)
s2.anova.ea_usefulness_expectancy.comparison <- emmeans(s2.anova.ea_usefulness_expectancy, ~dimension|group) %>% pairs()

# Get only significant contrasts for both classes
s2.anova.ea_usefulness_expectancy.comparison %>% 
  as_tibble() %>% 
  filter(p.value < 0.05) %>% 
  group_by(contrast) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(n == 2)

# Plot
s2.ea_usefulness_expectancy %>% 
  ggplot(aes(x = dimension, y = value, color = dimension)) +
  geom_jitter(alpha = 0.2) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", aes(color = dimension), width = 0.4) +
  stat_summary(fun = mean, geom = "point", aes(color = dimension), size = 5, shape = 15) +
  facet_wrap(~ group, nrow = 2) +
  xlab(NULL) +
  ylab(NULL) +
  theme(
    legend.position = "none"
  ) +
  scale_x_discrete(limits = rev(levels(s2.ea_usefulness_expectancy$dimension))) +
  coord_flip() +
  NULL
