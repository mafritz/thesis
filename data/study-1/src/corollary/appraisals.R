library(here)
library(tidyverse)
library(papaja)
library(grid)
library(gridExtra)

theme_set(theme_apa(box = TRUE))

options(scipen = 999)
options(digits = 5)

source(here("data/study-1/src/01-wrangle.R"))

s1.appraisals_over_time_graph <- s1.dew_emotions %>% 
  select(x, y, user, elapsedTime) %>% 
  mutate(
    elapsedTime = elapsedTime / 1000 / 60,
  ) %>% 
  gather(appraisal, value, -user, -elapsedTime) %>%
  mutate(
    appraisal = if_else(appraisal == "x", "Valence", "Control")
  ) %>% 
  ggplot(aes(x = elapsedTime, y = value, color = appraisal)) +
  # geom_vline(aes(xintercept = elapsedTime), alpha = .5, color = "grey") +
  geom_point(alpha = .5) +
  geom_smooth(method = "loess", se = FALSE, size = 2) +
  labs(x = "Elapsed time in minutes", y = "Appraisal evaluation") +
  theme(
    legend.position = "top", 
    legend.margin = NULL,
    legend.spacing.x = unit(1, "mm"),
    legend.title = element_blank()      
  ) +
  scale_color_viridis_d() +
  NULL

# Plotting by condition
s1.appraisal_over_time_valence <- ggplot(
    s1.dew_emotions, 
    aes(x = elapsedMinutes, y = x, color = group)
  ) +
  geom_point() +
  geom_smooth(se = FALSE, color = "red", method = "loess", size = 2, formula = "y ~ x") +
  labs(x = "Time in minutes", y = "Valence") +
  theme(legend.position = "none") +
  facet_wrap(~group, nrow = 3) +
  scale_color_viridis_d() +
  NULL

s1.appraisal_over_time_control <- ggplot(
    s1.dew_emotions, 
    aes(x = elapsedMinutes, y = y, color = group)
  ) +
  geom_point() +
  geom_smooth(se = FALSE, color = "red", method = "loess", size = 2, formula = "y ~ x") +
  labs(x = "Time in minutes", y = "Control/Power") +
  theme(legend.position = "none") +
  facet_wrap(~group, nrow = 3) +
  scale_color_viridis_d() +
  NULL

# Single user appraisals evolution for Valence and Control
s1.appraisal_evolution_user_valence <- ggplot(s1.dew_emotions, aes(x = elapsedMinutes, y = x, color = group)) +
  geom_point() +
  geom_smooth(se = FALSE, color = "red") +
  labs(x = "Time in minutes", y = "Valence") +
  facet_wrap(group~user, nrow = 3, labeller = labeller(~user)) +
  theme(legend.position = "none", ) +
  scale_color_viridis_d() +
  NULL

s1.appraisal_evolution_user_control <- ggplot(s1.dew_emotions, aes(x = elapsedMinutes, y = y, color = group)) +
  geom_point() +
  geom_smooth(se = FALSE, color = "red") +
  labs(x = "Time in minutes", y = "Control") +
  facet_wrap(group~user, nrow = 3) +
  theme(legend.position = "none") +
  scale_color_viridis_d() +
  NULL