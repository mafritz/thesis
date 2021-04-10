library(here)
library(tidyverse)
library(lmerTest)
library(papaja)
library(emmeans)
library(broom.mixed)
library(car)
library(apa)

theme_set(theme_apa(box = TRUE))

options(scipen = 999)
options(digits = 5)

source(here("data/study-1/src/01-wrangle.R"))

# Task to monitoring and vice-versa
s1.transitions_task_monitoring <- s1.raw_eyetracking_data %>%
  group_by(ParticipantName) %>%
  filter(
    monitoring_aoi == TRUE,
    lag(monitoring_aoi == FALSE),
    lag(task_aoi == TRUE)
  ) %>%
  summarise(
    num_transitions = n(),
    group = first(StudioTestName),
    transition = "Task to Perceiving",
    from = "Task",
    to = "Perceiving"
  )

s1.transitions_monitoring_task <- s1.raw_eyetracking_data %>%
  group_by(ParticipantName) %>%
  filter(
    task_aoi == TRUE,
    lag(task_aoi == FALSE),
    lag(monitoring_aoi == TRUE)
  ) %>%
  summarise(
    num_transitions = n(),
    group = first(StudioTestName),
    transition = "Perceiving to Task",
    from = "Perceiving",
    to = "Task"
  )

# Tasl to displaying and vice-versa
s1.transitions_task_displaying <- s1.raw_eyetracking_data %>%
  group_by(ParticipantName) %>%
  filter(
    displaying_aoi == TRUE,
    lag(displaying_aoi == FALSE),
    lag(task_aoi == TRUE)
  ) %>%
  summarise(
    num_transitions = n(),
    group = first(StudioTestName),
    transition = "Task to Expressing",
    from = "Task",
    to = "Expressing"
  )
s1.transitions_displaying_task <- s1.raw_eyetracking_data %>%
  group_by(ParticipantName) %>%
  filter(
    task_aoi == TRUE,
    lag(task_aoi == FALSE),
    lag(displaying_aoi == TRUE)
  ) %>%
  summarise(
    num_transitions = n(),
    group = first(StudioTestName),
    transition = "Expressing to Task",
    from = "Expressing",
    to = "Task"
  )

# Displaying to Expressing and vice-versa
s1.transitions_displaying_monitoring <- s1.raw_eyetracking_data %>%
  group_by(ParticipantName) %>%
  filter(
    monitoring_aoi == TRUE,
    lag(monitoring_aoi == FALSE),
    lag(displaying_aoi == TRUE)
  ) %>%
  summarise(
    num_transitions = n(),
    group = first(StudioTestName),
    transition = "Expressing to Perceiving",
    from = "Expressing",
    to = "Perceiving"
  )
s1.transitions_monitoring_displaying <- s1.raw_eyetracking_data %>%
  group_by(ParticipantName) %>%
  filter(
    displaying_aoi == TRUE,
    lag(displaying_aoi == FALSE),
    lag(monitoring_aoi == TRUE)
  ) %>%
  summarise(
    num_transitions = n(),
    group = first(StudioTestName),
    transition = "Perceiving to Expressing",
    from = "Perceiving",
    to = "Expressing"
  )

s1.transitions <- bind_rows(
  s1.transitions_displaying_monitoring,
  s1.transitions_monitoring_displaying,
  s1.transitions_task_displaying,
  s1.transitions_displaying_task,
  s1.transitions_task_monitoring,
  s1.transitions_monitoring_task
)

s1.transitions$transition <- factor(
  s1.transitions$transition,
  levels = c(
    "Expressing to Perceiving",
    "Perceiving to Expressing",
    "Expressing to Task",
    "Task to Expressing",
    "Perceiving to Task",
    "Task to Perceiving"
  )
)

# Order groups
s1.transitions$group = fct_recode(s1.transitions$group, Partner = "Other", Mutual = "Both")
s1.transitions$group <- factor(s1.transitions$group, levels = c("Self", "Partner", "Mutual"))

s1.transitions.descriptive <- s1.transitions %>% 
  group_by(transition, group) %>% 
  summarise(
    n = n(),
    mean = mean(num_transitions),
    sd = sd(num_transitions),
    ci.lower = mean - conf_int(num_transitions),
    ci.upper = mean + conf_int(num_transitions)
  )

s1.transitions.lmm <- mixed(
  num_transitions ~ group * transition + 
    (1 | ParticipantName),
  data = s1.transitions
)

s1.transitions.lmm.anova_table <- s1.transitions.lmm$anova_table
s1.transitions.lmm.anova_table$`Pr(>F)` <- round_ps_apa(s1.transitions.lmm.anova_table$`Pr(>F)`)

s1.transitions.contrasts <- emmeans(s1.transitions.lmm, pairwise ~ group | transition, adjust = "tukey")
tidy(s1.transitions.contrasts$contrasts)
s1.transitions.contrasts$contrasts

s1.transitions.graph <- ggplot(s1.transitions, aes(x = group, y = num_transitions, color = group)) +
  geom_jitter(alpha = 0.3, size = 2) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", aes(group = group, color = group), width = 0.4) +
  stat_summary(fun = mean, geom = "point", aes(group = group, color = group), size = 3, shape = 15) +
  facet_wrap(~transition, nrow = 3) +
  labs(
    x = NULL,
    y = "Number of transitions between screen zones"
  ) +
  scale_color_viridis_d() +
  theme(
    legend.position = "none"
  ) +
  NULL
