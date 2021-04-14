# Inference on EAU
library(here)
library(tidyverse)
library(lmerTest)
library(papaja)
library(emmeans)
library(broom.mixed)
library(visreg)
library(car)
library(afex)
library(directlabels)

options(scipen = 999)
options(digits = 5)

theme_set(theme_apa(box = TRUE))

emm_options(lmer.df = "asymptotic")

source(here("data/study-2/src/01-wrangle.R"))

s2.mlm.ea_usefulness <- mixed(
  formula = value ~ group * survey * dimension + (1 | participant / group),
  data = s2.ea_usefulness
)

s2.mlm.ea_usefulness.anova_table <- s2.mlm.ea_usefulness$anova_table
s2.mlm.ea_usefulness.anova_table$`Pr(>F)` <- round_ps_apa(s2.mlm.ea_usefulness.anova_table$`Pr(>F)`)

s2.mlm.ea_usefulness.comparison <- emmeans(
  s2.mlm.ea_usefulness,
  ~ survey | dimension,
  at = list(
    survey = c("Final", "Expectancy")
  )
) %>%
  pairs()


(s2.ea_usefulness.graph <- ggplot(s2.ea_usefulness, aes(x = survey, y = value, group = group, color = group)) +
  geom_jitter(alpha = 0.2) +
  geom_hline(yintercept = 5.5, linetype = "longdash", alpha = 0.3, show.legend = TRUE) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, position = position_dodge(width = 0.5)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 15, position = position_dodge(width = 0.5)) +
  labs(
    x = NULL,
    y = "Emotional Awareness Usefulness"
  ) +
  scale_y_continuous(limits = c(1, 10), breaks = c(1, 4, 7, 10)) +
  scale_x_discrete(label=abbreviate) +
  facet_wrap(~dimension, nrow = 3) +
  # guides(color = FALSE) +
  theme(
    legend.position = c(0.85, 0.1),
    legend.title = element_blank(),
    text = element_text(size = 11)
  ) +
  scale_color_viridis_d() +
  NULL)