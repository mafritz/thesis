# Inference EC
library(here)
library(tidyverse)
library(papaja)

options(scipen = 999)
options(digits = 5)

theme_set(theme_apa(box = TRUE))

source(here("data/study-2/src/01-wrangle.R"))


s2.ec_vs_eau <- s2.participants_aggregated %>% 
  pivot_longer(
    cols = Expectancy:Final,
    names_to = "survey",
    values_to = "EAU"
  )

s2.ec_vs_eau$survey <- factor(s2.ec_vs_eau$survey, c("Expectancy", "Demo", "Halfway", "Final"))

s2.ec_vs_eau %>%
  filter(
    !is.na(geco_total)
  ) %>% 
  ggplot(aes(
    x = geco_total,
    y = EAU,
    color = participant,
    size = num_emotions,
    group = survey
  )) +
  facet_wrap(~survey, nrow = 1) +
  geom_smooth(
    method = "lm", se = TRUE, alpha = 0.2, color = "gray", size = 2
  ) +
  geom_point() +
  theme(
    legend.position = "none"
  ) +
  scale_y_continuous(
    limits = c(0, 10)
  ) +
  labs(
    x = "Emotional Competence",
    y = "Emotion Awareness Usefulness"
  )

s2.eau_ec_subcompetences <- s2.ea_usefulness %>% inner_join(s2.geco_score)
#s2.eau_ec_subcompetences$sub_competence <- str_to_sentence(s2.eau_ec_subcompetences$sub_competence)

s2.eau_ec_subcompetences %>% 
  ggplot(
    aes(
      x = score,
      y = value,
      color = survey,
      group = survey
    )
  ) +
  geom_jitter(
    alpha = 0.2, size = 3
  ) +
  facet_grid(
    sub_competence ~ dimension
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    size = 1.5,
    alpha = 0.5
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box.spacing = unit(0, "cm"),
    legend.key.height = unit(0.2, "cm")
  ) +
  scale_y_continuous(
    breaks = c(0, 5, 10)
  ) +
  labs(
    group = NULL,
    color = NULL,
    x = "Sub-comptence score",
    y = "Emotion Awareness Usefulness"
  )

s2.geco_score_to_eau.wider <- s2.geco_score %>% 
  pivot_wider(names_from = "sub_competence", values_from = "score") %>% 
  inner_join(s2.participants_aggregated)

s2.sub_competences_to_expectancy.lm <- lm(
  Expectancy ~ recognition + understanding + regulation + management, 
  s2.geco_score_to_eau.wider
)

## Bayes

# library(easystats)
# library(rstanarm)
# library(see)
# 
# s2.geco_score_to_eau.wider$Expectancy.fake = runif(nrow(s2.geco_score_to_eau.wider), min = 0, max = 10)
# 
# model.expectancy <- stan_glm(
#   Expectancy ~ recognition + understanding + regulation + management, 
#   data = s2.geco_score_to_eau.wider,
#   prior = normal(location = c(0.58, 0.67, 1.14, .48))
# )
# 
# result.expectancy <- describe_posterior(model.expectancy)
# plot(result.expectancy, stack = FALSE, priors = TRUE)
# report(model.expectancy)
# describe_posterior(model.expectancy, test = c("p_direction", "rope", "bayesfactor"))