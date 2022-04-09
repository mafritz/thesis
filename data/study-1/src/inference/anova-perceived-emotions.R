library(here)
library(tidyverse)
library(afex)
library(papaja)
library(emmeans)

theme_set(theme_apa(box = TRUE))

options(scipen = 999)
options(digits = 5)

source(here("data/study-1/src/01-wrangle.R"))

# Total Visit Count
s1.visit_count <- s1.et_data_tidy %>%
  filter(
    measure == "Visit Count",
    aoi == "Monitoring",
    stat == "Sum"
  )

s1.anova.visit_count <- aov_car(value ~ group + Error(1 | user), data = s1.visit_count)
s1.anova.visit_count.comp <- emmeans(s1.anova.visit_count, "group")
s1.anova.visit_count.comp.p <- pairs(s1.anova.visit_count.comp)
s1.anova.visit_count.comp.es <- eff_size(s1.anova.visit_count.comp, sigma = sigma(s1.anova.visit_count$lm), df.residual(s1.anova.visit_count$lm))

s1.anova.visit_count.comp.summary <- as_tibble(s1.anova.visit_count.comp.p) %>% 
  mutate(
    cohens.d = as_tibble(s1.anova.visit_count.comp.es)$effect.size
  )

# Graph Visit Count
s1.anova.visit_count.graph <- ggplot(s1.visit_count, aes(x = group, y = value, color = group)) +
  geom_jitter(alpha = 0.3, size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", aes(group = group, color = group), width = 0.4) +
  stat_summary(fun = mean, geom = "point", aes(group = group, color = group), size = 5) +
  labs(
    x = NULL,
    y = "Total visits count"
  ) +
  theme(
    legend.position = "none"
  ) +
  NULL

# Total Visit Duration
s1.total_visit_duration <- s1.et_data_tidy %>%
  filter(
    measure == "Total Visit Duration",
    aoi == "Monitoring",
    stat == "Sum"
  )

s1.anova.total_visit_duration <- aov_car(value ~ group + Error(1 | user), data = s1.total_visit_duration)
s1.anova.total_visit_duration.comp <- emmeans(s1.anova.total_visit_duration, "group")
s1.anova.total_visit_duration.comp.p <- pairs(s1.anova.total_visit_duration.comp)
s1.anova.total_visit_duration.comp.es <- eff_size(s1.anova.total_visit_duration.comp, sigma = sigma(s1.anova.total_visit_duration$lm), df.residual(s1.anova.total_visit_duration$lm))

s1.anova.total_visit_duration.comp.summary <- as_tibble(s1.anova.total_visit_duration.comp.p) %>% 
  mutate(
    cohens.d = as_tibble(s1.anova.total_visit_duration.comp.es)$effect.size
  )

# Graph Visit Duration
s1.anova.total_visit_duration.graph <- ggplot(s1.total_visit_duration, aes(x = group, y = value, color = group)) +
  geom_jitter(alpha = 0.3, size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", aes(group = group, color = group), width = 0.4) +
  stat_summary(fun = mean, geom = "point", aes(group = group, color = group), size = 5) +
  labs(
    x = NULL,
    y = "Total visits duration in seconds"
  ) +
  theme(
    legend.position = "none"
  ) +
  NULL

## Bayes
## Bayes

# library(easystats)
# library(rstanarm)
# library(see)
# 
# model.visit <- stan_glm(value ~ group, data = s1.visit_count, prior = normal(116.5, 49.115))
# posteriors.visit <- get_parameters(model.visit)
# describe_posterior(model.visit, test = c("all"), rope_range = c(-25, 25))
# report(model.visit)
# plot(estimate_means(model.visit))
# estimate_contrasts(model.visit, test = "bf", bf_prior = model.visit)
# 
# ggplot(posteriors.visit) +
#   geom_density(aes(x = groupPartner), fill = "orange", alpha = 0.7) +
#   geom_density(aes(x = groupMutual), fill = "violet", alpha = 0.7) +
#   NULL
# 
# model.duration <- stan_glm(value ~ group, data = s1.total_visit_duration, prior = normal(49.978, 27.351))
# describe_posterior(model.duration, test = c("all"), rope_range = c(-4, 4))
# report(model.duration)
# plot(estimate_means(model.duration))
# estimate_contrasts(model.duration, test = "bf", bf_prior = model.duration)
