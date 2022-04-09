library(here)
library(tidyverse)
library(afex)
library(papaja)
library(emmeans)

theme_set(theme_apa(box = TRUE))

options(scipen = 999)
options(digits = 5)

source(here("data/study-1/src/01-wrangle.R"))

# One-way ANOVA
s1.anova.expressed_emotions <- aov_car(n ~ group + Error(1 | user), data = s1.aggregated_emotions)

# Compare means of groups
s1.anova.expressed_emotions.comp <- emmeans(s1.anova.expressed_emotions, "group")
s1.anova.expressed_emotions.comp.p <- pairs(s1.anova.expressed_emotions.comp)
s1.anova.expressed_emotions.comp.es <- eff_size(s1.anova.expressed_emotions.comp, sigma = sigma(s1.anova.expressed_emotions$lm), df.residual(s1.anova.expressed_emotions$lm))

s1.anova.expressed_emotions.comp.summary <- as_tibble(s1.anova.expressed_emotions.comp.p) %>% 
  mutate(
    cohens.d = as_tibble(s1.anova.expressed_emotions.comp.es)$effect.size
  )

# Plot
s1.anova.expressed_emotions.graph <- ggplot(s1.aggregated_emotions, aes(x = group, y = n, color = group)) +
  geom_jitter(alpha = 0.3, size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", aes(group = group, color = group), width = 0.4) +
  stat_summary(fun = mean, geom = "point", aes(group = group, color = group), size = 5, shape = 15) +
  labs(
    x = NULL,
    y = "Number of emotions expressed"
  ) +
  lims(y = c(0, 30)) +
  theme(
    legend.position = "none",
    text = element_text(size = 16)
  ) +
  NULL

# library(easystats)
# library(rstanarm)
# library(bayesplot)
# 
# model <- stan_glm(n ~ group, data = s1.aggregated_emotions, prior = normal(location = 18.8, scale = 7.10, autoscale = FALSE))
# describe_posterior(model, test = c("all"), rope_range = c(4, 4))
# 
# report(model)
# plot(estimate_means(model))
# 
# mcmc_areas(as.matrix(model),
#            pars = c("groupPartner", "groupMutual"),
#            prob = 0.95)
# 
# (group_diff <- emmeans(model, pairwise ~ group))
# 
# estimate_contrasts(model, test = "bf", bf_prior = model)
