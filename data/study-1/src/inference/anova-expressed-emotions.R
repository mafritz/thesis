library(here)
library(tidyverse)
library(afex)
library(papaja)
library(see)
library(emmeans)
library(performance)
library(effectsize)
library(patchwork)

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

s1.anova.expressed_emotions.comp.summary <- as_tibble(s1.anova.expressed_emotions.comp.p |> summary(infer = TRUE)) |>
  mutate(
    estimate = paste0(printnum(estimate), " [", printnum(lower.CL), ", ", printnum(upper.CL), "]")
  ) |> 
  select(-c(lower.CL, upper.CL)) |> 
  mutate(
    cohens.d = paste0(
      printnum(as_tibble(s1.anova.expressed_emotions.comp.es)$effect.size), 
      " [", 
      printnum(as_tibble(s1.anova.expressed_emotions.comp.es)$lower.CL), 
      ", ", 
      printnum(as_tibble(s1.anova.expressed_emotions.comp.es)$upper.CL),
      "]"
    )
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

# Assumptions

# Homogénéité de la variance (les groupes ont une variance similaire) -> À utiliser avec précaution
check_homogeneity(s1.anova.expressed_emotions)

# Normalité des résidus
s1.anova.expressed_emotions.normality_distribution <- plot(check_normality(s1.anova.expressed_emotions))
s1.anova.expressed_emotions.normality_plot <- plot(check_normality(s1.anova.expressed_emotions), type = "qq", detrend = TRUE)

s1.anova.expressed_emotions.assumptions <- s1.anova.expressed_emotions.normality_distribution + s1.anova.expressed_emotions.normality_plot
