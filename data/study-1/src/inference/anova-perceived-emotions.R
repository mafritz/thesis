library(here)
library(tidyverse)
library(afex)
library(papaja)
library(emmeans)
library(performance)
library(effectsize)

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

s1.anova.visit_count <- aov_4(value ~ group + (1 | user), data = s1.visit_count)
s1.anova.visit_count.comp <- emmeans(s1.anova.visit_count, "group")
s1.anova.visit_count.comp.p <- pairs(s1.anova.visit_count.comp)
s1.anova.visit_count.comp.es <- eff_size(s1.anova.visit_count.comp, sigma = sigma(s1.anova.visit_count$lm), df.residual(s1.anova.visit_count$lm))

s1.anova.visit_count.comp.summary <- as_tibble(s1.anova.visit_count.comp.p |> summary(infer = TRUE)) %>%
  mutate(
    estimate = paste0(printnum(estimate), " [", printnum(lower.CL), ", ", printnum(upper.CL), "]")
  ) |> 
  select(-c(lower.CL, upper.CL)) |> 
  mutate(
    cohens.d = paste0(
      printnum(as_tibble(s1.anova.visit_count.comp.es)$effect.size), 
      " [", 
      printnum(as_tibble(s1.anova.visit_count.comp.es)$lower.CL), 
      ", ", 
      printnum(as_tibble(s1.anova.visit_count.comp.es)$upper.CL),
      "]"
    )
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

# Homogénéité de la variance (les groupes ont une variance similaire) -> À utiliser avec précaution
check_homogeneity(s1.anova.visit_count)

# Normalité des résidus
plot(check_normality(s1.anova.visit_count))
plot(check_normality(s1.anova.visit_count), type = "qq")
plot(check_normality(s1.anova.visit_count), type = "qq", detrend = TRUE)

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

s1.anova.total_visit_duration.comp.summary <- as_tibble(s1.anova.total_visit_duration.comp.p |> summary(infer = TRUE)) %>%
  mutate(
    estimate = paste0(printnum(estimate), " [", printnum(lower.CL), ", ", printnum(upper.CL), "]")
  ) |> 
  select(-c(lower.CL, upper.CL)) |> 
  mutate(
    cohens.d = paste0(
      printnum(as_tibble(s1.anova.total_visit_duration.comp.es)$effect.size), 
      " [", 
      printnum(as_tibble(s1.anova.total_visit_duration.comp.es)$lower.CL), 
      ", ", 
      printnum(as_tibble(s1.anova.total_visit_duration.comp.es)$upper.CL),
      "]"
    )
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

# Homogénéité de la variance (les groupes ont une variance similaire) -> À utiliser avec précaution
check_homogeneity(s1.anova.total_visit_duration)

# Normalité des résidus
plot(check_normality(s1.anova.total_visit_duration))
plot(check_normality(s1.anova.total_visit_duration), type = "qq")
plot(check_normality(s1.anova.total_visit_duration), type = "qq", detrend = TRUE)
