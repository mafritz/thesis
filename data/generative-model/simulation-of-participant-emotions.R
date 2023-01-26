# Generate simulated participants
library(tidyverse)
library(jsonlite)
library(here)
library(papaja)
library(RColorBrewer)

theme_set(theme_apa(box = TRUE))

set.seed(1407)

# Import emotions from same task
gm.collected_emotions_fritz2015 <- read_csv(here("data/fritz2015/fritz2015_emotions.csv"))
gm.collected_emotions_perrier2017 <- read_csv(here("data/study-1/data/dew-emotions.csv"))

# Adapt column names
gm.collected_emotions_fritz2015 <- gm.collected_emotions_fritz2015 %>%
  rename(
    label = feeling
  ) %>% 
  select(user, x, y, label)

# Filter to both condition (same as Fritz 2015) and adapt colum names
gm.collected_emotions_perrier2017 <- gm.collected_emotions_perrier2017 %>%
  filter(
    str_detect(user, "b")
  ) %>% 
  select(user, x, y , label)

gm.collected_emotions_combined <- bind_rows(
  gm.collected_emotions_fritz2015,
  gm.collected_emotions_perrier2017
)


# Import EATMINT circumplex
gm.circumplex <- fromJSON(here("data/study-1/data/dew-configuration.json"))$circumplex$feelings

# Import english translation of feelings in the circumplex
gm.feelings_translation <- read_csv(here("data/study-1/data/feelings_translation.csv"))


gm.emotions_per_user <- gm.collected_emotions_combined %>%
  group_by(user) %>%
  summarise(
    n = n(),
    avg_valence = mean(x),
    avg_control = mean(y)
  )

gm.raw_dimensions <- gm.collected_emotions_combined %>%
  pivot_longer(
    cols = x:y,
    names_to = "dimension",
    values_to = "value"
  )

gm.circumplex <- gm.circumplex %>%
  mutate(
    quadrant = ceiling(angle / 90)
  )

gm.averaged_dimensions <- gm.raw_dimensions %>%
  group_by(dimension) %>%
  summarise(
    avg = mean(value),
    sd = sd(value),
    upper_sd = avg + sd * 0.75,
    lower_sd = avg - sd * 0.75
  )


gm.get_simulated_values <- function(n, m, s, lwr, upr, nnorm) {
  samp <- rnorm(nnorm, m, s)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= n) {
    return(sample(samp, n))
  }
  stop(simpleError("Not enough values to sample from. Try increasing nnorm."))
}

# DEW algorithm

dew.calculateSlope <- function(x, y) {
  slope <- pi / 2 - atan(y / x)
  if (x > 0 & y > 0) {
    slope <- slope - 0.5 * pi
  } else if (x < 0 & y < 0) {
    slope <- slope + 0.5 * pi
  } else if (x > 0 & y < 0) {
    slope <- slope - 0.5 * pi
  } else if (x < 0 & y > 0) {
    slope <- slope + 0.5 * pi
  }
  slope * 180 / pi + 90
}

dew.getFeeling <- function(slope) {
  q <- ceiling(slope / 90)
  feelings <- gm.circumplex %>%
    filter(quadrant == q) %>%
    .$label
  sample(x = feelings, size = 1)
}

# Set the number of emotions as the simulated participants in Fritz(2015) and Perrier (2017)
gm.simulated_n_emotions <- 26 #mean(gm.emotions_per_user$n) %>% round()

# Simulate higher/lower valence or control for IVs
gm.valence_positive <- gm.get_simulated_values(
  n = gm.simulated_n_emotions,
  m = gm.averaged_dimensions$upper_sd[1],
  s = gm.averaged_dimensions$sd[1],
  lwr = -100,
  upr = 100,
  nnorm = 1000
) %>% round()

gm.valence_negative <- gm.get_simulated_values(
  n = gm.simulated_n_emotions,
  m = gm.averaged_dimensions$lower_sd[1],
  s = gm.averaged_dimensions$sd[1],
  lwr = -100,
  upr = 100,
  nnorm = 1000
) %>% round()

gm.control_positive <- gm.get_simulated_values(
  n = gm.simulated_n_emotions,
  m = gm.averaged_dimensions$upper_sd[2],
  s = gm.averaged_dimensions$sd[2],
  lwr = -100,
  upr = 100,
  nnorm = 1000
) %>% round()

gm.control_negative <- gm.get_simulated_values(
  n = gm.simulated_n_emotions,
  m = gm.averaged_dimensions$lower_sd[2],
  s = gm.averaged_dimensions$sd[2],
  lwr = -100,
  upr = 100,
  nnorm = 1000
) %>% round()

gm.vp_cp <- tibble(
  valence = gm.valence_positive,
  control = gm.control_positive,
  condition = "Higher Valence | Higher Control",
  manipulated_quadrant = 1,
  id = 1:gm.simulated_n_emotions
)

gm.vp_cn <- tibble(
  valence = gm.valence_positive,
  control = gm.control_negative,
  condition = "Higher Valence | Lower Control",
  manipulated_quadrant = 2,
  id = 1:gm.simulated_n_emotions
)

gm.vn_cn <- tibble(
  valence = gm.valence_negative,
  control = gm.control_negative,
  condition = "Lower Valence | Lower Control",
  manipulated_quadrant = 3,
  id = 1:gm.simulated_n_emotions
)

gm.vn_cp <- tibble(
  valence = gm.valence_negative,
  control = gm.control_positive,
  condition = "Lower Valence | Higher Control",
  manipulated_quadrant = 4,
  id = 1:gm.simulated_n_emotions
)

gm.simulated_emotions <- bind_rows(gm.vp_cp, gm.vp_cn, gm.vn_cp, gm.vn_cn) %>%
  mutate(
    slope = map2_dbl(valence, control, dew.calculateSlope),
    label = map_chr(slope, dew.getFeeling),
    observed_quadrant = ceiling(slope / 90)
  )

gm.simulated_emotions <- gm.simulated_emotions %>%
  left_join(gm.feelings_translation, by = "label")

(gm.simulated_emotions.graph <- gm.simulated_emotions %>%
  pivot_longer(cols = valence:control, names_to = "dimension", values_to = "value") %>%
  mutate(
    dimension = str_to_title(as_factor(dimension))
  ) %>% 
  ggplot(aes(id, value, group = dimension, color = dimension)) +
  geom_hline(yintercept = 0, color = "black") +
  # geom_vline(aes(xintercept = id), alpha = 0.6, linetype = "dotted") +
  geom_segment(aes(x = id, y = 100, xend = id, yend = -100), linetype = "solid", color = "lightgray", size = 0.1) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(x = NULL, y = NULL) +
  expand_limits(y = c(-155, 100)) +
  scale_y_continuous(breaks = c(100, 0, -100), labels = c(100, 0, -100)) +
  theme(legend.position = "bottom") +
  facet_wrap(~condition, nrow = 2) +
  geom_text(aes(y = -154, label = abbreviate(label_en, minlength = 10)), angle = 90, hjust = 0, color = "black", size = 2.5) +
  scale_colour_brewer(palette = "Dark2") +
  NULL)

gm.cross_table <- table(gm.simulated_emotions$manipulated_quadrant, gm.simulated_emotions$observed_quadrant)
identical(gm.cross_table[1,1], gm.cross_table[2,2], gm.cross_table[3,3], gm.cross_table[4,4])
