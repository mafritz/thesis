# Exploratory data analysis DEW simulations

library(here)
source(here("data/dew-rt/src/01-wrangle.R"))

theme_set(theme_apa(box = TRUE))

dew_rt.sim_gew_graph_3 <- dew_rt.sim_gew %>%
  filter(
    as.numeric(order) <= 3
  ) %>%
  ggplot(
    aes(x = fct_rev(label), fill = order)
  ) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  labs(
    y = "Number of appearances with 3 suggestions",
    x = NULL,
    fill = "Position"
  ) +
  theme(
    legend.position = "right"
  ) +
  scale_fill_viridis_d() +
  facet_wrap(~space, strip.position = "bottom") +
  coord_flip() +
  NULL

dew_rt.sim_gew_graph_10 <- dew_rt.sim_gew %>%
  filter(
    as.numeric(order) <= 10
  ) %>%
  ggplot(
    aes(x = fct_rev(label), fill = order)
  ) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  labs(
    y = "Number of appearances with 10 suggestions",
    x = NULL,
    fill = "Position"
  ) +
  theme(
    legend.position = "right"
  ) +
  scale_fill_viridis_d() +
  facet_wrap(~space, strip.position = "bottom") +
  coord_flip() +
  NULL

dew_rt.sim_gillioz_et_al_graph_3 <- dew_rt.sim_gillioz_et_al %>% 
  filter(
  as.numeric(order) <= 3
) %>%
  ggplot(
    aes(x = fct_rev(label), fill = order)
  ) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  labs(
    y = "Number of appearances with 3 suggestions",
    x = NULL,
    fill = "Position"
  ) +
  theme(
    legend.position = "right"
  ) +
  scale_fill_viridis_d() +
  facet_wrap(~space, strip.position = "bottom") +
  coord_flip() +
  NULL

dew_rt.sim_gillioz_et_al_graph_10 <- dew_rt.sim_gillioz_et_al %>% 
  filter(
  as.numeric(order) <= 10
) %>%
  ggplot(
    aes(x = fct_rev(label), fill = order)
  ) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  labs(
    y = "Number of appearances with 10 suggestions",
    x = NULL,
    fill = "Position"
  ) +
  theme(
    legend.position = "right"
  ) +
  scale_fill_viridis_d() +
  facet_wrap(~space, strip.position = "bottom") +
  coord_flip() +
  NULL
