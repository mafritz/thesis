# Wrangle data from the DEW
library(here)
library(tidyverse)
library(papaja)
library(see)
library(RColorBrewer)  
library(jsonlite)

# Get list of affective spaces
dew_rt.affective_spaces <- fromJSON(here("data/dew-rt/data/affective_spaces_list.json"))

# Get radial simulation of GEW
dew_rt.sim_gew_radial <- read_csv(here("data/dew-rt/data/simulation_gew_radial.csv"))
dew_rt.sim_gew_radial$order <- as_factor(dew_rt.sim_gew_radial$order)
dew_rt.sim_gew_radial$space <- "Radial"

# Get vector simulation of GEW
dew_rt.sim_gew_vector <- read_csv(here("data/dew-rt/data/simulation_gew_vector.csv"))
dew_rt.sim_gew_vector$order <- as_factor(dew_rt.sim_gew_vector$order)
dew_rt.sim_gew_vector$space <- "Vector"

dew_rt.sim_gew <- bind_rows(
  dew_rt.sim_gew_radial,
  dew_rt.sim_gew_vector
)

# Get Gillioz et al. Arousal x Novelty
dew_rt.sim_gillioz_et_al_arousal_novelty <- read_csv(here("data/dew-rt/data/simulation_gillioz_et_al_vector_arousal_novelty.csv"))
dew_rt.sim_gillioz_et_al_arousal_novelty$order <- as_factor(dew_rt.sim_gillioz_et_al_arousal_novelty$order)
dew_rt.sim_gillioz_et_al_arousal_novelty$space <- "Arousal x Novelty"

# Get Gillioz et al. Valence x Power
dew_rt.sim_gillioz_et_al_valence_power <- read_csv(here("data/dew-rt/data/simulation_gillioz_et_al_vector_valence_power.csv"))
dew_rt.sim_gillioz_et_al_valence_power$order <- as_factor(dew_rt.sim_gillioz_et_al_valence_power$order)
dew_rt.sim_gillioz_et_al_valence_power$space <- "Valence x Power"

dew_rt.sim_gillioz_et_al <- bind_rows(
  dew_rt.sim_gillioz_et_al_arousal_novelty,
  dew_rt.sim_gillioz_et_al_valence_power
)

rm(
  dew_rt.sim_gew_radial, 
  dew_rt.sim_gew_vector,
  dew_rt.sim_gillioz_et_al_arousal_novelty,
  dew_rt.sim_gillioz_et_al_valence_power
)