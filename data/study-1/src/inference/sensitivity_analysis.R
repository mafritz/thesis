library(pwr)
library(esc)
library(tidyverse)
library(here)

source(here("data/study-1/src/01-wrangle.R"))

s1.detectable_f <- pwr.anova.test(k = 3, n = 16, sig.level = 0.05, power = 0.8)
s1.detectable_d <- cohens_d(f = s1.detectable_f$f)
s1.detectable_eta <- eta_squared(d = s1.detectable_d)

fritz2015_num_emotions <- fritz2015_emotions %>% 
  group_by(user) %>% 
  summarise(n = n()) %>% 
  pull()

s1.delta_emotions <- s1.detectable_d * sd(fritz2015_num_emotions)
s1.delta_processing <- s1.detectable_d * sd(fritz2015_eyetracking$Total_Fixation_Duration_Monitoring_Sum)
s1.delta_seeking <- s1.detectable_d * sd(fritz2015_eyetracking$Visit_Count_Monitoring_Sum)
