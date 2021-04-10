library(here)
library(tidyverse)
library(papaja)
library(psych)

options(scipen = 999)
options(digits = 5)

theme_set(theme_apa(box = TRUE))

source(here("data/study-2/src/01-wrangle.R"))

s2.ea_usefulness.wider <- s2.ea_usefulness %>%
  pivot_wider(
    names_from = "dimension",
    names_repair = "minimal",
    values_from = "value"
  )

names(s2.ea_usefulness.wider) <- str_replace_all(names(s2.ea_usefulness.wider), pattern = " ", replacement = "_")
names(s2.ea_usefulness.wider) <- str_replace_all(names(s2.ea_usefulness.wider), pattern = "-", replacement = "_")

s2.ea_usefulness_scale.expectancy <- s2.ea_usefulness.wider %>%
  filter(survey == "Expectancy") %>% 
  select(Frequency:Self_Regulation)

s2.ea_usefulness_scale.final <- s2.ea_usefulness.wider %>%
  filter(survey == "Final") %>% 
  select(Frequency:Self_Regulation)

s2.ea_usefulness_scale.expectancy.reliability.uni <- omega(s2.ea_usefulness_scale.expectancy, 1)
s2.ea_usefulness_scale.expectancy.reliability.all <- omega(s2.ea_usefulness_scale.expectancy, 7)

s2.ea_usefulness_scale.final.reliability.uni <- omega(s2.ea_usefulness_scale.final, 1)
s2.ea_usefulness_scale.final.reliability.all <- omega(s2.ea_usefulness_scale.final, 7)

