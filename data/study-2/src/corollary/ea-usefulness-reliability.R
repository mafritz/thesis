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

#names(s2.ea_usefulness.wider) <- str_replace_all(names(s2.ea_usefulness.wider), pattern = " ", replacement = "_")
#names(s2.ea_usefulness.wider) <- str_replace_all(names(s2.ea_usefulness.wider), pattern = "-", replacement = "_")

s2.ea_usefulness_scale.all <- s2.ea_usefulness.wider %>%
  select(Frequency:`Self-Regulation`)

s2.ea_usefulness_scale.scree <- scree(s2.ea_usefulness_scale.all, pc = FALSE)
s2.ea_usefulness_scale.factor_analysis <- omega(s2.ea_usefulness_scale.all, 3, fm = "minres", rotate = "oblimin")

