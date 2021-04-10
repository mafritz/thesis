library(tidyverse)
library(meta)
library(here)

source(here("data/study-1/src/01-wrangle.R"))

# Meta-analysis of expressing emotions
s1.meta_analysis_expressing <- metamean(
    n = c(14, 35), 
    mean = c(18.81, 13.80), 
    sd = c(7.10, 5.68), 
    studlab = c("Fritz 2015", "Perrier 2017")
  )
s1.meta_analysis_expressing.summary <- summary(s1.meta_analysis_expressing)

# Filter to other and both only condition
other_both_only <- s1.et_data %>%  filter(group != "Self")

# Meta-analysis of information processing
s1.meta_analysis_processing <- metamean(
  n = c(14, nrow(other_both_only)), 
  mean = c(
    mean(fritz2015_eyetracking$Total_Visit_Duration_Monitoring_Sum), 
    mean(other_both_only$Total_Visit_Duration_MONITORING_Sum)
    ), 
  sd = c(
    sd(fritz2015_eyetracking$Total_Visit_Duration_Monitoring_Sum), 
    sd(other_both_only$Total_Visit_Duration_MONITORING_Sum)
    ), 
  studlab = c("Fritz 2015", "Perrier 2017")
)
s1.meta_analysis_processing.summary = summary(s1.meta_analysis_processing)

# Meta-analysis of information seeking
s1.meta_analysis_seeking <- metamean(
  n = c(14, nrow(other_both_only)), 
  mean = c(
    mean(fritz2015_eyetracking$Visit_Count_Monitoring_Sum), 
    mean(other_both_only$Visit_Count_MONITORING_Sum)
    ), 
  sd = c(
    sd(fritz2015_eyetracking$Visit_Count_Monitoring_Sum), 
    sd(other_both_only$Visit_Count_MONITORING_Sum)
    ), 
  studlab = c("Fritz 2015", "Perrier 2017")
)
s1.meta_analysis_seeking.summary = summary(s1.meta_analysis_seeking)

# Remove unused
rm(other_both_only)