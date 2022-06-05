## --- script for examining limitations to sample representativity 


# --- packages import

library(rstatix)
library(Hmisc)
library(corrplot)
library(psych)
library(dplyr)
library(tidyverse)
library(broom)
library(sjlabelled)

# --- data import

load("data/df.Rdata")


# --- creates data for the study

# - details from German Microcensus 2020, see data/mz.xlsx

df_represent <- df_ipst %>% 
  dplyr::select(sex,age,edu,employ)



