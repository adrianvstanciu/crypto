### --- creates table with results from logistic regression

# packages
library(dplyr)
library(tidyverse)

# - empty table to later populate
logistic_table <- tibble(
  Model = vector("character",0),
  "Optimal cut-off" = vector("numeric",0),
  "Misclassification error" = vector("numeric",0),
  "Concordance" = vector("numeric",0),
  "Sensitivity" = vector("numeric",0),
  "Specificity" = vector("numeric",0)
)