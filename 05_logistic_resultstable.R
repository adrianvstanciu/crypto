### --- creates table with results from logistic regression

# packages
library(dplyr)
library(tidyverse)

load("data/table_logreg.Rdata")

# - empty table to later populate

res_cryptonow <- tibble(
  What =vector("character",0),
  Intercept=vector("numeric",0),
  Age=vector("numeric",0),
  Female=vector("numeric",0),
  Abitur=vector("numeric",0),
  OCH=vector("numeric",0),
  CON=vector("numeric",0),
  Investment=vector("numeric",0),
  Exchange=vector("numeric",0),
  Regularization=vector("numeric",0),
  Illegal=vector("numeric",0),
  "Good time"=vector("numeric",0),
)

res <- tibble(
  Predictor=c()
)