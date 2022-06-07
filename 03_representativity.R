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
load("data/ess2018.Rdata")

## installs developer tools
remotes::install_github("maksimrudnev/LittleHelpers") # useful for ess data

# --- import source script

source("scripts/helper_functions.R")

# --- creates data for the study

# --- here sample break down for age categories, gender, education and employment
df_represent <- df_ipst %>% 
  sjlabelled::remove_all_labels() %>%
  dplyr::mutate(age_cat = cut(age, 
                              breaks=c(18,30,40,50,60,70,Inf),
                              labels=c("18-30","30-40","40-50","50-60","60-70","70+"),
                              right=FALSE)) %>% # left break is closed, right is open
  dplyr::mutate(across(age_cat, as.double)) %>% 
  dplyr::select(sex,age_cat,edu,employ) %>% 
  pivot_longer(sex:employ) %>% 
  arrange(name) %>% 
  group_by(name) %>% 
  #na.omit() %>% 
  count(value) %>% 
  mutate(prop=100*n/sum(n), 
         cum=100*cumsum(prop/sum(prop))) %>%
  round_df()


# --- details from German Microcensus 2020, see data/mz.xlsx
# [---still to add---]


# --- details from ESS 2018

#path_ess = "C:\\Users\\stancian\\data_for_quantapsych\\ESS"
#ess_2018 <- read_spss(paste0(path_ess,"\\","ess_2018.sav")) %>% 
#  dplyr::select(idno,cntry,
#                gndr,eduyrs,agea,
#                ipcrtiv,imprich,ipeqopt,ipshabt,impsafe,impdiff,ipfrule,
#                ipudrst,ipmodst,ipgdtim,impfree,iphlppl,ipsuces,ipstrgv,
#                ipadvnt,ipbhprp,iprspot,iplylfr,impenv,imptrad,impfun,
#                dweight,pspwght,pweight)

# --- saves ess 2018 as Rdata
#save(list=c("ess_2018"),
#file="data/ess2018.Rdata")

# calculates alpha for schwartz higher order values
alpha_4hov_ess2018 <- cronbach_compute_ess_4hov(ess_2018)

# calculates schwartz 4 higher order values
ess2018_hov_cont <- LittleHelpers::ess_values(ess_2018,v2=FALSE,v4=TRUE,v10=FALSE,v21=FALSE,center = FALSE)
#write_sav(ess2018_hov_cont, "./data/ess/ess2018_hov_cont.sav")


# --- ess 2018 sample break down after gender, age, and education years

df_represent_ess2018 <- ess_2018 %>% 
  sjlabelled::remove_all_labels() %>%
  dplyr::select(gndr) %>% 
  pivot_longer(gndr) %>% 
  arrange(name) %>% 
  group_by(name) %>% 
  #na.omit() %>% 
  count(value) %>% 
  mutate(prop=100*n/sum(n), 
         cum=100*cumsum(prop/sum(prop))) %>%
  round_df()
