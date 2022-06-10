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
library(survey)
library(purrr)

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
  dplyr::select(sex,age_cat,edu) %>% 
  pivot_longer(sex:edu) %>% 
  arrange(name) %>% 
  group_by(name) %>% 
  #na.omit() %>% 
  count(value) %>% 
  mutate(prop=100*n/sum(n), 
         cum=100*cumsum(prop/sum(prop))) %>%
  round_df()


# categories

df_categ <- df_ipst %>% 
  sjlabelled::remove_all_labels() %>%
  dplyr::mutate(age_cat = cut(age, 
                              breaks=c(18,30,40,50,60,70,Inf),
                              labels=c("18-30","30-40","40-50","50-60","60-70","70+"),
                              right=FALSE)) %>% # left break is closed, right is open
  dplyr::select(sex,age_cat,edu)

age_categories <- get_labels(df_categ$age_cat)[-6]
edu_categories <- get_labels(df_ipst$edu)[-7]

edu_category <- c("Still in school","Grundschule","Volks-/ Hauptschulabschluss",
                  "Mittlere Reife","Polytechnische Oberschule","Abitur")

df_represent <- df_represent %>% 
  add_column(
    factor = c(age_categories,edu_category,"Men","Women"),
    survey = "equivalence"
  ) %>% 
  relocate(c(survey,factor), .before=name) 


# --- details from German Microcensus 2020, see data/mz.xlsx
# [---still to add---]


# --- details from ESS 2018

#path_ess = "C:\\Users\\stancian\\data_for_quantapsych\\ESS"
#ess_2018 <- read_spss(paste0(path_ess,"\\","ess_2018.sav")) %>% 
#  filter(cntry=="DE") %>% 
#  dplyr::select(idno,
#                gndr,edubde1,agea,
#                ipcrtiv,imprich,ipeqopt,ipshabt,impsafe,impdiff,ipfrule,
#                ipudrst,ipmodst,ipgdtim,impfree,iphlppl,ipsuces,ipstrgv,
#                ipadvnt,ipbhprp,iprspot,iplylfr,impenv,imptrad,impfun,
#                dweight,pspwght,pweight)

# --- saves ess 2018 as Rdata
#save(list=c("ess_2018"),
#file="data/ess2018.Rdata")
a<-ess_2018 %>% dplyr::select(starts_with("ip") | starts_with("im")) %>% names()
ess <- ess_2018 %>% 
  sjlabelled::set_na(edubde1,na=c(5555,7777,8888,9999)) %>% 
  sjlabelled::remove_all_labels() %>%
  mutate(across(all_of(a),as.double)) %>% 
  mutate_at(all_of(a),flip_item) 

# calculates alpha for schwartz higher order values
alpha_4hov_ess2018 <- cronbach_compute_ess_4hov(ess)

# calculates schwartz 4 higher order values
ess <- LittleHelpers::ess_values(ess,v2=FALSE,v4=TRUE,v10=FALSE,v21=FALSE,center = TRUE)
#write_sav(ess2018_hov_cont, "./data/ess/ess2018_hov_cont.sav")


# --- ess 2018 sample break down after gender, age, and education years

# weights data with design weight from ess

#ess_shrt <- ess %>% dplyr::select(idno,gndr,eduyrs,agea,dweight)
#ess_wght <- svydesign(id = ~idno,
#  data=ess_shrt,
#  weights=~dweight)

# !!! "Abschluss einer Förderschule (Sonderschule, Hilfsschule)"   needs to be under category "other"
# see table education in de_v2

ess<-ess %>% mutate_at(vars(edubde1),
                              function(x) case_when(x == 0 ~ 1,
                                                    x == 1 ~ 2,
                                                    x == 2 ~ 5555,
                                                    x == 3 ~ 3,
                                                    x == 4 ~ 4,
                                                    x == 5 ~ 5,
                                                    x == 6 ~ 6,
                                                    x == 7 ~ 7)) %>% 
  sjlabelled::set_na(edubde1,na=5555) %>% 
  rename(age=agea,
         sex=gndr,
         edu=edubde1,
         pvq_CON= Conservation,
         pvq_SEN= Self.Enhancement,
         pvq_STR= Self.Transcendence,
         pvq_OCH= Openness.to.Change) 


df_represent_ess2018 <- ess %>% 
  sjlabelled::remove_all_labels() %>%
  dplyr::select(sex,age,edu) %>%
  dplyr::mutate(age_cat = cut(age, 
                              breaks=c(18,30,40,50,60,70,Inf),
                              labels=c("18-30","30-40","40-50","50-60","60-70","70+"),
                              right=FALSE)) %>% # left break is closed, right is open
  dplyr::mutate(across(c(sex,age_cat), as.double)) %>%
  pivot_longer(c(sex,age_cat,edu)) %>% 
  arrange(name) %>% 
  group_by(name) %>% 
  na.omit() %>% 
  count(value) %>% 
  mutate(prop=100*n/sum(n), 
         cum=100*cumsum(prop/sum(prop)))

# - categories
df_categ_ess <- ess %>% 
  sjlabelled::remove_all_labels() %>% 
  dplyr::select(sex,age,edu) %>%
  dplyr::mutate(age_cat = cut(age, 
                              breaks=c(18,30,40,50,60,70,Inf),
                              labels=c("18-30","30-40","40-50","50-60","60-70","70+"),
                              right=FALSE))

age_categories_ess <- get_labels(df_categ_ess$age_cat)
#edu_categories_ess <- c("Grundschule nicht beendet","(noch) kein Abschluss, aber Grundschule beendet",
#                        "Volks- oder Hauptschule / Polytechn. Oberschule (8./9. Klasse)",
#                        "Mittlere Reife, Realschule / MSA / Polytechn. Oberschule (10. Klasse)",
#                        "Fachhochschulreife","Abitur, fachgebundene Hochschulreife / Erweiterte Oberschule (12. Klasse)")


df_represent_ess2018 <- df_represent_ess2018 %>% 
  add_column(
    factor = c(age_categories_ess,edu_category,"Men","Women"),
    survey = "ess2018"
  ) %>% 
  relocate(c(survey,factor), .before=name)

# --- create on df with the values from the equivalence study and ess2018

df_represent_joined <- full_join(df_represent,df_represent_ess2018)

# --- dissimilarity prop

dissim_prop <- df_represent_joined %>% group_by(factor) %>% 
  nest() %>% 
  mutate(data=map(data, ~ .x %>% 
                    mutate(diff=prop-lag(prop,default=prop[1])))) %>% 
  unnest(data) %>% 
  dplyr::select(-cum,-name,-value) %>% 
  rename(Dimension=factor,
         Data=survey,
         N=n,
         "%"=prop,
         "Difference %"=diff) %>% 
  arrange(replace(row_number(),27,n() -17)) %>% 
  relocate(Data, .before=Dimension) %>% 
  ungroup() %>% 
  mutate(Entry=row_number(), .before=Data)

# ---- Percentages of Schwawrtz HOV in the equivalence and ess datasets

df_represent_hov_equiv <- df_ipst %>% 
  sjlabelled::remove_all_labels() %>%
  dplyr::select(pvq_CON, pvq_SEN, pvq_STR, pvq_OCH) %>% 
  describe() %>% 
  as.data.frame() %>% 
  add_column(
    survey="equivalence",
    .before="n"
  ) %>% 
  add_column(
    value=c("CON","SEN","STR","OCH"),
    .after="survey"
  ) %>% 
  dplyr::select(-trimmed,-mad)

df_represent_hov_ess <- ess %>% 
  sjlabelled::remove_all_labels() %>%
  dplyr::select(pvq_CON, pvq_SEN, pvq_STR, pvq_OCH) %>% 
  describe() %>% 
  as.data.frame() %>% 
  add_column(
    survey="ess2018",
    .before="n"
  ) %>% 
  add_column(
    value=c("CON","SEN","STR","OCH"),
    .after="survey"
  ) %>% 
  dplyr::select(-trimmed,-mad)


df_represent_hov_joined <- full_join(df_represent_hov_equiv,df_represent_hov_ess) %>% 
  dplyr::select(-vars,-range,-skew,-kurtosis) %>% 
  rename(
    Data=survey,
    Value=value,
    M = mean,
    SD = sd,
    Med = median,
    Min = min,
    Max = max,
    SE = se
  ) %>% arrange(Value) %>% 
  mutate(Entry=row_number(), .before=Data)
  

nessvalues <- unique(df_represent_hov_joined[2,4])

# ---- save
save(list=c("df_represent_joined","dissim_prop","df_represent_hov_joined","nessvalues"),
     file="data/represent.Rdata")
