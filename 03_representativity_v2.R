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

# legend
# a = df_perage_heardof
# b = df_perage_holds
# c = df_perage_futurehold_heardof
# d = df_perage_futurehold_holdsnot 

#### --------------------------------------------------------------------------------

##### ----- AGE Categories 
# --- awareness: 1=has heard, 2=hasn't heard of cryptocurrencies
na.a <- table(is.na(df_ipst$krybws01))

a <- df_ipst %>% 
  sjlabelled::remove_all_labels() %>%
  dplyr::mutate(age_cat = cut(age, 
                              breaks=c(18,30,40,50,60,70,Inf),
                              labels=c("18-30","30-40","40-50","50-60","60-70","70+"),
                              right=FALSE)) %>% # left break is closed, right is open
  dplyr::mutate(across(age_cat, as.double)) %>%
  group_by(age_cat) %>% 
  nest() %>% 
  mutate(data=map(data, ~ .x %>% 
                    dplyr::select(krybws01) %>% 
                    pivot_longer(everything()) %>% 
                    group_by(name,value) %>% 
                    na.omit() %>% 
                    summarise(n=n()) %>% 
                    mutate(total_n=sum(n),
                           prop=100*n/total_n,
                           cum=100*cumsum(prop/sum(prop)))
                    
                    )) %>% 
  unnest(data) %>% 
  dplyr::select(-name)

# --- only among those who have heard of cryptocurrencies, do they 1=hold now, 2=held in the future, 3=never held?
x<-df_ipst %>% filter(krybws01==1)
na.b <-  table(is.na(x$krybst01))

b <- df_ipst %>% 
  filter(krybws01==1) %>% 
  sjlabelled::remove_all_labels() %>%
  dplyr::mutate(age_cat = cut(age, 
                              breaks=c(18,30,40,50,60,70,Inf),
                              labels=c("18-30","30-40","40-50","50-60","60-70","70+"),
                              right=FALSE)) %>% # left break is closed, right is open
  dplyr::mutate(across(age_cat, as.double)) %>%
  group_by(age_cat) %>% 
  nest() %>% 
  mutate(data=map(data, ~ .x %>% 
                    dplyr::select(krybst01) %>% 
                    pivot_longer(everything()) %>% 
                    group_by(name,value) %>% 
                    na.omit() %>% 
                    summarise(n=n()) %>% 
                    mutate(total_n=sum(n),
                           prop=100*n/total_n,
                           cum=100*cumsum(prop/sum(prop)))
                  
  )) %>% 
  unnest(data) %>% 
  dplyr::select(-name)

# --- only among those who have heard of cryptocurrencies, do they 1=intend to hold in the future, 2=not, or 3=undecided
na.c <-  table(is.na(x$krybsz01))

c <- df_ipst %>% 
  filter(krybws01==1) %>% 
  sjlabelled::remove_all_labels() %>%
  dplyr::mutate(age_cat = cut(age, 
                              breaks=c(18,30,40,50,60,70,Inf),
                              labels=c("18-30","30-40","40-50","50-60","60-70","70+"),
                              right=FALSE)) %>% # left break is closed, right is open
  dplyr::mutate(across(age_cat, as.double)) %>%
  group_by(age_cat) %>% 
  nest() %>% 
  mutate(data=map(data, ~ .x %>% 
                    dplyr::select(krybsz01) %>% 
                    pivot_longer(everything()) %>% 
                    group_by(name,value) %>% 
                    na.omit() %>% 
                    summarise(n=n()) %>% 
                    mutate(total_n=sum(n),
                           prop=100*n/total_n,
                           cum=100*cumsum(prop/sum(prop)))
                  
  )) %>% 
  unnest(data) %>% 
  dplyr::select(-name)

# --- only among those who have heard of cryptocurrencies but never held, do they 1=intend to hold in the future, 2=not, or 3=undecided
y<-df_ipst %>%  filter(krybws01==1 & krybst01==3) 
na.d <-  table(is.na(y$krybsz01))

d <- df_ipst %>% 
  filter(krybws01==1 & krybst01==3) %>% 
  sjlabelled::remove_all_labels() %>%
  dplyr::mutate(age_cat = cut(age, 
                              breaks=c(18,30,40,50,60,70,Inf),
                              labels=c("18-30","30-40","40-50","50-60","60-70","70+"),
                              right=FALSE)) %>% # left break is closed, right is open
  dplyr::mutate(across(age_cat, as.double)) %>%
  group_by(age_cat) %>% 
  nest() %>% 
  mutate(data=map(data, ~ .x %>% 
                    dplyr::select(krybsz01) %>% 
                    pivot_longer(everything()) %>% 
                    group_by(name,value) %>% 
                    na.omit() %>% 
                    summarise(n=n()) %>% 
                    mutate(total_n=sum(n),
                           prop=100*n/total_n,
                           cum=100*cumsum(prop/sum(prop)))
                  
  )) %>% 
  unnest(data) %>% 
  dplyr::select(-name)

#### --------------------------------------------------------------------------------
##### ----- GENDER
# --- awareness: 1=has heard, 2=hasn't heard of cryptocurrencies
a.g <- df_ipst %>% 
  sjlabelled::remove_all_labels() %>%
  group_by(sex) %>% 
  nest() %>% 
  mutate(data=map(data, ~ .x %>% 
                    dplyr::select(krybws01) %>% 
                    pivot_longer(everything()) %>% 
                    group_by(name,value) %>% 
                    na.omit() %>% 
                    summarise(n=n()) %>% 
                    mutate(total_n=sum(n),
                           prop=100*n/total_n,
                           cum=100*cumsum(prop/sum(prop)))
                  
  )) %>% 
  unnest(data) %>% 
  dplyr::select(-name)

# --- only among those who have heard of cryptocurrencies, do they 1=hold now, 2=held in the future, 3=never held?
b.g <- df_ipst %>% 
  filter(krybws01==1) %>% 
  sjlabelled::remove_all_labels() %>%
  group_by(sex) %>% 
  nest() %>% 
  mutate(data=map(data, ~ .x %>% 
                    dplyr::select(krybst01) %>% 
                    pivot_longer(everything()) %>% 
                    group_by(name,value) %>% 
                    na.omit() %>% 
                    summarise(n=n()) %>% 
                    mutate(total_n=sum(n),
                           prop=100*n/total_n,
                           cum=100*cumsum(prop/sum(prop)))
                  
  )) %>% 
  unnest(data) %>% 
  dplyr::select(-name)

# --- only among those who have heard of cryptocurrencies, do they 1=intend to hold in the future, 2=not, or 3=undecided
c.g <- df_ipst %>% 
  filter(krybws01==1) %>% 
  sjlabelled::remove_all_labels() %>%
  group_by(sex) %>% 
  nest() %>% 
  mutate(data=map(data, ~ .x %>% 
                    dplyr::select(krybsz01) %>% 
                    pivot_longer(everything()) %>% 
                    group_by(name,value) %>% 
                    na.omit() %>% 
                    summarise(n=n()) %>% 
                    mutate(total_n=sum(n),
                           prop=100*n/total_n,
                           cum=100*cumsum(prop/sum(prop)))
                  
  )) %>% 
  unnest(data) %>% 
  dplyr::select(-name)

# --- only among those who have heard of cryptocurrencies but never held, do they 1=intend to hold in the future, 2=not, or 3=undecided
d.g <- df_ipst %>% 
  filter(krybws01==1 & krybst01==3) %>% 
  sjlabelled::remove_all_labels() %>%
  group_by(sex) %>% 
  nest() %>% 
  mutate(data=map(data, ~ .x %>% 
                    dplyr::select(krybsz01) %>% 
                    pivot_longer(everything()) %>% 
                    group_by(name,value) %>% 
                    na.omit() %>% 
                    summarise(n=n()) %>% 
                    mutate(total_n=sum(n),
                           prop=100*n/total_n,
                           cum=100*cumsum(prop/sum(prop)))
                  
  )) %>% 
  unnest(data) %>% 
  dplyr::select(-name)



######### ------------------------------------------------------------------------
##### ----- EDUCATION
# --- awareness: 1=has heard, 2=hasn't heard of cryptocurrencies
a.e <- df_ipst %>% 
  sjlabelled::remove_all_labels() %>%
  group_by(edu) %>% 
  nest() %>% 
  mutate(data=map(data, ~ .x %>% 
                    dplyr::select(krybws01) %>% 
                    pivot_longer(everything()) %>% 
                    group_by(name,value) %>% 
                    na.omit() %>% 
                    summarise(n=n()) %>% 
                    mutate(total_n=sum(n),
                           prop=100*n/total_n,
                           cum=100*cumsum(prop/sum(prop)))
                  
  )) %>% 
  unnest(data) %>% 
  dplyr::select(-name)

# --- only among those who have heard of cryptocurrencies, do they 1=hold now, 2=held in the future, 3=never held?
b.e <- df_ipst %>% 
  filter(krybws01==1) %>% 
  sjlabelled::remove_all_labels() %>%
  group_by(edu) %>% 
  nest() %>% 
  mutate(data=map(data, ~ .x %>% 
                    dplyr::select(krybst01) %>% 
                    pivot_longer(everything()) %>% 
                    group_by(name,value) %>% 
                    na.omit() %>% 
                    summarise(n=n()) %>% 
                    mutate(total_n=sum(n),
                           prop=100*n/total_n,
                           cum=100*cumsum(prop/sum(prop)))
                  
  )) %>% 
  unnest(data) %>% 
  dplyr::select(-name)

# --- only among those who have heard of cryptocurrencies, do they 1=intend to hold in the future, 2=not, or 3=undecided
c.e <- df_ipst %>% 
  filter(krybws01==1) %>% 
  sjlabelled::remove_all_labels() %>%
  group_by(edu) %>% 
  nest() %>% 
  mutate(data=map(data, ~ .x %>% 
                    dplyr::select(krybsz01) %>% 
                    pivot_longer(everything()) %>% 
                    group_by(name,value) %>% 
                    na.omit() %>% 
                    summarise(n=n()) %>% 
                    mutate(total_n=sum(n),
                           prop=100*n/total_n,
                           cum=100*cumsum(prop/sum(prop)))
                  
  )) %>% 
  unnest(data) %>% 
  dplyr::select(-name)

# --- only among those who have heard of cryptocurrencies but never held, do they 1=intend to hold in the future, 2=not, or 3=undecided
d.e <- df_ipst %>% 
  filter(krybws01==1 & krybst01==3) %>% 
  sjlabelled::remove_all_labels() %>%
  group_by(edu) %>% 
  nest() %>% 
  mutate(data=map(data, ~ .x %>% 
                    dplyr::select(krybsz01) %>% 
                    pivot_longer(everything()) %>% 
                    group_by(name,value) %>% 
                    na.omit() %>% 
                    summarise(n=n()) %>% 
                    mutate(total_n=sum(n),
                           prop=100*n/total_n,
                           cum=100*cumsum(prop/sum(prop)))
                  
  )) %>% 
  unnest(data) %>% 
  dplyr::select(-name)

######### ------------------------------------------------------------------------
##### ---- compiles table

tb <- tibble(
 Variable = vector("character",0),
 Sample = vector("character",0),
 Case = vector("numeric",0),
 Level = vector("numeric",0),
 N = vector("numeric"),
 'Relative %' = vector("numeric",0)
)


pheno_table <- tb %>% # adds results for age categories
  add_row(
    Variable = "Age",
    Sample = "Awareness",
    Case = a$age_cat,
    Level = a$value,
    N = a$total_n,
    'Relative %' = a$prop
  ) %>% 
  add_row(
    Variable = "Age",
    Sample = "Holds",
    Case = b$age_cat,
    Level = b$value,
    N = b$total_n,
    'Relative %' = b$prop
  ) %>% 
  add_row(
    Variable = "Age",
    Sample = "Intention",
    Case = c$age_cat,
    Level = c$value,
    N = c$total_n,
    'Relative %' = c$prop
  ) %>% 
  add_row(
    Variable = "Age",
    Sample = "Intention, never held",
    Case = d$age_cat,
    Level = d$value,
    N = d$total_n,
    'Relative %' = d$prop
  ) %>%                 # adds results for gender
  add_row(
    Variable = "Gender",
    Sample = "Awareness",
    Case = a.g$sex,
    Level = a.g$value,
    N = a.g$total_n,
    'Relative %' = a.g$prop
  ) %>% 
  add_row(
    Variable = "Gender",
    Sample = "Holds",
    Case = b.g$sex,
    Level = b.g$value,
    N = b.g$total_n,
    'Relative %' = b.g$prop
  ) %>% 
  add_row(
    Variable = "Gender",
    Sample = "Intention",
    Case = c.g$sex,
    Level = c.g$value,
    N = c.g$total_n,
    'Relative %' = c.g$prop
  ) %>% 
  add_row(
    Variable = "Gender",
    Sample = "Intention, never held",
    Case = d.g$sex,
    Level = d.g$value,
    N = d.g$total_n,
    'Relative %' = d.g$prop
  ) %>%                 # adds results for education levels
  add_row(
    Variable = "Education",
    Sample = "Awareness",
    Case = a.e$edu,
    Level = a.e$value,
    N = a.e$total_n,
    'Relative %' = a.e$prop
  ) %>% 
  add_row(
    Variable = "Education",
    Sample = "Holds",
    Case = b.e$edu,
    Level = b.e$value,
    N = b.e$total_n,
    'Relative %' = b.e$prop
  ) %>% 
  add_row(
    Variable = "Education",
    Sample = "Intention",
    Case = c.e$edu,
    Level = c.e$value,
    N = c.e$total_n,
    'Relative %' = c.e$prop
  ) %>% 
  add_row(
    Variable = "Education",
    Sample = "Intention, never held",
    Case = d.e$edu,
    Level = d.e$value,
    N = d.e$total_n,
    'Relative %' = d.e$prop
  )


##### -------------------------------------------------
## - saves table

save(list="pheno_table",
     file="data/spreadpheno.Rdata")
