# --- Descriptive stats

# --- packages import
if (!require(rstatix)) {
  install.packages("rstatix")
  require(rstatix)
}
if (!require(Hmisc)) {
  install.packages("Hmisc")
  require(Hmisc)
}
if (!require(corrplot)) {
  install.packages("corrplot")
  require(corrplot)
}
if (!require(psych)) {
  install.packages("psych")
  require(psych)
}

library(rstatix)
library(Hmisc)
library(corrplot)
library(psych)
library(tidyverse)
library(sjlabelled)
# --- data import

load("data/df.Rdata")

# --- extra functions
source("scripts/helper_functions.R")


#########  gets percentages for female, abitur and employed #######

sample<-df_crypto %>% 
  dplyr::select(female,abitur,employed) %>% 
  remove_all_labels() %>% 
  #naturalize_labelled_df() %>%
  pivot_longer(cols=c(female,abitur,employed)) %>% 
  group_by(name,value) %>% 
  summarise(n=n()) %>% 
  mutate( total_n=sum(n),
          prop=100*n/sum(n),
          cum=100*cumsum(prop/sum(prop))) %>% 
  ungroup()

############## calculates correlations ########

crr_cor<-Hmisc::rcorr(as.matrix(df_crypto))$r %>% round_df()
crr_p<-Hmisc::rcorr(as.matrix(df_crypto))$P %>% round_df()


########## gets gradients of awareness, intention and behavior ########

list_krypto_gradient <- list("aware","intends","holds")

# awareness
# n missing on aware
na_aware<-summary(df_crypto$aware)[7]

gradient_aware <- df_crypto %>% 
  dplyr::select(aware) %>% 
  pivot_longer(everything()) %>% 
  group_by(name,value) %>% 
  summarise(n=n()) %>% 
  mutate(total_n=sum(n),
         prop=100*n/total_n,
         cum=100*cumsum(prop/sum(prop)))

# intention
tmpint<-df_crypto %>% 
  filter(aware==1 & holds==0)
na_intends<-summary(tmpint$intends)[7]

gradient_intention <- tmpint %>% 
  dplyr::select(intends) %>% na.omit() %>% 
  pivot_longer(everything()) %>% 
  group_by(name,value) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(total_n=sum(n),
         prop=100*n/total_n,
         cum=100*cumsum(prop/sum(prop)))

# behavior
tmphold<-df_crypto %>% 
  filter(aware==1)
na_holds<-summary(tmphold$holds)[7]

gradient_behavior <- tmphold %>% 
  dplyr::select(holds) %>% na.omit() %>% 
  pivot_longer(everything()) %>% 
  group_by(name,value) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(total_n=sum(n),
         prop=100*n/total_n,
         cum=100*cumsum(prop/sum(prop)))

# --- save data
save(list=c("sample","crr_cor","crr_p",
            "gradient_aware","gradient_intention","gradient_behavior"),
     file="data/descriptives.Rdata")
