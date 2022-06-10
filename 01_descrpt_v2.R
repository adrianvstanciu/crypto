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
library(dplyr)
library(tidyverse)

# --- data import

load("data/df.Rdata")

# --- list with crypto items for descriptive gradient

list_krypto_gradient <- list("krybws01","krybst01","krybsz01")

gradient_heardof <- df_ipst %>% 
  dplyr::select(krybws01) %>% 
  pivot_longer(everything()) %>% 
  group_by(name,value) %>% 
  summarise(n=n()) %>% 
  mutate(total_n=sum(n),
         prop=100*n/total_n,
         cum=100*cumsum(prop/sum(prop)))

gradient_holds <- df_ipst %>% 
  filter(krybws01==1) %>% 
  dplyr::select(krybst01) %>% 
  pivot_longer(everything()) %>% 
  group_by(name,value) %>% 
  summarise(n=n()) %>% 
  mutate(total_n=sum(n),
         prop=100*n/total_n,
         cum=100*cumsum(prop/sum(prop)))

gradient_futurehold_heardof <- df_ipst %>% 
  filter(krybws01==1) %>% 
  dplyr::select(krybsz01) %>% 
  pivot_longer(everything()) %>% 
  group_by(name,value) %>% 
  summarise(n=n()) %>% 
  mutate(total_n=sum(n),
         prop=100*n/total_n,
         cum=100*cumsum(prop/sum(prop)))

gradient_futurehold_holdsnot <- df_ipst %>% 
  filter(krybws01==1 & krybst01==3) %>% 
  dplyr::select(krybsz01) %>% 
  pivot_longer(everything()) %>% 
  group_by(name,value) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(total_n=sum(n),
         prop=100*n/total_n,
         cum=100*cumsum(prop/sum(prop)))

# --- ready to fill in table below
n_dim_awareness = gradient_heardof[1,4] %>% as.numeric()
n_dim_holds = gradient_holds[1,4] %>% as.numeric()
n_dim_future_aware = gradient_futurehold_heardof[1,4] %>% as.numeric()
n_dim_future_holdsnownot = gradient_futurehold_holdsnot[1,4] %>% as.numeric
# awareness
prop_heardofyes = gradient_heardof[1,5] %>% as.numeric()
prop_heardofno = gradient_heardof[2,5] %>% as.numeric()
# holding behavior
prop_holdsnow = gradient_holds[1,5] %>% as.numeric()
prop_holdspast = gradient_holds[2,5] %>% as.numeric()
prop_holdsnot = gradient_holds[3,5] %>% as.numeric()
# intention behavior among those with awareness
prop_future_aware_yes = gradient_futurehold_heardof[1,5] %>% as.numeric()
prop_future_aware_no = gradient_futurehold_heardof[2,5] %>% as.numeric()
prop_future_aware_unclear = gradient_futurehold_heardof[3,5] %>% as.numeric()
prop_future_aware_na = gradient_futurehold_heardof[4,5] %>% as.numeric()
# intention behavior among those who never held but heard of
prop_future_neverheld_yesfuture = gradient_futurehold_holdsnot[1,5] %>% as.numeric()
prop_future_neverheld_nofuture = gradient_futurehold_holdsnot[2,5] %>% as.numeric()
prop_future_neverheld_unclearfuture = gradient_futurehold_holdsnot[3,5] %>% as.numeric()
prop_future_neverheld_nafuture = gradient_futurehold_holdsnot[4,5] %>% as.numeric()

# ---- table gradient awareness - current hold - holds in the future
gradient_descriptive <- tibble(
  Dimension = c("Awareness","",
                "Holding","","",
                "Holding intention ","","","",
                "Holding intention if never held","","",""),
  N = c(n_dim_awareness,"",
                    n_dim_holds,"","",
                    n_dim_future_aware,"","","",
                    n_dim_future_holdsnownot,"","",""),
  Case = c("Yes","No",
           "Yes","Yes, in the past ","Never",
           "Yes","No","Unsure","Data missing",
           "Yes","No","Unsure","Data missing"),
  "%" = c(prop_heardofyes,prop_heardofno,
                         prop_holdsnow,prop_holdspast,prop_holdsnot,
                         prop_future_aware_yes,prop_future_aware_no,prop_future_aware_unclear,prop_future_aware_na,
                         prop_future_neverheld_yesfuture,prop_future_neverheld_nofuture,prop_future_neverheld_unclearfuture,prop_future_neverheld_nafuture)
)


# --- save data
save(list=c("gradient_descriptive"),
     file="data/gradient.Rdata")