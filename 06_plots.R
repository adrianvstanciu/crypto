#### ---- creates plots
## uses Ranjits code


library(tidyverse)
library(viridis)
library(stringr)

# - loads data
load("data/df.Rdata")

# creates english labels

df_ipst_relabeled <- df_ipst %>%
  mutate(
    krybws01 = krybws01 %>% as.numeric %>% recode_factor(
      `1` = "Has heard",
      `2` = "Has not heard"),
    krybst01 = krybst01 %>% as.numeric %>% recode_factor(
      `1` = "Currently holds",
      `2` = "Previously held",
      `3` = "Never held"
    ),
    krybsz01 = krybsz01 %>% as.numeric %>%  recode_factor(
      `1` = "Wants to hold",
      `2` = "Does not want to hold",
      `3` = "Uncertain"
    )
  )

# prepres data for plotting

df_long <- df_ipst_relabeled %>% 
  mutate(across(contains("kryb"), na_if, -77)) %>% 
  pivot_longer(cols = pvq_STR:pvq_OCH, names_to = "value_dim", values_to = "value_score") %>% 
  mutate(value_dim = factor(value_dim,
                            levels = c("pvq_STR", "pvq_SEN", "pvq_OCH", "pvq_CON"),
                            labels = c(
                              "STR",
                              "SEN",
                              "OCH",
                              "CON"
                            ))) 

# saves data and plotting function

save(list=c("df_long","df_ipst_relabeled"),
     file="data/plots.Rdata")