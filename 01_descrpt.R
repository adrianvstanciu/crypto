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

# --- data import

load("data/df.Rdata")

# --- extra functions
source("scripts/helper_functions.R")



# --- sample and item descriptions

# - categorial variables
# employment = employ
# sex = sex
# education = edu

sample<-df_ipst %>% 
  dplyr::select(sex,edu,employ) %>% 
  naturalize_labelled_df() %>%
  pivot_longer(cols=c(sex_fct,
                      edu_fct,
                      employ_fct)) %>% 
  group_by(name,value) %>% 
  summarise(n=n()) %>% 
  mutate( total_n=sum(n),
          prop=100*n/sum(n),
          cum=100*cumsum(prop/sum(prop))) %>% 
  ungroup()

# - ordinal variables 


  
# --- Correlations

corr_df <- df_ipst %>% 
  filter(krybws01==1) %>% 
  dplyr::select(age,
                starts_with("pvq"),starts_with("valigo"),
                starts_with("kry"))

correlations <- corr_df %>% 
  select(-kryerw11tb,-krykew10tb,-pvq_mean,-valigo_mean) %>% 
  cor_test(everything(),
           use="pairwise.complete.obs") %>% 
  subset(.,select=c(-statistic,-method))

# - sub-table with select correlations
# - pvq dimensions and hov with everything else
correlations_pvq <- correlations %>% 
  filter(grepl("pvq",var1))

# - valigo dimensions and hov with everything else
correlations_valigo <- correlations %>% 
  filter(grepl("valigo",var1))

# --- correlations in a different form

corr_v2 <- df_ipst %>% 
  filter(krybws01==1) %>% 
  dplyr::select(age,
                ends_with("_SEN"),
                ends_with("_STR"),
                ends_with("_CON"),
                ends_with("_OCH"),
                starts_with("kryftl"),
                kryvst01,
                starts_with("krymng"))


mydata.rcorr <- rcorr(as.matrix(corr_v2)) 
#mydata.rcorr

mydata.cor <- cor(corr_v2, method="spearman")
corrplot(mydata.cor)
heatmap(mydata.cor,symm=TRUE)

# --- correlations in a 3rd form

corr_v3 <- df_ipst %>% 
  cor_test(vars = (starts_with("pvq") & !ends_with("_mean") ),
           vars2 = c(age,
                     starts_with("kryftl"),
                     kryvst01,
                     starts_with("krymng")),
           use="pariwise.complete.obs") %>% 
  subset(.,select=c(-statistic,
                    -method ) )

# --- correlations Schwart 12

corr_v4 <- df_ipst %>% 
  cor_test(vars = (starts_with("pvq12") & !ends_with("_mean") ),
           vars2 = c(age,
                     starts_with("kryftl"),
                     kryvst01,
                     starts_with("krymng")),
           use="pariwise.complete.obs") %>% 
  subset(.,select=c(-statistic,
                    -method ) )

# --- save data
save(list=c("sample", "corr_df", "correlations",
            "corr_v3", "corr_v4","mydata.rcorr"),
     file="data/descriptives.Rdata")
