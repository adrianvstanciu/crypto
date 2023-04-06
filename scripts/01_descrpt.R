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

# color blind palette
cbPalette <- c("#999999","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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

descrbdata<-full_join(gradient_aware,gradient_intention) %>% full_join(gradient_behavior)


#png("img/fig_sample_cryptoadoption.png",units = "in",width=10, height= 6,res=300)
ggplot(descrbdata,aes(x=name,y=n,fill=factor(value)))+
  scale_fill_manual(values=cbPalette)+
  geom_col(aes(fill=factor(value)))+ 
  geom_hline(yintercept=c(714,637,538))+
  annotate("text",x="intends",y=714,label="Total sample, N=714",vjust=-0.5)+
  annotate("text",x="intends",y=637,label="Aware of cryptocurrency, N=637",vjust=-0.5)+
  annotate("text",x="intends",y=538,label="Aware of cryptocurrency 
           and never held cryptocurrency, N=538",vjust=-0.5)+
  geom_text(aes(label=paste0(round(prop,2)," %")),position=position_stack(),vjust=2) + 
  labs(y="Sample size", 
       x="Cryptocurrency levels",
       fill="Level") + theme_classic()
#dev.off()

# --- save data
save(list=c("sample","crr_cor","crr_p",
            "gradient_aware","gradient_intention","gradient_behavior"),
     file="data/descriptives.Rdata")
