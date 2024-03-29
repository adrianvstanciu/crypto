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

dftmp<-df_crypto %>% 
  dplyr::select(aware,intends,holds,age,female,abitur,employed,pvq_SEN,pvq_OCH,understands,investment,exchange,regularization,illegal,goodtime)
crr_cor<-Hmisc::rcorr(as.matrix(dftmp))$r %>% round_df()
crr_p<-Hmisc::rcorr(as.matrix(dftmp))$P %>% round_df()


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

# profile of those who heard
profaware<-df_crypto %>% 
  filter(aware==1) %>% 
  dplyr::select(age,female,abitur,employed, pvq_SEN,pvq_OCH)
psych::describe(profaware)

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

profintends<-df_crypto %>% 
  filter(aware==1 & holds == 0 & intends == 1) %>% 
  dplyr::select(age,female,abitur,employed, pvq_SEN,pvq_OCH)
psych::describe(profintends)

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

profholds<-df_crypto %>% 
  filter(aware==1 & holds == 1) %>% 
  dplyr::select(age,female,abitur,employed, pvq_SEN,pvq_OCH)
psych::describe(profholds)

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
       x="Cryptocurrency adoption",
       fill="Level") + theme_classic()
#dev.off()


# --- creates sankey diagram
#install.packages("circlize")
#install.packages("networkD3")

# install.packages("devtools")
# devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)

tmp<-df_crypto %>% dplyr::select(aware,intends,holds) 

tmp2<-tmp %>% dplyr::rename(Awareness=aware,
                     `Future intention`=intends,
                     Ownership=holds)

tmp2$Awareness<-factor(tmp2$Awareness,
                  levels=c(0,1),
                  labels=c("Not aware","Aware"))

tmp2$`Future intention`<-factor(tmp2$`Future intention`,
                    levels=c(0,1),
                    labels=c("No intention","Intention"))

tmp2$Ownership<-factor(tmp2$Ownership,
                  levels=c(0,1),
                  labels=c("No ownership","Ownership"))

df_skey<-tmp2 %>% make_long(Awareness,`Future intention`,Ownership)

count_tmp<-df_skey %>% group_by(x,node) %>% summarise(count=n())

df_skey_count <- df_skey %>% left_join(count_tmp)

#png("img/fig_sample_cryptoadoption_2.png",units = "in",width=10, height= 6,res=300)
skeypl <- ggplot(df_skey_count, aes(x = x
                              , next_x = next_x
                              , node = node
                              , next_node = next_node
                              , fill = factor(node)
                              , label = paste0(node, " (n = ", count, ")"))) +
  geom_sankey(flow.alpha = 0.5
              ,node.color = "gray30"
              ,show.legend = FALSE) + 
  geom_sankey_label(size=3,
                    fill="white") + 
  labs(x=NULL)+
  theme(legend.position = "none") +
  theme_sankey(base_size=16)

skeypl
#dev.off()


# --- save data
save(list=c("sample","crr_cor","crr_p",
            "gradient_aware","gradient_intention","gradient_behavior"),
     file="data/descriptives.Rdata")
