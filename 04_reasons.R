##### ------ Reasons for behavior

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
library(ggplot2)

# --- data import

load("data/df.Rdata")

## installs developer tools
remotes::install_github("maksimrudnev/LittleHelpers") # useful for ess data

# --- import source script

source("scripts/helper_functions.R")



###### ----------------------------------------------

# ----- Legend
# - a = heard of cryptocurrencies + holds now or held in the past cryptocurrencies
# - b = heard of cryptocurrencies + not yet held cryptocurrencies


# --- reasons for holding 

a <- df_ipst %>% filter(krybws01==1 & (krybst01==1 | krybst01==2))

reasonsfor <-a %>% dplyr::select(kryerw01:kryerw10) %>% rename(FOM = kryerw01,
                                                  Fun = kryerw02,
                                                  "Learn more" = kryerw03,
                                                  "Earn money fast" = kryerw04,
                                                  "Longterm investment" = kryerw05,
                                                  Inheritance = kryerw06,
                                                  "Support Blockchain tech" = kryerw07,
                                                  "Online shopping" = kryerw08,
                                                  "Money transfer" = kryerw09,
                                                  "Diversify investment portofolio" = kryerw10) %>% 
  pivot_longer(everything()) %>% 
  group_by(name,value) %>% 
  summarise(n=n()) %>% 
  mutate( total_n=sum(n),
          prop=100*n/sum(n),
          cum=100*cumsum(prop/sum(prop))) %>% 
  ungroup()

n_reasonsfor = reasonsfor[1,4]


# --- reasons for not holding

b <- df_ipst %>% filter(krybws01==1 & krybst01==3)

reasonsno <-b %>% dplyr::select(krykew01:krykew09) %>% rename("Cannot afford it" = krykew01,
                                                               "Risk too high" = krykew02,
                                                               "Insufficiently informed" = krykew03,
                                                               "State regulation insufficient" = krykew04,
                                                               "Alternative investment more attractive" = krykew05,
                                                               "No interest" = krykew06,
                                                               "Generally no interest in investing" = krykew07,
                                                               "Price instability is worrisome" = krykew08,
                                                               "Money needed for other reasons" = krykew09) %>% 
  pivot_longer(everything()) %>% 
  group_by(name,value) %>% 
  summarise(n=n()) %>% 
  mutate( total_n=sum(n),
          prop=100*n/sum(n),
          cum=100*cumsum(prop/sum(prop))) %>% 
  ungroup()

n_reasonsno = reasonsno[1,4]


###### --------------------
library("wesanderson")
mytheme <- theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
                 panel.background = element_rect(fill="white", color = "grey50"),
                 panel.grid.major.y = element_line(color="grey90"),
                 axis.title.y = element_text(vjust= 1, hjust=1, angle = 90)
)

theme_set(mytheme)


# reasons for holding
reasonsfor <- reasonsfor %>% sjlabelled::remove_all_labels() 

#plot.a <- reasonsfor %>%  ggplot(., 
#                       aes(x=name,y=prop,color=factor(value))) + 
#  geom_point(aes(x=name,y=prop,color=factor(value)),size=4,na.rm=TRUE) +
#  geom_text(aes(label = paste0(round(prop, digits=2),"%")), position=position_dodge(width=0.5), vjust=-0.9, size=3) +
#  labs(x="Named reason",
#       y="Frequency of named reason") +
#  scale_color_manual(name="Mentioned?",
#                     values=c("red","lightblue"),
#                     labels=c("No","Yes"))

plot.a <- reasonsfor %>%  ggplot(., 
                                aes(x=name,y=prop,fill=factor(value))) + 
  #geom_point(aes(x=name,y=prop,color=factor(value)),size=4,na.rm=TRUE) + 
  geom_bar(stat="identity",color="black") + 
  geom_text(aes(label = round(prop, digits=2)), 
            position=position_dodge(width=0.1), vjust=-0.1, size=3.5) +
  labs(x="Named reason",
       y=paste0("Frequency of named reason in % (N = ",n_reasonsfor,")")) + 
  guides(fill=guide_legend("Mentioned?")) +
  scale_fill_discrete(labels=c("No","Yes"))

# reasons for not holding

reasonsno <- reasonsno %>% sjlabelled::remove_all_labels() 

#plot.b <- reasonsno %>%  ggplot(., 
#                                 aes(x=name,y=prop,color=factor(value))) + 
#  #geom_point(aes(x=name,y=prop,color=factor(value)),size=4,na.rm=TRUE) + 
#  geom_bar(stat="identity") +
#  geom_text(aes(label = paste0(round(prop, digits=2),"%")), position=position_dodge(width=0.5), vjust=-0.9, size=3) +
#  labs(x="Named reason",
#       y="Frequency of named reason") +
#  scale_color_manual(name="Mentioned?",
#                     values=c("red","lightblue"),
#                     labels=c("No","Yes")) 

plot.b <- reasonsno %>%  ggplot(., 
                                aes(x=name,y=prop,fill=factor(value))) + 
  #geom_point(aes(x=name,y=prop,color=factor(value)),size=4,na.rm=TRUE) + 
  geom_bar(stat="identity",color="black") + 
  geom_text(aes(label = round(prop, digits=2)), 
            position=position_dodge(width=0.1), vjust=-0.1, size=3.5) +
  labs(x="Named reason",
       y=paste0("Frequency of named reason in % (N = ",n_reasonsno,")")) + 
  guides(fill=guide_legend("Mentioned?")) +
  scale_fill_discrete(labels=c("No","Yes"))

### ----- save
save(list=c("plot.a","plot.b","reasonsfor","n_reasonsfor","reasonsno","n_reasonsno"),
     file="data/reasons.Rdata")