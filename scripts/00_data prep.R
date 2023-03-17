### ---- Data preparation

r <- getOption("repos")
r["CRAN"] <-"https://cloud.r-project.org/"
options(repos=r)

## these are packages needed for these analyses
if (!require(tidyverse)) {
  install.packages("tidyverse")
  require(tidyverse)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}
if (!require(knitr)) {
  install.packages("knitr")
  require(knitr)
}
if (!require(wesanderson)) {
  install.packages("wesanderson")
  require(wesanderson)
}
if (!require(haven)) {
  install.packages("haven")
  require(haven)
}
library("tidyverse")
library("dplyr")
library("knitr")
library("wesanderson")
library("haven")

#### imports data
tempdf <- haven::read_sav("./data/data.sav",user_na = T)

######################## checks for straightlining #########

# --- exclude participants due to data quality concerns 
# (answered 15 or more questions with the same answer OR 
# the within person SD over all PVQ items was SD < 0.5, 
#which is considered too small and with too little variability)
library("readxl")

excludeid <- read_excel("data/exlude_ids.xlsx")
p0001<-as.list(excludeid[1])
p0002<-as.list(excludeid[2])

dropndf<-filter(tempdf, 
              !p_0001 %in% p0001$p_0001)
dropndf<-filter(tempdf, 
              !p_0002 %in% p0002$p_0002)

ndrop <- length(tempdf$lfdn)-length(tempdf$lfdn)

# --- selects only variables of interest
df <- dropndf %>% 
  dplyr::select(age,sex,edu, # demographics
                starts_with("inc"), # income
                employ, # employment
                starts_with("lgi"), # valigo value instrument
                starts_with("pvq"), # pvq value instrument
                starts_with("kry")) # krypto items


############ prepares the value indices ##############
# pvq = person value questionnaire (see Schwartz work) 

# --- Variable and Value labels in a data frame
df_labels <- register_labels(df)

# --- combines male and female pvq versions
pvq_m_names <- df %>%
  dplyr::select(intersect(starts_with("pvq"), ends_with("m"))) %>% 
  names()
pvq_f_names <- df %>%
  dplyr::select(intersect(starts_with("pvq"), ends_with("f"))) %>% 
  names()
#pvq_names <-cbind(pvq_m_names,pvq_f_names)
df <- df %>% mutate(pvqsd01 = coalesce(pvqsd01m,pvqsdt01f),
                       pvqses02 = coalesce(pvqses02m,pvqses02f),
                       pvqhed03 = coalesce(pvqhed03m,pvqhed03f),
                       pvqcoi04 = coalesce(pvqcoi04m,pvqcoi04f),
                       pvqunc05 = coalesce(pvqunc05m,pvqunc05f),
                       pvqpod06 = coalesce(pvqpod06m,pvqpod06f), 
                       pvqhum07 = coalesce(pvqhum07m,pvqhum07f),
                       pvqunn08 = coalesce(pvqunn08m,pvqunn08f),
                       pvqfac09 = coalesce(pvqfac09m,pvqfac09f),
                       pvqsti10 = coalesce(pvqsti10m,pvqsti10f),
                       pvqbec11 = coalesce(pvqbec11m,pvqbec11f),
                       pvqpor12 = coalesce(pvqpor12m,pvqpor12f),
                       pvqsep13 = coalesce(pvqsep13m,pvqsep13f),
                       pvqunt14 = coalesce(pvqunt14m,pvqunt14f), 
                       pvqcor15 = coalesce(pvqcor15m,pvqcor15f),
                       pvqsda16 = coalesce(pvqsda16m,pvqsda16f),
                       pvqach17 = coalesce(pvqach17m,pvqach17f),
                       pvqtra18 = coalesce(pvqtra18m,pvqtra18f),
                       pvqbed19 = coalesce(pvqbed19m,pvqbed19f),
                       pvqpor20 = coalesce(pvqpor20m,pvqpor20f),
                       pvqunn21 = coalesce(pvqunn21m,pvqunn21f),
                       pvqcoi22 = coalesce(pvqcoi22m,pvqcoi22f),
                       pvqsdt23 = coalesce(pvqsdt23m,pvqsdt23f),
                       pvqfac24 = coalesce(pvqfac24m,pvqfac24f),
                       pvqbec25 = coalesce(pvqbec25m,pvqbec25f),
                       pvqsep26 = coalesce(pvqsep26m,pvqsep26f),
                       pvqbed27 = coalesce(pvqbed27m,pvqbed27f),
                       pvqsti28 = coalesce(pvqsti28m,pvqsti28f),
                       pvqpod29 = coalesce(pvqpod29m,pvqpod29f),
                       pvqsda30 = coalesce(pvqsda30m,pvqsda30f),
                       pvqcor31 = coalesce(pvqcor31m,pvqcor31f),
                       pvqach32 = coalesce(pvqach32m,pvqach32f),
                       pvqtra33 = coalesce(pvqtra33m,pvqtra33f),
                       pvqunt34 = coalesce(pvqunt34m,pvqunt34f),
                       pvqses35 = coalesce(pvqses35m,pvqses35f),
                       pvqhed36 = coalesce(pvqhed36m,pvqhed36f),
                       pvqunc37 = coalesce(pvqunc37m,pvqunc37f),
                       pvqhum38 = coalesce(pvqhum38m,pvqhum38f),
                       pvqsdt39 = coalesce(pvqsdt39m,pvqsdt39f),
                       pvqtra40 = coalesce(pvqtra40m,pvqtra40f),
                       pvqpod41 = coalesce(pvqpod41m,pvqpod41f),
                       pvqcor42 = coalesce(pvqcor42m,pvqcor42f),
                       pvqsti43 = coalesce(pvqsti43m,pvqsti43f),
                       pvqpor44 = coalesce(pvqpor44m,pvqpor44f),
                       pvqunn45 = coalesce(pvqunn45m,pvqunn45f),
                       pvqhed46 = coalesce(pvqhed46m,pvqhed46f),
                       pvqbec47 = coalesce(pvqbec47m,pvqbec47f),
                       pvqach48 = coalesce(pvqach48m,pvqach48f),
                       pvqfac49 = coalesce(pvqfac49m,pvqfac49f),
                       pvqses50 = coalesce(pvqses50m,pvqses50f),
                       pvqcoi51 = coalesce(pvqcoi51m,pvqcoi51f),
                       pvqunc52 = coalesce(pvqunc52m,pvqunc52f),
                       pvqsep53 = coalesce(pvqsep53m,pvqsep53f),
                       pvqhum54 = coalesce(pvqhum54m,pvqhum54f),
                       pvqbed55 = coalesce(pvqbed55m,pvqbed55f),
                       pvqsda56 = coalesce(pvqsda56m,pvqsda56f),
                       pvqunt57 = coalesce(pvqunt57m,pvqunt57f)) %>% 
           dplyr::select(-ends_with("m"),
                     -ends_with("f"))

## creates list object with names of value items
# list used for calculating cronbach alpha and ipsatizes scores

# - pvq hov
pvq_hov <- list(
  pvq_STR = c("pvqbed19", "pvqbed27", "pvqbed55",
              "pvqbec11", "pvqbec25", "pvqbec47",
              "pvqunt14", "pvqunt34", "pvqunt57",
              "pvqunc05", "pvqunc37", "pvqunc52",
              "pvqunn08", "pvqunn21", "pvqunn45",
              "pvqhum07", "pvqhum38", "pvqhum54"),
  pvq_CON = c("pvqcoi04", "pvqcoi22", "pvqcoi51",
              "pvqcor15", "pvqcor31", "pvqcor42",
              "pvqtra18", "pvqtra33", "pvqtra40",
              "pvqses02", "pvqses35", "pvqses50",
              "pvqsep13", "pvqsep26", "pvqsep53",
              "pvqfac09", "pvqfac24", "pvqfac49"),
  pvq_SEN = c("pvqpor12", "pvqpor20", "pvqpor44",
              "pvqpod06", "pvqpod29", "pvqpod41",
              "pvqach17", "pvqach32", "pvqach48",
              "pvqhed03", "pvqhed36", "pvqhed46"),
  pvq_OCH = c("pvqsti10","pvqsti28", "pvqsti43",
              "pvqsda16", "pvqsda30", "pvqsda56",
              "pvqsd01", "pvqsdt23", "pvqsdt39")
)

# - list with pvq hov
pvq_STR = c("pvqbed19", "pvqbed27", "pvqbed55",
            "pvqbec11", "pvqbec25", "pvqbec47",
            "pvqunt14", "pvqunt34", "pvqunt57",
            "pvqunc05", "pvqunc37", "pvqunc52",
            "pvqunn08", "pvqunn21", "pvqunn45",
            "pvqhum07", "pvqhum38", "pvqhum54")
pvq_CON = c("pvqcoi04", "pvqcoi22", "pvqcoi51",
            "pvqcor15", "pvqcor31", "pvqcor42",
            "pvqtra18", "pvqtra33", "pvqtra40",
            "pvqses02", "pvqses35", "pvqses50",
            "pvqsep13", "pvqsep26", "pvqsep53",
            "pvqfac09", "pvqfac24", "pvqfac49")
pvq_SEN = c("pvqpor12", "pvqpor20", "pvqpor44",
            "pvqpod06", "pvqpod29", "pvqpod41",
            "pvqach17", "pvqach32", "pvqach48",
            "pvqhed03", "pvqhed36", "pvqhed46")
pvq_OCH = c("pvqsti10","pvqsti28", "pvqsti43",
            "pvqsda16", "pvqsda30", "pvqsda56",
            "pvqsd01", "pvqsdt23", "pvqsdt39")

# - list
pvq_hov_list <- list(names(pvq_hov))

########## calculates alpha reliability for pvq indices #######

# --- empty tibble object to be populated with cronbach values

cronb_df <- tibble(
  variable = vector("character",0),
  alpha = vector("numeric",0),
  ci_low = vector("numeric",0),
  ci_high = vector("numeric",0)
)

###### - cronbach alpha for pvq hov
for (i in pvq_hov){
  
  name = i
  # name <- deparse(substitute(i))
  tmp <- df %>% 
    dplyr::select(i) %>% 
    ltm::cronbach.alpha(.,na.rm=TRUE,CI=TRUE,B=100)
  
  cronb_df <- cronb_df %>% add_row(
    variable = name,
    alpha = tmp$alpha,
    ci_low = tmp$ci[1],
    ci_high = tmp$ci[2]
  )
  
  rm(tmp)
  cronb_df <- cronb_df %>% distinct(., alpha, .keep_all = T)
  
}

# renames the indexes
cronb_df$indexe <- names(pvq_hov)

# relocates column with index names for readability purposes
cronb_df <- cronb_df %>% 
  relocate(indexe, .before=alpha) %>% 
  subset(.,select=-variable) %>% round_df()

# alpha reliabilities and 95 ci
cronb_df


######### Ipsatization and then Scale construction #################

names_pvq <- df %>% dplyr::select(starts_with("pvq")) %>% names()

df_ipst <- df 
df_ipst$pvq_mean <- rowMeans(df_ipst[names_pvq], na.rm = FALSE)

df_ipst <- df_ipst %>% rowwise() %>% 
  mutate(across(all_of(names_pvq), ~.x - pvq_mean)) %>% 
  ungroup() %>% 
  scale_builder(., pvq_hov, score_fun = "mean") %>% 
  dplyr::select(-one_of(pvq_SEN),-one_of(pvq_STR),
         -one_of(pvq_CON),-one_of(pvq_OCH))

# ---- prepare data
# this is the dataset to be used for further analyses
df_crypto <- df_ipst %>% 
  mutate_at(vars("krybws01"), 
            function(x) case_when(
              x == 1 ~ 1, # has heard
              x == 2 ~ 0 # not yet heard
            )) %>% 
  mutate_at(vars("krybst01"),
            function(x) case_when(
              x == 1 ~ 1, # currently holds crypto
              x == 2 ~ 1, # held in the past crypto
              x == 3 ~ 0 # never held crypto
            )) %>% 
  mutate_at(vars("krybsz01"),
            function(x) case_when(
              x == 1 ~ 1, # future yes crypto
              x == 2 ~ 0, # future no crypto
              x == 3 ~ 0 # future unsure crypto
            )) %>% 
  mutate_at(vars("sex"),
            function(x) case_when(
              x == 1 ~ 0, # male
              x == 2 ~ 1 # female
            )) %>% 
  mutate_at(vars("edu"),
            function(x) case_when(
              x == 8 ~ 1, # Abitur
              x == 1 ~ 0, # Grundschule nicht beendet
              x == 2 ~ 0, # noch kein Abschluss, Grundschule beendet
              x == 3 ~ 0, # Volks- oder Hauptschulabschluss
              x == 4 ~ 0, # Mittlere Reife
              x == 7 ~ 0, # Fachhochshulreife
              x == 9 ~ 0 # anderer Schulabschluss
            )) %>% 
  mutate_at(vars("employ"),
            function(x) case_when(
              x == 1 ~ 1, # Angestellt (employed)
              x == 2 ~ 0, # Selbstständig
              x == 3 ~ 0, # Arbeitslos und Arbeitssuchend
              x == 4 ~ 0, # Arbeitslos und nicht Arbeitssuchen
              x == 5 ~ 0, # Hausfrau/Hausmann
              x == 6 ~ 0, # Schüler/Student
              x == 7 ~ 0, # Auszubildender/Praktikant
              x == 8 ~ 0, # Renter
              x == 9 ~ 0 # andere
            )) %>% 
  rename(female=sex,
         abitur=edu,
         employed=employ,
         investment=krymng01,
         exchange=krymng02,
         regularization=krymng03,
         illegal=krymng04,
         goodtime=krymng05,
         aware=krybws01,
         holds=krybst01,
         intends=krybsz01) %>% 
   set_na(investment:goodtime,na=c(-77,8)) %>% 
  dplyr::select(female,abitur,age,employed, # covariates
                aware:intends, # outcome variables
                pvq_STR:pvq_OCH, # ipsatized values
                investment:goodtime # beliefs about crypto
                )

# - inspects data
psych::describe(df_crypto)

# --- schwartz circle 
# uses LittleHelpers source package from Maksim Rudnev: https://github.com/MaksimRudnev/LittleHelpers/

#pdf("img/schwartz_circle.pdf", height = 7, width = 12)
#LittleHelpers::schwartz_circle()
#dev.off()

## --- save data

save(list=c("df", "df_ipst","df_crypto",
            "df_labels","cronb_df",
            "pvq_hov","ndrop"),
     file="data/df.Rdata")
