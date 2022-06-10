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

# --- import source script

source("scripts/helper_functions.R")

# --- imports data
tempdf<-haven::read_sav("./data/data_project_1036337_2022_02_28.sav",user_na = T)



# --- exclude participants due to data quality concerns (answered 15 or more questions with the same answer OR 
# the within person SD over all PVQ items was SD < 0.5, which is considered too small and with too little variability)
library("readxl")

excludeid <- read_excel("data/exlude_ids.xlsx")
p0001<-as.list(excludeid[1])
p0002<-as.list(excludeid[2])

dropndf<-filter(tempdf, 
              !p_0001 %in% p0001$p_0001)
dropndf<-filter(tempdf, 
              !p_0002 %in% p0002$p_0002)

ndrop <- length(tempdf$lfdn)-length(dropndf$lfdn)

# --- selects only variables of interest
df <- dropndf %>% 
  dplyr::select(age,sex,edu, # demographics
                starts_with("inc"), # income
                employ, # employment
                starts_with("lgi"), # valigo value instrument
                starts_with("pvq"), # pvq value instrument
                starts_with("kry")) # krypto items

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

## --- indices

# - pvq dimensions
pvq_dim <- list(
  pvq_STR_bed = c("pvqbed19", "pvqbed27", "pvqbed55"),
  pvq_STR_bec = c("pvqbec11", "pvqbec25", "pvqbec47"),
  pvq_STR_unt = c("pvqunt14", "pvqunt34", "pvqunt57"),
  pvq_STR_unc = c("pvqunc05", "pvqunc37", "pvqunc52"),
  pvq_STR_unn = c("pvqunn08", "pvqunn21", "pvqunn45"),
  pvq_STR_hum = c("pvqhum07", "pvqhum38", "pvqhum54"),
  pvq_CON_coi = c("pvqcoi04", "pvqcoi22", "pvqcoi51"),
  pvq_CON_cor = c("pvqcor15", "pvqcor31", "pvqcor42"),
  pvq_CON_tr = c("pvqtra18", "pvqtra33", "pvqtra40"),
  pvq_CON_ses = c("pvqses02", "pvqses35", "pvqses50"),
  pvq_CON_sep = c("pvqsep13", "pvqsep26", "pvqsep53"),
  pvq_CON_fac = c("pvqfac09", "pvqfac24", "pvqfac49"),
  pvq_SEN_por = c("pvqpor12", "pvqpor20", "pvqpor44"),
  pvq_SEN_pod = c("pvqpod06", "pvqpod29", "pvqpod41"),
  pvq_SEN_ach = c("pvqach17", "pvqach32", "pvqach48"),
  pvq_SEN_he = c("pvqhed03", "pvqhed36", "pvqhed46"),
  pvq_OCH_st = c("pvqsti10","pvqsti28", "pvqsti43"),
  pvq_OCH_sda = c("pvqsda16", "pvqsda30", "pvqsda56"),
  pvq_OCH_sdt = c("pvqsd01", "pvqsdt23", "pvqsdt39")
)

pvq_12dim <- list(
  pvq12_STR_ben = c("pvqbed19", "pvqbed27", "pvqbed55",
                  "pvqbec11", "pvqbec25", "pvqbec47"),
  pvq12_STR_un = c("pvqunt14", "pvqunt34", "pvqunt57",
                 "pvqunc05", "pvqunc37", "pvqunc52",
                 "pvqunc05", "pvqunc37", "pvqunc52"),
  pvq12_CON_hum = c("pvqhum07", "pvqhum38", "pvqhum54"),
  pvq12_CON_cov = c("pvqcoi04", "pvqcoi22", "pvqcoi51",
                  "pvqcor15", "pvqcor31", "pvqcor42"),
  pvq12_CON_trd = c("pvqtra18", "pvqtra33", "pvqtra40"),
  pvq12_CON_sec = c("pvqses02", "pvqses35", "pvqses50",
                  "pvqsep13", "pvqsep26", "pvqsep53"),
  pvq12_CON_fac = c("pvqfac09", "pvqfac24", "pvqfac49"),
  pvq12_SEN_pwr = c("pvqpor12", "pvqpor20", "pvqpor44",
                  "pvqpod06", "pvqpod29", "pvqpod41"),
  pvq12_SEN_ach = c("pvqach17", "pvqach32", "pvqach48"),
  pvq12_SEN_he = c("pvqhed03", "pvqhed36", "pvqhed46"),
  pvq12_OCH_st = c("pvqsti10","pvqsti28", "pvqsti43"),
  pvq12_OCH_sdr = c("pvqsda16", "pvqsda30", "pvqsda56",
                  "pvqsd01", "pvqsdt23", "pvqsdt39")
)

# - list with pvq dim
pvq_STR_bed = c("pvqbed19", "pvqbed27", "pvqbed55")
pvq_STR_bec = c("pvqbec11", "pvqbec25", "pvqbec47")
pvq_STR_unt = c("pvqunt14", "pvqunt34", "pvqunt57")
pvq_STR_unc = c("pvqunc05", "pvqunc37", "pvqunc52")
pvq_STR_unn = c("pvqunn08", "pvqunn21", "pvqunn45")
pvq_STR_hum = c("pvqhum07", "pvqhum38", "pvqhum54")
pvq_CON_coi = c("pvqcoi04", "pvqcoi22", "pvqcoi51")
pvq_CON_cor = c("pvqcor15", "pvqcor31", "pvqcor42")
pvq_CON_tr = c("pvqtra18", "pvqtra33", "pvqtra40")
pvq_CON_ses = c("pvqses02", "pvqses35", "pvqses50")
pvq_CON_sep = c("pvqsep13", "pvqsep26", "pvqsep53")
pvq_CON_fac = c("pvqfac09", "pvqfac24", "pvqfac49")
pvq_SEN_por = c("pvqpor12", "pvqpor20", "pvqpor44")
pvq_SEN_pod = c("pvqpod06", "pvqpod29", "pvqpod41")
pvq_SEN_ach = c("pvqach17", "pvqach32", "pvqach48")
pvq_SEN_he = c("pvqhed03", "pvqhed36", "pvqhed46")
pvq_OCH_st = c("pvqsti10","pvqsti28", "pvqsti43")
pvq_OCH_sda = c("pvqsda16", "pvqsda30", "pvqsda56")
pvq_OCH_sdt = c("pvqsd01", "pvqsdt23", "pvqsdt39")

# - list with pvq 12 (10 old and 2 new from 2014)
pvq_STR_ben = c("pvqbed19", "pvqbed27", "pvqbed55",
                "pvqbec11", "pvqbec25", "pvqbec47")
pvq12_STR_un = c("pvqunt14", "pvqunt34", "pvqunt57",
               "pvqunc05", "pvqunc37", "pvqunc52",
               "pvqunc05", "pvqunc37", "pvqunc52")
pvq12_CON_hum = c("pvqhum07", "pvqhum38", "pvqhum54")
pvq12_CON_cov = c("pvqcoi04", "pvqcoi22", "pvqcoi51",
                "pvqcor15", "pvqcor31", "pvqcor42")
pvq12_CON_trd = c("pvqtra18", "pvqtra33", "pvqtra40")
pvq12_CON_sec = c("pvqses02", "pvqses35", "pvqses50",
                "pvqsep13", "pvqsep26", "pvqsep53")
pvq12_CON_fac = c("pvqfac09", "pvqfac24", "pvqfac49")
pvq12_SEN_pwr = c("pvqpor12", "pvqpor20", "pvqpor44",
                "pvqpod06", "pvqpod29", "pvqpod41")
pvq12_SEN_ach = c("pvqach17", "pvqach32", "pvqach48")
pvq12_SEN_he = c("pvqhed03", "pvqhed36", "pvqhed46")
pvq12_OCH_st = c("pvqsti10","pvqsti28", "pvqsti43")
pvq12_OCH_sdr = c("pvqsda16", "pvqsda30", "pvqsda56",
                "pvqsd01", "pvqsdt23", "pvqsdt39")

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
pvq_list <- list(names(pvq_dim))
pvq_hov_list <- list(names(pvq_hov))
pvq_12dim_list <- list(names(pvq_12dim))

# - list with valigo dimensions
valigo_dim <- list(
  vlg_SEN_por = c("lgipow01", "lgipow02", "lgipow03"),
  vlg_SEN_ach = c("lgiach01", "lgiach02", "lgiach03"),
  vlg_SEN_he = c("lgihed01", "lgihed02", "lgihed03"),
  vlg_OCH_st = c("lgisti01", "lgisti02", "lgisti03"),
  vlg_OCH_sel = c("lgisel01", "lgisel02", "lgisel03"),
  vlg_STR_uni = c("lgiuni01", "lgiuni02", "lgiuni03"),
  vlg_STR_ben = c("lgiben01", "lgiben02", "lgiben03"),
  vlg_CON_conf = c("lgicon01", "lgicon02", "lgicon03"),
  vlg_CON_tr = c("lgitra01", "lgitra02", "lgitra03"),
  vlg_CON_sec = c("lgisec01", "lgisec02", "lgisec03")
)

# - valigo dimensions
vlg_SEN_por = c("lgipow01", "lgipow02", "lgipow03")
vlg_SEN_ach = c("lgiach01", "lgiach02", "lgiach03")
vlg_SEN_he = c("lgihed01", "lgihed02", "lgihed03")
vlg_OCH_st = c("lgisti01", "lgisti02", "lgisti03")
vlg_OCH_sel = c("lgisel01", "lgisel02", "lgisel03")
vlg_STR_uni = c("lgiuni01", "lgiuni02", "lgiuni03")
vlg_STR_ben = c("lgiben01", "lgiben02", "lgiben03")
vlg_CON_conf = c("lgicon01", "lgicon02", "lgicon03")
vlg_CON_tr = c("lgitra01", "lgitra02", "lgitra03")
vlg_CON_sec = c("lgisec01", "lgisec02", "lgisec03")

# - list valigo hov

valigo_hov <- list(
  valigo_SEN = c("lgipow01", "lgipow02", "lgipow03",
                 "lgiach01", "lgiach02", "lgiach03",
                 "lgihed01", "lgihed02", "lgihed03"),
  valigo_OCH = c("lgisti01", "lgisti02", "lgisti03",
                 "lgisel01", "lgisel02", "lgisel03"),
  valigo_STR = c("lgiuni01", "lgiuni02", "lgiuni03",
                 "lgiben01", "lgiben02", "lgiben03"),
  valigo_CON = c("lgicon01", "lgicon02", "lgicon03",
                 "lgitra01", "lgitra02", "lgitra03",
                 "lgisec01", "lgisec02", "lgisec03")
)

# - valigo hov

valigo_SEN = c("lgipow01", "lgipow02", "lgipow03",
               "lgiach01", "lgiach02", "lgiach03",
               "lgihed01", "lgihed02", "lgihed03")
valigo_OCH = c("lgisti01", "lgisti02", "lgisti03",
               "lgisel01", "lgisel02", "lgisel03")
valigo_STR = c("lgiuni01", "lgiuni02", "lgiuni03",
               "lgiben01", "lgiben02", "lgiben03")
valigo_CON = c("lgicon01", "lgicon02", "lgicon03",
               "lgitra01", "lgitra02", "lgitra03",
               "lgisec01", "lgisec02", "lgisec03")

valigo_list <- list(names(valigo_dim))
valigo_hov_list <- list(names(valigo_hov))
# --- Cronbaach alpha

cronb_df <- tibble(
  variable = vector("character",0),
  alpha = vector("numeric",0),
  ci_low = vector("numeric",0),
  ci_high = vector("numeric",0)
)


## cronbach alpha table for loop ---
# - pvq dimensions
for (i in pvq_dim){
  
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

## rename indexe --- 
cronb_df$indexe <- names(pvq_dim)

# - valigo dimensions
for (i in valigo_dim){
  
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
cronb_df[20:29,]$indexe <- names(valigo_dim)

# - pvq hov
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
cronb_df[30:33,]$indexe <- names(pvq_hov)

# - valigo hov
for (i in valigo_hov){
  
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
cronb_df[34:37,]$indexe <- names(valigo_hov)
cronb_df <- cronb_df %>% relocate(indexe, .before=alpha) %>% subset(.,select=-variable) %>% round_df()


# - cronbach alpha for Schwartz 12 dimensions
cronb_tmp <- tibble(
  variable = vector("character",0),
  alpha = vector("numeric",0),
  ci_low = vector("numeric",0),
  ci_high = vector("numeric",0)
)

for (i in pvq_12dim){
  
  name = i
  # name <- deparse(substitute(i))
  tmp <- df %>% 
    dplyr::select(i) %>% 
    ltm::cronbach.alpha(.,na.rm=TRUE,CI=TRUE,B=100)
  
  cronb_tmp  <- cronb_tmp  %>% add_row(
    variable = name,
    alpha = tmp$alpha,
    ci_low = tmp$ci[1],
    ci_high = tmp$ci[2]
  )
  
  rm(tmp)
  cronb_tmp  <- cronb_tmp  %>% distinct(., alpha, .keep_all = T)
  
}
cronb_tmp$indexe <- names(pvq_12dim)
cronb_tmp <- cronb_tmp %>%relocate(indexe, .before=alpha) %>% subset(.,select=-variable) %>% round_df()

# - joins cronbach alpha tables

cronbach <- full_join(cronb_df, cronb_tmp)

# --- Ipsatization and then Scale construction

names_pvq <- df %>% dplyr::select(starts_with("pvq")) %>% names()
names_valigo <- df %>% dplyr::select(starts_with("lgi")) %>% dplyr::select(-lgioq01,-lgioq02) %>%  names()


df_ipst <- df 
df_ipst$pvq_mean <- rowMeans(df_ipst[names_pvq], na.rm = FALSE)
df_ipst$valigo_mean <- rowMeans(df_ipst[names_valigo], na.rm = FALSE)

df_ipst <- df_ipst %>% rowwise() %>% 
  mutate(across(all_of(names_pvq), ~.x - pvq_mean),
         across(all_of(names_valigo), ~.x - valigo_mean)) %>% 
  ungroup() %>% 
  scale_builder(., pvq_dim, score_fun = "mean") %>% 
  scale_builder(., pvq_12dim, score_fun = "mean") %>% 
  scale_builder(., valigo_dim, score_fun = "mean") %>% 
  scale_builder(., pvq_hov, score_fun = "mean") %>% 
  scale_builder(., valigo_hov, score_fun = "mean") %>% 
  dplyr::select(-one_of(valigo_SEN),-one_of(valigo_STR),
         -one_of(valigo_CON),-one_of(valigo_OCH),
         -one_of(pvq_SEN),-one_of(pvq_STR),
         -one_of(pvq_CON),-one_of(pvq_OCH))

# --- cleans data

df_ipst <- df_ipst %>% 
  sjlabelled::set_na(krybsz01,na=0) %>% 
  mutate_at(vars("kryftl07"), flip_item)

# --- schwartz circle 
# uses LittleHelpers source package from Maksim Rudnev: https://github.com/MaksimRudnev/LittleHelpers/

#pdf("img/schwartz_circle.pdf", height = 7, width = 12)
#LittleHelpers::schwartz_circle()
#dev.off()

## --- save data

save(list=c("df", "df_ipst",
            "df_labels","cronbach",
            "pvq_dim","pvq_hov",
            "valigo_dim","valigo_hov","ndrop"),
     file="data/df.Rdata")
