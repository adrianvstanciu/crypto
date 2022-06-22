#### ---- logistic model

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
library(InformationValue)

# --- data import

load("data/df.Rdata")

# ---- prepare data
a <- df_ipst %>% 
  filter(krybws01==1) %>% 
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
  rename(female=sex,
         abitur=edu,
         investment=krymng01,
         exchange=krymng02,
         regularization=krymng03,
         illegal=krymng04,
         goodtime=krymng05) %>% 
  set_na(investment,na=8) %>% 
  set_na(exchange,na=8) %>% 
  set_na(regularization,na=8) %>% 
  set_na(illegal,na=8) %>% 
  set_na(goodtime,na=8) %>% 
  set_na(investment:goodtime,na=-77)

##### ---- correlations among beliefs items ---- ####
tmp_corr <- a %>% dplyr::select(investment,exchange,regularization,illegal,goodtime)
tbl_corr <- rcorr(as.matrix(tmp_corr))

##### ---- missing data imputation
### ---- impute missing values on investment to goodtime (the moderators)
shortData <- a %>% sjlabelled::remove_all_labels() %>% 
  dplyr::select(krybst01, # hold crpyto now
                krybsz01, # intent to hold crypto future
                age, female,abitur, # covs
                pvq_OCH, pvq_CON, pvq_STR, pvq_SEN, # values
                investment, exchange,regularization,illegal,goodtime) # beliefs on crypto

# - initiate imputed values
ini <- mice::mice(shortData,maxit=0,pri=F)
# - sets which variables i don't want as predictors
pred <- ini$pred
#pred[,c("female","abitur")] <- 0
# - choose a prediction method for imputing variables. here specifies that these variables should not be imputed
meth <- ini$meth
meth[c("krybst01","krybsz01","age","female","abitur","pvq_OCH","pvq_CON","pvq_STR","pvq_SEN")] <- ""
# - imputes data: 5 multiple imputations, and 10 iterations
impData <- mice::mice(shortData, m=5,maxit=10,printFlag = TRUE,pred=pred,meth=meth,seed=1244)
Datim <- mice::complete(impData,"long",include=TRUE)
# - writes imputed data on the machine
#write.table(Datim,"data/imputeddata.txt",sep="\t",dec=",",row.names=FALSE)

# ---- lostic regression
# --- model 1: intercept + covariates(female,abitur, age)
# holds now
m1.1 <- with(impData,glm(krybst01 ~ age + female + abitur, 
            data=shortData, family = binomial))
t1.1<-summary(mice::pool(m1.1)) %>% as.data.frame()
# intention 
m1.2 <- with(impData,glm(krybsz01 ~ age + female + abitur, 
                         data=shortData, family = binomial))
t1.2<-summary(mice::pool(m1.2)) %>% as.data.frame()
# --- model 2a: intercept + covariates(female,abitur, age) + OCH + SEN
# --- model 2b: intercept + covariates(female,abitur, age) + CON + STR
# holds now
m2a.1 <- with(impData,glm(krybst01 ~ age + female + abitur + pvq_OCH + pvq_SEN, 
                          data=shortData, family = binomial))
m2b.1 <- with(impData,glm(krybst01 ~ age + female + abitur + pvq_CON + pvq_STR, 
                          data=shortData, family = binomial))

t2a.1<-summary(mice::pool(m2a.1)) %>% as.data.frame()
t2b.1<-summary(mice::pool(m2b.1)) %>% as.data.frame()
# intention
m2a.2 <- with(impData,glm(krybsz01 ~ age + female + abitur + pvq_OCH + pvq_SEN, 
                          data=shortData, family = binomial))
m2b.2 <- with(impData,glm(krybsz01 ~ age + female + abitur + pvq_CON + pvq_STR, 
                          data=shortData, family = binomial))

t2a.2<-summary(mice::pool(m2a.2)) %>% as.data.frame()
t2b.2<-summary(mice::pool(m2b.2)) %>% as.data.frame()
# --- model 3a: intercept + covariates(female,abitur, age) + OCH + SEN + 5 beliefs
# --- model 3b: intercept + covariates(female,abitur, age) + CON + STR + 5 beliefs
# holds now
m3a.1 <- with(impData,glm(krybst01 ~ age + female + abitur + pvq_OCH + pvq_SEN + 
                            investment + exchange + regularization + illegal + goodtime, 
                          data=shortData, family = binomial))
m3b.1 <- with(impData,glm(krybst01 ~ age + female + abitur + pvq_CON + pvq_STR + 
                            investment + exchange + regularization + illegal + goodtime, 
                          data=shortData, family = binomial))

t3a.1<-summary(mice::pool(m3a.1)) %>% as.data.frame()
t3b.1<-summary(mice::pool(m3b.1)) %>% as.data.frame()
# intention
m3a.2 <- with(impData,glm(krybsz01 ~ age + female + abitur + pvq_OCH + pvq_SEN + 
                            investment + exchange + regularization + illegal + goodtime, 
                          data=shortData, family = binomial))
m3b.2 <- with(impData,glm(krybsz01 ~ age + female + abitur + pvq_CON + pvq_STR + 
                            investment + exchange + regularization + illegal + goodtime, 
                          data=shortData, family = binomial))

t3a.2<-summary(mice::pool(m3a.2)) %>% as.data.frame()
t3b.2<-summary(mice::pool(m3b.2)) %>% as.data.frame()
# --- model 4a: intercept + covariates(female,abitur, age) + OCH + SEN + 5 beliefs + OCH*5 beliefs + SEN*5 beliefs
# --- model 4b: intercept + covariates(female,abitur, age) + CON + STR + 5 beliefs + CON*5 beliefs + STR*5 beliefs
# holds now
m4a.1 <- with(impData,glm(krybst01 ~ age + female + abitur + pvq_OCH + pvq_SEN + 
                            investment + exchange + regularization + illegal + goodtime +
                            pvq_OCH*investment + pvq_OCH*exchange + pvq_OCH*regularization + pvq_OCH*illegal + pvq_OCH*goodtime +
                            pvq_SEN*investment + pvq_SEN*exchange + pvq_SEN*regularization + pvq_SEN*illegal + pvq_SEN*goodtime, 
                          data=shortData, family = binomial))
m4b.1 <- with(impData,glm(krybst01 ~ age + female + abitur + pvq_CON + pvq_STR + 
                            investment + exchange + regularization + illegal + goodtime +
                            pvq_CON*investment + pvq_CON*exchange + pvq_CON*regularization + pvq_CON*illegal + pvq_CON*goodtime +
                            pvq_STR*investment + pvq_STR*exchange + pvq_STR*regularization + pvq_STR*illegal + pvq_STR*goodtime, 
                          data=shortData, family = binomial))

t4a.1<-summary(mice::pool(m4a.1)) %>% as.data.frame()
t4b.1<-summary(mice::pool(m4b.1)) %>% as.data.frame()
# intention
m4a.2 <- with(impData,glm(krybsz01 ~ age + female + abitur + pvq_OCH + pvq_SEN + 
                            investment + exchange + regularization + illegal + goodtime +
                            pvq_OCH*investment + pvq_OCH*exchange + pvq_OCH*regularization + pvq_OCH*illegal + pvq_OCH*goodtime +
                            pvq_SEN*investment + pvq_SEN*exchange + pvq_SEN*regularization + pvq_SEN*illegal + pvq_SEN*goodtime, 
                          data=shortData, family = binomial))
m4b.2 <- with(impData,glm(krybsz01 ~ age + female + abitur + pvq_CON + pvq_STR + 
                            investment + exchange + regularization + illegal + goodtime +
                            pvq_CON*investment + pvq_CON*exchange + pvq_CON*regularization + pvq_CON*illegal + pvq_CON*goodtime +
                            pvq_STR*investment + pvq_STR*exchange + pvq_STR*regularization + pvq_STR*illegal + pvq_STR*goodtime, 
                          data=shortData, family = binomial))

t4a.2<-summary(mice::pool(m4a.2)) %>% as.data.frame()
t4b.2<-summary(mice::pool(m4b.2)) %>% as.data.frame()

### creates table with results

res_hyp_cryptonow<-tribble(
    ~Predictor,   ~b_m1,  ~Test_m1,   ~Df_m1,    ~p_m1,     ~b_m2,   ~Test_m2,   ~Df_m2,   ~p_m2,      ~b_m3,   ~Test_m3,    ~Df_m3,   ~p_m3,
  "Intercept",t1.1[1,2],t1.1[1,4],t1.1[1,5],t1.1[1,6],t2a.1[1,2],t2a.1[1,4],t2a.1[1,5],t2a.1[1,6],t3a.1[1,2],t3a.1[1,4],t3a.1[1,5],t3a.1[1,6],
  "Age",        t1.1[2,2],t1.1[2,4],t1.1[2,5],t1.1[2,6],t2a.1[2,2],t2a.1[2,4],t2a.1[2,5],t2a.1[2,6],t3a.1[2,2],t3a.1[2,4],t3a.1[2,5],t3a.1[2,6],
  "Female",     t1.1[3,2],t1.1[3,4],t1.1[3,5],t1.1[3,6],t2a.1[3,2],t2a.1[3,4],t2a.1[3,5],t2a.1[3,6],t3a.1[3,2],t3a.1[3,4],t3a.1[3,5],t3a.1[3,6],
  "Abitur",     t1.1[4,2],t1.1[4,4],t1.1[4,5],t1.1[4,6],t2a.1[4,2],t2a.1[4,4],t2a.1[4,5],t2a.1[4,6],t3a.1[4,2],t3a.1[4,4],t3a.1[4,5],t3a.1[4,6],
  "OCH",            NaN,        NaN,       NaN,       NaN,t2a.1[5,2],t2a.1[5,4],t2a.1[5,5],t2a.1[5,6],t3a.1[5,2],t3a.1[5,4],t3a.1[5,5],t3a.1[5,6],
  "SEN",            NaN,        NaN,       NaN,       NaN,t2a.1[6,2],t2a.1[6,4],t2a.1[6,5],t2a.1[6,6],t3a.1[6,2],t3a.1[6,4],t3a.1[6,5],t3a.1[6,6],
  "Investment",     NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3a.1[7,2],t3a.1[7,4],t3a.1[7,5],t3a.1[7,6],
  "Exchange",       NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3a.1[8,2],t3a.1[8,4],t3a.1[8,5],t3a.1[8,6],
  "Regularization", NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3a.1[9,2],t3a.1[9,4],t3a.1[9,5],t3a.1[9,6],
  "Illegal",        NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3a.1[10,2],t3a.1[10,4],t3a.1[10,5],t3a.1[10,6],
  "Good time",      NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3a.1[11,2],t3a.1[11,4],t3a.1[11,5],t3a.1[11,6]
)
res_nohyp_cryptonow<-tribble(
  ~Predictor,   ~b_m1,  ~Test_m1,   ~Df_m1,    ~p_m1,     ~b_m2,   ~Test_m2,   ~Df_m2,   ~p_m2,      ~b_m3,   ~Test_m3,    ~Df_m3,   ~p_m3,
  "Intercept",t1.1[1,2],t1.1[1,4],t1.1[1,5],t1.1[1,6],t2b.1[1,2],t2b.1[1,4],t2b.1[1,5],t2b.1[1,6],t3b.1[1,2],t3b.1[1,4],t3b.1[1,5],t3b.1[1,6],
  "Age",        t1.1[2,2],t1.1[2,4],t1.1[2,5],t1.1[2,6],t2b.1[2,2],t2b.1[2,4],t2b.1[2,5],t2b.1[2,6],t3b.1[2,2],t3b.1[2,4],t3b.1[2,5],t3b.1[2,6],
  "Female",     t1.1[3,2],t1.1[3,4],t1.1[3,5],t1.1[3,6],t2b.1[3,2],t2b.1[3,4],t2b.1[3,5],t2b.1[3,6],t3b.1[3,2],t3b.1[3,4],t3b.1[3,5],t3b.1[3,6],
  "Abitur",     t1.1[4,2],t1.1[4,4],t1.1[4,5],t1.1[4,6],t2b.1[4,2],t2b.1[4,4],t2b.1[4,5],t2b.1[4,6],t3b.1[4,2],t3b.1[4,4],t3b.1[4,5],t3b.1[4,6],
  "CON",            NaN,        NaN,       NaN,       NaN,t2b.1[5,2],t2b.1[5,4],t2b.1[5,5],t2b.1[5,6],t3b.1[5,2],t3b.1[5,4],t3b.1[5,5],t3b.1[5,6],
  "STR",            NaN,        NaN,       NaN,       NaN,t2b.1[6,2],t2b.1[6,4],t2b.1[6,5],t2b.1[6,6],t3b.1[6,2],t3b.1[6,4],t3b.1[6,5],t3b.1[6,6],
  "Investment",     NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3b.1[7,2],t3b.1[7,4],t3b.1[7,5],t3b.1[7,6],
  "Exchange",       NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3b.1[8,2],t3b.1[8,4],t3b.1[8,5],t3b.1[8,6],
  "Regularization", NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3b.1[9,2],t3b.1[9,4],t3b.1[9,5],t3b.1[9,6],
  "Illegal",        NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3b.1[10,2],t3b.1[10,4],t3b.1[10,5],t3b.1[10,6],
  "Good time",      NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3b.1[11,2],t3b.1[11,4],t3b.1[11,5],t3b.1[11,6]
)
res_hyp_cryptofutr<-tribble(
  ~Predictor,   ~b_m1,  ~Test_m1,   ~Df_m1,    ~p_m1,     ~b_m2,   ~Test_m2,   ~Df_m2,   ~p_m2,      ~b_m3,   ~Test_m3,    ~Df_m3,   ~p_m3,
  "Intercept",t1.2[1,2],t1.2[1,4],t1.2[1,5],t1.2[1,6],t2a.2[1,2],t2a.2[1,4],t2a.2[1,5],t2a.2[1,6],t3a.2[1,2],t3a.2[1,4],t3a.2[1,5],t3a.2[1,6],
  "Age",        t1.2[2,2],t1.2[2,4],t1.2[2,5],t1.2[2,6],t2a.2[2,2],t2a.2[2,4],t2a.2[2,5],t2a.2[2,6],t3a.2[2,2],t3a.2[2,4],t3a.2[2,5],t3a.2[2,6],
  "Female",     t1.2[3,2],t1.2[3,4],t1.2[3,5],t1.2[3,6],t2a.2[3,2],t2a.2[3,4],t2a.2[3,5],t2a.2[3,6],t3a.2[3,2],t3a.2[3,4],t3a.2[3,5],t3a.2[3,6],
  "Abitur",     t1.2[4,2],t1.2[4,4],t1.2[4,5],t1.2[4,6],t2a.2[4,2],t2a.2[4,4],t2a.2[4,5],t2a.2[4,6],t3a.2[4,2],t3a.2[4,4],t3a.2[4,5],t3a.2[4,6],
  "OCH",            NaN,        NaN,       NaN,       NaN,t2a.2[5,2],t2a.2[5,4],t2a.2[5,5],t2a.2[5,6],t3a.2[5,2],t3a.2[5,4],t3a.2[5,5],t3a.2[5,6],
  "SEN",            NaN,        NaN,       NaN,       NaN,t2a.2[6,2],t2a.2[6,4],t2a.2[6,5],t2a.2[6,6],t3a.2[6,2],t3a.2[6,4],t3a.2[6,5],t3a.2[6,6],
  "Investment",     NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3a.2[7,2],t3a.2[7,4],t3a.2[7,5],t3a.2[7,6],
  "Exchange",       NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3a.2[8,2],t3a.2[8,4],t3a.2[8,5],t3a.2[8,6],
  "Regularization", NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3a.2[9,2],t3a.2[9,4],t3a.2[9,5],t3a.2[9,6],
  "Illegal",        NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3a.2[10,2],t3a.2[10,4],t3a.2[10,5],t3a.2[10,6],
  "Good time",      NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3a.2[11,2],t3a.2[11,4],t3a.2[11,5],t3a.2[11,6]
)
res_nohyp_cryptofutr<-tribble(
  ~Predictor,   ~b_m1,  ~Test_m1,   ~Df_m1,    ~p_m1,     ~b_m2,   ~Test_m2,   ~Df_m2,   ~p_m2,      ~b_m3,   ~Test_m3,    ~Df_m3,   ~p_m3,
  "Intercept",t1.2[1,2],t1.2[1,4],t1.2[1,5],t1.2[1,6],t2b.2[1,2],t2b.2[1,4],t2b.2[1,5],t2b.2[1,6],t3b.2[1,2],t3b.2[1,4],t3b.2[1,5],t3b.2[1,6],
  "Age",        t1.2[2,2],t1.2[2,4],t1.2[2,5],t1.2[2,6],t2b.2[2,2],t2b.2[2,4],t2b.2[2,5],t2b.2[2,6],t3b.2[2,2],t3b.2[2,4],t3b.2[2,5],t3b.2[2,6],
  "Female",     t1.2[3,2],t1.2[3,4],t1.2[3,5],t1.2[3,6],t2b.2[3,2],t2b.2[3,4],t2b.2[3,5],t2b.2[3,6],t3b.2[3,2],t3b.2[3,4],t3b.2[3,5],t3b.2[3,6],
  "Abitur",     t1.2[4,2],t1.2[4,4],t1.2[4,5],t1.2[4,6],t2b.2[4,2],t2b.2[4,4],t2b.2[4,5],t2b.2[4,6],t3b.2[4,2],t3b.2[4,4],t3b.2[4,5],t3b.2[4,6],
  "CON",            NaN,        NaN,       NaN,       NaN,t2b.2[5,2],t2b.2[5,4],t2b.2[5,5],t2b.2[5,6],t3b.2[5,2],t3b.2[5,4],t3b.2[5,5],t3b.2[5,6],
  "STR",            NaN,        NaN,       NaN,       NaN,t2b.2[6,2],t2b.2[6,4],t2b.2[6,5],t2b.2[6,6],t3b.2[6,2],t3b.2[6,4],t3b.2[6,5],t3b.2[6,6],
  "Investment",     NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3b.2[7,2],t3b.2[7,4],t3b.2[7,5],t3b.2[7,6],
  "Exchange",       NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3b.2[8,2],t3b.2[8,4],t3b.2[8,5],t3b.2[8,6],
  "Regularization", NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3b.2[9,2],t3b.2[9,4],t3b.2[9,5],t3b.2[9,6],
  "Illegal",        NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3b.2[10,2],t3b.2[10,4],t3b.2[10,5],t3b.2[10,6],
  "Good time",      NaN,        NaN,       NaN,       NaN,        NaN,        NaN,        NaN,        NaN,t3b.2[11,2],t3b.2[11,4],t3b.2[11,5],t3b.2[11,6]
)

## compared nested models using mice:: package D3 (calculates likelihood ratio test)
# to see if the fuller model (model with more predictors) is fitting the data better

# crypto now
c1<-mice::D3(m2a.1,m1.1)
c2<-mice::D3(m2b.1,m1.1)
c3<-mice::D3(m3a.1,m2a.1)
c4<-mice::D3(m3b.1,m2b.1)
c9<-mice::D3(m4a.1,m3a.1)
c10<-mice::D3(m4b.1,m3b.1)

a<-summary(c1)[2] %>% as.data.frame() %>% add_column("Comparison"="m2a.1 ~ m1.1")
b<-summary(c2)[2] %>% as.data.frame() %>% add_column("Comparison"="m2b.1 ~ m1.1")
c<-summary(c3)[2] %>% as.data.frame() %>% add_column("Comparison"="m3a.1 ~ m2a.1")
d<-summary(c4)[2] %>% as.data.frame() %>% add_column("Comparison"="m3b.1 ~ m2b.1")
i<-summary(c9)[2] %>% as.data.frame() %>% add_column("Comparison"="m4a.1 ~ m3a.1")
j<-summary(c10)[2] %>% as.data.frame() %>% add_column("Comparison"="m4b.1 ~ m3b.1")

cnow <- full_join(a,b) %>% full_join(.,c) %>% full_join(.,d) %>% full_join(.,i) %>% full_join(.,j) %>% 
  rename(Test=comparisons.statistic,
         Df1=comparisons.df1,
         Df2=comparisons.df2,
         "Df comparison"=comparisons.dfcom,
         p=comparisons.p.value,
         Riv=comparisons.riv) %>% 
  dplyr::select(-comparisons.test) %>% 
  relocate("Comparison", .before=Test)

# intention crypto future
c5<-mice::D3(m2a.2,m1.2)
c6<-mice::D3(m2b.2,m1.2)
c7<-mice::D3(m3a.2,m2a.2)
c8<-mice::D3(m3b.2,m2b.2)
c11<-mice::D3(m4a.2,m3a.2)
c12<-mice::D3(m4b.2,m3b.2)

e<-summary(c5)[2] %>% as.data.frame() %>% add_column("Comparison"="m2a.2 ~ m1.2")
f<-summary(c6)[2] %>% as.data.frame() %>% add_column("Comparison"="m2b.2 ~ m1.2")
g<-summary(c7)[2] %>% as.data.frame() %>% add_column("Comparison"="m3a.2 ~ m2a.2")
h<-summary(c8)[2] %>% as.data.frame() %>% add_column("Comparison"="m3b.2 ~ m2b.2")
k<-summary(c11)[2] %>% as.data.frame() %>% add_column("Comparison"="m4a.2 ~ m3a.2")
l<-summary(c12)[2] %>% as.data.frame() %>% add_column("Comparison"="m4b.2 ~ m3b.2")

cfutr <- full_join(e,f) %>% full_join(.,g) %>% full_join(.,h) %>% full_join(.,k) %>% full_join(.,l) %>% 
  rename(Test=comparisons.statistic,
         Df1=comparisons.df1,
         Df2=comparisons.df2,
         "Df comparison"=comparisons.dfcom,
         p=comparisons.p.value,
         Riv=comparisons.riv) %>% 
  dplyr::select(-comparisons.test) %>% 
  relocate("Comparison", .before=Test)

# checked all t4 tables if any interaction term was significant
# interaction effects in either of the four models were significant
# thus for the paper will be reported models 3 (main effects only)
# all tables will be for the supplement

# table with model fit indices for the regression based on imputed data
# intercept and covariates
x1.1<-mice::pool(m1.1)$glanced[1,] %>% 
  add_column(model="m1.1",.before="null.deviance") %>% 
  add_column(outcome="Crypto hold",.after="model") %>% 
  add_column(value="none", .after="outcome")
x1.2<-mice::pool(m1.2)$glanced[1,] %>% 
  add_column(model="m1.2",.before="null.deviance") %>% 
  add_column(outcome="Crypto future",.after="model") %>% 
  add_column(value="none", .after="outcome")
# values added
x2a.1<-mice::pool(m2a.1)$glanced[1,] %>% 
  add_column(model="m2a.1",.before="null.deviance") %>% 
  add_column(outcome="Crypto hold",.after="model") %>% 
  add_column(value="OCH & SEN", .after="outcome")
x2b.1<-mice::pool(m2b.1)$glanced[1,] %>% 
  add_column(model="m2b.1",.before="null.deviance") %>% 
  add_column(outcome="Crypto hold",.after="model") %>% 
  add_column(value="CON & STR", .after="outcome")
x2a.2<-mice::pool(m2a.2)$glanced[1,] %>% 
  add_column(model="m2a.2",.before="null.deviance") %>% 
  add_column(outcome="Crypto future",.after="model") %>% 
  add_column(value="OCH & SEN", .after="outcome")
x2b.2<-mice::pool(m2b.2)$glanced[1,] %>% 
  add_column(model="m2b.2",.before="null.deviance") %>% 
  add_column(outcome="Crypto future",.after="model") %>% 
  add_column(value="CON & STR", .after="outcome")
# beliefs added
x3a.1<-mice::pool(m3a.1)$glanced[1,] %>% 
  add_column(model="m3a.1",.before="null.deviance") %>% 
  add_column(outcome="Crypto hold",.after="model") %>% 
  add_column(value="OCH & SEN", .after="outcome")
x3b.1<-mice::pool(m3b.1)$glanced[1,] %>% 
  add_column(model="m3b.1",.before="null.deviance") %>% 
  add_column(outcome="Crypto hold",.after="model") %>% 
  add_column(value="CON & STR", .after="outcome")
x3a.2<-mice::pool(m3a.2)$glanced[1,] %>% 
  add_column(model="m3a.2",.before="null.deviance") %>% 
  add_column(outcome="Crypto future",.after="model") %>% 
  add_column(value="OCH & SEN", .after="outcome")
x3b.2<-mice::pool(m3b.2)$glanced[1,] %>% 
  add_column(model="m3b.2",.before="null.deviance") %>% 
  add_column(outcome="Crypto future",.after="model") %>% 
  add_column(value="CON & STR", .after="outcome")
# interaction effects added
x4a.1<-mice::pool(m4a.1)$glanced[1,] %>% 
  add_column(model="m4a.1",.before="null.deviance") %>% 
  add_column(outcome="Crypto hold",.after="model") %>% 
  add_column(value="OCH & SEN", .after="outcome")
x4b.1<-mice::pool(m4b.1)$glanced[1,] %>% 
  add_column(model="m4b.1",.before="null.deviance") %>% 
  add_column(outcome="Crypto hold",.after="model") %>% 
  add_column(value="CON & STR", .after="outcome")
x4a.2<-mice::pool(m4a.2)$glanced[1,] %>% 
  add_column(model="m4a.2",.before="null.deviance") %>% 
  add_column(outcome="Crypto future",.after="model") %>% 
  add_column(value="OCH & SEN", .after="outcome")
x4b.2<-mice::pool(m4b.2)$glanced[1,] %>% 
  add_column(model="m4b.1",.before="null.deviance") %>% 
  add_column(outcome="Crypto future",.after="model") %>% 
  add_column(value="CON & STR", .after="outcome")


mfit_cryptonow <- full_join(x1.1,x2a.1) %>% full_join(.,x2b.1) %>% full_join(.,x3a.1) %>% full_join(.,x3b.1) %>% 
  full_join(.,x4a.1) %>% full_join(.,x4b.1)
mfit_cryptofutr <- full_join(x1.2,x2a.2) %>% full_join(.,x2b.2) %>% full_join(.,x3a.2) %>% full_join(.,x3b.2) %>% 
  full_join(.,x4a.2) %>% full_join(.,x4b.2) 

# creates nicer tables

mfit_cryptonow <- mfit_cryptonow %>% 
  rename(Model=model,
         Outcome=outcome,
         Values=value,
         "Deviance (null)"=null.deviance,
         "Df (null)"=df.null,
         LogLik=logLik,
         AIC=AIC,
         BIC=BIC,
         Deviance=deviance,
         Df = df.residual,
          N = nobs
         ) %>% 
  dplyr::select(-Outcome) %>% 
  relocate(N, .after=Model)

mfit_cryptofutr <- mfit_cryptofutr %>% 
  rename(Model=model,
         Outcome=outcome,
         Values=value,
         "Deviance (null)"=null.deviance,
         "Df (null)"=df.null,
         LogLik=logLik,
         AIC=AIC,
         BIC=BIC,
         Deviance=deviance,
         Df = df.residual,
         N = nobs
  ) %>% 
  dplyr::select(-Outcome) %>% 
  relocate(N, .after=Model)

# --- save
save(list=c("impData","tbl_corr","mfit_cryptonow","mfit_cryptofutr","cnow","cfutr",
            "res_hyp_cryptonow","res_nohyp_cryptonow","res_hyp_cryptofutr","res_nohyp_cryptofutr"),
     file="data/table_logreg.Rdata")