#### - imputes data
# see https://stats.stackexchange.com/questions/99334/fast-missing-data-imputation-in-r-for-big-data-that-is-more-sophisticated-than-s

library(mice)
library(Amelia)
library(dplyr)
library(tidyverse)
library(lattice)

# - loads data

load("data/df.Rdata")


##### steps in imputing data
# 1 select variables from original data that will be used for logistic regression
# 2 use Amelia/mice package on the data with missing values to bootstrap the missing values
# 3 run your model


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

# - subsets only variables of interest
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
write.table(Datim,"data/imputeddata.txt",sep="\t",dec=",",row.names=FALSE)


### --- checks if imputations are good
com <- mice::complete(impData,"long",inc=T)
stripplot(impData)
densityplot(impData)

### - run model while accounting for uncertainty due to imputed values
# 
fit <- with(impData, glm(krybst01 ~ age + female + abitur + pvq_OCH + pvq_CON +
                           regularization , 
                         data=trainingData, family = binomial))
summary(mice::pool(fit))
