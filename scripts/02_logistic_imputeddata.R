## see here: https://francish.net/post/accounting-for-missing-data/
## also see here: https://francish.netlify.app/post/01_missing/
## for marginal effects, see: # see https://stackoverflow.com/questions/75720633/plot-calculated-average-marginal-effects-from-multiply-imputed-data-in-r


#install.packages("stargazer")
#install.packages("textreg")
#install.packages("margins")
#install.packages("mfx")
#install.packages("sjPlot)
#install.packages("pscl") # mcfadden pseudo-r^2 for logit regressions
#install.packages("marginaleffects")

library(mice)
library(Amelia)
library(dplyr)
library(tidyverse)
library(lattice)
library(sjlabelled)
library(stargazer) # to show side by side output (helpful for comparing results with missing and imputed data)
library(textreg)
library(margins)
library(mfx) # for marginal effects in logit estimator
library(sjPlot) # plots predicted probabilities
library(pscl) # mcfadden pseudo-r^2 for logit regressions
library(marginaleffects) # for calculating marginal effects on mids object (over multiply imputed datasets; object from mice package)

# - loads data

load("data/df.Rdata")
shortData <- df_crypto %>% remove_all_labels() #df_crypto from script 00_data prep

# -- set global option to turn off scientific notations
options(scipen=999)

# -- color blind palette
cbPalette <- c("#999999","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


######################################### analyses described #############

### logit regressions are run, and reported are marginal effects 
# marginal effect is the slope of the tangent line at the xy coordinate
# subjects of factor "x"  have probability of having the value of the dichotomous outcome
# that is about lowr ci - upper ci higher (or lower depending on sing) that those
# subjects of factor "x" who are compared against
# with a continuous variable such as age, marginal effects can go up or down with 
# specified levels of the continuous variable

# - turns into factors
#shortData$female<-to_factor(shortData$female)
#shortData$abitur<-to_factor(shortData$abitur)

## -- check pattern of missing data
#png("img/fig_pattern_missing.png",units = "in",width=8, height= 6,res=300)
mice::md.pattern(shortData,rotate.names = T)
#dev.off()

# - adds label to outcome variables
set_label(shortData$aware)<-"Is aware of cryptocurrencies"
set_label(shortData$holds)<-"Holds now or has held in the past cryptocurrencies"
set_label(shortData$intends)<-"Intends to buy cryptocurrencies in the future"
set_label(shortData$understands)<-"How well one understands cryptocurrencies"
set_label(shortData$age)<-"Age"
set_label(shortData$female)<-"Is female"
set_label(shortData$abitur)<-"Has Abitur"
set_label(shortData$employed)<-"Has Abitur"
set_label(shortData$pvq_OCH)<-"Value of openness to change"
set_label(shortData$pvq_SEN)<-"Value of self-enhancement"
set_label(shortData$pvq_CON)<-"Value of conservation"
set_label(shortData$pvq_STR)<-"Value of self-transcendence"
set_label(shortData$investment)<-"Cryptocurrencies are an investment opportunity and less a payment method"
set_label(shortData$exchange)<-"Cryptocurrencies can be easily exchanged for cash"
set_label(shortData$regularization)<-"The State regulates cryptocurrencies"
set_label(shortData$illegal)<-"Cryptocurrencies facilitate illegal activities"
set_label(shortData$goodtime)<-"Good time to buy cryptocurrencies"

######################################## impute missing data ###############
# initiate imputed values only for the 5 beliefs measurement 
# imputation by pmm = predictive mean matching
# see Rubin (1986) https://doi.org/10.2307/1391390
# see Little (1988) https://doi.org/10.2307/1391878
# uses 100 imputations (m = 100) for stable p-values and 95% ci
impData <- mice::mice(shortData,
                  method=c("","","","","","","","","","","","pmm",
                           "pmm","pmm","pmm","pmm",""), 
                  seed = 1234,
                  m=100,maxit = 1)

# 3rd imputed data set is taken as the data with imputed information
#Datim <- mice::complete(impData,3,seed=1234)

# version where 100 imputed data sets (multiple imputation) are stacked
#Datim2 <- mice::complete(impData,action="long",include=TRUE,seed=1234)


# - check the density plot of imputed data
#png("img/fig_densityplot_imputations.png",units = "in",width=8, height= 6,res=300)
mice::densityplot(impData, 
                  data=~investment + exchange + regularization + illegal + goodtime)
#dev.off()

# - check the pattern of imputed data
#mice::md.pattern(Datim2,rotate.names = T)

############################ --- analyse imputed data ############
# find quantile for age to use for plots
# use the 25% and 75% quantile 
# 25% quantile = 31
# 75% quantile = 56
quantile(shortData$age)

# use this for calculating average marginal effects over multiply imputed data
# an warning message will be shown if df = Inf
# this means that the normal distribution is used when computing p-values and ci
# marginaleffects::avg_slopes(--here the mice::as.mids object---)
 
# creates label for the grid plot by age
label_age<-as_labeller(c(`25`="Age=25",
                        `45`="Age=45",
                        `65`="Age=65"))

# creates label for the grid plot by age
label_abitur<-as_labeller(c(`1`="Abitur",
                         `0`="No Abitur"))


# creates label for the grid plot by belief good time
label_belief1 <-as_labeller(c(`1`="Disagrees it is a good time to buy",
                            `3`="Neutral",
                            `5`="Agrees it is a good time to buy"))

# creates label for the grid plot by belief exchange
label_belief2 <-as_labeller(c(`1`="Disagrees it can be exchanged for money",
                              `3`="Neutral",
                              `5`="Agrees it can be exchanged for money"))

# creates labels for good time, exchange and for understands
lab.goodtime<-as_labeller(c(`1`="Disagrees it is a good time to buy",
                            `3`="Neutral",
                            `5`="Agrees it is a good time to buy"))

lab.exchange<-as_labeller(c(`1`="Disagrees it is an exchange opportunity",
                            `3`="Neutral",
                            `5`="Agrees it is an exchange opportunity"))

lab.understands<-as_labeller(c(`1`="Doesn't understand cryotpcurrencies",
                               `3`="Neutal",
                               `5`="Understands well cryptocurrencies"))
################## aware of crypto models ##############
## - step 1
m_impute0.1a <- with(impData,
                     glm(aware ~ age + female + abitur + employed +   
                                pvq_OCH + pvq_SEN , 
                              family=binomial(link="logit")))

#summary(pool(m_impute0.1a))

# - average marginal effects
marg_eff_m0.1a <- marginaleffects::avg_slopes(m_impute0.1a)
marg_eff_m0.1a  
mice::glance(marg_eff_m0.1a)

# datagrid for plotting marginal effects
nd0.1<-datagrid(newdata=shortData,
                female=c(0,1),
                abitur=c(0,1))

datplot0.1<-slopes(m_impute0.1a,
                   variables="pvq_OCH",
                   newdata=nd0.1)

#png("img/fig_ME_aware.png",units = "in",width=9, height= 6,res=300)
ggplot(datplot0.1,aes(x=factor(abitur),y=estimate,color=factor(female),shape=factor(female),
                      ymin=conf.low,ymax=conf.high))+
  #scale_color_manual(values=cbPalette)+
  geom_pointrange() + 
  labs(y="Marginal effects of 'Openness to change' (95% CI) on 
       'Is aware of cryptocurrency'",
       x="Has Abitur",
       color="Is female",
       shape="Is female") + 
  scale_color_grey()+ 
  theme_classic()
#dev.off()


## - step 2 (model does not converge)
m_impute0.2a <-with(impData, 
                    glm(aware ~ age + female + abitur +  employed + 
                    pvq_OCH + pvq_SEN + 
                    investment + exchange + regularization + illegal + goodtime, 
               family=binomial(link="logit")) )

#summary(pool(m_impute0.2a))

#################### intention to hold crypto models ########
## models for intention to buy in future are estimated only
## in data filtered for those 
## - who are aware of crypto but never held crypto before
intdata_imp<-impData %>% filter(aware==1 & holds==0)


## - step 1
m_impute1.1b <- with(intdata_imp, glm(intends ~ age + female + abitur +  employed +
                      pvq_OCH + pvq_SEN, 
                    family=binomial(link="logit")) )


# - marginal effects to be reported
marg_eff_m1.1b <-marginaleffects::avg_slopes(m_impute1.1b)
marg_eff_m1.1b 
mice::glance(marg_eff_m1.1b)

## - step 2
m_impute1.2b <- with(intdata_imp,glm(intends ~ age + female + abitur + employed +
                      pvq_OCH + pvq_SEN + 
                      investment + exchange + regularization + illegal + goodtime + 
                        understands,  
                    family=binomial(link="logit")) )

# - marginal effects to be reported
marg_eff_m1.2b <-marginaleffects::avg_slopes(m_impute1.2b)
marg_eff_m1.2b
mice::glance(marg_eff_m1.2b)


# creates datagrid for calculating marginal probabilities and later plotting them
shortData2<-shortData %>% filter(aware==1 & holds==0)

nd1.1<-datagrid(newdata=shortData2,
                goodtime=c(1,2,3,4,5),
                understands=c(1,3,5))

# marginal probabilities of good time to buy crypto
# plotted at levels of gender and three age groups

datplot1.2<-slopes(m_impute1.2b,
                      variables="pvq_SEN",
                      newdata=nd1.1)

#png("img/fig_ME_intention.png",units = "in",width=9, height= 6,res=300)
ggplot(datplot1.2,aes(x=factor(goodtime),y=estimate,color=factor(understands), shape=factor(understands),
                         ymin=conf.low,ymax=conf.high))+ 
  #scale_color_manual(values=cbPalette)+
  geom_pointrange() + 
  labs(y="Marginal effects of 'Self-enhancement' (95% CI) on 
       'Intends to buy cryptocurrency'",
       x="It is a good time to buy cryptocurrency",
       color="Understands cryptocurrencies",
       shape="Understands cryptocurrencies") + 
  scale_color_grey()+
  theme_classic()
#dev.off()


#################### holds or held crpyto models #############
## - step 1
m_impute2.1a <-with(impData, glm(holds ~ age + female + abitur + employed +  
                    pvq_OCH +  pvq_SEN , 
                  family=binomial(link="logit")) )


# - marginal effects to be reported
marg_eff_m2.1a <-marginaleffects::avg_slopes(m_impute2.1a)
marg_eff_m2.1a
mice::glance(marg_eff_m2.1a)


## - step 2
m_impute2.2a <-with(impData,glm(holds ~ age + female + abitur + employed + 
                    pvq_OCH + pvq_SEN + 
                    investment + exchange + regularization + illegal + goodtime +
                      understands, 
                  family=binomial(link="logit")))

# - marginal effects to be reported
marg_eff_m2.2a <-marginaleffects::avg_slopes(m_impute2.2a)
marg_eff_m2.2a
mice::glance(marg_eff_m2.2a)

# ME self-enhancement plotted for understands, good time, age, female
# creates datagrid for calculating marginal probabilities and later plotting them
nd2.2_1<-datagrid(newdata=shortData,
            age=seq(from=min(na.omit(shortData$age)),to=max(na.omit(shortData$age)),length.out=100),
            female=c(0,1),
            understands=c(1,3,5),
            goodtime=c(1,3,5))

# marginal probabilities of self-enhancement
# at levels of female and age
datplot2.2a_1<-slopes(m_impute2.2a,
                variables="pvq_SEN",
                newdata=nd2.2_1)


#png("img/fig_ME_behavior.png",units = "in",width=10, height= 8,res=300)
ggplot(datplot2.2a_1,aes(x=age,y=estimate,color=factor(female),shape=factor(understands),
                         ymin=conf.low,ymax=conf.high))+
  #scale_color_manual(values=cbPalette)+
  geom_line(linewidth=1.5) + 
  geom_ribbon(alpha=.05) + 
  facet_grid(goodtime ~ understands, 
             labeller=labeller(goodtime=lab.goodtime,
                               understands=lab.understands)) + 
  labs(y="Marginal effects of 'Self-enhancement' (95% CI) on 
       'Owns or has owned in the past cryptocurrency'",
       x="Age",
       color="Is female",
       facet="Good time to buy cryptocurrency") + 
  scale_color_grey() + 
  theme_classic()
#dev.off()

# ME self-enhancement plotted for exchange, good time, age, female
# creates datagrid for calculating marginal probabilities and later plotting them
nd2.2_2<-datagrid(newdata=shortData,
                  age=seq(from=min(na.omit(shortData$age)),to=max(na.omit(shortData$age)),length.out=100),
                  female=c(0,1),
                  understands=c(1,3,5),
                  exchange=c(1,3,5))

# marginal probabilities of self-enhancement
# at levels of female and age
datplot2.2a_2<-slopes(m_impute2.2a,
                      variables="pvq_SEN",
                      newdata=nd2.2_2)


#png("img/fig_ME_behavior_2_color.png",units = "in",width=10, height= 8,res=300)
ggplot(datplot2.2a_2,aes(x=age,y=estimate,color=factor(female),shape=factor(understands),
                         ymin=conf.low,ymax=conf.high))+
  scale_color_manual(values=cbPalette)+
  geom_line(linewidth=1.5) + 
  geom_ribbon(alpha=.05) + 
  facet_grid(exchange ~ understands, 
             labeller=labeller(exchange=lab.exchange,
                               understands=lab.understands)) + 
  labs(y="Marginal effects of 'Self-enhancement' (95% CI) on 
       'Owns or has owned in the past cryptocurrency'",
       x="Age",
       color="Is female",
       facet="It is an exchange opportunity") + 
  #scale_color_grey() + 
  theme_classic()
#dev.off()

###################################### save objects in Rdata format #########

#save(list = c("shortData","impData",
#              str_subset(ls(),"m_"),
#              str_subset(ls(),"marg_eff"),
#              str_subset(ls(),"datplot")),
#     file="data/logitreg.Rdata")
