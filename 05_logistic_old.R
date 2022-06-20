
##### logistic regression (see http://r-statistics.co/Logistic-Regression-With-R.html)
# Legende
# model 1: intercept + covariates(female,abitur, age)
# model 2: intercept + covariates(female,abitur, age) + OCH + CON
# model 3: intercept + covariates(female,abitur, age) + OCH + CON + beliefs(investment,exchange,regularization,illegal, goodtime)
###### ---- holds crypto now or held in the past --- ############

# --- check class bias (checks if levels in the outcome variable equal in size are)
classbiasnow=table(a$krybst01) 
classbiasfuture=table(a$krybsz01)

in_now_zero = a[which(shortData$krybst01==1),] # all 1's
in_now_one = a[which(shortData$krybst01==0),] # all 0's

# subsamples from original data
set.seed(1234)
input_ones_training_rows <- sample(1:nrow(in_now_one),0.5*nrow(in_now_zero)) # 1's for training
input_zeros_training_rows <- sample(1:nrow(in_now_zero),0.5*nrow(in_now_zero)) # 0's for training. Pick as many 0's as 1's

# creates training data - data to run the model on
training_zeros <- in_now_zero[input_zeros_training_rows,]
training_ones <- in_now_one[input_ones_training_rows,]
trainingData <- rbind(training_zeros,training_ones)

# creates test data - data to check for error misclassification (% mismatch of predicted vs. actuals, irrespective of 1's and 0's)
test_ones <- in_now_one[-input_ones_training_rows,]
test_zeros <- in_now_zero[-input_zeros_training_rows,]
testData <- rbind(test_ones,test_zeros)

##### --- model 1
# - runs model
mod1 <- glm(krybst01 ~ age + female + abitur, 
            data=trainingData, family = binomial)
pred.mod1 <- plogis(predict(mod1,testData))
# missclassification error
optcutoff <- optimalCutoff(testData$krybst01,pred.mod1)[1] 
# optimal cutoff that improves the prediction of 1's and 0's 
## reduces the missclassification error
misclserr<-misClassError(testData$krybst01,pred.mod1,threshold=optcutoff) # the lower the value, the better the model is
# concordance
## ideally, the model-calculated-probability-scores of all Positive's (1's) should be greater than
## the model-calculated-probability-scores of ALL the negatives (0's)
## this model is concordant and a highly reliable one
## of all combinations of 1-0 pairs, Concordance is the % of pairs whose scores of actual positive's are greater than
## the scores of actual negative's
## for a perfect mode, this will be 100%
cncrd1<-Concordance(testData$krybst01,pred.mod1)[1] %>% as.numeric()
# specificity and sensitivity
## sensitivity (true positive rate) is the % of 1's correctly predicted by the model
## specificity is the % of 0's correctly predicted by the model
snstv1<-sensitivity(testData$krybst01,pred.mod1,threshold=optcutoff)
spcf1<-specificity(testData$krybst01,pred.mod1,threshold=optcutoff)
# confusion matrix
cm1<-confusionMatrix(testData$krybst01,pred.mod1,threshold=optcutoff)

##### --- model 2
# - runs model
mod2 <- glm(krybst01 ~ age + female + abitur + pvq_OCH + pvq_CON, 
            data=trainingData, family = binomial)
pred.mod2 <- plogis(predict(mod2,testData))
# missclassification error
optcutoff2 <- optimalCutoff(testData$krybst01,pred.mod2)[1] 
misclserr2<-misClassError(testData$krybst01,pred.mod2,threshold=optcutoff2) # the lower the value, the better the model is
# concordance
cncrd2<-Concordance(testData$krybst01,pred.mod2)
# specificity and sensitivity
snstv2<-sensitivity(testData$krybst01,pred.mod2,threshold=optcutoff2)
spcf2<-specificity(testData$krybst01,pred.mod2,threshold=optcutoff2)
# confusion matrix
cm2<-confusionMatrix(testData$krybst01,pred.mod2,threshold=optcutoff2)

#### ---- model 3.a: investment
# - runs model
mod3.a <- glm(krybst01 ~ age + female + abitur + pvq_OCH + pvq_CON +
                investment , 
              data=trainingData, family = binomial)
pred.mod3.a <- plogis(predict(mod3.a,testData))
# missclassification error
optcutoff3.a <- optimalCutoff(testData$krybst01,pred.mod3.a)[1] 
misclserr3.a<-misClassError(testData$krybst01,pred.mod3.a,threshold=optcutoff3.a) # the lower the value, the better the model is
# concordance
cncrd3.a<-Concordance(testData$krybst01,pred.mod3.a)
# specificity and sensitivity
snstv3.a<-sensitivity(testData$krybst01,pred.mod3.a,threshold=optcutoff3.a)
spcf3.a<-specificity(testData$krybst01,pred.mod3.a,threshold=optcutoff3.a)
# confusion matrix
cm3.a<-confusionMatrix(testData$krybst01,pred.mod3.a,threshold=optcutoff3.a)

#### ---- model 3.b: money exchange
# - runs model
mod3.b <- glm(krybst01 ~ age + female + abitur + pvq_OCH + pvq_CON +
                exchange , 
              data=trainingData, family = binomial)
pred.mod3.b <- plogis(predict(mod3.b,testData))
# missclassification error
optcutoff3.b <- optimalCutoff(testData$krybst01,pred.mod3.b)[1] 
misclserr3.b<-misClassError(testData$krybst01,pred.mod3.b,threshold=optcutoff3.b) # the lower the value, the better the model is
# concordance
cncrd3.b<-Concordance(testData$krybst01,pred.mod3.b)
# specificity and sensitivity
snstv3.b<-sensitivity(testData$krybst01,pred.mod3.b,threshold=optcutoff3.b)
spcf3.b<-specificity(testData$krybst01,pred.mod3.b,threshold=optcutoff3.b)
# confusion matrix
cm3.b<-confusionMatrix(testData$krybst01,pred.mod3.b,threshold=optcutoff3.b)

#### ---- model 3.c: state regulated
# - runs model
mod3.c <- glm(krybst01 ~ age + female + abitur + pvq_OCH + pvq_CON +
                regularization , 
              data=trainingData, family = binomial)
pred.mod3.c <- plogis(predict(mod3.c,testData))
# missclassification error
optcutoff3.c <- optimalCutoff(testData$krybst01,pred.mod3.c)[1] 
misclserr3.c<-misClassError(testData$krybst01,pred.mod3.c,threshold=optcutoff3.c) # the lower the value, the better the model is
# concordance
cncrd3.c<-Concordance(testData$krybst01,pred.mod3.c)
# specificity and sensitivity
snstv3.c<-sensitivity(testData$krybst01,pred.mod3.c,threshold=optcutoff3.c)
spcf3.c<-specificity(testData$krybst01,pred.mod3.c,threshold=optcutoff3.c)
# confusion matrix
cm3.c<-confusionMatrix(testData$krybst01,pred.mod3.c,threshold=optcutoff3.c)

#### ---- model 3.d: illegal commerce
# - runs model
mod3.d <- glm(krybst01 ~ age + female + abitur + pvq_OCH + pvq_CON +
                illegal , 
              data=trainingData, family = binomial)
pred.mod3.d <- plogis(predict(mod3.d,testData))
# missclassification error
optcutoff3.d <- optimalCutoff(testData$krybst01,pred.mod3.d)[1] 
misclserr3.d<-misClassError(testData$krybst01,pred.mod3.d,threshold=optcutoff3.d) # the lower the value, the better the model is
# concordance
cncrd3.d<-Concordance(testData$krybst01,pred.mod3.d)
# specificity and sensitivity
snstv3.d<-sensitivity(testData$krybst01,pred.mod3.d,threshold=optcutoff3.d)
spcf3.d<-specificity(testData$krybst01,pred.mod3.d,threshold=optcutoff3.d)
# confusion matrix
cm3.d<-confusionMatrix(testData$krybst01,pred.mod3.d,threshold=optcutoff3.d)

#### ---- model 3.e: good time to buy crypto
# - runs model
mod3.e <- glm(krybst01 ~ age + female + abitur + pvq_OCH + pvq_CON +
                goodtime , 
              data=trainingData, family = binomial)
pred.mod3.e <- plogis(predict(mod3.e,testData))
# missclassification error
optcutoff3.e <- optimalCutoff(testData$krybst01,pred.mod3.e)[1] 
misclserr3.e<-misClassError(testData$krybst01,pred.mod3.e,threshold=optcutoff3.e) # the lower the value, the better the model is
# concordance
cncrd3.e<-Concordance(testData$krybst01,pred.mod3.e)
# specificity and sensitivity
snstv3.e<-sensitivity(testData$krybst01,pred.mod3.e,threshold=optcutoff3.e)
spcf3.e<-specificity(testData$krybst01,pred.mod3.e,threshold=optcutoff3.e)
# confusion matrix
cm3.e<-confusionMatrix(testData$krybst01,pred.mod3.e,threshold=optcutoff3.e)



#### ---- model 4
# - runs model
mod4 <- glm(krybst01 ~ age + female + abitur + pvq_OCH + pvq_CON +
              investment + exchange + regularization + illegal + goodtime, 
            data=trainingData, family = binomial)
pred.mod3 <- plogis(predict(mod3,testData))
# missclassification error
optcutoff3 <- optimalCutoff(testData$krybst01,pred.mod3)[1] 
misclserr3<-misClassError(testData$krybst01,pred.mod3,threshold=optcutoff3) # the lower the value, the better the model is
# concordance
cncrd3<-Concordance(testData$krybst01,pred.mod3)
# specificity and sensitivity
snstv3<-sensitivity(testData$krybst01,pred.mod3,threshold=optcutoff3)
spcf3<-specificity(testData$krybst01,pred.mod3,threshold=optcutoff3)
# confusion matrix
cm3<-confusionMatrix(testData$krybst01,pred.mod3,threshold=optcutoff3)




###### ---- intention hold crypto in the future --- ############
classbiasfuture=table(a$krybsz01)

in_now_zero2 = a[which(a$krybsz01==1),] # all 1's
in_now_one2 = a[which(a$krybsz01==0),] # all 0's

# subsamples from original data
set.seed(1234)
input_ones_training_rows2 <- sample(1:nrow(in_now_one2),0.5*nrow(in_now_zero2)) # 1's for training
input_zeros_training_rows2 <- sample(1:nrow(in_now_zero2),0.5*nrow(in_now_zero2)) # 0's for training. Pick as many 0's as 1's

# creates training data - data to run the model on
training_zeros2 <- in_now_zero2[input_zeros_training_rows2,]
training_ones2 <- in_now_one2[input_ones_training_rows2,]
trainingData2 <- rbind(training_zeros2,training_ones2)

# creates test data - data to check for error misclassification (% mismatch of predicted vs. actuals, irrespective of 1's and 0's)
test_ones2 <- in_now_one2[-input_ones_training_rows2,]
test_zeros2 <- in_now_zero2[-input_zeros_training_rows2,]
testData2 <- rbind(test_ones2,test_zeros2)

# --- model 1
# - runs model
mod1.2 <- glm(krybsz01 ~ age + female + abitur, 
              data=trainingData2, family = binomial)
pred.mod1.2 <- plogis(predict(mod1.2,testData2))
# missclassification error
optcutoff.2 <- optimalCutoff(testData2$krybsz01,pred.mod1.2)[1]
misclserr.2<-misClassError(testData2$krybsz01,pred.mod1.2,threshold=optcutoff.2) # the lower the value, the better the model is
# concordance
cncrd.2<-Concordance(testData2$krybsz01,pred.mod1.2)
# specificity and sensitivity
snstv.2<-sensitivity(testData2$krybsz01,pred.mod1.2,threshold=optcutoff.2)
spcf.2<-specificity(testData2$krybsz01,pred.mod1.2,threshold=optcutoff.2)
# confusion matrix
cm.2<-confusionMatrix(testData2$krybsz01,pred.mod1.2,threshold=optcutoff.2)

