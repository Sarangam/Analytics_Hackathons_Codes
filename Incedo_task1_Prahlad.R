###INCEDO DATA HACK###

setwd("C:/Users/TOSHIBA/Desktop/incedo/Task1")

##################
##EDA-Asumptions##
##################

#1)Drug Use or Alchohol use have more probabilty of getting addicted?
#(Assump-->drugs consumed have more probaility)

#2)Race against probability of getting addicted?
#(Assump-->blacks in usa have more probability of consuming
#because of their past/read in freakonomics)

#3) education vs prbability of getting addicted
#(ASSUMP-->less education,more prone of havin drugs/alchohol)

#4) drug use consumption vs probability of getting addicted
#(Assump-->more the consumption,more the chances of being addicted)

#5) question asked & region and cluster 
#(Assum-->is there a particular question asked more in one regionand cluster )

#6) no of question asked & subtopic 
#(Assump-->is there a particular question asked more in one subtopic)

#7) no of questions & year of survey taken 
#(Assump-->is number of questions increased over the years) 

#8) question asked & sample vs probability of getting addicted
#(Assump-->wih test processing we can get the key words out and check samples
# suppose--cocaine-200 gms,etc..) 


#################
##LOAD PACKAGES##
#################

library(data.table)
library(magrittr)
library(ggplot2)
library(magrittr)
library(plyr)
library(dplyr)
library(dummies)
library(tm)
library(RWeka)
library(car)
library(MASS)


#########################################
##Data Formatting & Adding Derived Vars##
#########################################

#Read data#
data_train <- fread("train_file.csv",stringsAsFactors = FALSE)
data_test <- fread("test_file.csv",stringsAsFactors = FALSE)
#convert to upper
names(data_train) %<>% toupper()
names(data_test) %<>% toupper()
##Create Derived Vars##
data_train[,QUESTIONS_PER_SUB_TOPIC := length(unique(QUESTIONCODE)),by=SUBTOPIC]
data_train[,QUESTIONS_PER_LOCATION := length(unique(QUESTIONCODE)),by=LOCATIONDESC]
data_train[,QUESTIONS_PER_YEAR := length(unique(QUESTIONCODE)),by=YEAR]
#Calculated respondents in each year#
data_train[,RESPONDENTS_PER_YEAR:= max(seq(.N)),by=YEAR]
data_train[,RESPONDENTS_EACH_YEAR_PER_LOCATION:=max(seq(.N)),by=list(YEAR,LOCATIONDESC) ]
data_train[,RESPONDENTS_PERC:=(RESPONDENTS_EACH_YEAR_PER_LOCATION/RESPONDENTS_PER_YEAR)*100]
#Convert to factor#
data_train[,SUBTOPIC:=as.factor(SUBTOPIC)]
data_train[,GRADE:=as.factor(GRADE)]
data_train[,STRATIFICATIONTYPE:=as.factor(STRATIFICATIONTYPE)]
#Create Dummy Vars#
q <- c("SEX","RACE","QUESTIONCODE")
data_train <- dummy.data.frame(data_train, names = q , sep = "_")
data_train <- data.table(data_train)
names(data_train)%<>%toupper()
colnames(data_train) <- gsub(" ","_",colnames(data_train))
#Remove Unwanted Vars#
data_train[,SEX_TOTAL:=NULL]
data_train[,RACE_TOTAL:=NULL]
data_train[,QUESTIONCODE_QNHALLUCDRUG:=NULL]


#########
###EDA###
#########


#########
##EDA-1##
#########

data_train %>% select(SUBTOPIC,GREATER_RISK_PROBABILITY) %>% group_by(SUBTOPIC) %>% summarise(MEAN_PROBABILITY = mean(GREATER_RISK_PROBABILITY,na.rm=TRUE))
#There is greater risk of addiction on subtopic "0"

#########
##EDA-2##
#########
options(scipen=999)
#How race has an negative impact on addiction
ggplot(data_train,aes(x=RACE,y=GREATER_RISK_PROBABILITY))+geom_col()+theme(axis.text.x=element_text(angle=45,hjust=1))
# We could see that race black,white,hispanic or latino has higher risk of addiction
# compared to rest of the group


#########
##EDA-3##
#########
#How grade has an negative impact on addiction
ggplot(data_train,aes(x=GRADE,y=GREATER_RISK_PROBABILITY))+geom_col()+theme(axis.text.x=element_text(angle=45,hjust=1))
# We could see that grade 4 has higher risk of addiction
# compared to rest


#########
##EDA-4##
#########

#How sample size of drug/alchohol negatively impacts addiction

#SUBTOPIC-0
ggplot(data_train[data_train$SUBTOPIC==0,],aes(x=SAMPLE_SIZE,y=GREATER_RISK_PROBABILITY))+geom_smooth(se=FALSE)
# interesting to see,as more amount in consumed the probability has reduced
# between sample 0 to 2500 approx,probablity has reduced from 0.5 to 0.4
# and then on from 2500 there is a steep negative slope

#SUBTOPIC-1
ggplot(data_train[data_train$SUBTOPIC==1,],aes(x=SAMPLE_SIZE,y=GREATER_RISK_PROBABILITY))+geom_smooth(se=FALSE)

# interesting to see,as more amount in consumed the probability has reduced
# from sample 0 to 2500 approx,probablity has reduced from 0.25 to 0.15
# and then on a negative slope

# In both cases, we could say that at 2500 gms approx there is bump
# of consumption it could be stage of withdrawal
# beyoind which the probability decreases; it is a proxy threshold

cor(data_train$SAMPLE_SIZE,data_train$GREATER_RISK_PROBABILITY)

#############
##EDA-5,6,7##
#############

# Does number of question differ per sub-topic
table(data_train$SUBTOPIC,data_train$QUESTIONCODE)
# We could see that there are particular questions asked
# more for one sub topic compared to other

# Does number of question differ per year
table(data_train$YEAR,data_train$QUESTIONS_PER_YEAR)
# We could see that the number of questions have increased
# from year 1991 to 2015 along with
# subtopic "1" is more than subtopic "0"

# Does number of question differ per location
ggplot(data_train,aes(x=LOCATIONDESC))+geom_bar()+theme(axis.text.x=element_text(angle=45,hjust=1))
# We could see that there are the number of
# questions asked vary for all the cities

###################
##CheckDist of Dep# 
###################

hist(data_train$GREATER_RISK_PROBABILITY)

#skewed positively

#####################
###Text Processing###
#####################

#Check if the same number of questions are presnt in train and test#
unique(data_train$GREATER_RISK_QUESTION)
unique(data_test$GREATER_RISK_QUESTION)
setdiff(data_train$GREATER_RISK_QUESTION,data_test$GREATER_RISK_QUESTION)


# Create corpus
corpus <- VCorpus(VectorSource(data_train$GREATER_RISK_QUESTION))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords,stopwords("english"))
# Add bigrams,used cocaine,tried marijuana etc..
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
# Create matrix
frequencies <- TermDocumentMatrix(corpus,control = list(tokenize = BigramTokenizer))
frequencies
rm(corpus)
# Check for sparsity
findFreqTerms(frequencies, lowfreq=100)
# Remove sparse terms
sparse <- removeSparseTerms(frequencies, 0.94)
# Convert to a data frame
data_train_text_vars <- data.frame(as.matrix(sparse))
p <- rownames(data_train_text_vars)
data_train_text_vars <- data.table(data_train_text_vars)
data_train_text_vars<- transpose(data_train_text_vars)
#Rename
colnames(data_train_text_vars) <- p
colnames(data_train_text_vars) <- gsub(" ","_",colnames(data_train_text_vars))
rm(frequencies,sparse)
data_train <- cbind(data_train,data_train_text_vars)
names(data_train) %<>% toupper()
rm(data_train_text_vars)


#######################################################
##Data Formatting & Adding Derived Vars For Test Data##
#######################################################

#Read data#
data_test <- fread("test_file.csv",stringsAsFactors = FALSE)
#convert to upper
names(data_test) %<>% toupper()
##Create Derived Vars##
data_test[,QUESTIONS_PER_SUB_TOPIC := length(unique(QUESTIONCODE)),by=SUBTOPIC]
data_test[,QUESTIONS_PER_LOCATION := length(unique(QUESTIONCODE)),by=LOCATIONDESC]
data_test[,QUESTIONS_PER_YEAR := length(unique(QUESTIONCODE)),by=YEAR]
#Calculated respondents in each year#
data_test[,RESPONDENTS_PER_YEAR:= max(seq(.N)),by=YEAR]
data_test[,RESPONDENTS_EACH_YEAR_PER_LOCATION:=max(seq(.N)),by=list(YEAR,LOCATIONDESC) ]
data_test[,RESPONDENTS_PERC:=(RESPONDENTS_EACH_YEAR_PER_LOCATION/RESPONDENTS_PER_YEAR)*100]
#Convert to factor#
data_test[,SUBTOPIC:=as.factor(SUBTOPIC)]
data_test[,GRADE:=as.factor(GRADE)]
data_test[,STRATIFICATIONTYPE:=as.factor(STRATIFICATIONTYPE)]
#Create Dummy Vars#
q <- c("SEX","RACE","QUESTIONCODE")
data_test <- dummy.data.frame(data_test, names = q , sep = "_")
data_test <- data.table(data_test)
names(data_test)%<>%toupper()
colnames(data_test) <- gsub(" ","_",colnames(data_test))
#Remove Unwanted Vars#
data_test[,SEX_TOTAL:=NULL]
data_test[,RACE_TOTAL:=NULL]
data_test[,QUESTIONCODE_QNHALLUCDRUG:=NULL]


###Text Processing-Test data###

# Create corpus
corpus <- VCorpus(VectorSource(data_test$GREATER_RISK_QUESTION))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords,stopwords("english"))
# Add bigrams,used cocaine,tried marijuana etc..
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
# Create matrix
frequencies <- TermDocumentMatrix(corpus,control = list(tokenize = BigramTokenizer))
frequencies
rm(corpus)
# Check for sparsity
findFreqTerms(frequencies, lowfreq=100)
# Remove sparse terms
sparse <- removeSparseTerms(frequencies, 0.94)
# Convert to a data frame
data_test_text_vars <- data.frame(as.matrix(sparse))
n <- rownames(data_test_text_vars)
setdiff(p,n)
setdiff(n,p)
data_train[,USED_METHAMPHETAMINES:=NULL]
data_train[,EVER_DRANK:=NULL]
#Rename
data_test_text_vars <- data.table(data_test_text_vars)
data_test_text_vars<- transpose(data_test_text_vars)
colnames(data_test_text_vars) <- n
colnames(data_test_text_vars) <- gsub(" ","_",colnames(data_test_text_vars))
rm(frequencies,sparse)
data_test <- cbind(data_test,data_test_text_vars)
names(data_test) %<>% toupper()
data_test[,USED_HEROIN:=NULL]
rm(data_test_text_vars)



###################################
##Model-1-Regression(Benchmark)####
###################################

#Sudy is done through clustering,we could cluster and predict
table(data_train$STRATIFICATIONTYPE)
# The number of samples are not the same

#Check or NA'S
summary(data_train)

#Select non-text vars
data_train_without_text <- data_train[,c("YEAR","LOCATIONDESC","SUBTOPIC",                         
                              "SAMPLE_SIZE","GREATER_RISK_PROBABILITY",                                   
                              "SEX_FEMALE","SEX_MALE","RACE_AMERICAN_INDIAN_OR_ALASKA_NATIVE",         
                              "RACE_ASIAN","RACE_BLACK_OR_AFRICAN_AMERICAN","RACE_HISPANIC_OR_LATINO",                       
                              "RACE_MULTIPLE_RACE","RACE_NATIVE_HAWAIIAN_OR_OTHER_PACIFIC_ISLANDER",
                              "RACE_WHITE","GRADE","QUESTIONCODE_H41","QUESTIONCODE_H42",
                              "QUESTIONCODE_H43","QUESTIONCODE_H44","QUESTIONCODE_H45",
                              "QUESTIONCODE_H46","QUESTIONCODE_H47","QUESTIONCODE_H48",
                              "QUESTIONCODE_H49","QUESTIONCODE_H50","QUESTIONCODE_H51",
                              "QUESTIONCODE_H52","QUESTIONCODE_H53","QUESTIONCODE_H54",
                              "QUESTIONCODE_H55","QUESTIONCODE_H56","QUESTIONCODE_H57",
                              "QUESTIONCODE_H58",
                              "QUESTIONS_PER_SUB_TOPIC",                       
                              "QUESTIONS_PER_LOCATION","QUESTIONS_PER_YEAR",                            
                              "RESPONDENTS_PER_YEAR","RESPONDENTS_EACH_YEAR_PER_LOCATION",            
                              "RESPONDENTS_PERC","STRATIFICATIONTYPE")]


mod1 <- lm(GREATER_RISK_PROBABILITY ~.,data=data_train_without_text)
summary(mod1)

##Diagnostics for mod-1##

#Few vars are multicollinera,dropping them
mod1 <- lm(GREATER_RISK_PROBABILITY ~.-STRATIFICATIONTYPE-QUESTIONS_PER_SUB_TOPIC-
             QUESTIONS_PER_LOCATION,data=data_train_without_text)
summary(mod1)
#Check for multicollinearity now
vif(mod1)
#"SUBTOPIC" is multicollinear >10
mod1 <- lm(GREATER_RISK_PROBABILITY ~.-STRATIFICATIONTYPE-QUESTIONS_PER_SUB_TOPIC-
             QUESTIONS_PER_LOCATION-SUBTOPIC,data=data_train_without_text)
summary(mod1)
#r squared at 0.8548 and adjusted at 0.8545


##Do stepwise regression and select best parameters based on AIC##

stepwise_model <- step(mod1,trace=0,direction = "both")
summary(stepwise_model)
#no change in rsquared#

##Does adding text variables have any importance##

mod2 <- lm(GREATER_RISK_PROBABILITY ~.-STRATID1-STRATID2-STRATID3-GREATER_RISK_QUESTION-
        DESCRIPTION-GEOLOCATION-PATIENT_ID-STRATIFICATIONTYPE-QUESTIONS_PER_SUB_TOPIC-
        QUESTIONS_PER_LOCATION-SUBTOPIC,data=data_train)
#Looks like there is lot of multicollinearity in text vars#

##Using mod1 as benchmark##

predictions_mod1 <- predict(mod1,newdata=data_test)
result_mod1 <- data_test[,"PATIENT_ID"]
result_mod1 <- cbind(result_mod1,predictions_mod1)
write.csv(result_mod1,"result_mod1_task1.csv",row.names = FALSE)


hist(predictions_mod1)
hist(data_train$GREATER_RISK_PROBABILITY)

##LASSO REGRESSION##

x <- model.matrix(GREATER_RISK_PROBABILITY~.-STRATIFICATIONTYPE-QUESTIONS_PER_SUB_TOPIC-
                    QUESTIONS_PER_LOCATION-SUBTOPIC,data=data_train_without_text) 

data_test_without_text <- data_test[,c("YEAR","LOCATIONDESC","SUBTOPIC",                         
                                       "SAMPLE_SIZE",                                   
                                       "SEX_FEMALE","SEX_MALE","RACE_AMERICAN_INDIAN_OR_ALASKA_NATIVE",         
                                       "RACE_ASIAN","RACE_BLACK_OR_AFRICAN_AMERICAN","RACE_HISPANIC_OR_LATINO",                       
                                       "RACE_MULTIPLE_RACE","RACE_NATIVE_HAWAIIAN_OR_OTHER_PACIFIC_ISLANDER",
                                       "RACE_WHITE","GRADE","QUESTIONCODE_H41","QUESTIONCODE_H42",
                                       "QUESTIONCODE_H43","QUESTIONCODE_H44","QUESTIONCODE_H45",
                                       "QUESTIONCODE_H46","QUESTIONCODE_H47","QUESTIONCODE_H48",
                                       "QUESTIONCODE_H49","QUESTIONCODE_H50","QUESTIONCODE_H51",
                                       "QUESTIONCODE_H52","QUESTIONCODE_H53","QUESTIONCODE_H54",
                                       "QUESTIONCODE_H55","QUESTIONCODE_H56","QUESTIONCODE_H57",
                                       "QUESTIONCODE_H58",
                                       "QUESTIONS_PER_SUB_TOPIC",                       
                                       "QUESTIONS_PER_LOCATION","QUESTIONS_PER_YEAR",                            
                                       "RESPONDENTS_PER_YEAR","RESPONDENTS_EACH_YEAR_PER_LOCATION",            
                                       "RESPONDENTS_PERC","STRATIFICATIONTYPE")]


test_x <-model.matrix(~.-STRATIFICATIONTYPE-QUESTIONS_PER_SUB_TOPIC-
                        QUESTIONS_PER_LOCATION-SUBTOPIC,data=data_test_without_text) 
  
y <- data_train_without_text$GREATER_RISK_PROBABILITY

lambda_cv<-cv.glmnet(x,y,alpha=1) 
bestlam <- lambda_cv$lambda.min 
lasso_mod1<-glmnet(x,y,alpha=1,lambda=bestlam) 
lasso_pred_mod1<-predict(lasso_mod1 ,s=bestlam ,newx=test_x)
pq <- data.frame(lasso_pred_mod1)
result_lasso_mod1 <- data_test[,"PATIENT_ID"]
result_lasso_mod1 <- cbind(result_lasso_mod1,pq)
hist(result_lasso_mod1$X1)

#Lambda is close to zero which is lsquares


rm(mod1,result_mod1,predictions_mod1,mod2,stepwise_model,bestlam,
   x,test_x,y,data_test_without_text,pq,lasso_mod1,result_lasso_mod1)


#########################
## Model2-random Forest##-
#########################

library(h2o)
localH2O <- h2o.init(nthreads = -1)
h2o.init()
train.h2o <- as.h2o(data_train_without_text)
test.h2o <- as.h2o(data_test)

#dependent variable (Geater_Risk_Probabibility)
y.dep <- 5
#independent variables#
x.indep <- c(1:4,6:40)

rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, 
                                  training_frame = train.h2o, ntrees = 500,
                                  stopping_metric = "MAE",stopping_rounds=5,
                                  nfolds=3,
                                  mtries = 10, max_depth = 5, seed = 1122)

rforest.model

h2o.performance(rforest.model)
#CV results- Mean R2 is 0.7841,mean MAE is 7.305

#Metrics on OOB

#MSE:  93.80585
#RMSE:  9.685342
#MAE:  7.36591
#RMSLE:  0.5502758
#Mean Residual Deviance :  93.80585

#check variable importance
h2o.varimp(rforest.model)

#Predictions
predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))
sub_rf_1 <- data.frame(Patient_ID = data_test$PATIENT_ID,Greater_Risk_Probability =  predict.rforest$predict)
write.csv(sub_rf_1, file = "sub_rf_1.csv", row.names = F)

hist(sub_rf_1$Greater_Risk_Probability)
hist(data_train$GREATER_RISK_PROBABILITY)

##################
##With text Vars##
##################

train.h2o <- as.h2o(data_train)
y.dep <- 42
x.indep <- c(2:4,7:17,19:37,41,43:79)
rforest.model_2 <- h2o.randomForest(y=y.dep, x=x.indep, 
                                    training_frame = train.h2o, ntrees = 500,
                                    stopping_metric = "MAE",stopping_rounds=5,
                                    nfolds=3,
                                    mtries = 10, max_depth = 5, seed = 1122)


rforest.model_2
h2o.performance(rforest.model_2)
# CV Results-Mean R2 is 0.8076,mean MAE is 6.880

#Metrics on OOB

#MSE:  88.37458
#RMSE:  9.400776
#MAE:  7.102535
#RMSLE:  0.542595
#Mean Residual Deviance :  88.37458



#check variable importance
h2o.varimp(rforest.model_2)

#Predictions
predict.rforest <- as.data.frame(h2o.predict(rforest.model_2, test.h2o))
sub_rf_2 <- data.frame(Patient_ID = data_test$PATIENT_ID,Greater_Risk_Probability =  predict.rforest$predict)
write.csv(sub_rf_2, file = "sub_rf_2.csv", row.names = F)


hist(sub_rf_2$Greater_Risk_Probability)
hist(data_train$GREATER_RISK_PROBABILITY)


#################
## Model3- GBM ##
#################

train.h2o <- as.h2o(data_train_without_text)

#dependent variable (Geater_Risk_Probabibility)
y.dep <- 5
#independent variables#
x.indep <- c(1:2,4,6:33,36:39)


gbm <- h2o.gbm(
  training_frame = train.h2o,   
  x=x.indep,                     
  y=y.dep,
  nfolds=3,
  ntrees = 200,                
  learn_rate = 0.1,           
  max_depth = 5,              
  sample_rate = 0.7,        
  col_sample_rate = 0.7,      
  stopping_rounds = 5,
  stopping_metric="MAE",
  stopping_tolerance = 0.01)             


gbm
h2o.performance(gbm)
#CV Result-r2 is 0.8157,MAE is 5.514

#Metrics on Train

#MSE:  71.4445
#RMSE:  8.452485
#MAE:  5.182986
#RMSLE:  0.3817625
#Mean Residual Deviance :  71.4445


#check variable importance
h2o.varimp(gbm)

#Prediction
predict.gbm <- as.data.frame(h2o.predict(gbm, test.h2o))
sub_gbm_1 <- data.frame(Patient_ID = data_test$PATIENT_ID, Greater_Risk_Probability =  predict.gbm$predict)
write.csv(sub_gbm_1, file = "sub_gbm_1.csv", row.names = F)


hist(sub_gbm_1$Greater_Risk_Probability)

##################
##With text Vars##
##################

train.h2o <- as.h2o(data_train)
y.dep <- 42
x.indep <- c(2:4,7:17,19:37,41,43:79)


gbm_2 <- h2o.gbm(
  training_frame = train.h2o,   
  x=x.indep,                     
  y=y.dep,
  nfolds=3,
  ntrees = 200,                
  learn_rate = 0.1,           
  max_depth = 5,              
  sample_rate = 0.7,        
  col_sample_rate = 0.7,      
  stopping_rounds = 5,
  stopping_metric="MAE",
  stopping_tolerance = 0.01)             


gbm_2
h2o.performance(gbm_2)

#Cv results-Mean R2 is 0.95,Mean MAE is 3.402

#It could be a Case of overfitting if we predict using this model


