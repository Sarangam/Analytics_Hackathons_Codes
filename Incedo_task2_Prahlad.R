###INCEDO DATA HACK-TASK2###

setwd("C:/Users/TOSHIBA/Desktop/incedo/Task2")


#################
##LOAD PACKAGES##
#################

library(data.table)
library(magrittr)
library(ggplot2)
library(magrittr)
library(tm)
library(RWeka)
library(car)
library(MASS)
library(quanteda)
library(stringr)
library(qdap)

#########################################
##Data Formatting & Adding Derived Vars##
#########################################

#Read data#
data_train <- fread("train_dataset.csv",stringsAsFactors = FALSE,na.strings="")
data_test <- fread("test_dataset.csv",stringsAsFactors = FALSE,na.strings="")
#convert to upper
names(data_train) %<>% toupper()
names(data_test) %<>% toupper()
##Create Dependent Vars##
data_train[,AVG_SCORE := rowMeans(.SD,na.rm=TRUE),.SDcols=c("SCORE_1","SCORE_2","SCORE_3","SCORE_4","SCORE_5")]
summary(data_train)
#The dependent var is between the range 0 and 3 and 0 and 2
#Check for NA'S
sapply(data_train,function(x) sum(is.na(x)))
#Remove NA's
data_train <- data_train[complete.cases(data_train),]
#Levels for Clarity and Coherent
data_train[,CLARITY:= factor(CLARITY,levels=c("worst","average","above_average","excellent"))]
data_train[,COHERENT:=factor(COHERENT,levels=c("worst","average","above_average","excellent"))]
#Create number of characters in essay
data_train[,TOTAL_CHARACTERS:=nchar(ESSAYTEXT)]
#Create number of words in essay
data_train[,TOTAL_WORDS:=sapply(strsplit(ESSAYTEXT, " "), length)]
#Create number of sentences in essay
data_train[,TOTAL_SENTENCES:=nsentence(ESSAYTEXT)]
#Create number of punctuations
data_train[,PUNCTUATIONS_1:=str_count(ESSAYTEXT,",")]
data_train[,PUNCTUATIONS_2:=str_count(ESSAYTEXT,fixed("."))]
#Create number of misspelled words
data_train[,TOTAL_MISSPELLED:=sapply(ESSAYTEXT,function(x)length(which_misspelled(x,suggest=FALSE)))]
#Create weightage of essayset
data_train[,ESSAY_WEIGHTAGE:=ifelse(ESSAYSET %in% c(1,2,5,6) ,"HIGH","LOW")]
#Create essay_difficulty for essayset
data_train[,ESSAY_DIFFICULTY:=ifelse(ESSAYSET %in% c(5,6),"HIGH","LOW")]


#######
##EDA##
#######

#Which essayset scored maximum?Which essayset was easy or tough?
data_train[,mean(AVG_SCORE),by=list(ESSAYSET,ESSAY_WEIGHTAGE)]
#Essayset5,6 was tough

#Looking at the distribution for each essay set
ggplot(data=data_train,aes(x=AVG_SCORE))+geom_density()+facet_wrap(~ESSAYSET,scales="free_x")
#Essayset5,6 was tough


#####################
###Text Processing###
#####################

#Check if the same essaytext are presnt in train and test#
setdiff(data_train$ESSAYTEXT,data_test$ESSAYTEXT)


# Create One corpus for train and text
corpus <- VCorpus(VectorSource(data_train$ESSAYTEXT))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords,stopwords("english"))
corpus <- tm_map(corpus,removeNumbers)
corpus <- tm_map(corpus, stemDocument)
# Create matrix
frequencies <- TermDocumentMatrix(corpus)
frequencies
rm(corpus)
# Check for sparsity
findFreqTerms(frequencies, lowfreq=100)
# Remove sparse terms
sparse <- removeSparseTerms(frequencies, 0.95)
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




###################################################
##Data Formatting & Adding Derived Vars For Test##
#################################################

#Read data#
data_test <- fread("test_dataset.csv",stringsAsFactors = FALSE,na.strings="")
#convert to upper
names(data_test) %<>% toupper()
summary(data_test)
#Levels for Clarity and Coherent
data_test[,CLARITY:= factor(CLARITY,levels=c("worst","average","above_average","excellent"))]
data_test[,COHERENT:=factor(COHERENT,levels=c("worst","average","above_average","excellent"))]
#Create number of characters in essay
data_test[,TOTAL_CHARACTERS:=nchar(ESSAYTEXT)]
#Create number of words in essay
data_test[,TOTAL_WORDS:=sapply(strsplit(ESSAYTEXT, " "), length)]
#Create number of sentences in essay
data_test[,TOTAL_SENTENCES:=nsentence(ESSAYTEXT)]
#Create number of punctuations
data_test[,PUNCTUATIONS_1:=str_count(ESSAYTEXT,",")]
data_test[,PUNCTUATIONS_2:=str_count(ESSAYTEXT,fixed("."))]
#Create number of misspelled words
data_test[,TOTAL_MISSPELLED:=sapply(ESSAYTEXT,function(x)length(which_misspelled(x,suggest=FALSE)))]
#Create weightage of essayset
data_test[,ESSAY_WEIGHTAGE:=ifelse(ESSAYSET %in% c(1,2,5,6) ,"HIGH","LOW")]
#Create essay_difficulty for essayset
data_test[,ESSAY_DIFFICULTY:=ifelse(ESSAYSET %in% c(5,6),"HIGH","LOW")]


###################################
###Text Processing for Test Data###
###################################

# Create One corpus for train and text
corpus <- VCorpus(VectorSource(data_test$ESSAYTEXT))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords,stopwords("english"))
corpus <- tm_map(corpus,removeNumbers)
corpus <- tm_map(corpus, stemDocument)
# Create matrix
frequencies <- TermDocumentMatrix(corpus)
frequencies
rm(corpus)
# Check for sparsity
findFreqTerms(frequencies, lowfreq=100)
# Remove sparse terms
sparse <- removeSparseTerms(frequencies, 0.95)
# Convert to a data frame
data_test_text_vars <- data.frame(as.matrix(sparse))
p <- rownames(data_test_text_vars)
data_test_text_vars <- data.table(data_test_text_vars)
data_test_text_vars<- transpose(data_test_text_vars)
#Rename
colnames(data_test_text_vars) <- p
colnames(data_test_text_vars) <- gsub(" ","_",colnames(data_test_text_vars))
rm(frequencies,sparse)
data_test <- cbind(data_test,data_test_text_vars)
names(data_test) %<>% toupper()
rm(data_test_text_vars)


#Remove different vars#

data_test[,AMOUNT:=NULL]
data_test[,BLACK:=NULL]
data_test[,GOOD:=NULL]
data_test[,IMPROV:=NULL]
data_test[,KEEP:=NULL]
data_test[,REPLIC:=NULL]
data_test[,STRETCH:=NULL]
data_test[,TALK:=NULL]
data_test[,THEN:=NULL]
data_test[,TRIAL:=NULL]
data_test[,WEIGHT:=NULL]
data_train[,ANIM:=NULL]
data_train[,BAMBOO:=NULL]
data_train[,EUCALYPTUS:=NULL]
data_train[,SHOW:=NULL]
data_train[,SPECI:=NULL]
data_train[,WORD:=NULL]
data_train[,TIME:=NULL]
   
setdiff(sd,dd)

###################################
##Model-1-Regression(Benchmark)####
###################################

#Unwanted vars#

data_train[,SCORE_1:=NULL]
data_train[,SCORE_2:=NULL]
data_train[,SCORE_3:=NULL]
data_train[,SCORE_4:=NULL]
data_train[,SCORE_5:=NULL]
data_train[,ID:=NULL]
data_train[,MIN_SCORE:=NULL]
data_train[,MAX_SCORE:=NULL]
data_train[,ESSAYTEXT:=NULL]
data_train[,ESSAY_DIFFICULTY:=NULL]
data_train[,ESSAY_WEIGHTAGE:=NULL]

mod1 <- lm(AVG_SCORE ~.,data=data_train)

summary(mod1)
#Multiple R-squared:  0.7771,	Adjusted R-squared:  0.7761 

##Diagnostics for mod-1##

#Check for multicollinearity now
vif(mod1)
#VIF of all vars is <10

##Do stepwise regression and select best parameters based on AIC##
stepwise_model <- step(mod1,trace=0,direction = "both")
summary(stepwise_model)
#no change in adusted rsquared#


##Using mod1 as benchmark##

predictions_mod1 <- predict(mod1,newdata=data_test)
result_mod1 <- data_test[,c("ID","ESSAYSET")]
result_mod1 <- cbind(result_mod1,predictions_mod1)
write.csv(result_mod1,"result_mod1_task2.csv",row.names = FALSE)


#########################
## Model2-random Forest##-
#########################

library(h2o)
localH2O <- h2o.init(nthreads = -1)
h2o.init()
train.h2o <- as.h2o(data_train)
test.h2o <- as.h2o(data_test)

#dependent variable (AVG_SCORE)
y.dep <- 4
#independent variables#
x.indep <- c(1:3,5:67)

rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, 
                                  training_frame = train.h2o, ntrees = 200,
                                  stopping_metric = "MSE",stopping_rounds=5,
                                  nfolds=3,
                                  mtries = 10, max_depth = 5, seed = 1122)

rforest.model

h2o.performance(rforest.model)
#CV results- Mean R2 is 0.7719,mean MSE is 0.317

#Metrics on OOB

#MSE:  0.14962
#RMSE:  0.3868075
#MAE:  0.3213028
#RMSLE:  0.2286431
#Mean Residual Deviance :  0.14962

#check variable importance
h2o.varimp(rforest.model)

#Predictions
predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))
sub_rf_1 <- data.frame(ID = data_test$ID,ESSAYSET=data_test$ESSAYSET,ESSAYSCORE=predict.rforest$predict)
write.csv(sub_rf_1, file = "sub_rf_1.csv", row.names = F)


#################
## Model3- GBM ##
#################


#dependent variable (AVG_sCORE)
y.dep <- 4
#independent variables#
x.indep <- c(1:3,5:67)


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
  stopping_metric="MSE",
  stopping_tolerance = 0.01)             


gbm
h2o.performance(gbm)
#CV Result-r2 is 0.8156,MSE is 0.1154

#Metrics on Train

#MSE:  0.09201124
#RMSE:  0.3033336
#MAE:  0.2402412
#RMSLE:  0.1829851
#Mean Residual Deviance :  0.09201124


#check variable importance
h2o.varimp(gbm)

#Prediction
predict.gbm <- as.data.frame(h2o.predict(gbm, test.h2o))
sub_gbm_1 <- data.frame(ID = data_test$ID,ESSAYSET=data_test$ESSAYSET,ESSAYSCORE=predict.gbm$predict)
write.csv(sub_gbm_1, file = "sub_gbm_1.csv", row.names = F)


h2o.shutdown()
