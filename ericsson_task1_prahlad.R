###ERICSSON DATA HACK###

setwd("C:/Users/TOSHIBA/Desktop/Ericsson/Task1")


#################
##LOAD PACKAGES##
#################

library(data.table)
library(magrittr)
library(lubridate)
library(tm)
library(RWeka)
library(quanteda)


#########################################
##Data Formatting & Adding Derived Vars##
#########################################

#Read data#

data_train <- fread("train.csv",stringsAsFactors = FALSE,na.strings="")
data_test <- fread("test.csv",stringsAsFactors = FALSE,na.strings="")
#convert to upper
names(data_train) %<>% toupper()
names(data_test) %<>% toupper()


##Create Derived Vars##

#Positive Sentences&NegativeSentences&Total_Sentences#
data_train[,NO_SENTENCES_POSITIVE:= nsentence(POSITIVES)]
data_train[,NO_SENTENCES_NEGATIVE:=nsentence(NEGATIVES)]
#data_train[,NO_SENTENCES_SENTIMENT:=NO_SENTENCES_POSITIVE+NO_SENTENCES_NEGATIVE]
data_train[,NO_SENTENCES_SUMMARY:=nsentence(SUMMARY)]
#Total Sentences#
data_train[,TOTAL_SENTENCES:=NO_SENTENCES_SUMMARY+NO_SENTENCES_NEGATIVE+NO_SENTENCES_POSITIVE]
#Calculate Positive words,Negative Words,Total Words
#Remove Numbers
data_train[,POSITIVES:=removeNumbers(POSITIVES)]
data_train[,NEGATIVES:=removeNumbers(NEGATIVES)]
#Remove Punctuation#
data_train[,POSITIVES:= removePunctuation(POSITIVES)]
data_train[,NEGATIVES:=removePunctuation(NEGATIVES)]
#Remove Stopwords#
data_train[,POSITIVES:=removeWords(POSITIVES,stopwords("english"))]
data_train[,NEGATIVES:=removeWords(NEGATIVES,stopwords("english"))]
#Remove Whitespace#
data_train[,POSITIVES:=stripWhitespace(POSITIVES)]
data_train[,NEGATIVES:=stripWhitespace(NEGATIVES)]
#Positive And Negative Words#
data_train[,NO_WORDS_POSITIVE:=sapply(strsplit(POSITIVES, " "), length)]
data_train[,NO_WORDS_NEGATIVE:=sapply(strsplit(NEGATIVES," "),length)]
#Total Words In Sentiment#
#data_train[,NO_WORDS_SENTIMENT:=NO_WORDS_POSITIVE+NO_WORDS_NEGATIVE]
data_train[,POSITIVITY:=round((NO_WORDS_POSITIVE/(NO_WORDS_POSITIVE+NO_WORDS_NEGATIVE)),3)]
data_train[,NEGATIVITY:=round((NO_WORDS_NEGATIVE/(NO_WORDS_POSITIVE+NO_WORDS_NEGATIVE)),3)]
#Total Words In Summary#
data_train[,SUMMARY:=removeNumbers(SUMMARY)]
data_train[,SUMMARY:= removePunctuation(SUMMARY)]
data_train[,SUMMARY:=removeWords(SUMMARY,stopwords("english"))]
data_train[,SUMMARY:=stripWhitespace(SUMMARY)]
data_train[,NO_WORDS_SUMMARY:=sapply(strsplit(SUMMARY, " "), length)]
#Total Words#
data_train[,TOTAL_WORDS:=NO_WORDS_SUMMARY+NO_WORDS_POSITIVE+NO_WORDS_NEGATIVE]
data_train[,POSITIVES:=tolower(POSITIVES)]
data_train[,NEGATIVES:=tolower(NEGATIVES)]
data_train[,SUMMARY:=tolower(SUMMARY)]
#Total Characters#
data_train[,TOTAL_CHARS_POSITIVES:=nchar(POSITIVES)]
data_train[,TOTAL_CHARS_NEGATIVES:=nchar(NEGATIVES)]
data_train[,TOTAL_CHARS_SUMMARY:=nchar(SUMMARY)]
data_train[,TOTAL_CHARS:=TOTAL_CHARS_NEGATIVES+TOTAL_CHARS_POSITIVES+TOTAL_CHARS_SUMMARY]

#Count of job title by date&place#
data_train[,COUNT_JOB_TITLE_PLACE:=.N,by=c("JOB_TITLE","PLACE")]
data_train[,TOTAL_JOB_TITLE:=.N,by=JOB_TITLE]
#job-title perc
data_train[,JOB_TITLE_PERC:=round((COUNT_JOB_TITLE_PLACE/TOTAL_JOB_TITLE)*100,0)]

#reviews on each day
data_train[,COUNT_DATE:=.N,by=DATE]


#Countof status by place
data_train[,COUNT_STATUS_PLACE :=.N,by=c("STATUS","PLACE")]
data_train[,TOTAL_STATUS_PLACE:=.N,by=PLACE]
data_train[,STATUS_PERC:= round((COUNT_STATUS_PLACE/TOTAL_STATUS_PLACE)*100,0)]

#Date Vars#

data_train[,DATE:=gsub(" ","",DATE)]
data_train[,DATE:=as.Date(DATE,format="%b%d,%Y")]
data_train[,MONTH:=month(DATE)]
data_train[,YEAR:=year(DATE)]
data_train[,DAY:=day(DATE)]

#job-title in a year
data_train[,COUNT_JOB_TITLE_YEAR:=.N,by=c("JOB_TITLE","YEAR")]
data_train[,TOTAL_YEAR:=.N,by=YEAR]
data_train[,JOB_TITLE_YEAR_PERC:=round((COUNT_JOB_TITLE_YEAR/TOTAL_YEAR)*100,0)]


#Likes Received Per day,month,year

data_train[,LIKES_DAY:=sum(SCORE_6),by=c("ID","DAY")]
data_train[,LIKES_MONTH:=sum(SCORE_6),by=c("ID","MONTH")]
data_train[,LIKES_YEAR:=sum(SCORE_6),by=c("YEAR","ID")]

#Find the most frequent words in Positives,
#Negatives And Summary#

bow_function<- function(data,var,n){
  corpus <- VCorpus(VectorSource(data[[var]]))
  corpus <- tm_map(corpus, stemDocument)
  # Create matrix
  frequencies <- TermDocumentMatrix(corpus)
  #frequencies <- DocumentTermMatrix(corpus,control = list(weighting=function(x)weightTfIdf(x,normalize=FALSE)))
  rm(corpus)
  sparse <- removeSparseTerms(frequencies,n)
  # Convert to a data frame
  data_train_text_vars <- data.frame(as.matrix(sparse))
  p <- rownames(data_train_text_vars)
  p <- paste(p,var,sep="_")
  data_train_text_vars <- data.table(data_train_text_vars)
  data_train_text_vars<- transpose(data_train_text_vars)
  #Rename
  colnames(data_train_text_vars) <- p
  colnames(data_train_text_vars) <- gsub(" ","_",colnames(data_train_text_vars))
  #Calculate TF-IDF
  idf <- log(nrow(data_train_text_vars)/colSums(data_train_text_vars))
  data_train_text_vars <- as.data.frame(data_train_text_vars)
  for(word in names(data_train_text_vars)){
    data_train_text_vars[,word] <- data_train_text_vars[,word] * idf[word]
  }
  data <- cbind(data,data_train_text_vars)
  names(data) %<>% toupper()
  data <- data.table(data)
  return(data)
}

data_train <- bow_function(data_train,"POSITIVES",0.95)
data_train <- bow_function(data_train,"NEGATIVES",0.95)
data_train <- bow_function(data_train,"SUMMARY",0.95)


#Check MIssing Values#
sapply(data_train,function(x) sum(is.na(x)))
#Imputing scores by startup as it is startup related#
data_train[,.(mean(SCORE_1,na.rm=TRUE),mean(SCORE_2,na.rm=TRUE),
              mean(SCORE_3,na.rm=TRUE),mean(SCORE_4,na.rm=TRUE),
              mean(SCORE_5,na.rm=TRUE)),by=PLACE]
#The scores are changing#
data_train[,SCORE_1:=ifelse(SCORE_1 %in% NA,mean(SCORE_1,na.rm=TRUE),SCORE_1),by=PLACE]
data_train[,SCORE_2:=ifelse(SCORE_2 %in% NA,mean(SCORE_2,na.rm=TRUE),SCORE_1),by=PLACE]
data_train[,SCORE_3:=ifelse(SCORE_3 %in% NA,mean(SCORE_3,na.rm=TRUE),SCORE_1),by=PLACE]
data_train[,SCORE_4:=ifelse(SCORE_4 %in% NA,mean(SCORE_4,na.rm=TRUE),SCORE_1),by=PLACE]
data_train[,SCORE_5:=ifelse(SCORE_5 %in% NA,mean(SCORE_5,na.rm=TRUE),SCORE_1),by=PLACE]
#Remove Vars With High missing values#
data_train[,ADVICE_TO_MGMT:=NULL]
data_train[,LOCATION:=NULL]
data_train[,STATUS:=as.factor(STATUS)]
data_train[,OVERALL:=as.factor(OVERALL)]
data_train[,PLACE:=as.factor(PLACE)]


#######################################################
##Data Formatting & Adding Derived Vars For Test Data##
#######################################################

##Create Derived Vars##

#Positive Sentences&NegativeSentences&Total_Sentences#
data_test[,NO_SENTENCES_POSITIVE:= nsentence(POSITIVES)]
data_test[,NO_SENTENCES_NEGATIVE:=nsentence(NEGATIVES)]
#data_test[,NO_SENTENCES_SENTIMENT:=NO_SENTENCES_POSITIVE+NO_SENTENCES_NEGATIVE]
data_test[,NO_SENTENCES_SUMMARY:=nsentence(SUMMARY)]
#Total Sentences#
data_test[,TOTAL_SENTENCES:=NO_SENTENCES_SUMMARY+NO_SENTENCES_POSITIVE+NO_SENTENCES_NEGATIVE]

#Positive words,Negative Words,Total Words
#Remove Numbers
data_test[,POSITIVES:=removeNumbers(POSITIVES)]
data_test[,NEGATIVES:=removeNumbers(NEGATIVES)]
#Remove Punctuation#
data_test[,POSITIVES:= removePunctuation(POSITIVES)]
data_test[,NEGATIVES:=removePunctuation(NEGATIVES)]
#Remove Stopwords#
data_test[,POSITIVES:=removeWords(POSITIVES,stopwords("english"))]
data_test[,NEGATIVES:=removeWords(NEGATIVES,stopwords("english"))]
#Remove Whitespace#
data_test[,POSITIVES:=stripWhitespace(POSITIVES)]
data_test[,NEGATIVES:=stripWhitespace(NEGATIVES)]
#Positive And Negative Words#
data_test[,NO_WORDS_POSITIVE:=sapply(strsplit(POSITIVES, " "), length)]
data_test[,NO_WORDS_NEGATIVE:=sapply(strsplit(NEGATIVES," "),length)]
#Total Words In Sentiment#
#data_test[,NO_WORDS_SENTIMENT:=NO_WORDS_POSITIVE+NO_WORDS_NEGATIVE]
data_test[,POSITIVITY:=round((NO_WORDS_POSITIVE/(NO_WORDS_POSITIVE+NO_WORDS_NEGATIVE)),3)]
data_test[,NEGATIVITY:=round((NO_WORDS_NEGATIVE/(NO_WORDS_POSITIVE+NO_WORDS_NEGATIVE)),3)]
#Total Words In Summary#
data_test[,SUMMARY:=removeNumbers(SUMMARY)]
data_test[,SUMMARY:= removePunctuation(SUMMARY)]
data_test[,SUMMARY:=removeWords(SUMMARY,stopwords("english"))]
data_test[,SUMMARY:=stripWhitespace(SUMMARY)]
data_test[,NO_WORDS_SUMMARY:=sapply(strsplit(SUMMARY, " "), length)]
#Total Words#
data_test[,TOTAL_WORDS:=NO_WORDS_POSITIVE+NO_WORDS_SUMMARY+NO_WORDS_NEGATIVE]
data_test[,POSITIVES:=tolower(POSITIVES)]
data_test[,NEGATIVES:=tolower(NEGATIVES)]
data_test[,SUMMARY:=tolower(SUMMARY)]
#Total Characters#
data_test[,TOTAL_CHARS_POSITIVES:=nchar(POSITIVES)]
data_test[,TOTAL_CHARS_NEGATIVES:=nchar(NEGATIVES)]
data_test[,TOTAL_CHARS_SUMMARY:=nchar(SUMMARY)]
data_test[,TOTAL_CHARS:=TOTAL_CHARS_NEGATIVES+TOTAL_CHARS_POSITIVES+TOTAL_CHARS_SUMMARY]

#Count of job title by date&place#
data_test[,COUNT_JOB_TITLE_PLACE:=.N,by=c("JOB_TITLE","PLACE")]
data_test[,TOTAL_JOB_TITLE:=.N,by=JOB_TITLE]
#job-title perc
data_test[,JOB_TITLE_PERC:=round((COUNT_JOB_TITLE_PLACE/TOTAL_JOB_TITLE)*100,0)]

#reviews on each day
data_test[,COUNT_DATE:=.N,by=DATE]


#Countof status by place
data_test[,COUNT_STATUS_PLACE :=.N,by=c("STATUS","PLACE")]
data_test[,TOTAL_STATUS_PLACE:=.N,by=PLACE]
data_test[,STATUS_PERC:= round((COUNT_STATUS_PLACE/TOTAL_STATUS_PLACE)*100,0)]


#Date Var#
data_test[,DATE:=gsub(" ","",DATE)]
data_test[,DATE:=as.Date(DATE,format="%b%d,%Y")]
data_test[,MONTH:=month(DATE)]
data_test[,YEAR:=year(DATE)]
data_test[,DAY:=day(DATE)]

#job-title in a year
data_test[,COUNT_JOB_TITLE_YEAR:=.N,by=c("JOB_TITLE","YEAR")]
data_test[,TOTAL_YEAR:=.N,by=YEAR]
data_test[,JOB_TITLE_YEAR_PERC:=round((COUNT_JOB_TITLE_YEAR/TOTAL_YEAR)*100,0)]


#Likes Received Per day,month,year

data_test[,LIKES_DAY:=sum(SCORE_6),by=c("ID","DAY")]
data_test[,LIKES_MONTH:=sum(SCORE_6),by=c("ID","MONTH")]
data_test[,LIKES_YEAR:=sum(SCORE_6),by=c("ID","YEAR")]


#BOW-TFIDF#
data_test <- bow_function(data_test,"POSITIVES",0.95)
data_test <- bow_function(data_test,"NEGATIVES",0.95)
data_test <- bow_function(data_test,"SUMMARY",0.95)


#Check MIssing Values#
sapply(data_test,function(x) sum(is.na(x)))
data_test[,DAY:=ifelse(DAY %in% NA,na.locf(DAY),DAY)]
data_test[,MONTH:=ifelse(MONTH %in% NA,na.locf(MONTH),MONTH)]
data_test[,YEAR:=ifelse(YEAR %in% NA,na.locf(YEAR),YEAR)]

#Remove Vars With high Misisng Vals#
data_test[,ADVICE_TO_MGMT:=NULL]
data_test[,LOCATION:=NULL]
data_test[,STATUS:=as.factor(STATUS)]
data_test[,PLACE:=as.factor(PLACE)]


#Check var names in both#
setdiff(colnames(data_train),colnames(data_test))
setdiff(colnames(data_test),colnames(data_train))
#Remove Uncommon Vars#
data_train[,TAKE_NEGATIVES:=NULL]
#data_test[,CULTUR_POSITIVES:=NULL]



###########################################
##Model-1-Ordinal Regression(Benchmark)####
###########################################

#Remove NA's#
data_train <- data_train[,-c("SUMMARY")]
data_train <- data_train[complete.cases(data_train),]


library(h2o)
localH2O <- h2o.init(nthreads = -1)
h2o.init()
train.h2o <- as.h2o(data_train)
test.h2o <- as.h2o(data_test)


#dependent variable (AVG_SCORE)
y.dep <- 8
#independent variables#
x.indep <- c(2,4,9:119)


log_model <- h2o.glm(y=y.dep, x=x.indep, family="ordinal",
                                  training_frame = train.h2o,
                                  remove_collinear_columns = TRUE,
                                  nfolds=3,seed = 1122,balance_classes = FALSE)

log_model@model$coefficients_table

log_model
#cv-accuracy-0.37
h2o.performance(log_model)

#Predictions
predict_log_model <- as.data.frame(h2o.predict(log_model, test.h2o))
sub_log_1 <- data.frame(ID = data_test$ID,overall=predict_log_model$predict)
write.csv(sub_log_1, file = "sub_log_1.csv", row.names = F)


#########################
## Model2-random Forest##-
#########################


rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, 
                                  training_frame = train.h2o, ntrees = 500,
                                  stopping_metric = "misclassification",stopping_rounds=5,
                                  nfolds=3,balance_classes=FALSE,max_depth = 7, seed = 1122)
#                                  mtries=10)

rforest.model

h2o.performance(rforest.model)

#CV results- Mean aacuracy is 0.5033

#check variable importance
h2o.varimp(rforest.model)


#Predictions
predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))
sub_rf_1 <- data.frame(ID = data_test$ID,overall=predict.rforest$predict)
write.csv(sub_rf_1, file = "sub_rf_1.csv", row.names = F)


#Grid Search# 

#hyper_params = list(max_depth = seq(1,10,2))

#grid <- h2o.grid(
#  hyper_params = hyper_params,
#  search_criteria = list(strategy = "Cartesian"),
#  algorithm="randomForest",
#  grid_id="depth_grid",
#  x = x.indep, 
#  y = y.dep, 
#  training_frame = train.h2o, 
#  ntrees = 100,
#  mtries =8,
#  seed = 1234,                                                             
#  stopping_rounds = 5,
#  stopping_tolerance = 0.01,
#  stopping_metric = "misclassification"
#)

#grid                                                                       

## sort the grid models by decreasing mis
#sortedGrid <- h2o.getGrid("depth_grid", sort_by="misclassification", decreasing = TRUE)    
#sortedGrid



#################
## Model3- GBM ##
#################


gbm <- h2o.gbm(
  training_frame = train.h2o,   
  x=x.indep,                     
  y=y.dep,
  nfolds=3,
  ntrees = 500,                
  learn_rate = 0.1,
  balance_classes = FALSE,
  max_depth = 7,              
  sample_rate = 0.7,        
  col_sample_rate = 0.7,      
  stopping_rounds = 5,
  stopping_metric="misclassification",
  stopping_tolerance = 0.01)             


gbm
h2o.performance(gbm)
#CV Result-Mean accracy is 0.5357



#check variable importance
h2o.varimp(gbm)



#Prediction
predict.gbm <- as.data.frame(h2o.predict(gbm, test.h2o))
sub_gbm_1 <- data.frame(ID = data_test$ID,order=predict.gbm$predict)
write.csv(sub_gbm_1, file = "sub_gbm_1.csv", row.names = F)


h2o.shutdown()






