###ERICSSON DATA HACK###

setwd("C:/Users/TOSHIBA/Desktop/Ericsson/Task2")


#################
##LOAD PACKAGES##
#################

library(data.table)
library(magrittr)
library(lubridate)
library(tm)
library(RWeka)
library(quanteda)
library(missForest)
options(scipen=9999)

#########################################
##Data Formatting & Adding Derived Vars##
#########################################

#Read data#

data_train <- fread("train_file.csv",stringsAsFactors = FALSE,na.strings="")
data_test <- fread("test_file.csv",stringsAsFactors = FALSE,na.strings="")
#convert to upper
names(data_train) %<>% toupper()
names(data_test) %<>% toupper()

##Create Derived Vars##


#Create number of characters#
data_train[,TITLE:=gsub(" ","",TITLE)]
data_train[,TOTAL_CHARACTERS:=nchar(TITLE)]

#Creator Checkout#
data_train[,TOTAL_CHECKOUTS_CREATOR:=sum(CHECKOUTS),by=c("CREATOR")]
data_train[,TOTAL_CHECKOUTS_CREATOR:=ifelse(CREATOR %in% NA,NA,TOTAL_CHECKOUTS_CREATOR)]

#Subject Checkout#
data_train[,TOTAL_CHECKOUTS_SUBJECTS:=sum(CHECKOUTS),by=c("SUBJECTS")]
data_train[,TOTAL_CHECKOUTS_SUBJECTS:=ifelse(SUBJECTS %in% NA,NA,TOTAL_CHECKOUTS_SUBJECTS)]
#PUblisher Checkout#
data_train[,TOTAL_CHECKOUTS_PUBLISHER:=sum(CHECKOUTS),by=c("PUBLISHER")]
data_train[,TOTAL_CHECKOUTS_PUBLISHER:=ifelse(PUBLISHER %in% NA,NA,TOTAL_CHECKOUTS_PUBLISHER)]

#Publicationyear Checkout#
data_train[,TOTAL_CHECKOUTS_PUBLICATIONYEAR:=sum(CHECKOUTS),by=c("PUBLICATIONYEAR")]
data_train[,TOTAL_CHECKOUTS_PUBLICATIONYEAR:=ifelse(PUBLICATIONYEAR %in% NA,NA,TOTAL_CHECKOUTS_PUBLICATIONYEAR)]

#CheckoutsRate#
data_train[,TOTAL_CHECKOUTS_TITLE_RATE:=round((CHECKOUTS/sum(CHECKOUTS))*100,2)]
data_train[,TOTAL_CHECKOUTS_CREATOR_RATE:=round((TOTAL_CHECKOUTS_CREATOR/sum(TOTAL_CHECKOUTS_CREATOR,na.rm=TRUE))*100,2)]
data_train[,TOTAL_CHECKOUTS_SUBJECTS_RATE:=round((TOTAL_CHECKOUTS_SUBJECTS/sum(TOTAL_CHECKOUTS_SUBJECTS,na.rm=TRUE))*100,2)]
data_train[,TOTAL_CHECKOUTS_PUBLISHER_RATE:=round((TOTAL_CHECKOUTS_PUBLISHER/sum(TOTAL_CHECKOUTS_PUBLISHER,na.rm=TRUE))*100,2)]
data_train[,TOTAL_CHECKOUTS_PUBLICATIONYEAR_RATE:=round((TOTAL_CHECKOUTS_PUBLICATIONYEAR/sum(TOTAL_CHECKOUTS_PUBLICATIONYEAR,na.rm=TRUE))*100,2)]


#Checkouts Rate Monthly#
data_train[,CHECKOUTS_TITLE_MONTHLY:=round((CHECKOUTS/30)*100,2)]
data_train[,CHECKOUTS_CREATOR_MONTHLY:=round((TOTAL_CHECKOUTS_CREATOR/30)*100,2)]
data_train[,CHECKOUTS_SUBJECTS_MONTHLY:=round((TOTAL_CHECKOUTS_SUBJECTS/30)*100,2)]
data_train[,CHECKOUTS_PUBLISHER_MONTHLY:=round((TOTAL_CHECKOUTS_PUBLISHER/30)*100,2)]
data_train[,CHECKOUTS_PUBLICATIONYEAR_MONTHLY:=round((TOTAL_CHECKOUTS_PUBLICATIONYEAR/30)*100,2)]


#Copyright renewal#
data_train[,TOTAL_PUBLISHES:=sapply(strsplit(PUBLICATIONYEAR,","),length)]
data_train[,TOTAL_PUBLISHES:=ifelse(PUBLICATIONYEAR %in% NA,NA,TOTAL_PUBLISHES)]
data_train[,mean(TOTAL_PUBLISHES,na.rm=T),by="MATERIAL_TYPE"]


#Remove extra characters in Publication Year#
data_train[,PUBLICATIONYEAR:=gsub(" ","",PUBLICATIONYEAR)]
data_train[,PUBLICATIONYEAR:=gsub("[a-zA-Z?.©ÂÅ()]","",PUBLICATIONYEAR)]
data_train[,PUBLICATIONYEAR:=gsub("-",",",PUBLICATIONYEAR)]
data_train[,PUBLICATIONYEAR:=gsub("\\[|\\]",",",PUBLICATIONYEAR)]
data_train[,PUBLICATIONYEAR:=gsub("[\u008d<>]","",PUBLICATIONYEAR)]

#Calcalte Renewals#

#Latest_Renewal#
data_train[,LATEST_RENEWAL:=sapply(strsplit(PUBLICATIONYEAR,","),function(x) max(as.numeric(x),na.rm=T))]
data_train[,LATEST_RENEWAL:=ifelse(LATEST_RENEWAL %in% Inf|LATEST_RENEWAL %in% -Inf,NA,LATEST_RENEWAL)]

data_train[,LATEST_RENEWAL:=ifelse(nchar(LATEST_RENEWAL)==2,paste(19,LATEST_RENEWAL,sep=""),
                                   ifelse(nchar(LATEST_RENEWAL)==3,paste(LATEST_RENEWAL,0,sep=""),
                                   ifelse(nchar(LATEST_RENEWAL)==1,paste(190,LATEST_RENEWAL,sep=""),
                                   ifelse(nchar(LATEST_RENEWAL)==6,substr(LATEST_RENEWAL,3,6),
                                   ifelse(nchar(LATEST_RENEWAL)==8,substr(LATEST_RENEWAL,5,8),LATEST_RENEWAL)))))]

data_train[,LATEST_RENEWAL:=as.numeric(LATEST_RENEWAL)]

#Oldest Renewal#
data_train[,OLDEST_RENEWAL:=sapply(strsplit(PUBLICATIONYEAR,","),function(x) min(as.numeric(x),na.rm=T))]
data_train[,OLDEST_RENEWAL:=ifelse(OLDEST_RENEWAL %in% Inf|OLDEST_RENEWAL %in% -Inf,NA,OLDEST_RENEWAL)]

data_train[,OLDEST_RENEWAL:=ifelse(nchar(OLDEST_RENEWAL)==2,paste(19,OLDEST_RENEWAL,sep=""),
                                    ifelse(nchar(OLDEST_RENEWAL)==3,paste(OLDEST_RENEWAL,0,sep=""),
                                    ifelse(nchar(OLDEST_RENEWAL)==1,paste(190,OLDEST_RENEWAL,sep=""),
                                    ifelse(nchar(OLDEST_RENEWAL)==6,substr(OLDEST_RENEWAL,3,6),
                                    ifelse(nchar(OLDEST_RENEWAL)==8,substr(OLDEST_RENEWAL,1,4),OLDEST_RENEWAL)))))]

data_train[,OLDEST_RENEWAL:=as.numeric(OLDEST_RENEWAL)]


#Check MIssing Values#
sapply(data_train,function(x) sum(is.na(x)))
#70%is missing value

####################################
#Impute missing vars for Predictors#
####################################

#By Algorithm

#missforest

data_train_mis <- subset(data_train,select=-c(ID,USAGECLASS                          
                                              ,CHECKOUTTYPE,CHECKOUTYEAR                        
                                              ,CHECKOUTMONTH,                           
                                               TITLE,CREATOR,                             
                                               SUBJECTS,PUBLISHER,                           
                                               PUBLICATIONYEAR,MATERIALTYPE))

imputed_data_train <- missForest(data_train_mis)
complete_data_train <- imputed_data_train$ximp
complete_data_train[,OLDEST_RENEWAL:=round(OLDEST_RENEWAL,0)]
complete_data_train[,LATEST_RENEWAL:=round(LATEST_RENEWAL,0)]
MATERIALTYPE<- data_train$MATERIALTYPE
data_train_selected <- cbind(MATERIALTYPE,complete_data_train)

#Calculate Difference between Renewals#
data_train_selected[,DIFF_RENEWAL:=abs(LATEST_RENEWAL-OLDEST_RENEWAL)]
data_train_selected[,mean(DIFF_RENEWAL),by=MATERIALTYPE]

#Calculate Total Renewals
data_train_selected[,TOTAL_RENEWALS:=abs(TOTAL_PUBLISHES-1)]
rm(complete_data_train,imputed_data_train,data_train_mis)


#######################################################
##Data Formatting & Adding Derived Vars For Test Data##
#######################################################

##Create Derived Vars##

#Create number of characters#
data_test[,TITLE:=gsub(" ","",TITLE)]
data_test[,TOTAL_CHARACTERS:=nchar(TITLE)]

#Creator Checkout#
data_test[,TOTAL_CHECKOUTS_CREATOR:=sum(CHECKOUTS),by=c("CREATOR")]
data_test[,TOTAL_CHECKOUTS_CREATOR:=ifelse(CREATOR %in% NA,NA,TOTAL_CHECKOUTS_CREATOR)]

#Subject Checkout#
data_test[,TOTAL_CHECKOUTS_SUBJECTS:=sum(CHECKOUTS),by=c("SUBJECTS")]
data_test[,TOTAL_CHECKOUTS_SUBJECTS:=ifelse(SUBJECTS %in% NA,NA,TOTAL_CHECKOUTS_SUBJECTS)]
#PUblisher Checkout#
data_test[,TOTAL_CHECKOUTS_PUBLISHER:=sum(CHECKOUTS),by=c("PUBLISHER")]
data_test[,TOTAL_CHECKOUTS_PUBLISHER:=ifelse(PUBLISHER %in% NA,NA,TOTAL_CHECKOUTS_PUBLISHER)]

#Publicationyear Checkout#
data_test[,TOTAL_CHECKOUTS_PUBLICATIONYEAR:=sum(CHECKOUTS),by=c("PUBLICATIONYEAR")]
data_test[,TOTAL_CHECKOUTS_PUBLICATIONYEAR:=ifelse(PUBLICATIONYEAR %in% NA,NA,TOTAL_CHECKOUTS_PUBLICATIONYEAR)]

#CheckoutsRate in a month#
data_test[,TOTAL_CHECKOUTS_TITLE_RATE:=round((CHECKOUTS/sum(CHECKOUTS))*100,2)]
data_test[,TOTAL_CHECKOUTS_CREATOR_RATE:=round((TOTAL_CHECKOUTS_CREATOR/sum(TOTAL_CHECKOUTS_CREATOR,na.rm=TRUE))*100,2)]
data_test[,TOTAL_CHECKOUTS_SUBJECTS_RATE:=round((TOTAL_CHECKOUTS_SUBJECTS/sum(TOTAL_CHECKOUTS_SUBJECTS,na.rm=TRUE))*100,2)]
data_test[,TOTAL_CHECKOUTS_PUBLISHER_RATE:=round((TOTAL_CHECKOUTS_PUBLISHER/sum(TOTAL_CHECKOUTS_PUBLISHER,na.rm=TRUE))*100,2)]
data_test[,TOTAL_CHECKOUTS_PUBLICATIONYEAR_RATE:=round((TOTAL_CHECKOUTS_PUBLICATIONYEAR/sum(TOTAL_CHECKOUTS_PUBLICATIONYEAR,na.rm=TRUE))*100,2)]


#Checkouts Monthly#
data_test[,CHECKOUTS_TITLE_MONTHLY:=round((CHECKOUTS/30)*100,2)]
data_test[,CHECKOUTS_CREATOR_MONTHLY:=round((TOTAL_CHECKOUTS_CREATOR/30)*100,2)]
data_test[,CHECKOUTS_SUBJECTS_MONTHLY:=round((TOTAL_CHECKOUTS_SUBJECTS/30)*100,2)]
data_test[,CHECKOUTS_PUBLISHER_MONTHLY:=round((TOTAL_CHECKOUTS_PUBLISHER/30)*100,2)]
data_test[,CHECKOUTS_PUBLICATIONYEAR_MONTHLY:=round((TOTAL_CHECKOUTS_PUBLICATIONYEAR/30)*100,2)]

#Copyright renewal#
data_test[,TOTAL_PUBLISHES:=sapply(strsplit(PUBLICATIONYEAR,","),length)]
data_test[,TOTAL_PUBLISHES:=ifelse(PUBLICATIONYEAR %in% NA,NA,TOTAL_PUBLISHES)]

#Remove extra characters in Publication Year#
data_test[,PUBLICATIONYEAR:=gsub(" ","",PUBLICATIONYEAR)]
data_test[,PUBLICATIONYEAR:=gsub("[a-zA-Z?.©ÂÅ()]","",PUBLICATIONYEAR)]
data_test[,PUBLICATIONYEAR:=gsub("-",",",PUBLICATIONYEAR)]
data_test[,PUBLICATIONYEAR:=gsub("\\[|\\]",",",PUBLICATIONYEAR)]
data_test[,PUBLICATIONYEAR:=gsub("[\u008d<>]","",PUBLICATIONYEAR)]

#Calcalte Renewals#

#Latest_Renewal#
data_test[,LATEST_RENEWAL:=sapply(strsplit(PUBLICATIONYEAR,","),function(x) max(as.numeric(x),na.rm=T))]
data_test[,LATEST_RENEWAL:=ifelse(LATEST_RENEWAL %in% Inf|LATEST_RENEWAL %in% -Inf,NA,LATEST_RENEWAL)]

data_test[,LATEST_RENEWAL:=ifelse(nchar(LATEST_RENEWAL)==2,paste(19,LATEST_RENEWAL,sep=""),
                                   ifelse(nchar(LATEST_RENEWAL)==3,paste(LATEST_RENEWAL,0,sep=""),
                                          ifelse(nchar(LATEST_RENEWAL)==1,paste(190,LATEST_RENEWAL,sep=""),
                                                 ifelse(nchar(LATEST_RENEWAL)==6,substr(LATEST_RENEWAL,3,6),
                                                        ifelse(nchar(LATEST_RENEWAL)==8,substr(LATEST_RENEWAL,5,8),LATEST_RENEWAL)))))]

data_test[,LATEST_RENEWAL:=as.numeric(LATEST_RENEWAL)]

#Oldest Renewal#
data_test[,OLDEST_RENEWAL:=sapply(strsplit(PUBLICATIONYEAR,","),function(x) min(as.numeric(x),na.rm=T))]
data_test[,OLDEST_RENEWAL:=ifelse(OLDEST_RENEWAL %in% Inf|OLDEST_RENEWAL %in% -Inf,NA,OLDEST_RENEWAL)]

data_test[,OLDEST_RENEWAL:=ifelse(nchar(OLDEST_RENEWAL)==2,paste(19,OLDEST_RENEWAL,sep=""),
                                   ifelse(nchar(OLDEST_RENEWAL)==3,paste(OLDEST_RENEWAL,0,sep=""),
                                          ifelse(nchar(OLDEST_RENEWAL)==1,paste(190,OLDEST_RENEWAL,sep=""),
                                                 ifelse(nchar(OLDEST_RENEWAL)==6,substr(OLDEST_RENEWAL,3,6),
                                                        ifelse(nchar(OLDEST_RENEWAL)==8,substr(OLDEST_RENEWAL,1,4),OLDEST_RENEWAL)))))]

data_test[,OLDEST_RENEWAL:=as.numeric(OLDEST_RENEWAL)]


#Check MIssing Values#
sapply(data_test,function(x) sum(is.na(x)))
#70%is missing value

####################################
#Impute missing vars for Predictors#
####################################

#By Algorithm

#missforest
data_test_mis <- subset(data_test,select=-c(ID,USAGECLASS                          
                                              ,CHECKOUTTYPE,CHECKOUTYEAR                        
                                              ,CHECKOUTMONTH,                           
                                              TITLE,CREATOR,                             
                                              SUBJECTS,PUBLISHER,                           
                                              PUBLICATIONYEAR))

imputed_data_test <- missForest(data_test_mis)
complete_data_test <- imputed_data_test$ximp
complete_data_test[,OLDEST_RENEWAL:=round(OLDEST_RENEWAL,0)]
complete_data_test[,LATEST_RENEWAL:=round(LATEST_RENEWAL,0)]
data_test_selected <- complete_data_test

#Calculate Difference between Renewals#

data_test_selected[,DIFF_RENEWAL:=abs(LATEST_RENEWAL-OLDEST_RENEWAL)]
#Calculate Total Renewals
data_test_selected[,TOTAL_RENEWALS:=abs(TOTAL_PUBLISHES-1)]


rm(complete_data_test,imputed_data_test,data_test_mis)




###########################################
##Model-1-Nominal Regression(Benchmark)####
##########################################


library(h2o)
localH2O <- h2o.init(nthreads = -1)
h2o.init()
data_train_selected$MATERIALTYPE <- as.factor(data_train_selected$MATERIALTYPE)
train.h2o <- as.h2o(data_train_selected)
test.h2o <- as.h2o(data_test_selected)


#dependent variable (AVG_SCORE)
y.dep <- 1
#independent variables#
x.indep <- c(2:22)


log_model <- h2o.glm(y=y.dep, x=x.indep, family="multinomial",
                     training_frame = train.h2o,
                     remove_collinear_columns = TRUE,
                     nfolds=3,seed = 1122,balance_classes = FALSE)

log_model@model$coefficients_table

h2o.performance(log_model)

#Predictions
predict_log_model <- as.data.frame(h2o.predict(log_model, test.h2o))
sub_log_1 <- data.frame(ID = data_test$ID,MaterialType=predict_log_model$predict)
write.csv(sub_log_1, file = "sub_log_1.csv", row.names = F)


#########################
## Model2-random Forest##-
#########################


rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, 
                                  training_frame = train.h2o, ntrees = 500,
                                  stopping_metric = "misclassification",stopping_rounds=5,
                                  nfolds=3,balance_classes=TRUE,mtries = 8, max_depth = 5, seed = 1122)

rforest.model

h2o.performance(rforest.model)

#CV results- Mean aacuracy is 0.738

#check variable importance
h2o.varimp(rforest.model)


#Predictions
predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))
sub_rf_BC <- data.frame(ID = data_test$ID,MaterialType=predict.rforest$predict)
write.csv(sub_rf_BC, file = "sub_rf_BC.csv", row.names = F)


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
  balance_classes = TRUE,
  max_depth = 5,              
  sample_rate = 0.7,        
  col_sample_rate = 0.7,      
  stopping_rounds = 5,
  stopping_metric="misclassification",
  stopping_tolerance = 0.01)             


gbm
h2o.performance(gbm)
#CV Result-Mean accracy is 0.766



#check variable importance
h2o.varimp(gbm)



#Prediction
predict.gbm <- as.data.frame(h2o.predict(gbm, test.h2o))
sub_gbm_bc <- data.frame(ID = data_test$ID,MaterialType=predict.gbm$predict)
write.csv(sub_gbm_bc, file = "sub_gbm_bc.csv", row.names = F)


h2o.shutdown()








