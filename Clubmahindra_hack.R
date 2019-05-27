##########################
##ClubMahindra Data Hack##
##########################


setwd("C:/Users/TOSHIBA/Desktop/ClubMahindra")


#################
##EDA-QUESTIONS##
#################

#1) Average number of days for each resort_type?Does it vary?

#2) How many days before the stay booking happens?Does it vary?

#3)Which room type,cluster,resort_region is generating more revenue?

#4) Which roomtype,count of roomtype inside a resort is generating more revenue?

#4)Which season is generating more revenue?
#Christmas--> holidaying season gnerates more revenue

#5)Which Age buckets is preeferring which room type?
#Assump--> More kids bigger place like villa

#6) Add Derived Vars-->1)No_of_days_proxy2)No_of_Nights3)Weekday_or_weekend
#                      4)Total_passengers_proxy5)Days_before_staying
#                      6)Total_Reservations_memberid 7)Total_Reservations_Resort_type
#                      8)Number_Reservations_Each_Day 

#7) Did people stayinng during weekday earn more revenue or weekend?


#################
##LOAD PACKAGES##
#################

library(data.table)
library(magrittr)
library(dummies)
library(car)
library(MASS)
library(glmnet)

#########################################
##Data Formatting & Adding Derived Vars##
#########################################

data_train <- fread("train.csv",stringsAsFactors = FALSE)
View(head(data_train,10))
names(data_train) %<>% toupper()
data_train[,BOOKING_DATE:=as.Date(BOOKING_DATE,"%d/%m/%y")]
data_train[,CHECKIN_DATE:=as.Date(CHECKIN_DATE,"%d/%m/%y")]
data_train[,CHECKOUT_DATE:=as.Date(CHECKOUT_DATE,"%d/%m/%y")]
##Create Number Of NIGHTS Stayed
data_train[,NO_NIGHTS_STAY:=CHECKOUT_DATE-CHECKIN_DATE]
data_train[,NO_NIGHTS_STAY:=as.numeric(NO_NIGHTS_STAY)]
data_train[,DIFF_NIGHTS:= ROOMNIGHTS-NO_NIGHTS_STAY]
#We can say that they do not match,relying on dates
##Create Number Of DAYS Stayed
data_train[,NO_DAYS_STAY:=NO_NIGHTS_STAY+1]
data_train[,NO_DAYS_STAY:=as.numeric(NO_DAYS_STAY)]
##Create Number Of DAYS before checking in
data_train[,NO_DAYS_BEFORE_CHECKIN:=CHECKIN_DATE-BOOKING_DATE]
data_train[,NO_DAYS_BEFORE_CHECKIN:=as.numeric(NO_DAYS_BEFORE_CHECKIN)]
##Create total passengers
data_train[,CHECK_TRAVELLING:=TOTAL_PAX-(NUMBEROFADULTS+NUMBEROFCHILDREN)]
#We can see that they do not match, create a proxy total passengers
data_train[,TOTAL_PASSENGERS:=NUMBEROFADULTS+NUMBEROFCHILDREN]
##Was the CHECKIN DATE a weekday or weekend
data_train[,CHECKIN_DAY_TYPE:=ifelse(weekdays(CHECKIN_DATE) %in% c('Saturday','Sunday'),1,0)]
##Was the CHECKOUT DATE a weekday or weekend
data_train[,CHECKOUT_DAY_TYPE:=ifelse(weekdays(CHECKOUT_DATE) %in% c('Saturday','Sunday'),1,0)]
##Was the STAY during the weekend or weekday
data_train[,STAY_DAY_TYPE:=ifelse(CHECKIN_DAY_TYPE==0&CHECKOUT_DAY_TYPE==0,0,1)]
##Total Reservations made by member id
data_train[,TOTAL_RESERVATIONS:=sum(seq(.N)),by=MEMBERID]
##Total Reservations in each resort type 
data_train[,TOTAL_RESERVATIONS_RESORT_TYPE:= sum(seq(.N)),by=list(MEMBERID,RESORT_TYPE_CODE)]
##Total Reservations in each room type 
data_train[,TOTAL_RESERVATIONS_ROOM_TYPE:= sum(seq(.N)),by=list(MEMBERID,ROOM_TYPE_BOOKED_CODE)]
##Total Reservations on each day 
data_train[,TOTAL_RESERVATIONS_EACH_DAY:= sum(seq(.N)),by=list(BOOKING_DATE)]
#Check For NA'S
sapply(data_train,function(x) sum(is.na(x)))
#Since NA'S For Seasoned code, Replace by addinga Seasoned var
data_train[,SEASON_TYPE := ifelse(month(data_train$CHECKIN_DATE)>=11 | month(data_train$CHECKIN_DATE)<=2,"WINTER",ifelse(month(data_train$CHECKIN_DATE)>=3 & month(data_train$CHECKIN_DATE)<=5,"SUMMER",
ifelse(month(data_train$CHECKIN_DATE)>=6 & month(data_train$CHECKIN_DATE)<=8,"RAINY","AUTUMN")))]
#Convert to Factor Variables
factor_vars <- c("CHANNEL_CODE","MAIN_PRODUCT_CODE","RESORT_REGION_CODE","RESORT_TYPE_CODE",
                 "ROOM_TYPE_BOOKED_CODE","SEASON_TYPE","STATE_CODE_RESORT","BOOKING_TYPE_CODE",
                 "MEMBER_AGE_BUCKETS","CLUSTER_CODE","STAY_DAY_TYPE","CHECKIN_DAY_TYPE","CHECKOUT_DAY_TYPE")
data_train[,(factor_vars):= lapply(.SD,as.factor),.SDcols=factor_vars]
#Remove Unwanted Vars
data_train[,DIFF_NIGHTS:=NULL]
data_train[,CHECK_TRAVELLING:=NULL]
data_train[,TOTAL_PAX:=NULL]
data_train[,ROOMNIGHTS:=NULL]
##Dummy vars##
data_train <- dummy.data.frame(data_train, names = factor_vars , sep = "_")
data_train <- data.table(data_train)
names(data_train)%<>%toupper()
colnames(data_train) <- gsub(" ","_",colnames(data_train))



#######
##EDA##
#######


#1) Average number of days for each resort_type?Does it vary?
data_train[,.(mean(NO_DAYS_STAY),.N),by=RESORT_TYPE_CODE]
#Resort type-2 has higher avg of 3.68 compared to the rest
#It varies


#2) How many days before the stay booking happens?Does it vary?
data_train[,.(mean(NO_DAYS_BEFORE_CHECKIN),.N),by=RESORT_TYPE_CODE]
#People book Resort type-5 56 days prior compared to rest,highest demand
#It varies


#3)Which room type,resort type,cluster,resort_region is generating more revenue?

data_train[,.(mean(AMOUNT_SPENT_PER_ROOM_NIGHT_SCALED),.N),by=CLUSTER_CODE]
#Clustercode-C has highest avg of 7.87 compared to rest,ClusterF also has generated
# 7.711 taking number into consid..,Lowest ids Clustecode-A generating 7.53

data_train[,.(mean(AMOUNT_SPENT_PER_ROOM_NIGHT_SCALED),.N),by=RESORT_TYPE_CODE]
#Resorttype-5 generates highest revenue of 7.89 as we have seen above demand also high,
#Lowest being Resorttype-3 generating 7.49

data_train[,.(mean(AMOUNT_SPENT_PER_ROOM_NIGHT_SCALED),.N),by=RESORT_REGION_CODE]
#Resort-region-1 and region-2 generates maximum compared to region-3

#4) Which roomtype,count of roomtype inside a resort is generating more revenue?
data_train[,.(mean(AMOUNT_SPENT_PER_ROOM_NIGHT_SCALED),.N),by=list(RESORT_TYPE_CODE,ROOM_TYPE_BOOKED_CODE)]
#Resorttype-2 and Roomtype-1,Roomtype-2 are genrating maximum revenue compared to rest
#Lowest being Resorttype-3 and Roomtype-3

#4)Which season is generating more revenue?
#Christmas--> holidaying season gnerates more revenue
data_train[,.(mean(AMOUNT_SPENT_PER_ROOM_NIGHT_SCALED),.N),by=SEASON_HOLIDAYED_CODE]
#Seasoncode-1 generates maximum revenue of 7.99 compared to rest,
#Lowest being Season code-4


#5)Which Age buckets is preferring which room type?
#Assump--> More kids bigger place like villa
table(data_train$MEMBER_AGE_BUCKETS,data_train$ROOM_TYPE_BOOKED_CODE)


#6) Did weekday earn more revenue or weekend?
data_train[,mean(AMOUNT_SPENT_PER_ROOM_NIGHT_SCALED),by=STAY_DAY_TYPE]
#Both the tye of days stayed i.e weekday and weekend have generated same revenue

##Check histogram of dep variable
hist(data_train$AMOUNT_SPENT_PER_ROOM_NIGHT_SCALED)

#######################################################
##Data Formatting & Adding Derived Vars For Test Data##
#######################################################

data_test <- fread("test.csv",stringsAsFactors = FALSE)
names(data_test) %<>% toupper()
data_test[,BOOKING_DATE:=as.Date(BOOKING_DATE,"%d/%m/%y")]
data_test[,CHECKIN_DATE:=as.Date(CHECKIN_DATE,"%d/%m/%y")]
data_test[,CHECKOUT_DATE:=as.Date(CHECKOUT_DATE,"%d/%m/%y")]
##Create Number Of NIGHTS Stayed
data_test[,NO_NIGHTS_STAY:=CHECKOUT_DATE-CHECKIN_DATE]
data_test[,NO_NIGHTS_STAY:=as.numeric(NO_NIGHTS_STAY)]
data_test[,DIFF_NIGHTS:= ROOMNIGHTS-NO_NIGHTS_STAY]
#We can say that they do not match,relying on dates
##Create Number Of DAYS Stayed
data_test[,NO_DAYS_STAY:=NO_NIGHTS_STAY+1]
data_test[,NO_DAYS_STAY:=as.numeric(NO_DAYS_STAY)]
##Create Number Of DAYS before checking in
data_test[,NO_DAYS_BEFORE_CHECKIN:=CHECKIN_DATE-BOOKING_DATE]
data_test[,NO_DAYS_BEFORE_CHECKIN:=as.numeric(NO_DAYS_BEFORE_CHECKIN)]
##Create total passengers
data_test[,CHECK_TRAVELLING:=TOTAL_PAX-(NUMBEROFADULTS+NUMBEROFCHILDREN)]
#We can see that they do not match, create a proxy total passengers
data_test[,TOTAL_PASSENGERS:=NUMBEROFADULTS+NUMBEROFCHILDREN]
##Was the CHECKIN DATE a weekday or weekend
data_test[,CHECKIN_DAY_TYPE:=ifelse(weekdays(CHECKIN_DATE) %in% c('Saturday','Sunday'),1,0)]
##Was the CHECKOUT DATE a weekday or weekend
data_test[,CHECKOUT_DAY_TYPE:=ifelse(weekdays(CHECKOUT_DATE) %in% c('Saturday','Sunday'),1,0)]
##Was the STAY during the weekend or weekday
data_test[,STAY_DAY_TYPE:=ifelse(CHECKIN_DAY_TYPE==0&CHECKOUT_DAY_TYPE==0,0,1)]
##Total Reservations made by member id
data_test[,TOTAL_RESERVATIONS:=sum(seq(.N)),by=MEMBERID]
##Total Reservations in each resort type 
data_test[,TOTAL_RESERVATIONS_RESORT_TYPE:= sum(seq(.N)),by=list(MEMBERID,RESORT_TYPE_CODE)]
##Total Reservations in each room type 
data_test[,TOTAL_RESERVATIONS_ROOM_TYPE:= sum(seq(.N)),by=list(MEMBERID,ROOM_TYPE_BOOKED_CODE)]
##Total Reservations on each day 
data_test[,TOTAL_RESERVATIONS_EACH_DAY:= sum(seq(.N)),by=list(BOOKING_DATE)]
#Check For NA'S
sapply(data_test,function(x) sum(is.na(x)))
#Since NA'S For Seasoned code, Replace by addinga Seasoned var
data_test[,SEASON_TYPE := ifelse(month(data_test$CHECKIN_DATE)>=11 | month(data_test$CHECKIN_DATE)<=2,"WINTER",ifelse(month(data_test$CHECKIN_DATE)>=3 & month(data_test$CHECKIN_DATE)<=5,"SUMMER",
                                                                                                                         ifelse(month(data_test$CHECKIN_DATE)>=6 & month(data_test$CHECKIN_DATE)<=8,"RAINY","AUTUMN")))]
#Convert to Factor Variables
factor_vars <- c("CHANNEL_CODE","MAIN_PRODUCT_CODE","RESORT_REGION_CODE","RESORT_TYPE_CODE",
                 "ROOM_TYPE_BOOKED_CODE","SEASON_TYPE","STATE_CODE_RESORT","BOOKING_TYPE_CODE",
                 "MEMBER_AGE_BUCKETS","CLUSTER_CODE","STAY_DAY_TYPE","CHECKIN_DAY_TYPE","CHECKOUT_DAY_TYPE")
data_test[,(factor_vars):= lapply(.SD,as.factor),.SDcols=factor_vars]
#Remove Unwanted Vars
data_test[,DIFF_NIGHTS:=NULL]
data_test[,CHECK_TRAVELLING:=NULL]
data_test[,TOTAL_PAX:=NULL]
data_test[,ROOMNIGHTS:=NULL]
##Dummy vars##
data_test <- dummy.data.frame(data_test, names = factor_vars , sep = "_")
data_test <- data.table(data_test)
names(data_test)%<>%toupper()
colnames(data_test) <- gsub(" ","_",colnames(data_test))



###################################
##Model-1-Regression(Benchmark)####
###################################

data_train_selected <- data_train[,-c("RESERVATION_ID","BOOKING_DATE","CHECKIN_DATE","CHECKOUT_DATE",
                                      "ROOMNIGHTS","SEASON_HOLIDAYED_CODE","STATE_CODE_RESIDENCE","TOTAL_PAX","MEMBERID",
                                      "RESORT_ID","RESERVATIONSTATUSID_CODE")]

mod1 <- lm(AMOUNT_SPENT_PER_ROOM_NIGHT_SCALED ~.,data=data_train_selected)

summary(mod1)
#Multiple R-squared:  0.1107,	Adjusted R-squared:  0.1105 

##Diagnostics for mod-1##
vif(mod1)
#There is multicollinearity present


##Build a Ridge Reg Model##

x <- model.matrix(AMOUNT_SPENT_PER_ROOM_NIGHT_SCALED ~.,data=data_train_selected) 
y <- data_train_selected$AMOUNT_SPENT_PER_ROOM_NIGHT_SCALED
lambda_cv<-cv.glmnet(x,y,alpha=0,nfolds=5,standardize=T,type.measure = "mse") 
bestlam <- lambda_cv$lambda.min 
ridge_mod1<-glmnet(x,y,alpha=0,lambda=bestlam) 

##Using Ridge as benchmark##

data_test_selected <- data_test[,-c("RESERVATION_ID","BOOKING_DATE","CHECKIN_DATE","CHECKOUT_DATE",
                                      "ROOMNIGHTS","SEASON_HOLIDAYED_CODE","STATE_CODE_RESIDENCE","TOTAL_PAX","MEMBERID",
                                      "RESORT_ID","RESERVATIONSTATUSID_CODE")]

test_x <-model.matrix(~.,data=data_test_selected) 
ridge_pred_mod1<-predict(ridge_mod1 ,s=bestlam ,newx=test_x)
pq <- data.frame(ridge_pred_mod1)
result_ridge_mod1 <- data_test[,"RESERVATION_ID"]
result_ridge_mod1 <- cbind(result_ridge_mod1,pq)
write.csv(result_ridge_mod1,"result_ridge_mod1.csv",row.names = FALSE)

rm(pq,x,y,bestlam,lambda_cv,ridge_mod1,ridge_pred_mod1,test_x)

hist(data_train$AMOUNT_SPENT_PER_ROOM_NIGHT_SCALED)
hist(result_ridge_mod1$X1)

#########################
## Model2-random Forest##-
#########################

library(h2o)
localH2O <- h2o.init(nthreads = -1)
h2o.init()
train.h2o <- as.h2o(data_train_selected)
test.h2o <- as.h2o(data_test_selected)

#dependent variable (AVG_SCORE)
y.dep <- 57
#independent variables#
x.indep <- c(1:56,58:75)

rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, 
                                  training_frame = train.h2o, ntrees = 500,
                                  stopping_metric = "MSE",stopping_rounds=5,
                                  nfolds=5,
                                  mtries = 10, max_depth = 5, seed = 1122)

rforest.model

h2o.performance(rforest.model)
#CV results- Mean R2 is 0.089,mean MSE is 1.082,mean RMSE is 1.04

#Metrics on OOB

  
#MSE:  1.083051
#RMSE:  1.040697
#MAE:  0.7738967
#RMSLE:  0.1330158
#Mean Residual Deviance :  1.083051


#check variable importance
h2o.varimp(rforest.model)

#Predictions
predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))
sub_rf_1 <- data.frame(reservation_id=data_test$RESERVATION_ID,amount_spent_per_room_night_scaled=predict.rforest$predict)
write.csv(sub_rf_1, file = "sub_rf_1.csv", row.names = F)


#################
## Model3- GBM ##
#################


#dependent variable (AVG_sCORE)
y.dep <- 57
#independent variables#
x.indep <- c(1:56,58:75)


gbm <- h2o.gbm(
  training_frame = train.h2o,   
  x=x.indep,                     
  y=y.dep,
  nfolds=5,
  ntrees = 300,                
  learn_rate = 0.1,           
  max_depth = 5,              
  sample_rate = 0.7,        
  col_sample_rate = 0.7,
  stopping_rounds = 5,
  stopping_metric="MSE",
  stopping_tolerance = 0.01)             


gbm
h2o.performance(gbm)
#CV Result-Mean r2 is 0.1473,MSE is 1.0132,RMSE is 1.006 

#Metrics on Train

#MSE:  1.006
#RMSE:  1.003
#MAE:  0.7427



#check variable importance
h2o.varimp(gbm)

#Prediction
predict.gbm <- as.data.frame(h2o.predict(gbm, test.h2o))
sub_gbm_1 <- data.frame(reservation_id = data_test$RESERVATION_ID,amount_spent_per_room_night_scaled
=predict.gbm$predict)
write.csv(sub_gbm_1, file = "sub_gbm_1.csv", row.names = F)


h2o.shutdown()




