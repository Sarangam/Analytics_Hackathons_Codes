###LTFS DATA HACK###

setwd("C:/Users/TOSHIBA/Desktop/LTFS")


##################
##EDA-Asumptions##
##################



#1)Table of  beauraue score description vs loan_default
#(Assump-->low risk customer defaulted less)

#2) dealer_manufacturer vs asset_Cost vs loan_Default
#(Assump-->more the asset cost more loans more the chancs for defaulting)

#3) Primary Account Active,default %= active accounts|default accounts/total accounts vs loan_Default
#(ASSUMP-->less active acconts, defaults less; more over due accounts defaults more)

# Closed_accounts = 1-(active+overdue/no of accounts)

#4) disbursed % for earlier loan = disbursed/sanctioned vs loan_default
#(Assump-->higher previous disbursed loan amount,lesser default)

#5) default_6 months for previous loan vs loan_Default
#(Assum-->if no emi in last 6 months for previous loan,higher default rate )

#6) time since first loan  vs loan_Default
#(Assump-->loan after long time,less default) 

#7) Enquiries vs loan_Default
#(Assump-->Expensive vehicle,more calls,higher default)

#8) DEMO-->Age,area,employment vs loan_default

#9) Current Loan --> does ltv,laon tenure,asset cost give info?

#10 does combining primary and secondary give info


#################
##load packages##
#################

library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(caret)
library (caTools)
library(e1071)
library(ROCR)

#############################
##Read  & format train Data##
#############################


data_train <- read.csv("train.csv",stringsAsFactors = FALSE,na.strings = NA)
colnames(data_train) <- gsub("\\.","_",colnames(data_train)) 

#convert to upper
names(data_train) %<>% toupper()

#########
##EDA-1##
#########

x <-table(data_train$PERFORM_CNS_SCORE_DESCRIPTION,data_train$LOAN_DEFAULT)

prop.table(x,1)

#as low risk to high risk inreases the default rate also increases,
# not mch of info is in each category of risk and it is not ranked too,we can 
#group to one category and also no info as one category

data_train$PERFORM_CNS_SCORE_DESCRIPTION <- gsub(".\\-","",data_train$PERFORM_CNS_SCORE_DESCRIPTION)

#convert not scored to no history vailable

data_train$PERFORM_CNS_SCORE_DESCRIPTION <- gsub("Not Scored:.*","No Bureau History Available",data_train$PERFORM_CNS_SCORE_DESCRIPTION)


#########
##EDA-2##
#########


data_train %>% select(MANUFACTURER_ID,ASSET_COST,LOAN_DEFAULT) %>% group_by(MANUFACTURER_ID) %>% summarise(MEAN_COST = mean(ASSET_COST,na.rm=TRUE),COUNT_DEFAULTS = sum(LOAN_DEFAULT))

#interesting to see manufacturer with low cost vehicles has more defaults compared
# to manufacurer having high cost vehicles

# supplier and manufacturer link#

data_train$SUPPLIER_MANUFACTURER_ID <- paste(data_train$MANUFACTURER_ID,data_train$SUPPLIER_ID,sep="_")
data_train %>% select(SUPPLIER_MANUFACTURER_ID,ASSET_COST,LOAN_DEFAULT) %>% group_by(SUPPLIER_MANUFACTURER_ID) %>% summarise(MEAN_COST = mean(ASSET_COST,na.rm=TRUE),COUNT_DEFAULTS = sum(LOAN_DEFAULT))%>% arrange(desc(COUNT_DEFAULTS))

# dealers tied up with manufcaturer id 86,45 are more defaulted

# branch id gives any info
data_train %>% select(BRANCH_ID,ASSET_COST,LOAN_DEFAULT) %>% group_by(BRANCH_ID) %>% summarise(MEAN_COST = mean(ASSET_COST,na.rm=TRUE),COUNT_DEFAULTS = sum(LOAN_DEFAULT))
# not much info


#########
##EDA-3##
#########

#Primary#
data_train$PRI_ACTIVE_PERC <- ifelse(!data_train$PRI_ACTIVE_ACCTS,0,data_train$PRI_ACTIVE_ACCTS/data_train$PRI_NO_OF_ACCTS)
data_train$PRI_OVERDUE_PERC <- ifelse(!data_train$PRI_OVERDUE_ACCTS,0,data_train$PRI_OVERDUE_ACCTS/data_train$PRI_NO_OF_ACCTS)

#Calculate closed accts#
data_train$PRI_CLOSED_ACCTS <- data_train$PRI_NO_OF_ACCTS-(data_train$PRI_ACTIVE_ACCTS+data_train$PRI_OVERDUE_ACCTS)
data_train$PRI_CLOSED_ACCTS <- ifelse(data_train$PRI_CLOSED_ACCTS<0,0,data_train$PRI_CLOSED_ACCTS)
data_train$PRI_CLOSED_PERC <- 1-((data_train$PRI_ACTIVE_ACCTS+data_train$PRI_OVERDUE_ACCTS)/data_train$PRI_NO_OF_ACCTS)
data_train$PRI_CLOSED_PERC <- ifelse(data_train$PRI_CLOSED_PERC<0,0,data_train$PRI_CLOSED_PERC)

#Secondary#
data_train$SEC_ACTIVE_PERC <- ifelse(!data_train$SEC_ACTIVE_ACCTS,0,data_train$SEC_ACTIVE_ACCTS/data_train$SEC_NO_OF_ACCTS)
data_train$SEC_OVERDUE_PERC <- ifelse(!data_train$SEC_OVERDUE_ACCTS,0,data_train$SEC_OVERDUE_ACCTS/data_train$SEC_NO_OF_ACCTS)
data_train$SEC_CLOSED_ACCTS <- data_train$SEC_NO_OF_ACCTS-(data_train$SEC_ACTIVE_ACCTS+data_train$SEC_OVERDUE_ACCTS)
data_train$SEC_CLOSED_ACCTS <- ifelse(data_train$SEC_CLOSED_ACCTS<0,0,data_train$SEC_CLOSED_ACCTS)
data_train$SEC_CLOSED_PERC <- 1-((data_train$SEC_ACTIVE_ACCTS+data_train$SEC_OVERDUE_ACCTS)/data_train$SEC_NO_OF_ACCTS)
data_train$SEC_CLOSED_PERC <- ifelse(data_train$SEC_CLOSED_PERC<0,0,data_train$SEC_CLOSED_PERC)

#How active_perc affects loan default-logistic curve
ggplot(data_train,aes(x=PRI_ACTIVE_PERC,y=LOAN_DEFAULT))+geom_smooth(method="glm",method.args = list(family = "binomial"))

#How previous overdue_perc affects loan default- logistic curve
ggplot(data_train,aes(x=PRI_OVERDUE_PERC,y=LOAN_DEFAULT))+geom_smooth(method="glm",method.args = list(family = "binomial"))
# as previous overdue_perc increases the laon_deFault increases

#How closed_perc affects loan default- logistic curve
ggplot(data_train,aes(x=PRI_CLOSED_ACCTS,y=LOAN_DEFAULT))+geom_smooth(method="glm",method.args = list(family = "binomial"))
# as previous closed_perc increases the laon_deFault decreases


#########
##EDA-4##
#########

data_train$PRI_DISBURSED_PERC <- ifelse(!data_train$PRI_DISBURSED_AMOUNT,0,data_train$PRI_DISBURSED_AMOUNT/data_train$PRI_SANCTIONED_AMOUNT)

#How previous disbursed_perc affects loan default- logistic curve
ggplot(data_train,aes(x=PRI_DISBURSED_PERC,y=LOAN_DEFAULT))+geom_smooth(method="glm",method.args = list(family = "binomial"))
# as more amount is given for previous loans, lesser the rate of default


#########
##EDA-5##
#########

x <- table(data_train$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS,data_train$LOAN_DEFAULT)
prop.table(x,1)
# as number of times delinquency keeps increasing from 1 to 4, default increases
# we could say that as a person who has defaulted 4 times in last 6 months,prob
# of default is 30% but after that no information

ggplot(data_train,aes(x=DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS,y=LOAN_DEFAULT))+geom_smooth(method="glm",method.args = list(family = "binomial"))
# calculate delinquent %
data_train$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS_PERC <- ifelse(!data_train$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS,0,data_train$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS/data_train$NEW_ACCTS_IN_LAST_SIX_MONTHS)
ggplot(data_train,aes(x=DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS_PERC,y=LOAN_DEFAULT))+geom_smooth(method="glm",method.args = list(family = "binomial"))


#########
##EDA-6##
#########

data_train$CREDIT_HISTORY_YEARS <- substr(data_train$CREDIT_HISTORY_LENGTH,1,4)
data_train$CREDIT_HISTORY_MONTHS <- substr(data_train$CREDIT_HISTORY_LENGTH,6,10) 
data_train$CREDIT_HISTORY_YEARS <- gsub("yrs","",data_train$CREDIT_HISTORY_YEARS)
data_train$CREDIT_HISTORY_MONTHS <- gsub("mon","",data_train$CREDIT_HISTORY_MONTHS)
data_train$CREDIT_HISTORY_LENGTH <- (as.numeric(data_train$CREDIT_HISTORY_YEARS)*12)+(as.numeric(data_train$CREDIT_HISTORY_MONTHS))
data_train$CREDITE_HISTORY_YEARS <- NULL
data_train$CREDIT_HISTORY_MONTHS <- NULL

#How credit history length affects loan default- logistic curve
ggplot(data_train,aes(x=CREDIT_HISTORY_LENGTH,y=LOAN_DEFAULT))+geom_smooth(method="glm",method.args = list(family = "binomial"))
# not much info,but we can say that as time from first loan increases
# rate of default decreases,he/she taking loan after long time


#########
##EDA-7##
#########


#How customer calls has a r/shp with loan default- logistic curve
ggplot(data_train,aes(x=NO_OF_INQUIRIES,y=LOAN_DEFAULT))+geom_smooth(method="glm",method.args = list(family = "binomial"))

# as calls increases,the default rate increases, it
# forms a good logistic curves


#########
##EDA-8##
#########

#Demographic#

#Emplyment#

x <- table(data_train$EMPLOYMENT_TYPE,data_train$LOAN_DEFAULT)
prop.table(x,1)

#not much information

# Age#

data_train$DAY_MONTH <- substr(data_train$DATE_OF_BIRTH,1,5)
data_train$YEAR <- substr(data_train$DATE_OF_BIRTH,7,8)
data_train$YEAR <- paste(19,data_train$YEAR,sep="")
data_train$DATE_OF_BIRTH <- paste(data_train$DAY_MONTH,data_train$YEAR,sep="-")
data_train$DATE_OF_BIRTH <- as.Date(data_train$DATE_OF_BIRTH,format="%d-%m-%Y")
data_train$AGE <- as.numeric(round((difftime(Sys.Date(),data_train$DATE_OF_BIRTH,units="days"))/365,0))
data_train$DAY_MONTH <- NULL
data_train$YEAR <- NULL

#How age has a r/shp with loan default- logistic curve
ggplot(data_train,aes(x=AGE,y=LOAN_DEFAULT))+geom_smooth(method="glm",method.args = list(family = "binomial"))

# as age increases,the default rate decreases



#########
##EDA-9##
#########


# LTV

#How LTV has a r/shp with loan default- logistic curve
ggplot(data_train,aes(x=LTV,y=LOAN_DEFAULT))+geom_smooth(method="glm",method.args = list(family = "binomial"))
# as LTV increases,the default rate increases

# ASSETCOST

#How assetcost has a r/shp with loan default- logistic curve
ggplot(data_train,aes(x=ASSET_COST,y=LOAN_DEFAULT))+geom_smooth(method="glm",method.args = list(family = "binomial"))
# as assetcost increases,the default rate increases, forming a true logistic curve


# Tenure of loan

data_train$AVERAGE_ACCT_YEARS <- substr(data_train$AVERAGE_ACCT_AGE,1,4)
data_train$AVERAGE_ACCT_MONTHS<- substr(data_train$AVERAGE_ACCT_AGE,6,10) 
data_train$AVERAGE_ACCT_YEARS <- gsub("yrs","",data_train$AVERAGE_ACCT_YEARS)
data_train$AVERAGE_ACCT_MONTHS <- gsub("mon","",data_train$AVERAGE_ACCT_MONTHS)
data_train$AVERAGE_ACCT_AGE <- (as.numeric(data_train$AVERAGE_ACCT_YEARS)*12)+(as.numeric(data_train$AVERAGE_ACCT_MONTHS))
data_train$AVERAGE_ACCT_YEARS <- NULL
data_train$AVERAGE_ACCT_MONTHS <- NULL

#How loan tenure has a r/shp with loan default- logistic curve
ggplot(data_train,aes(x=AVERAGE_ACCT_AGE,y=LOAN_DEFAULT))+geom_smooth(method="glm",method.args = list(family = "binomial"))
# as loan tenure increases,the default rate decreases


##########
##EDA-10##
##########

data_train$TOTAL_OVERDUE_ACCTS <- data_train$PRI_OVERDUE_ACCTS+data_train$SEC_OVERDUE_ACCTS
data_train$TOTAL_ACTIVE_ACCTS <- data_train$PRI_ACTIVE_ACCTS+data_train$SEC_ACTIVE_ACCTS
data_train$TOTAL_CLOSED_ACCTS <- data_train$PRI_CLOSED_ACCTS+data_train$SEC_CLOSED_ACCTS
# overdue accts increases, loan defalt increases



##list of vars from exploration which gives information from
## no default to default from logistic curve

#1) CNS_DESCR-->0.15 TO 0.30
#2) Manfacturer_id--> some info present
#3) pri_Active_perc --> 0.19-0.22/pri_Active_Accts-->0 to 0.2
#4) pri_overdue_perc --> 0.20-0.40/pri_overdue_accts--> o to 1
#5) pri_closed_perc --> 0.16 to 0.29/pri_closed_Accts-->0 TO 0.2
#6) pri_disbursed_perc --> 0 to 0.25
#7) delinquent_acct_last_six --> 0.2 to 0.8,delinquent_perc--> 0 to1
#8) credt_hitory --> 0.05 to 0.22
#9) customer_Calls --> 0.2 to 1.0
#10)age--> 0.12 to 0.24 and emplyment-->no info
#11)ltv-->0.1 to 0.3 and asset_cost--> 0 to 1
#12) loan tenure-->0.15 to 0.22
#13) total_overdue_accts --> 0.2 to 0.8,total_closed_Accts-->0 to 0.2
#14) cns score-->0.18 to 0.24
#15) new accts in last six months-->0 to 0.20
#16) primary_installment--> 0 to 0.2




data_train_selected <- select(data_train,PRI_OVERDUE_ACCTS,PRI_CLOSED_ACCTS,DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS,
                              DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS_PERC,NO_OF_INQUIRIES,ASSET_COST,TOTAL_OVERDUE_ACCTS,PERFORM_CNS_SCORE_DESCRIPTION,PRI_ACTIVE_ACCTS,
                              PRI_DISBURSED_PERC,NEW_ACCTS_IN_LAST_SIX_MONTHS,PRIMARY_INSTAL_AMT,AGE,TOTAL_CLOSED_ACCTS,LOAN_DEFAULT)


## imputing infinite values for percentages,where it can go to inf##
# if it is inf it is "-1"
data_train_selected$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS_PERC <- ifelse(data_train_selected$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS_PERC %in% Inf,-1,data_train_selected$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS_PERC)
data_train_selected$PRI_DISBURSED_PERC <- ifelse(data_train_selected$PRI_DISBURSED_PERC %in% Inf,-1,data_train_selected$PRI_DISBURSED_PERC)
#Converting to factors#
data_train_selected$PERFORM_CNS_SCORE_DESCRIPTION <- as.factor(data_train_selected$PERFORM_CNS_SCORE_DESCRIPTION)
data_train_selected$LOAN_DEFAULT <- as.factor(data_train_selected$LOAN_DEFAULT)
# Remove NA's
data_train_selected <- data_train_selected[complete.cases(data_train_selected),]


######################################################
## Data formatting,adding derived vars for test data##
######################################################

data_test <- read.csv("test.csv",stringsAsFactors = FALSE,na.strings=NA)
colnames(data_test) <- gsub("\\.","_",colnames(data_test)) 
names(data_test) %<>% toupper()
data_test$PERFORM_CNS_SCORE_DESCRIPTION <- gsub(".\\-","",data_test$PERFORM_CNS_SCORE_DESCRIPTION)
data_test$PERFORM_CNS_SCORE_DESCRIPTION <- gsub("Not Scored:.*","No Bureau History Available",data_test$PERFORM_CNS_SCORE_DESCRIPTION)
data_test$PERFORM_CNS_SCORE_DESCRIPTION <- as.factor(data_test$PERFORM_CNS_SCORE_DESCRIPTION)
data_test$PRI_ACTIVE_PERC <- ifelse(!data_test$PRI_ACTIVE_ACCTS,0,data_test$PRI_ACTIVE_ACCTS/data_test$PRI_NO_OF_ACCTS)
data_test$PRI_OVERDUE_PERC <- ifelse(!data_test$PRI_OVERDUE_ACCTS,0,data_test$PRI_OVERDUE_ACCTS/data_test$PRI_NO_OF_ACCTS)
#Calculated closed accts#
data_test$PRI_CLOSED_ACCTS <- data_test$PRI_NO_OF_ACCTS-(data_test$PRI_ACTIVE_ACCTS+data_test$PRI_OVERDUE_ACCTS)
data_test$PRI_CLOSED_ACCTS <- ifelse(data_test$PRI_CLOSED_ACCTS<0,0,data_test$PRI_CLOSED_ACCTS)
data_test$PRI_CLOSED_PERC <- 1-((data_test$PRI_ACTIVE_ACCTS+data_test$PRI_OVERDUE_ACCTS)/data_test$PRI_NO_OF_ACCTS)
data_test$PRI_CLOSED_PERC <- ifelse(data_test$PRI_CLOSED_PERC<0,0,data_test$PRI_CLOSED_PERC)
#Secondary#
data_test$SEC_ACTIVE_PERC <- ifelse(!data_test$SEC_ACTIVE_ACCTS,0,data_test$SEC_ACTIVE_ACCTS/data_test$SEC_NO_OF_ACCTS)
data_test$SEC_OVERDUE_PERC <- ifelse(!data_test$SEC_OVERDUE_ACCTS,0,data_test$SEC_OVERDUE_ACCTS/data_test$SEC_NO_OF_ACCTS)
data_test$SEC_CLOSED_ACCTS <- data_test$SEC_NO_OF_ACCTS-(data_test$SEC_ACTIVE_ACCTS+data_test$SEC_OVERDUE_ACCTS)
data_test$SEC_CLOSED_ACCTS <- ifelse(data_test$SEC_CLOSED_ACCTS<0,0,data_test$SEC_CLOSED_ACCTS)
data_test$SEC_CLOSED_PERC <- 1-((data_test$SEC_ACTIVE_ACCTS+data_test$SEC_OVERDUE_ACCTS)/data_test$SEC_NO_OF_ACCTS)
data_test$SEC_CLOSED_PERC <- ifelse(data_test$SEC_CLOSED_PERC<0,0,data_test$SEC_CLOSED_PERC)
data_test$PRI_DISBURSED_PERC <- ifelse(!data_test$PRI_DISBURSED_AMOUNT,0,data_test$PRI_DISBURSED_AMOUNT/data_test$PRI_SANCTIONED_AMOUNT)
data_test$CREDIT_HISTORY_YEARS <- substr(data_test$CREDIT_HISTORY_LENGTH,1,4)
data_test$CREDIT_HISTORY_MONTHS <- substr(data_test$CREDIT_HISTORY_LENGTH,6,10) 
data_test$CREDIT_HISTORY_YEARS <- gsub("yrs","",data_test$CREDIT_HISTORY_YEARS)
data_test$CREDIT_HISTORY_MONTHS <- gsub("mon","",data_test$CREDIT_HISTORY_MONTHS)
data_test$CREDIT_HISTORY_LENGTH <- (as.numeric(data_test$CREDIT_HISTORY_YEARS)*12)+(as.numeric(data_test$CREDIT_HISTORY_MONTHS))
data_test$CREDITE_HISTORY_YEARS <- NULL
data_test$CREDIT_HISTORY_MONTHS <- NULL
data_test$DAY_MONTH <- substr(data_test$DATE_OF_BIRTH,1,5)
data_test$YEAR <- substr(data_test$DATE_OF_BIRTH,7,8)
data_test$YEAR <- paste(19,data_test$YEAR,sep="")
data_test$DATE_OF_BIRTH <- paste(data_test$DAY_MONTH,data_test$YEAR,sep="-")
data_test$DATE_OF_BIRTH <- as.Date(data_test$DATE_OF_BIRTH,format="%d-%m-%Y")
data_test$AGE <- as.numeric(round((difftime(Sys.Date(),data_test$DATE_OF_BIRTH,units="days"))/365,0))
data_test$DAY_MONTH <- NULL
data_test$YEAR <- NULL
data_test$AVERAGE_ACCT_YEARS <- substr(data_test$AVERAGE_ACCT_AGE,1,4)
data_test$AVERAGE_ACCT_MONTHS<- substr(data_test$AVERAGE_ACCT_AGE,6,10) 
data_test$AVERAGE_ACCT_YEARS <- gsub("yrs","",data_test$AVERAGE_ACCT_YEARS)
data_test$AVERAGE_ACCT_MONTHS <- gsub("mon","",data_test$AVERAGE_ACCT_MONTHS)
data_test$AVERAGE_ACCT_AGE <- (as.numeric(data_test$AVERAGE_ACCT_YEARS)*12)+(as.numeric(data_test$AVERAGE_ACCT_MONTHS))
data_test$AVERAGE_ACCT_YEARS <- NULL
data_test$AVERAGE_ACCT_MONTHS <- NULL
data_test$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS_PERC <- ifelse(!data_test$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS,0,data_test$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS/data_test$NEW_ACCTS_IN_LAST_SIX_MONTHS)
data_test$TOTAL_OVERDUE_ACCTS <- data_test$PRI_OVERDUE_ACCTS+data_test$SEC_OVERDUE_ACCTS
data_test$TOTAL_ACTIVE_ACCTS <- data_test$PRI_ACTIVE_ACCTS+data_test$SEC_ACTIVE_ACCTS
data_test$TOTAL_CLOSED_ACCTS <- data_test$PRI_CLOSED_ACCTS+data_test$SEC_CLOSED_ACCTS
data_test$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS_PERC <- ifelse(data_test$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS_PERC %in% Inf,-1,data_test$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS_PERC)
data_test$PRI_DISBURSED_PERC <- ifelse(data_test$PRI_DISBURSED_PERC %in% Inf,-1,data_test$PRI_DISBURSED_PERC)
data_test$MANUFACTURER_ID <- as.factor(data_test$MANUFACTURER_ID)

##Select only requierd from test##

data_test_selected <- select(data_test,PRI_OVERDUE_ACCTS,PRI_CLOSED_ACCTS,DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS,
                              DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS_PERC,NO_OF_INQUIRIES,ASSET_COST,TOTAL_OVERDUE_ACCTS,PERFORM_CNS_SCORE_DESCRIPTION,PRI_ACTIVE_ACCTS,
                              PRI_DISBURSED_PERC,NEW_ACCTS_IN_LAST_SIX_MONTHS,PRIMARY_INSTAL_AMT,AGE,TOTAL_CLOSED_ACCTS)


######################
##balance of classes##
######################

x <- table(data_train_selected$LOAN_DEFAULT)

prop.table(x)

# 22% is defaulted and 79% is not;imbalanced  


###################################################
###Model Building-Benchmark model-Log Regressin###
###################################################

##MOdel-1##

mod1 = glm(LOAN_DEFAULT ~.,family=binomial,data=data_train_selected)

summary(mod1)

#Do stepwise regression and select best parameters based on AIC

backwards = step(mod1,trace=0)

summary(backwards)

#Equation and Best Set of vars-featre selection#

formula(backwards)

##Check metrics##

predictions_train <- predict(mod1, type = 'response',data_train_selected)

# Threshold at 0.5

predictions_train <- as.factor(ifelse(predictions_train>0.25,"1","0"))

confusionMatrix(predictions_train,data_train_selected$LOAN_DEFAULT,positive="0")

##Accuracy at .79,sensitivy-0.99,specificity-0.0018

##Plotting ROC curve

ROCRpred <- prediction(predictions_train, data_train_selected$LOAN_DEFAULT,label.ordering = c(1, 0))

ROCRperf <- performance(ROCRpred, 'tpr','fpr')

plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))


#Finding AUC

auc <- performance(ROCRpred, 'auc')

auc@y.values[[1]]

## worse than random guessing,AUC IS 0.42


## Selecting a threshold##


# user-defined different cost for false negative and false positive
cost_fp <- 100
cost_fn <- 200
roc_dt <- data.frame( fpr = ROCRperf@x.values[[1]], tpr = ROCRperf@y.values[[1]] )

# cost with the specified false positive and false negative cost 
# false postive rate * number of negative instances * false positive cost + 
# false negative rate * number of positive instances * false negative cost
cost <- ROCRperf@x.values[[1]] * cost_fp * sum(data_train_selected[["LOAN_DEFAULT"]] == 1 ) + 
  ( 1 - ROCRperf@y.values[[1]] ) * cost_fn * sum( data_train_selected[["LOAN_DEFAULT"]] == 0 )

cost_dt <- data.frame( cutoff = ROCRpred@cutoffs[[1]], cost = cost )

# optimal cutoff value, and the corresponding true positive and false positive rate
best_index  <- which.min(cost)
best_cost   <- cost_dt[ best_index, "cost" ]
best_tpr    <- roc_dt[ best_index, "tpr" ]
best_fpr    <- roc_dt[ best_index, "fpr" ]
best_cutoff <- ROCRpred@cutoffs[[1]][ best_index ]
normalize <- function(v) ( v - min(v) ) / diff( range(v) )
col_ramp <- colorRampPalette( c( "green", "orange", "red", "black" ) )(100)   
col_by_cost <- col_ramp[ ceiling( normalize(cost) * 99 ) + 1 ]
cost_plot <- ggplot( cost_dt, aes( cutoff, cost ) ) +
  geom_line( color = "blue", alpha = 0.5 ) +
  geom_point( color = col_by_cost, size = 4, alpha = 0.5 ) +
  ggtitle( "Cost" ) +
  geom_vline( xintercept = best_cutoff, alpha = 0.8, linetype = "dashed", color = "steelblue4" )	


##Threshold selected is .25##

predictions_log <- predict(mod1,newdata=data_test_selected,type="response")
predictions_log <- ifelse(predictions_log>0.25,1,0)
log_results <- select(data_test,UNIQUEID)
log_results <- cbind(log_results,predictions_log)

rm(cost_dt,cost_plot,mod1,roc_dt,ROCRperf,ROCRpred,predictions_train,predictions_log
   ,best_cost,best_cutoff,best_fpr,best_index,best_tpr,col_by_cost,col_ramp,cost,cost_fn,cost_fp)




#########################
## Model2-random Forest##-
#########################


install.packages("h2o")

library(h2o)

localH2O <- h2o.init(nthreads = -1)

h2o.init()

train.h2o <- as.h2o(data_train_selected)

test.h2o <- as.h2o(data_test_selected)

#Relevel

train.h2o["LOAN_DEFAULT"] <- h2o.relevel(x = train.h2o["LOAN_DEFAULT"], y = "1")

#dependent variable (LOAN_DEFAULT)

y.dep <- 15

#independent variables

x.indep <- c(1:14)

rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, 
                                  training_frame = train.h2o, ntrees = 200,
                                  stopping_metric = "misclassification",stopping_rounds=5,
                                  balance_classes = TRUE,
                                  mtries = 4, max_depth = 5, seed = 1122)

h2o.performance(rforest.model)
# AUC is better than benchmark,0.57


#check variable importance

h2o.varimp(rforest.model)

predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))

sub_rf_miss_2 <- data.frame(UNIQUE_ID = data_test$UNIQUEID, LOAN_DEFAULT =  predict.rforest$predict)

write.csv(sub_rf_miss_2, file = "sub_rf_miss_2.csv", row.names = F)




#################
## Model3- GBM ##
#################


gbm <- h2o.gbm(
  training_frame = train.h2o,   
  x=x.indep,                     
  y=y.dep,
  distribution = "bernoulli",
  balance_classes = FALSE,
  ntrees = 200,                
  learn_rate = 0.1,           
  max_depth = 5,              
  sample_rate = 0.7,        
  col_sample_rate = 0.7,      
  stopping_rounds = 5,
  stopping_metric="misclassification",
  stopping_tolerance = 0.01)             


h2o.performance(gbm)


#check variable importance

h2o.varimp(gbm)

predict.gbm <- as.data.frame(h2o.predict(gbm, test.h2o))

sub_gbm_miss_2 <- data.frame(UNIQUE_ID = data_test$UNIQUEID, LOAN_DEFAULT =  predict.gbm$predict)

write.csv(sub_gbm_miss_2, file = "sub_gbm_miss_2.csv", row.names = F)
