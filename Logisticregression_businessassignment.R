

credit_data <- read.csv("D:/R Excel Sessions/Assignments/Logistic Regression/creditcard.csv")

head(credit_data)
dim(credit_data)
str(credit_data)
View(credit_data)
summary(credit_data)

credit_data$majorcards <- as.factor(credit_data$majorcards)

library(dplyr)

credit_num <- select_if(credit_data,is.numeric)

str(credit_num)

credit_factor <- select_if(credit_data,is.factor)

str(credit_factor)
pairs(credit_num)
cor(credit_num)

####Run MLR to see which variables are significant

credit_data$card <- as.numeric(credit_data$card)
credit_model <- lm(card~reports+age+income+share+expenditure+owner+selfemp+dependents+months+majorcards+active,data=credit_data)

summary(credit_model)

###reports,income,share,owner,dependents,majorcards,active are significant variables

########Run a logistic regression on significant variables

str(credit_data)
credit_data$card <- as.factor(credit_data$card)

logit=glm(card ~ reports+income+share+factor(owner)+dependents+factor(majorcards)+active,family= "binomial",data=credit_data)

summary(logit)

# Confusion Matrix Table


logit_predict=predict(logit,type=c("response"),credit_data)
logit_predict
confusion<-table(prob>0.5,credit_data$card)
confusion
#library(pROC)
install.packages("ROCR")
library("ROCR")

Prediction <- prediction(logit_predict,credit_data$card)
rocrperf<-performance(Prediction,"tpr","fpr")
str(rocrperf)
rocrperf@x.values
plot(rocrperf,colorize=T)

auc<-performance(Prediction,"auc")
auc@y.values

#########################################################################

bank_data <- read.csv("D:/R Excel Sessions/Assignments/Logistic Regression/bank-full.csv")

View(bank_data)
str(bank_data)
dim(bank_data)

library(Hmisc)
describe(bank_data)
colnames(bank_data)

library(dplyr)

bank_num <- select_if(bank_data,is.numeric)

str(bank_num)

bank_cat <-select_if(bank_data,is.factor)
str(bank_cat)

install.packages("stats")
library(stats)

pairs(bank_num)
cor(bank_num)

####Boxplot

boxplot(bank_data$balance~bank_data$job,notch=TRUE,main="Average yearly Balance by type of job ")
boxplot(bank_data$balance~bank_data$loan,notch=TRUE,main="Average yearly Balance by loan  ")

boxplot(bank_data$campaign~bank_data$poutcome,notch=TRUE,main="number of contacts performed during this campaign by outcome of the previous campaign  ")


####Categorical vs categorical

xtabs(~job+education,bank_data)
plot(xtabs(~job+education,bank_data),main="Type of job vs Education")

xtabs(~default+housing,bank_data)
plot(xtabs(~default+housing,bank_data),main="Housing loan vs Credit in Default")



library(gmodels)

CrossTable(bank_data$education,bank_data$y,chisq = T,prop.t = F)

###Use Chi-sq value to find p value, which will show significance.
#p value <0.05 it shows that education and subscription of a term deposit has a significant relation

CrossTable(bank_data$default,bank_data$y,chisq = T,prop.t = F)
#p value <0.05 it shows that Credit default and subscription of a term deposit has a significant relation

CrossTable(bank_data$housing,bank_data$y,chisq = T,prop.t = F)
#p value <0.05 it shows that housing loan and subscription of a term deposit has a significant relation

CrossTable(bank_data$loan,bank_data$y,chisq = T,prop.t = F)
#p value <0.05 it shows that Personal loan and subscription of a term deposit has a significant relation

CrossTable(bank_data$poutcome,bank_data$y,chisq = T,prop.t = F)
#p value >0.05 it shows that outcome of the previous marketing campaign and subscription of a term deposit has a significant relation

CrossTable(bank_data$month,bank_data$y,chisq = T,prop.t = F)
#p value >0.05 it shows that Last contact month and subscription of a term deposit has a significant relation

CrossTable(bank_data$job,bank_data$y,chisq = T,prop.t = F)
#p value >0.05 it shows that Type of job and subscription of a term deposit has a significant relation


####Continuous va Continuous

scatter.smooth(bank_data$age,bank_data$balance,xlab="Age",ylab = "Average yrly balance")


####Run Decision tree to see features importance

str(bank_data)
colnames(bank_data)

table(bank_data$y)
prop.table(table(bank_data$y))

#Bagging and Boosting

library(caret)
library(C50)
acc<- c()

for(i in 1:500)
  
{
  print(i)
  
  intraininglocal <- createDataPartition(bank_data$y,p=0.70,list = F)
  
  training <- bank_data[intraininglocal,]
  testing <- bank_data[-intraininglocal,]
  
  model <- C5.0(training$y~.,data=training,trials=10)
  summary(model)
  
  pred <- predict.C5.0(model,testing[,-17])
  
  a<- table(testing$y,pred)
  acc <- c(acc,sum(diag(a))/sum(a))
}

summary(acc)

summary(model)

####As per the variable we can remove marital while building the final model

####Logistic model

bank_logit=glm(y ~ .-marital,family= "binomial",data=bank_data)

summary(bank_logit)

# Confusion Matrix Table

banklogit_predict <- predict(bank_logit,type=c("response"),bank_data)
banklogit_predict
confusion<-table(banklogit_predict>0.5,bank_data$y)
confusion

(38952+1828)/(38952+3461+970+1828)


#library(pROC)
install.packages("ROCR")
library("ROCR")

Prediction <- prediction(banklogit_predict,bank_data$y)
rocrperf<-performance(Prediction,"tpr","fpr")
str(rocrperf)
rocrperf@x.values
plot(rocrperf,colorize=T)

auc<-performance(Prediction,"auc")
auc@y.values
#####The  model gives 90% accuracy


