companydata <-  read.csv("D:/R Excel Sessions/Assignments/Decision Trees/Company_data.csv")

library(Hmisc)

describe(companydata)
head(companydata)
str(companydata)
dim(companydata)
table(companydata$Sales)

colnames(companydata)
str(companydata)

#to check number of rows with missing values

nrow(companydata)-sum(complete.cases(companydata))


#Exlcude the observation for which sales value is zero

install.packages("dplyr")
library(dplyr)

companydata2 <- companydata %>% filter(Sales!=0)
dim(companydata2)
list(companydata2$Sales)


#Converting sales data into categories

companydata2$Sales_new <- ifelse(companydata2$Sales<5,"Low",ifelse(companydata2$Sales<10,"Med","High")) 
companydata2$Sales <- NULL

head(companydata2)
table(companydata2$Sales_new)


str(companydata2)
companydata2$Sales_new <- as.factor(companydata2$Sales_new)
str(companydata2)

head(companydata2)


###Data Exploration and Analysis
table(companydata2$CompPrice,companydata2$Sales_new)
table(companydata2$Income,companydata2$Sales_new)
table(companydata2$Advertising,companydata2$Sales_new)
table(companydata2$Population,companydata2$Sales_new)
table(companydata2$Price,companydata2$Sales_new)
table(companydata2$ShelveLoc,companydata2$Sales_new)
table(companydata2$Urban,companydata2$Sales_new)
table(companydata2$US,companydata2$Sales_new)

####Data Exploration shows that variables like ShelveLOC,Urban, Price ,US seems to be some of the important features

View(companydata2)
table(companydata2$Sales_new)

colnames(companydata2)
head(companydata2)

#Scale only numeric data
#newdata <- companydata2 %>% mutate_if(is.numeric,scale)

head(newdata)

library(caret)
library(C50)

intraininglocal <- createDataPartition(companydata2$Sales_new,p=0.70,list = F)

training <- companydata2[intraininglocal,]
testing <- companydata2[-intraininglocal,]

model <- C5.0(training$Sales_new~.,data=training)
summary(model)

colnames(testing)
View(testing)

pred <- predict.C5.0(model,testing[,-11])

a<- table(testing$Sales_new,pred)
sum(diag(a))/sum(a)

plot(model)

#to see the number of wrong predictions
newpred <- predict.C5.0(model,training)

#Training accuracy
confusionMatrix(newpred,training$Sales_new)

#####Training accuracy is 93% and testing accuracy is 64% ,so the model is overfit

#####Interpretation#################

#For Shelveloc-Good and Price>=97 sales is high, for Price>97 and store not in US, population>298 sales is high.
#For the store which is in US,company price >109 and Income>51 , the sales is high
#For company price>109 and Income<=51 and Education level is >=15, it indicates high sales
# If the shelving location is Medium, Price <-94  and Age <= 34, it indicates high sales
#If the shelving location is Medium ,Price <-94 and Age > 34 and country is not US , company price >130 ,indicates high sales
#If the shelving location is Medium, Price <-94 and Age > 34 and country is US and education level <=15 ,Population <=422 indicates high sales
#If the shelving location is Medium, Price <- 124 , Age <-65, Advertising budget >10 and company price > 120, that indicates high sales


#Bagging Method

acc<- c()

for(i in 1:1000)
  
{

  print(i)
  
intraininglocal <- createDataPartition(companydata2$Sales_new,p=0.70,list = F)

training1 <- newdata[intraininglocal,]
testing <- newdata[-intraininglocal,]

dim(training1)
dim(testing)

fittree <- C5.0(training1$Sales_new~.,data=training1)

pred <- predict.C5.0(fittree,testing[,-11])

a<- table(testing$Sales_new,pred)
acc <- c(acc,sum(diag(a))/sum(a))

}

summary(acc)


####Boosting####################

intraininglocal <- createDataPartition(companydata2$Sales_new,p=0.70,list = F)

training1 <- newdata[intraininglocal,]
testing <- newdata[-intraininglocal,]

dim(training1)
dim(testing)

fittree <- C5.0(training1$Sales_new~.,data=training1,trials=3)

pred <- predict.C5.0(fittree,testing[,-11])

a<- table(testing$Sales_new,pred)
acc <- c(acc,sum(diag(a))/sum(a))

summary(acc)

#####Bagging and Boosting


acc<- c()

for(i in 1:1000)
  
{
  
  print(i)
  
  intraininglocal <- createDataPartition(companydata2$Sales_new,p=0.70,list = F)
  
  training1 <- newdata[intraininglocal,]
  testing <- newdata[-intraininglocal,]
  
  dim(training1)
  dim(testing)
  
  fittree <- C5.0(training1$Sales_new~.,data=training1,trials=5)
  
  pred <- predict.C5.0(fittree,testing[,-11])
  
  a<- table(testing$Sales_new,pred)
  acc <- c(acc,sum(diag(a))/sum(a))
  
}

 
summary(acc)


#####Using Rpart######

install.packages("rpart")
library(rpart)

intraininglocal <- createDataPartition(companydata2$Sales_new,p=0.70,list = F)

training1 <- newdata[intraininglocal,]
testing <- newdata[-intraininglocal,]


model <- rpart(Sales_new~.,data=training1,method = "class",control = rpart.control(cp=0))

#CP is the complexity parameter

summary(model)
printcp(model)
plotcp(model)

#Prune the model based on optimal cp value
model_pruned <- prune(model,cp=0.013)

#Compute the accuracy of the pruned tree

testing$pred <- predict(model_pruned,testing,type = "class")

accuracy_postpruned <- mean(testing$pred==testing$Sales_new)

accuracy_postpruned

####Decision Tree on Fraud data#########

#Use decision trees to prepare a model on fraud data 
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good"

#Data Description :
  
# Undergrad : person is under graduated or not
#Marital.Status : marital status of a person
#Taxable.Income : Taxable income is the amount of how much tax an individual owes to the government 
#Work Experience : Work experience of an individual person
#Urban : Whether that person belongs to urban area or not


frauddata <- read.csv("D:/R Excel Sessions/Assignments/Decision Trees/Fraud_check.csv")

head(frauddata)
describe(frauddata)

nrow(frauddata)-sum(complete.cases(frauddata))
# No missings in the data

str(frauddata)

frauddata$Taxable.Income_new <- ifelse(frauddata$Taxable.Income<=30000,"Risky","Good")

table(frauddata$Taxable.Income,frauddata$Taxable.Income_new)

table(frauddata$Taxable.Income_new)

table(frauddata$Undergrad,frauddata$Taxable.Income_new)
table(frauddata$Marital.Status,frauddata$Taxable.Income_new)
table(frauddata$City.Population,frauddata$Taxable.Income_new)
table(frauddata$Work.Experience,frauddata$Taxable.Income_new)
table(frauddata$Urban,frauddata$Taxable.Income_new)

hist(frauddata$Taxable.Income)

frauddata$Taxable.Income <- NULL
colnames(frauddata)


library(dplyr)

#Scaling only numeric columns
#frauddata_new <- frauddata %>% mutate_if(is.numeric,scale)
head(frauddata_new)
str(frauddata_new)
colnames(frauddata_new)

frauddata$Taxable.Income_new <- as.factor(frauddata$Taxable.Income_new)

library(caret)
intraininglocal <- createDataPartition(frauddata$Taxable.Income_new,p=0.70,list = F)

training <- frauddata[intraininglocal,]
testing <- frauddata[-intraininglocal,]

library(C50)
model <- C5.0(training$Taxable.Income_new~.,data=training)
summary(model)

colnames(testing)

pred <- predict.C5.0(model,testing[,-6])

a<- table(testing$Taxable.Income_new,pred)

acc <- c()
acc <- c(acc,sum(diag(a))/sum(a))

summary(acc)
plot(model)

########Bagging####################

acc<- c()

for(i in 1:1000)
  
{
  
  print(i)
  
  intraininglocal <- createDataPartition(frauddata$Taxable.Income_new,p=0.70,list = F)
  
  training1 <- frauddata[intraininglocal,]
  testing <- frauddata[-intraininglocal,]
  
  model <- C5.0(training1$Taxable.Income_new~.,data=training1)
  summary(model)
  
  pred <- predict.C5.0(model,testing[,-6])
  
  a<- table(testing$Taxable.Income_new,pred)
  acc <- c(acc,sum(diag(a))/sum(a))

  
}

summary(acc)

#Bagging and Boosting

acc<- c()

for(i in 1:1000)
  
{
    print(i)
  
  intraininglocal <- createDataPartition(frauddata$Taxable.Income_new,p=0.70,list = F)
  
  training2 <- frauddata[intraininglocal,]
  testing2 <- frauddata[-intraininglocal,]
  
  model <- C5.0(training2$Taxable.Income_new~.,data=training2,trials=10)
  summary(model)
  
  pred <- predict.C5.0(model,testing2[,-6])
  
  a<- table(testing2$Taxable.Income_new,pred)
  acc <- c(acc,sum(diag(a))/sum(a))
  }

summary(acc)

