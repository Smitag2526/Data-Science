data("iris")
library(caret)
library(C50)


############Bagging####################

acc<- c()

for(i in 1:1000)
{
  
print(i)
  
#Data Partition
  
inTraininglocal <- createDataPartition(iris$Species,p=0.70,list = F)
training1 <- iris[inTraininglocal,]  
testing <- iris[-inTraininglocal,]  
  
#Model builiding

fittree <- C5.0(training1$Species~.,data = training1)

#Predictions

pred <- predict.C5.0(fittree,testing[,-5])


a<- table(testing$Species,pred)
acc <- c(acc,sum(diag(a))/sum(a))
  
}

summary(acc)
boxplot(acc)


#Boosting


#Data Partition for model builiding and testing

inTraininglocal <- createDataPartition(iris$Species,p=0.70,list=F)

training <- iris[inTraininglocal,]
testing <- iris[-inTraininglocal,]                                       

#Model builiding

model <- C5.0(training$Species~.,data=training,trials=10) #Trials Boost

#Generate the model summary

summary(model)

#Predict for test data set

pred <- predict.C5.0(model,testing[,-5])

a<- table(testing$Species,pred)

acc <- c(acc,sum(diag(a))/sum(a))

summary(acc)


#Bagging And Boosting

acc<- c()

for(i in 1:1000)
{
  
  print(i)
  
  #Data Partition
  
  inTraininglocal <- createDataPartition(iris$Species,p=0.70,list = F)
  training1 <- iris[inTraininglocal,]  
  testing <- iris[-inTraininglocal,]  
  
  #Model builiding
  
  fittree <- C5.0(training1$Species~.,data = training1,trials=10)
  
  #Predictions
  
  pred <- predict.C5.0(fittree,testing[,-5])
  
  
  a<- table(testing$Species,pred)
  acc <- c(acc,sum(diag(a))/sum(a))
  
}

summary(acc)
boxplot(acc)
