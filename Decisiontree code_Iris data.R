data("iris")

install.packages("caret")
library(caret)

install.packages("C50")
library(C50)

#Selecting rows for the randomly selected 70% observations
inTraininglocal <- createDataPartition(iris$Species,p=0.70,list = F)

#collecting the randomly selected training observations from Iris data 
training <- iris[inTraininglocal,]
testing <- iris[-inTraininglocal,]

#Model builiding
model <- C5.0(training$Species~.,data = training)

#Generate the model summary
summary(model)

#Predict for test data set

pred <- predict.C5.0(model,testing[,-5])

a<- table(testing$Species,pred)
sum(diag(a))/sum(a)

plot(model)

#to see the number of wrong predictions
newpred <- predict.C5.0(model,training[,-5])

View(newpred)

#Attach newpred variable to original data and compare


