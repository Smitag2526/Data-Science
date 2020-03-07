
KNN <- read.csv("D:/R Excel Sessions/Day 4/KNN.csv")

head(KNN)
attach(KNN)
KNN<-KNN[,-1]
table(KNN$diagnosis)
#3 is for counting how many observations are under whhich class, in KNN data we have two classes
round(prop.table(table(KNN$diagnosis))*100,digits=1)
#4 is for to calulate in terms of percentage how many observations are lying under one class, digit is to show that figure till one decemal place
normalize<-function(x){return((x-min(x))/(max(x)-min(x)))}
#create normalize function
KNN1<-as.data.frame(lapply(KNN[2:31],normalize ))
KNN11<-cbind.data.frame(KNN1,"diagnosis"=KNN$diagnosis)
View(KNN11)
library(caret)
library(class)
library(gmodels)
View(KNN_test)

acc15<-c()
acc16<-c()
acc17<-c()
acc18<-c()
acc19<-c()
acc20<-c()
acc21<-c()
acc22<-c()
acc23<-c()
acc24<-c()
acc25<-c()
acc26<-c()
acc27<-c()
acc28<-c()
acc29<-c()
acc30<-c()


for(i in 15:30)
{
  print(i)
  intraining<-createDataPartition(KNN11$diagnosis,p=.7,list = F)
  KNN_train<-KNN11[intraining,]
  KNN_test<-KNN11[-intraining,]
  
  
  wbcd_test_pred<-knn(train = KNN_train[,-31],test = KNN_test[,-31],cl=KNN_train$diagnosis,k=i)
  #Building Model
  #CrossTable(x=KNN_test$diagnosis,Y=wbcd_test_pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
  a<-table(KNN_test$diagnosis,wbcd_test_pred)
  
  acc[i] <- c(acc[i],sum(diag(a))/sum(a))
  
}

warnings()
summary(acc15)
summary(acc20)



library(caret)
install.packages("e1071")
library(e1071)
library(C50)

set.seed(3032)
intraining<-createDataPartition(KNN11$diagnosis,p=.7,list = F)
KNN_train<-KNN11[intraining,]
KNN_test<-KNN11[-intraining,]

dim(KNN_train)
dim(KNN_test)



trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(diagnosis ~., data = KNN_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)



knn_fit

test_pred <- predict(knn_fit,newdata = KNN_test)
test_pred

confusionMatrix(test_pred,KNN_test$diagnosis)

#Testing accuracy is 98%

#Let's check training accuracy

train_pred <- predict(knn_fit,newdata = KNN_train)
confusionMatrix(train_pred,KNN_train$diagnosis)

#Training accuracy is 97%, so the model is perfect

#With Bagging


acc<- c()

for(i in 1:100)
  
{
  
  print(i)
  
  intraining<-createDataPartition(KNN11$diagnosis,p=.7,list = F)
  KNN_train<-KNN11[intraining,]
  KNN_test<-KNN11[-intraining,]
  

  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

  knn_fit <- train(diagnosis ~., data = KNN_train, method = "knn",
                   trControl=trctrl,
                   preProcess = c("center", "scale"),
                   tuneLength = 10)
  
 
  test_pred <- predict(knn_fit,newdata = KNN_test)
  
}  

confusionMatrix(test_pred,KNN_test$diagnosis)

  
############Another method to find best K value using classification error with plot 
library(caret)
library(class)


intraining<-createDataPartition(KNN11$diagnosis,p=.7,list = F)
KNN_train<-KNN11[intraining,]
KNN_test<-KNN11[-intraining,]

set.seed(40)
calc_class_err =function(actual,predicted){mean(actual!= predicted)}

#To check how calc_class_err works on k=9

calc_class_err(actual = KNN_test$diagnosis,
         predicted =knn(train = KNN_train[,-31],test = KNN_test[,-31],cl=KNN_train$diagnosis,k=9)
 )


k_to_try <- c()
k_to_try = 1:100
err_k = rep(x=0 , times=length(k_to_try))

for (i in (k_to_try)) 
{
  print(i)
  wbcd_test_pred = knn(train = KNN_train[,-31],test = KNN_test[,-31]
                       ,cl=KNN_train$diagnosis,k=k_to_try[i])

  err_k[i] =calc_class_err(KNN_test$diagnosis,wbcd_test_pred)  
}


#Plot to see the value of k for which classification error is minimum
plot(err_k,type="b",col="blue",xlab = "K",ylab = "Classification error",
     main="K vs classification error")

#Add line for min- error

abline(h=min(err_k),col="red")

min(err_k)

#To see which K gives the minimum error 
which(err_k==min(err_k))

#k=7


####Let's confirm the value of k using bagging and boosting

acc<- c()

for(i in 1:100)
  
{
    for (i in (k_to_try)) 
  {
    print(i)
 
    intraining<-createDataPartition(KNN11$diagnosis,p=.7,list = F)
    KNN_train<-KNN11[intraining,]
    KNN_test<-KNN11[-intraining,]
      
    wbcd_test_pred = knn(train = KNN_train[,-31],test = KNN_test[,-31],cl=KNN_train$diagnosis,k=k_to_try[i])
    
    err_k[i] =calc_class_err(KNN_test$diagnosis,wbcd_test_pred)  
    
  }
  
}

#Plot to see the value of k for which classification error is minimum
plot(err_k,type="b",col="blue",xlab = "K",ylab = "Classification error",
     main="K vs classification error")

#Add line for min- error

abline(h=min(err_k),col="red")

min(err_k)

#To see which K gives the minimum error 
which(err_k==min(err_k))

#Since there are multiple values of k for which error is minimum, we choose the maximim value

max(which(err_k==min(err_k)))

