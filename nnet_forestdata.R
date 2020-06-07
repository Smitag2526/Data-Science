forestfires_data <- read.csv("D:/R Excel Sessions/Assignments/Neural Networks/forestfires.csv")

head(forestfires_data)
dim(forestfires_data)

table(forestfires_data$month)

table(forestfires_data$month,forestfires_data$size_category)
table(forestfires_data$day,forestfires_data$size_category)

table(forestfires_data$month,forestfires_data$day,forestfires_data$size_category)

table(forestfires_data$FFMC,forestfires_data$size_category)
barplot(table(forestfires_data$FFMC))

table(forestfires_data$DMC,forestfires_data$size_category)
barplot(table(forestfires_data$DMC))

table(forestfires_data$DC,forestfires_data$size_category)
barplot(table(forestfires_data$DC))

table(forestfires_data$ISI,forestfires_data$size_category)
barplot(table(forestfires_data$ISI))

table(forestfires_data$temp,forestfires_data$size_category)
barplot(table(forestfires_data$temp))

table(forestfires_data$RH,forestfires_data$size_category)
barplot(table(forestfires_data$RH))

table(forestfires_data$wind,forestfires_data$size_category)

table(forestfires_data$rain,forestfires_data$size_category)

table(forestfires_data$area,forestfires_data$size_category)

prop.table(table(forestfires_data$size_category))
#EDA shows month,rain,wind, area,temp have a strong correlation with Size category-burnt area of the forest

#Removing columns month and day since the dummy variables are created for the same

forest_newdata <- forestfires_data[,-c(1,2)]
dim(forest_newdata)

library(Hmisc)
describe(forest_newdata)

head(forest_newdata)


##########PCA ############################

colnames(forest_newdata)
head(forest_newdata)

normalize<-function(x){return((x-min(x))/(max(x)-min(x)))}
#create normalize function
forest_scaled<-as.data.frame(lapply(forest_newdata[1:28],normalize ))
head(forest_scaled)

pca <- princomp(forest_scaled,cor=T,scores = T,covmat = NULL)

summary(pca)

####Attach 1st 4 PC scores to the data
pca_data3 <- cbind(pca_data2,pca$scores[,1:4])

head(pca_data3)
colnames(pca_data3)
str(pca_data3)

ggplot(pca_data3,aes(x=Comp.1,y=Comp.2,color=size_category,shape=size_category))+
  geom_hline(yintercept = 0,lty=2)+
  geom_vline(xintercept = 0,lty=2)+
  geom_point(alpha=0.8)

###Using PCA we can see that data seems noisy

#####Feature selection using Random Forest#####################

library(randomForest)

model <- randomForest(forest_newdata$size_category~.,data = forest_newdata,ntree=1000)

importance(model)

###As per variable importance we can remove variables like area,monthapr,monthdec,
###monthjun,monthmay,monthoct
colnames(forest_newdata)


#Simple ANN with single Neuron

######To build ANN algorithm prepare data excl PCA scores######

nnet_data <- pca_data3[,-c(30:33)]
head(nnet_data)


library(caret)
intraininglocal <- createDataPartition(nnet_data$size_category,p=0.7,list = F)

forest_training <- nnet_data[intraininglocal,]
forest_Testing <- nnet_data[-intraininglocal,]

head(forest_training)
str(forest_training)
dim(forest_training)
dim(forest_Testing)

#####Build neuralnet model########################

####Since we have to predict class which is a factor variable we will use nnet()
library(nnet)

str(forest_Testing)

forest_model <- nnet(size_category~.,forest_training,size=1)

colnames(forest_Testing)

###The "compute" function then creates the prediction variable
model_results <- compute(forest_model, forest_Testing[1:28])

#A "results" variable compares the predicted data with the actual data

results <- data.frame(actual=forest_Testing$size_category,prediction=model_results$net.result)

results

#######To check the accuracy on test data################
colnames(forest_Testing)
newprediction1 <- predict(forest_model,type="class",newdata=forest_Testing[,-29])
newprediction1

str(newprediction1)

newprediction1 <- as.factor(newprediction1)

confusionMatrix(newprediction1,forest_Testing$size_category)

#####96% accuracy ########################

#####Model with 2 hidden layers########################
forest_model2 <- nnet(size_category~.,forest_training,size=2)

newprediction2 <- predict(forest_model2,type="class",newdata=forest_Testing[,-29])
newprediction2

newprediction2 <- as.factor(newprediction2)

confusionMatrix(newprediction2,forest_Testing$size_category)

#####Model with 1 hidden layer gives the best accuracy

install.packages("Metrics")
library(Metrics)

totalError <- c()

cv<-10

###Divide train data in 10 equal portions
nrow(forest_training)
cvDivider <- floor(nrow(forest_training)/(cv+1))
cvDivider

datasetIndex <- c((cv*cvDivider):(cv*cvDivider+cvDivider))

dataTest <- forest_training[datasetIndex,]

##Everything else to train
dataTrain <- forest_training[-datasetIndex,]

dim(dataTest)
dim(dataTrain)

###Using bootstrap method to test the accuracy

acc <- c()
for (cv in seq(1:cv))
{
  
  datasetIndex <- c((cv*cvDivider):(cv*cvDivider+cvDivider))
  
  dataTest <- forest_training[datasetIndex,]
  
  ##Everything else to train
  dataTrain <- forest_training[-datasetIndex,]
  
  forest_model <- nnet(size_category~.,dataTrain,size=1,maxit=500,trace=T)
  pred <- predict(forest_model,type="class",newdata=dataTest[,-29])
  
  
  a<- table(dataTest$size_category,pred)
  acc <- c(acc,sum(diag(a))/sum(a))
  
}

summary(acc)

####Mean accuracy is 62% and median accuracy is 92%