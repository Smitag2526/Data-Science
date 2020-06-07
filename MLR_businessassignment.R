
#Prepare a prediction model for profit of 50_startups data.
Do transformations for getting better predictions of profit and
make a table containing R^2 value for each prepared model.

R&D Spend -- Research and devolop spend in the past few years
Administration -- spend on administration in the past few years
Marketing Spend -- spend on Marketing in the past few years
State -- states from which data is collected
Profit  -- profit of each state in the past few years

startupdata <- read.csv(file.choose())

head(startupdata)

dim(startupdata)

#Regression model and summary


colnames(startupdata)

str(startupdata)

#Changing text data to numeric
startupdata$State_num <- as.integer(startupdata$State)

str(startupdata)

View(startupdata)

#Scatter plot matrix
pairs(startupdata)

#Correlation Matrix
cor(startupdata1)

model.startup <- lm(Profit~State_num+R.D.Spend+Administration+Marketing.Spend,data = startupdata)
summary(model.startup)

startupdata1 <- startupdata[,-c(4)]

colnames(startupdata1)
startupdata2 <- scale(startupdata1)
startupdata2<- data.frame(startupdata2)


head(startupdata2)

# Build a model with normalized data

model.startup2 <- lm(Profit~State_num+R.D.Spend+Administration+Marketing.Spend,data = startupdata2)
summary(model.startup)

R^2 =0.9507

#Startup Data
Prepare a prediction model for profit of 50_startups data.
Do transformations for getting better predictions of profit and
make a table containing R^2 value for each prepared model.

R&D Spend -- Research and devolop spend in the past few years
Administration -- spend on administration in the past few years
Marketing Spend -- spend on Marketing in the past few years
State -- states from which data is collected
Profit  -- profit of each state in the past few years

#Prediction intervals for new observations

pred<- predict(model.startup,startupdata=data.frame(Profit=78239.91))
pred

##Error computation
pred_E<-predict(model.startup)
Error<-data.frame(startupdata,"Pred"= pred_E,"Error"=startupdata$Profit-pred_E)
Error

library(car)

#MultiCollinearity
vif(model.startup)

#Diagnostic plot: Residual plot,QQplot,Std residual vs fitted
plot(model.startup)

#Residuals vs Regressors

residualPlots(model.startup)

#Added variable plots
avPlots(model.startup)

#QQ plots of standardized residuals

qqPlot(model.startup)

#Deletion Diagnostic

influenceIndexPlot(model.startup)

# Toyota Corolla  

Consider only the below columns and prepare a prediction model for predicting Price.

Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

Toyotadata <- read.csv(file.choose()) 

View(Toyotadata)

colnames(Toyotadata)
dataset <- subset(Toyotadata, select =c(3,4,7,9,13,14,16,17,18))

colnames(dataset)

pairs(dataset)

cor(dataset)

model.toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,dataset)
summary(model.toyota)

library(car)
vif(model.toyota)

plot(model.toyota)

residualPlots(model.toyota)

avPlots(model.toyota)

qqPlot(model.toyota)

influenceIndexPlot(model.toyota)

#Remove outlier
dataset1 <- dataset[-c(222),]

dim(dataset)
dim(dataset1)

model.toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,dataset1)
summary(model.toyota)

influenceIndexPlot(model.toyota)

dataset2 <- dataset1[-c(81),]

model.toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,dataset2)
summary(model.toyota)

influenceIndexPlot(model.toyota)

dataset3 <- dataset2[-c(961),]

dim(dataset3)
View(dataset3)

model.toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,dataset3)
summary(model.toyota)

influenceIndexPlot(model.toyota)

dataset4 <- dataset3[-c(961),]

model.toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,dataset4)
summary(model.toyota)

influenceIndexPlot(model.toyota)

dataset3[961,]
dataset4[961,]

dataset5 <- dataset4[-c(961),]

model.toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,dataset5)
summary(model.toyota)

influenceIndexPlot(model.toyota)

dataset5[961,]

influenceIndexPlot(model.toyota)

dataset6<- dataset5[-c(961),]

model.toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,dataset6)
summary(model.toyota)

influenceIndexPlot(model.toyota)

dataset7<- dataset6[-c(961),]

model.toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,dataset7)
summary(model.toyota)

influenceIndexPlot(model.toyota)

#Comments:- I'm getting multiple outliers for the observation 961. R^2=0.8779, Adj.R^2=0.8772. Age ,KM, HP, CC,Gears , Quarterly Tax, Weight are the significant variables


#Predict Price of the computer

A dataframe containing :
price : price in US dollars of 486 PCs

speed : clock speed in MHz

hd : size of hard drive in MB

ram : size of Ram in in MB

screen : size of screen in inches

cd : is a CD-ROM present ?
  
  multi : is a multimedia kit (speakers, sound card) included ?
  
  premium : is the manufacturer was a "premium" firm (IBM, COMPAQ) ?
  
  ads : number of 486 price listings for each month

trend : time trend indicating month starting from January of 1993 to November of 1995.


computer_data <- read.csv(file.choose())

head(computer_data)
dim(computer_data)

pairs(computer_data)

cor(computer_data)

computer_data$cd_num <- as.numeric(computer_data$cd)

head(computer_data,n=10)

computer_data$multi_num <- as.numeric(computer_data$multi)
computer_data$premium_num <- as.numeric(computer_data$premium)

head(computer_data,n=10)

colnames(computer_data)

computer_dataset <- computer_data[,-c(1,7,8,9)]

head(computer_dataset,n=10)

model.computer <- lm(price~speed+hd+ram+screen+ads+trend+cd_num+multi_num+premium_num,computer_dataset)
summary(model.computer)

dataset_normalized <- scale(computer_dataset)

head(dataset_normalized)
dataset_normalized <-data.frame(dataset_normalized)

model.computer <- lm(price~speed+hd+ram+screen+ads+trend+cd_num+multi_num+premium_num,dataset_normalized)
summary(model.computer)

library(car)
vif(model.computer)

plot(model.computer)

residualPlots(model.computer)

avPlots(model.computer)

qqPlot(model.computer)

influenceIndexPlot(model.computer)

computer_dataset1 <- computer_dataset[-c(1441,1701),]

model.computer <- lm(price~speed+hd+ram+screen+ads+trend+cd_num+multi_num+premium_num,computer_dataset1)
summary(model.computer)

qqPlot(model.computer)

influenceIndexPlot(model.computer)

computer_dataset2 <- computer_dataset1[-c(994),]

model.computer <- lm(price~speed+hd+ram+screen+ads+trend+cd_num+multi_num+premium_num,computer_dataset2)
summary(model.computer)


#Prediction intervals for new observations

pred<- predict(model.computer,computer_dataset2=data.frame(price=1499))
pred
                                                           

##Error computation
pred_E<-predict(model.computer)
Error<-data.frame(computer_dataset2,"Pred"= pred_E,"Error"=computer_dataset2$price-pred_E)
Error

# Comment :- R^2 = 0.7778, Adj R^2= 0.7775.  All variables speed,HD, Ram, Screen , Ads, Trend , cd, multi and premium are significant 

