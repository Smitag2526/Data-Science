Price
Age
Kilometers
HP
Gears
CC
Doors
QuartTax
Weight


##The code to get correlation and Scatter plot in the same diagram
# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0(r)
  text(0.5, 0.5, txt)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}
pairs(Cars[,1:5], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)

library(car)

library(readxl)

Toyoto <- read_excel(file.choose())
View(Toyoto)
colnames(Toyoto)

#Remove some unnecessary columns from the dataset
dataset <- subset(Toyoto,select = c(1,3,4,7,9,13,14,15,16,17,18))


colnames(dataset)

#Scatter plot matrix
pairs(dataset)

#Correlation Matrix
cor(dataset)

#Regression Model and Summary

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Cylinders+Gears+Weight,data=dataset)

summary(model.Toyota)

# Age,KM , HP,Gears and Weights are significant

library(car)

#MultiCollinearity
vif(model.Toyota)

#Vif is not running because there is a perfect multicollinearity between the variables. To find which variable is linearly dependent use alias function
ld.vars <- attributes(alias(model.Toyota)$Complete)$dimnames[[1]]

ld.vars

#remove the linearly dependent variables and run the m model again


#Regression Model Excl Cylinders 

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=dataset)

summary(model.Toyota)

#MultiCollinearity
vif(model.Toyota)


#Diagnostic plot: Residual plot,QQplot,Std residual vs fitted
plot(model.Toyota)

#Residuals vs Regressors

residualPlots(model.Toyota)

#Added variable plots
avPlots(model.Toyota)

#QQ plots of standardized residuals

qqPlot(model.Toyota)

#Deletion Diagnostic

influenceIndexPlot(model.Toyota)

#Model without cc and doors
model.Toyota <- lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight,data=dataset)

summary(model.Toyota)

#Predictions
pred <- predict(model.Toyota)

final <- data.frame(dataset,"pred"=pred,dataset$Price-pred)

View(final)


#Removing outliers

dataset1 <- dataset[-c(81,222,602),]

dim(dataset)
dim(dataset1)

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset1)

summary(model.Toyota)

#MultiCollinearity
vif(model.Toyota)

#Diagnostic plot: Residual plot,QQplot,Std residual vs fitted
plot(model.Toyota)

#Residuals vs Regressors

residualPlots(model.Toyota)

#Added variable plots
avPlots(model.Toyota)

#QQ plots of standardized residuals

qqPlot(model.Toyota)

#Deletion Diagnostic

influenceIndexPlot(model.Toyota)

#Removing 147,958

#Looking at the qqplot,

dataset1 <- dataset[-c(81,222,602,147,958),]

dim(dataset)
dim(dataset1)

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset1)

summary(model.Toyota)

#Deletion Diagnostic

influenceIndexPlot(model.Toyota)

qqPlot(model.Toyota)

#Removing 146,956 and other outliers from the original dataset
dataset1 <- dataset[-c(81,222,602,147,958,146,956),]

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset1)

summary(model.Toyota)

influenceIndexPlot(model.Toyota)

qqPlot(model.Toyota)

#Removing 145,954
dataset1 <- dataset[-c(81,222,602,147,958,146,956,145,954),]

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset1)

summary(model.Toyota)

influenceIndexPlot(model.Toyota)

qqPlot(model.Toyota)

#Removing 144,952

dataset1 <- dataset[-c(81,222,602,147,958,146,956,145,954,144,952),]

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset1)

summary(model.Toyota)

influenceIndexPlot(model.Toyota)

qqPlot(model.Toyota)

#Removing 143,950
dataset1 <- dataset[-c(81,222,602,147,958,146,956,145,954,144,952,143,950),]

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset1)

summary(model.Toyota)

influenceIndexPlot(model.Toyota)

qqPlot(model.Toyota)

#Removing 142,948
dataset1 <- dataset[-c(81,222,602,147,958,146,956,145,954,144,952,143,950,142,948),]

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset1)

summary(model.Toyota)

influenceIndexPlot(model.Toyota)

qqPlot(model.Toyota)


#Removing 141,946
dataset1 <- dataset[-c(81,222,602,147,958,146,956,145,954,144,952,143,950,142,948,141,946),]

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset1)

summary(model.Toyota)

influenceIndexPlot(model.Toyota)

qqPlot(model.Toyota)


#Removing 140,944
dataset1 <- dataset[-c(81,222,602,147,958,146,956,145,954,144,952,143,950,142,948,141,946,140,944),]

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset1)

summary(model.Toyota)

influenceIndexPlot(model.Toyota)

qqPlot(model.Toyota)


#Removing 139,942
dataset1 <- dataset[-c(81,222,602,147,958,146,956,145,954,144,952,143,950,142,948,141,946,140,944,139,942),]

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset1)

summary(model.Toyota)

influenceIndexPlot(model.Toyota)

qqPlot(model.Toyota)

#Removing 138,940
dataset1 <- dataset[-c(81,222,602,147,958,146,956,145,954,144,952,143,950,142,948,141,946,140,944,139,942,138,940),]


model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset1)

summary(model.Toyota)

influenceIndexPlot(model.Toyota)

qqPlot(model.Toyota)

#Removing 137,938
dataset1 <- dataset[-c(81,222,602,147,958,146,956,145,954,144,952,143,950,142,948,141,946,140,944,139,942,138,940,137,938),]


model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset1)

summary(model.Toyota)

influenceIndexPlot(model.Toyota)

qqPlot(model.Toyota)

#Removing 136,936
dataset1 <- dataset[-c(81,222,602,147,958,146,956,145,954,144,952,143,950,142,948,141,946,140,944,139,942,138,940,137,938,136,936),]


model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset1)

summary(model.Toyota)

influenceIndexPlot(model.Toyota)

qqPlot(model.Toyota)

#Removing 135,934
dataset1 <- dataset[-c(81,222,602,147,958,146,956,145,954,144,952,143,950,142,948,141,946,140,944,139,942,138,940,137,938,136,936,135,934),]


model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset1)

summary(model.Toyota)

influenceIndexPlot(model.Toyota)

qqPlot(model.Toyota)

#Removing 134,932
dataset1 <- dataset[-c(81,222,602,147,958,146,956,145,954,144,952,143,950,142,948,141,946,140,944,139,942,138,940,137,938,136,936,135,934,134,932),]


model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset1)

summary(model.Toyota)

influenceIndexPlot(model.Toyota)

qqPlot(model.Toyota)


#R2=.875, Adj. R2=.8744

colnames(dataset1)
pred <- predict(model.Toyota)

final <- data.frame(dataset1,"pred"=pred,dataset1$Price-pred)

View(final)
