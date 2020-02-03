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
Toyoto <- read_excel("Toyoto.xlsx")
View(Toyoto)
colnames(Toyoto)

#Remove some unnecessary columns from the dataset
dataset <- subset(Toyoto,select = c(1,3,4,7,9,13,14,15,16,18))

colnames(dataset)
install.packages("deplyr")
#check with Srini for the error

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

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset)

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

#Removing 958

dataset2 <- dataset1[-c(958),]

dim(dataset2)

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset2)

summary(model.Toyota)

#Deletion Diagnostic

influenceIndexPlot(model.Toyota)

#Removing 988

dataset3 <- dataset2[-c(988),]

dim(dataset3)

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset3)

summary(model.Toyota)


#Deletion Diagnostic

influenceIndexPlot(model.Toyota)


#Removing 652

dataset4 <- dataset3[-c(652),]
dim(dataset4)

model.Toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Weight,data=dataset4)

summary(model.Toyota)

#Deletion Diagnostic

influenceIndexPlot(model.Toyota)

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
























































