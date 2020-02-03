update.packages(checkBuilt = TRUE)
install.packages("car")
library(car)

install.packages("Rcpp")
library("Rcpp")

cars <- read.csv(file.choose())
dataset <- cars
head(dataset)

#Scatter plot matrix

pairs(dataset)

#Correlation Matrix

cor(dataset)

#Regression Model and Summary

model.car <- lm(MPG~HP+VOL+SP+WT,data=dataset)
summary(model.car)

#MultiCollinearity

vif(model.car)

#Diagnostic plot: Residual plot,QQplot,Std residual vs fitted

plot(model.car)

#Residuals vs Regressors

residualPlots(model.car)

#Added variable plots

avPlots(model.car)

#QQ plots of standardized residuals

qqPlot(model.car)

#Deletion Diagnostic

influenceIndexPlot(model.car)

View(dataset)

#Remove outlier 77th Observation
dataset <- dataset[-c(77),]

View(dataset)
# Run all steps performed earlier again

#Scatter plot matrix

pairs(dataset)

#Correlation Matrix

cor(dataset)

#Regression Model and Summary

model.car <- lm(MPG~HP+VOL+SP+WT,data=dataset)
summary(model.car)

#MultiCollinearity

vif(model.car)

#Diagnostic plot: Residual plot,QQplot,Std residual vs fitted

plot(model.car)


#Residuals vs Regressors

residualPlots(model.car)

#Added variable plots

avPlots(model.car)

#QQ plots of standardized residuals

qqPlot(model.car)

#Deletion Diagnostic

influenceIndexPlot(model.car)

# Drop WT because VIF is high for the same and build model again

#Regression Model and Summary

model.car <- lm(MPG~HP+VOL+SP,data=dataset)
summary(model.car)

#MultiCollinearity

vif(model.car)

#Diagnostic plot: Residual plot,QQplot,Std residual vs fitted

plot(model.car)


#Residuals vs Regressors

residualPlots(model.car)

#Added variable plots

avPlots(model.car)

#QQ plots of standardized residuals

qqPlot(model.car)

#Deletion Diagnostic

influenceIndexPlot(model.car)

# Since 79th is the outlier remove 79th observation and build the model again

#Remove outlier 79th Observation

dataset1 <- cars[-c(77,79),]

View(cars)
dim(cars)
View(dataset)
dim(dataset)
# Run all steps performed earlier again

#Regression Model and Summary

model1.car <- lm(MPG~HP+VOL+SP,data=dataset1)
summary(model1.car)



#Deletion Diagnostic

influenceIndexPlot(model1.car)

#80th Obervation is again outlier
dataset2 <- cars[-c(77,79,80),]

#Regression Model and Summary

model2.car <- lm(MPG~HP+VOL+SP,data=dataset2)
summary(model2.car)


#MultiCollinearity

vif(model2.car)

#Diagnostic plot: Residual plot,QQplot,Std residual vs fitted

plot(model.car)


#Residuals vs Regressors

residualPlots(model.car)

#Added variable plots

avPlots(model.car)

#QQ plots of standardized residuals

qqPlot(model.car)


#Deletion Diagnostic

influenceIndexPlot(model2.car)

