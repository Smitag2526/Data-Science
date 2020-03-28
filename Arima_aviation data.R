aviation <- read.csv("D:/ExcelR Sessions/Day 4/Aviation.csv")

head(aviation)
# Converting data into time series object
amts<-ts(aviation$Sales,frequency = 4,start=c(86))
View(amts)
plot(amts)

# dividing entire data into training and testing data 
train<-amts[1:38]
test<-amts[39:42] # Considering only 4 Quarters of data for testing because data itself is Quarterly
# seasonal data
# converting time series object
train<-ts(train,frequency = 4)
test<-ts(test,frequency = 4)
plot(train)
acf(train)
pacf(train)
a<-arima(train,order=c(1,1,8),method="ML")
# Auto.Arima model on the price agg data 
library(forecast)
model_AA <- auto.arima(train)
model_AA
pred_AA <- data.frame(forecast(model_AA))
acf(model_AA$residuals)
pacf(model_AA$residuals)
####There are no significant information in the residuals that means the model we have built is good

windows()
plot(forecast(model_AA,h=12),xaxt="n")
