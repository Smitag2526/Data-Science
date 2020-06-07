#Forecast the Airlines Passengers data set. Prepare a document for each model explaining 
how many dummy variables you have created and RMSE value for each model. Finally which model you will use for 


library(readxl)
airlines <- read_excel(file.choose())

head(airlines)
airlines <-as.data.frame(airlines)
head(airlines)

plot(airlines$Passengers,type="l")

View(airlines)

dim(airlines)

# So creating 11 dummy variables 

X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
View(X)

colnames(X)<-month.abb # Assigning month names
trakdata<-cbind(airlines,X)

View(trakdata)

trakdata["t"] <- 1:96
trakdata["log_passengers"] <- log(trakdata["Passengers"])
trakdata["t_square"]<- trakdata["t"]*trakdata["t"]

colnames(trakdata)
##Data Partition
train <- trakdata[1:80,]
test<-trakdata[81:96,]

#Linear model
linear_model <- lm(Passengers~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear

###Exponential
expo_model <- lm(log_passengers~t,data=train)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata =test))
View(expo_pred)
rmse_expo<-sqrt(mean((test$Passengers-expo_pred$fit)^2,na.rm = T))
rmse_expo

###Quadratic#####

Quad_model <- lm(Passengers~t+t_square,data=train)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata =test))
View(Quad_pred)
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm = T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
Sea_add_pred<-data.frame(predict(sea_add_model,interval='predict',newdata =test))
View(Sea_add_pred)
rmse_Sea_add<-sqrt(mean((test$Passengers-Sea_add_pred$fit)^2,na.rm = T))
rmse_Sea_add

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

table_rmse<-data.frame('Model'=c("rmse_linear","rmse_expo","rmse_Quad","rmse_Sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),'RMSE'=c(rmse_linear,rmse_expo,rmse_Quad,rmse_Sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)


# Use entire data : Additive seasonality has least RMSE value
new_model <- lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=trakdata)


library(forecast)
library(fpp)
library(smooth)
library(tseries)

#######Using Halt-winters 

# Converting data into time series object
airline_ts<-ts(airlines$Passengers,frequency = 12,start=c(95))
View(airline_ts)
length(airline_ts)
plot(airline_ts)

# dividing entire data into training and testing data 
train<-airline_ts[1:80]
test<-airline_ts[85:96] 

# seasonal data

# converting time series object
train<-ts(train,frequency = 12)
test<-ts(test,frequency = 12)

# Plotting time series data
plot(train) # Visualization shows that it has level, trend, seasonality => Additive seasonality


# Assuming time series data has only level parameter
hw_a<-HoltWinters(train,alpha = 0.6,beta = F,gamma = F)
hwa_pred<-data.frame(predict(hw_a,n.ahead=12))
hwa_pred
# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(hw_a,h=12))
hwa_mape<-MAPE(hwa_pred$fit,test)*100


# Assuming time series data has level and trend parameter 
hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)

hwab_pred<-data.frame(predict(hw_ab,n.ahead = 12))
# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(hw_ab,h=12))
hwab_mape<-MAPE(hwab_pred$fit,test)*100


# with alpha = 0.2, beta = 0.1, gamma = 0.1 
# Assuming time series data has level,trend and seasonality 
hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)

hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 12))
# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(hw_abg,h=12))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100

# With out optimum values 
hw_na<-HoltWinters(train,beta = F,gamma = F)
hwna_pred<-data.frame(predict(hw_na,n.ahead = 12))
hwna_pred
plot(forecast(hw_na,h=12))
hwna_mape<-MAPE(hwna_pred$fit,test)*100

hw_nab<-HoltWinters(train,gamma=F)
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=12))
hwnab_pred
plot(forecast(hw_nab,h=12))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100

hw_nabg<-HoltWinters(train)
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =12))
hwnabg_pred
plot(forecast(hw_nabg,h=12))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100

df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))
colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)

#####Conclusion############################

hwab_mape gives the lowest MAPE value so the model with level and trend with no seasonality can be considered as the best model


###################################################################################################

#Forecast the CocaCola prices data set. 

library(readxl)
Cocacola <- read_excel(file.choose())
head(Cocacola)
Cocacola <- as.data.frame(Cocacola)
dim(Cocacola)

plot(Cocacola$Sales,type="l")


# Converting data into time series object
sales<-ts(Cocacola$Sales,frequency = 4,start=c(86))
View(sales)
plot(sales)

# dividing entire data into training and testing data 
train<-sales[1:38]
test<-sales[39:42] # Considering only 4 Quarters of data for testing because data itself is Quarterly

# converting time series object
train<-ts(train,frequency = 4)
test<-ts(test,frequency = 4)

# Plotting time series data
plot(train) # Visualization shows that it has level, trend, seasonality => Additive seasonality


library(forecast)
library(fpp)
library(smooth)
library(tseries)

#######Using Halt-winters 

# Assuming time series data has only level parameter
hw_a<-HoltWinters(train,alpha = 0.6,beta = F,gamma = F)
hwa_pred<-data.frame(predict(hw_a,n.ahead=4))
hwa_pred
# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(hw_a,h=4))
hwa_mape<-MAPE(hwa_pred$fit,test)*100

# Assuming time series data has level and trend parameter 

hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)

hwab_pred<-data.frame(predict(hw_ab,n.ahead = 4))
# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(hw_ab,h=4))
hwab_mape<-MAPE(hwab_pred$fit,test)*100


# with alpha = 0.2, beta = 0.1, gamma = 0.1 
# Assuming time series data has level,trend and seasonality 
hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)

hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 4))
# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(hw_abg,h=4))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100

# With out optimum values 
hw_na<-HoltWinters(train,beta = F,gamma = F)
hwna_pred<-data.frame(predict(hw_na,n.ahead = 4))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape<-MAPE(hwna_pred$fit,test)*100

hw_nab<-HoltWinters(train,gamma=F)
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=4))
hwnab_pred
plot(forecast(hw_nab,h=4))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100

hw_nabg<-HoltWinters(train)
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =4))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100

df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))
colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)

# Based on the MAPE value who choose holts winter exponential tecnique which assumes the time series
# Data level, trend, seasonality characters with default values of alpha, beta and gamma



#####Using ARIMA Model################################################

library(readxl)
Cocacola <- read_excel(file.choose())
head(Cocacola)
Cocacola <- as.data.frame(Cocacola)
dim(Cocacola)

plot(Cocacola$Sales,type="l")


# Converting data into time series object
sales<-ts(Cocacola$Sales,frequency = 4,start=c(86))
View(sales)
plot(sales)

# dividing entire data into training and testing data 
train<-sales[1:38]
test<-sales[39:42] # Considering only 4 Quarters of data for testing because data itself is Quarterly

# converting time series object
train<-ts(train,frequency = 4)
test<-ts(test,frequency = 4)

# Plotting time series data
plot(train) # Visualization shows that it has level, trend, seasonality => Additive seasonality
acf(train)
pacf(train)

library(forecast)
library(fpp)
library(smooth)
library(tseries)

ndiffs(sales)
####ndiffs gives that 1st difference time series is stationary

a<-arima(train,order=c(1,1,8),method="ML")

model_AA <- auto.arima(train)
model_AA

######Conclusion :
#####Best Model is ARIMA (0,1,0)(0,1,0)[4] which is no trend , no seasonality

