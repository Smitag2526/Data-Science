pnorm(50,60,10)
1-0.15

pnorm(680,711,29)

#What is p(697<=X<-740)

#p(X<=740)
pnorm(740,711,29)

#P(X<=697)
pnorm(697,711,29)

#p(697<=X<-740)
0.84-0.31
211*29*1.960

211.29*1.64

1990+346.51

1990-346.51

#[1643.49,2336.51]

qnorm(0.95)
pt(-1.1786,49)

2*0.1221

pt(.2357,49)
1-0.5926
0.4074*2

#One sample one tail t test
H0:<=0.3
Ha:>0.3

Hence we will need to include the options alternative="greater",mu=0.3.

x<- c(0.593,0.142,0.329,0.691,0.231,0.793,0.519,0.392,0.418)

y<- c(0.593,0.142,0.329,0.691,0.231,0.793,0.519,0.392,0.418)


stats::t.test(x,alterative="greater",mu=0.3)
1-pt(2.20,8)

#Two Sample 2 tailed test

Control <- c( 91, 87, 99, 77, 88, 91) 

Treat <- c( 101, 110, 103, 93, 99, 104)


t.test(Control,Treat,alternative="two.sided")

#ANOVA

pain <- c(4, 5, 4, 3, 2, 4, 3, 4, 4, 6, 8, 4, 5, 4, 6, 5, 8, 6, 6, 7, 6, 6, 7, 5, 6, 5, 5 )


drug <- c(rep("A"),rep("B"),rep("C"))

migraine <- data.frame(pain,drug)

results <- aov(pain~drug,data=migraine)
summary(results)

boxplot(pain~drug,migraine)

# Regression equation

Newspaperdata <- read.csv(file.choose())

head(Newspaperdata)

reg_model <- lm(data=Newspaperdata[,-1],sunday~daily)

summary(reg_model)

daily<-250

#Prediction 

sunday = 13.83+(1.33*daily)
sunday <- 13.83+(1.33*250)

our daily circulaiton is 250 k, We can expect 346.33 sunday editions to be launched 

#Prediction using command

sunday <- predict(reg_model,newdata = data.frame(daily))
                  

sunday




# To find Error

model <- lm(sunday~daily,Newspaperdata)
summary(model)

pred <- predict(model)

final <- data.frame(Newspaperdata,"pred"=pred,Newspaperdata$sunday-pred)

final


wcat_data <- read.csv(file.choose())

head(wcat_data)
dim(wcat_data)

#Lattice package is for dot plot
install.packages("lattice")

library(lattice)

#Prediction for new values using WCAT data file
attach(wcat_data)
dotplot(AT,main="Dot plot for AT prediction", col="dodgerblue4" )
dotplot(Waist,main="Dot plot for WC", col="dodgerblue4")
boxplot(AT,col="dodgerblue4")
boxplot(Waist,col="dodgerblue4")

#Regression model and it's summary

reg.model <- lm(AT~Waist,wcat_data)

summary(reg.model)

#Prediction intervals for new observations

pred<- predict(reg.model,newdata=data.frame(Waist=76.85))
pred

pred<- predict(reg.model,newdata=data.frame(Waist=c(76.85,82)))
pred

##Error computation
pred_E<-predict(reg.model)
Error<-data.frame(wcat_data,"Pred"= pred_E,"Error"=wcat_data$AT-pred_E)
Error


#Build model on Cars data

cars <- read.csv(file.choose())

head(cars)
dim(cars)

#Scatter plot matrix

pairs(cars)

#Correlation Matrix

cor(cars)


#Regression model and summary

model.car <- lm(MPG~.,data=cars)

summary(model.car)
