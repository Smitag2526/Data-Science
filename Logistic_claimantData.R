# Logistic Regression

claimants <- read.csv("D:/ExcelR Sessions/Day 4/Claimants.csv")

str(claimants)
logit=glm(ATTORNEY ~ factor(CLMSEX) + factor(CLMINSUR) + factor(SEATBELT) 
          + CLMAGE + LOSS,family= "binomial",data=claimants)
summary(logit)

# Odds Ratio

exp(coef(logit))
# Confusion Matrix Table

prob=predict(logit,type=c("response"),claimants)
prob
confusion<-table(prob>0.5,claimants$ATTORNEY)
confusion

#Accuracy
(380+393)/(380+125+198+393)

# Model Accuracy

Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy
## ROC Curve

#Extract from the fitted model object the vector of fitted probabilities:

install.packages("pROC")
library(pROC)
roccurve <- roc(claimants$ATTORNEY ~ prob)
plot(roccurve)

auc <- auc(claimants$ATTORNEY ~ prob)
auc
