install.packages("survival")
library(survival)
survival_unemployment1<-read.csv("D:/ExcelR Sessions/Day 4/survival_unemployment1.csv")

attach(survival_unemployment1)

# Define variables 
time <- spell
event <- event
X <- cbind(logwage, ui, age)
group <- ui
kmsurvival <- survfit(Surv(time,event) ~ 1)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")
kmsurvival1 <- survfit(Surv(time, event) ~ group)
plot(kmsurvival1, xlab="Time", ylab="Survival Probability")

