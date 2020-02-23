#Load Titanic data

Titanic <- read.csv(file.choose())

head(Titanic)

install.packages("arules")
library(arules)

Titanic <- Titanic[,-c(1)]

colnames(Titanic)

rules <- apriori(Titanic)
arules::inspect(rules)
rules.sorted <- sort(rules,by="lift")
arules::inspect(rules.sorted)

#rules with rhs containing "survived " only

rules <- apriori(Titanic,parameter = list(minlen=1,supp=0.1,conf=0.5)
                 ,appearance = list(rhs=c("Survived=No","Survived=Yes")
                 ),control = list(verbose=F))


arules::inspect(rules)

