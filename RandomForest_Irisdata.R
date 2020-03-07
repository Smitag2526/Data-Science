
library(caret)
install.packages("randomForest")
library(randomForest)

datasets::iris

model <- randomForest(iris$Species~.,data = iris,ntree=1000)

#View the forest results

print(model)

#Prediction

pred <- predict(model,iris[,-5])

table(pred,iris$Species)
