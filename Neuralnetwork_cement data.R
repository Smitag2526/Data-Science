# Load the Concrete data

concrete_data <- read.csv("D:/R Excel Sessions/Day 4/concrete.csv")

head(concrete_data)
library(caret)


# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
##### Neural Networks -------------------# Load the Concrete data# custom normalization functionnormalize <- function(x) {   return((x - min(x)) / (max(x) - min(x)))}

concrete_norm <- as.data.frame(lapply(concrete_data, normalize))

head(concrete_norm)

# create training and test data
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]
## Training a model on the data ----
# train the neuralnet model

install.packages("neuralnet")
library(neuralnet)


# simple ANN with only a single hidden neuron

concrete_model <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                            data = concrete_train)
# visualize the network topology
windows()
plot(concrete_model)

## Evaluating model performance ----
# obtain model results
colnames(concrete_test)
model_results <- compute(concrete_model, concrete_test[1:8])
# obtain predicted strength values
predicted_strength <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)

## Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden =c(5,2))
# plot the network
windows()
plot(concrete_model2)
# evaluate the results as we did before
head(concrete_test)
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result

print(model_results2)
head(concrete_test)
