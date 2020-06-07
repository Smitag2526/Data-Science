
library("recommenderlab")
library(caTools)

#movie rating data
book_rate_data <- read.csv('D:/R Excel Sessions/Assignments/Recommendation Engine/Book11.csv')

#metadata about the variable
str(book_rate_data)
colnames(book_rate_data)

dataset <- subset(book_rate_data,select = c(2,3,4))

head(dataset)
colnames(dataset)


#rating distribution
hist(dataset$Book.Rating)

#the datatype should be realRatingMatrix inorder to build recommendation engine
book_rate_data_matrix <- as(dataset, 'realRatingMatrix')

#Popularity based 

book_recomm_model1 <- Recommender(book_rate_data_matrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(book_recomm_model1, book_rate_data_matrix["276737",], n=5)
as(recommended_items1, "list")



####Using UBCF (User based collaborative filtering)

Rec.model<-Recommender(book_rate_data_matrix, method = "UBCF")

##This model computes internally the cosine similarity between all users represented as vectors, which in R is as simple as:
##crossprod(a,b)/sqrt(crossprod(a)*crossprod(b))


dataset2 <- as(dataset,"transactions")
dataset3 <- as(dataset2,"binaryratingmatrix")


Rec.model= Recommender(dataset,method="UBCF", 
                      param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))

