
listings <- read.csv("D:/R Excel Sessions/Assignments/Association Rules/listings.csv")

head(listings)

listings$Price_band <- cut(listings$price,breaks = c(0,1000,3000,5000,7000,10000),labels = c("Less_than_1k","1k-3k","3k-5k","5k-7k",">7k"))

levels(listings$Price_band)
View(listings)

library(arules)
colnames(listings)

listings_new <- listings[,-c(1:5,7:8,10,14:15)]

str(listings)

colnames(listings_new)



#Preparing a band for minimum_nights

listings_new$minimum_nights[listings_new$minimum_nights > 11] <- "More_10_nights"


View(listings_new)


#Factorization

listings_new$minimum_nights <- as.factor(listings_new$minimum_nights)
listings_new$number_of_reviews <- as.factor(listings_new$number_of_reviews)
listings_new$availability_365 <- as.factor((listings_new$availability_365))

str(listings_new)

levels(listings_new$Price_band)
attach(listing)



arules::inspect(rules)
rules.sorted <- sort(rules,by="lift")
arules::inspect(rules.sorted)


rules<-apriori(listings_new,parameter=list(support=.2,confidence=.1))
arules::inspect(rules)
rules<-apriori(listings_new,parameter=list(support=.2,confidence=.1),appearance = list(rhs=c("Price_band=Less_than_1k","Price_band=1k-3k","Price_band=3k-5k","Price_band=5k-7k","Price_band=>7k"),default="lhs"),control=list(verbose=F))
arules::inspect(rules)

