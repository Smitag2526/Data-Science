library(rvest)
library(XML)
library(magrittr)

library("syuzhet")
library(lubridate)

library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)

getwd()

setwd("D:/R Excel Sessions/Assignments/Text mining")


aurl <- "https://www.amazon.in/Samsung-Galaxy-Midnight-128GB-Storage/product-reviews/B07HGJKDRR/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"

amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)

write.table(amazon_reviews,"samsung.txt",row.names = F)

getwd()

###Samsung Galaxy M40 reviews

txt= readLines("D:/R Excel Sessions/Assignments/Text mining/samsung.txt")

txt <- iconv(txt, "UTF-8") #Unicode Transformation Format. The '8' means it uses 8-bit blocks to represent a character

x <- get_nrc_sentiment(txt)

View(txt)

View(x)

txt[4]
get_nrc_sentiment('happy')
get_nrc_sentiment('boring')

get_sentiment('boring',method="afinn")
get_sentiment('happy',method="afinn")


example <- get_sentences(txt)

nrc_data <- get_nrc_sentiment(example)

barplot(colSums(nrc_data),
        las=1,
        col=rainbow(10),
        ylab=count,
        main = "Emotion scores")


sentiment_vector<-get_sentiment(example,method="bing")
sentiment_afinn<-get_sentiment(example,method="afinn")
sentiment_nrc<-get_sentiment(example,method="nrc")

sum(sentiment_afinn)
mean(sentiment_afinn)
summary(sentiment_afinn)

windows()
plot(sentiment_vector,type='l',maim='Plot trajectory',xlab='Narative time',ylab='Emotional valence')
abline(h=0,color='red')

plot(
  sentiment_vector, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

##Shape smoothing and normalization using a Fourier based transformation and low pass filtering is achieved using the get_transformed_values function as shown below.
ft_values <- get_transformed_values(
  sentiment_vector, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  padding_factor = 2,
  scale_vals = TRUE,
  scale_range = FALSE
)
plot(
  ft_values, 
  type ="l", 
  main ="Nokia lumia reviews using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

#Most Negative and Positive reviews
negative<-example[which.min(sentiment_vector)]
positive<-example[which.max(sentiment_vector)]


######################################################################################

https://www.excelr.com/extracting-amazon-reviews-using-r

https://github.com/Surya-Murali/Web-Scraping-and-Amazon-Reviews-Analysis/blob/master/AmazonReviewsAnalysis.R


############################################################################


