library(wordcloud)
library(tm)
library(e1071)
library(gmodels)

cars_raw<-read.csv("/home/sathvik/EC8/ML/Lab/Lab6/2009.csv")
cars<-data.frame("type"=tweets_raw$airline_sentiment, "text"=tweets_raw$text)
str(tweets)