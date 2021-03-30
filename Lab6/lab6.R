# 171EC146
# Sathvik S Prabhu

# Dataset: Twitter Airline Sentiment

library(wordcloud)
library(tm)
library(e1071)
library(gmodels)

tweets_raw<-read.csv("/home/sathvik/EC8/ML/Lab/Lab6/Tweets.csv")
tweets<-data.frame("type"=tweets_raw$airline_sentiment, "text"=tweets_raw$text)
str(tweets)

tweets[1,]
table(tweets$type)
tweets[1:3,]

tweets_corpus <- Corpus(VectorSource(tweets$text))
print(tweets_corpus)
inspect(tweets_corpus[1:3])
tweets[1:3,]

corpus_clean <- tm_map(tweets_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
inspect(corpus_clean[1:3])

corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
inspect(corpus_clean[1:3])

tweets_dtm <- DocumentTermMatrix(corpus_clean)
tweets_dtm

# Data preparation: Training and testing sets
# size=14640
# about 80:20 ratio
tweets_raw_train <- tweets[1:12000, ]
tweets_raw_test<- tweets[12001:14640, ]
tweets_dtm_train <- tweets_dtm[1:12000, ]
tweets_dtm_test<- tweets_dtm[12001:14640, ]
tweets_corpus_train <- corpus_clean[1:12000]
tweets_corpus_test<- corpus_clean[12001:14640]

prop.table(table(tweets_raw_train$type))
prop.table(table(tweets_raw_test$type))

# Visualizing text data – word clouds
wordcloud(tweets_corpus_train, min.freq = 40, random.order = FALSE)

negative <- subset(tweets_raw_train, type == "negative")
neutral <- subset(tweets_raw_train, type == "neutral")
positive <- subset(tweets_raw_train, type == "positive")

wordcloud(negative$text, max.words = 40,scale = c(3, 0.5))
wordcloud(neutral$text, max.words = 40, scale = c(3, 0.5))
wordcloud(positive$text, max.words = 40, scale = c(3, 0.5))

#Data preparation – creating indicator features for frequent words
# findFreqTerms(tweets_dtm_train, 5)

Dictionary <- function(x) {
  if( is.character(x) ) {
    return (x)
  }
  stop('x is not a character vector')
}
tweets_dict <- Dictionary(findFreqTerms(tweets_dtm_train, 5))
# 2449 fequent words

tweets_train <- DocumentTermMatrix(tweets_corpus_train, list(dictionary = tweets_dict))
tweets_test<- DocumentTermMatrix(tweets_corpus_test,list(dictionary = tweets_dict))

convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}

tweets_train <- apply(tweets_train, MARGIN = 2, convert_counts)
tweets_test<- apply(tweets_test, MARGIN = 2, convert_counts)

# Step 3 – training a model on the data
tweets_classifier <- naiveBayes(tweets_train, tweets_raw_train$type)

# Step 4 – evaluating model performance
tweets_test_pred <- predict(tweets_classifier, tweets_test)
CrossTable(tweets_test_pred, tweets_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
# Accuracy: 79.89%

# Step 5 – improving model performance
# Using the laplace smoothing parameter.
tweets_classifier2 <- naiveBayes(tweets_train, tweets_raw_train$type, laplace = 1)
tweets_test_pred2 <- predict(tweets_classifier2, tweets_test)
CrossTable(tweets_test_pred2, tweets_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
# Accuracy: 81.17%. Up by 1.3%