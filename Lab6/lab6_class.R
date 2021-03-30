library(wordcloud)
library(tm)
library(e1071)
library(gmodels)

sms_raw <-read.csv("/home/sathvik/EC8/ML/Lab/Lab6/sms_spam.csv")
str(sms_raw)
sms_raw[1,1]
sms_raw[1,2]
sms_raw[1:3,]

sms_raw$type <- factor(sms_raw$type)
str(sms_raw)                       
table(sms_raw$type)

sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:3])
sms_raw[1:3,2]

corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
inspect(corpus_clean[1:3])

corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
inspect(corpus_clean[1:3])

sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_dtm

# Data preparation: Training and testing sets
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test<- sms_raw[4170:5559, ]
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test<- sms_dtm[4170:5559, ]
sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test<- corpus_clean[4170:5559]

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# Visualizing text data – word clouds
wordcloud(sms_corpus_train, min.freq = 40, random.order = FALSE)

spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

#Data preparation – creating indicator features for frequent words
#findFreqTerms(sms_dtm_train, 5)

Dictionary <- function(x) {
  if( is.character(x) ) {
    return (x)
  }
  stop('x is not a character vector')
}
sms_dict <- Dictionary(findFreqTerms(sms_dtm_train, 5))

sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test<- DocumentTermMatrix(sms_corpus_test,list(dictionary = sms_dict))

convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}

sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test<- apply(sms_test, MARGIN = 2, convert_counts)

# Step 3 – training a model on the data

sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)

# Step 4 – evaluating model performance
sms_test_pred <- predict(sms_classifier, sms_test)

CrossTable(sms_test_pred, sms_raw_test$type,
             prop.chisq = FALSE, prop.t = FALSE,
             dnn = c('predicted', 'actual'))

predict(sms_classifier, "Marvel Mobile Play the official Ultimate Spider-man")

# Test sentence
text= c("Marvel Mobile Play the official Ultimate Spider-man","claim free prize cash")
test_corpus <- Corpus(VectorSource(text))
test_corpus_clean <- tm_map(test_corpus, tolower)
test_corpus_clean <- tm_map(test_corpus_clean, removeNumbers)
test_corpus_clean <- tm_map(test_corpus_clean, removeWords, stopwords())
test_corpus_clean <- tm_map(test_corpus_clean, removePunctuation)
test_corpus_clean <- tm_map(test_corpus_clean, stripWhitespace)
inspect(test_corpus_clean)

test_dtm<- DocumentTermMatrix(test_corpus_clean,list(dictionary = sms_dict))
test<- apply(test_dtm, MARGIN = 2, convert_counts)

predict(sms_classifier, test)


# Step 5 – improving model performance
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type,
                              laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
               prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
               dnn = c('predicted', 'actual'))
