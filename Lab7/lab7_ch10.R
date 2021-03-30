# Chapter 10
# 171EC146: Sathvik S Prabhu

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
#wordcloud(sms_corpus_train, min.freq = 40, random.order = FALSE)

spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")

#wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
#wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

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

predicted_prob <- predict(sms_classifier, sms_test,type="raw")
head(predicted_prob)

## Chapter 10
sms_results<-read.csv("/home/sathvik/EC8/ML/Lab/Lab7/sms_results.csv")
table(sms_results$actual_type, sms_results$predict_type)
library(gmodels)
CrossTable(sms_results$actual_type, sms_results$predict_type)

library(caret)
#sms_results=data.frame("predict_type"=sms_test_pred,"actual_type"=sms_raw_test$type)

confusionMatrix(sms_results$predict_type,sms_results$actual_type,positive = "spam")

sensitivity(sms_results$predict_type, sms_results$actual_type,
            positive = "spam")
specificity(sms_results$predict_type, sms_results$actual_type,
              negative = "ham")
posPredValue(sms_results$predict_type, sms_results$actual_type,
             positive = "spam")

library(vcd)
Kappa(table(sms_results$actual_type, sms_results$predict_type))

library(irr)
kappa2(sms_results[1:2])

# Visualizing performance tradeoffs
library(ROCR)
pred <- prediction(predictions = sms_results$prob_spam,labels = sms_results$actual_type)

perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve for SMS spam filter",
     col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)

# Estimating future performance
credit <- read.csv("/home/sathvik/EC8/ML/Lab/Lab5/credit.csv")

library(caret)
library(C50)
library(irr)

# Holdout method
random_ids <- order(runif(1000))
credit_train <- credit[random_ids[1:500],]
credit_validate <- credit[random_ids[501:750], ]
credit_test <- credit[random_ids[751:1000], ]

in_train <- createDataPartition(credit$default, p = 0.75,
                                list = FALSE)
credit_train <- credit[in_train, ]
head(credit_train[,1:5])
credit_test <- credit[-in_train, ]
head(credit_test[,1:5])

# Cross Validation
folds <- createFolds(credit$default, k = 10)
str(folds)

credit01_train <- credit[folds$Fold01, ]
credit01_test <- credit[-folds$Fold01, ]

set.seed(123)
folds <- createFolds(credit$default, k = 10)
str(folds)

cv_results <- lapply(folds, function(x) {
  credit_train <- credit[x, ]
  credit_test <- credit[-x, ]
  credit_train$default<-as.factor(credit_train$default)
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})

# kappa statistics
str(cv_results)
mean(unlist(cv_results))
