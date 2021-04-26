# 171EC146 Sathvik S Prabhu

## Chapter 10
sms_results<-read.csv("/home/sathvik/EC8/ML/Lab/Lab7/sms_results.csv")
table(sms_results$actual_type, sms_results$predict_type)
library(gmodels)
CrossTable(sms_results$actual_type, sms_results$predict_type)

library(caret)
sms_results$predict_type<-as.factor(sms_results$predict_type)
sms_results$actual_type<-as.factor(sms_results$actual_type)
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
credit <- read.csv("/home/sathvik/EC8/ML/Lab/Lab5/credit.csv", stringsAsFactors= T)
str(credit)

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

## Chapter 11

# Creating a simple tuned model
credit <- read.csv("/home/sathvik/EC8/ML/Lab/Lab5/credit.csv", stringsAsFactors= T)
set.seed(300)
credit$default<-as.factor(credit$default)
m <- train(default ~ ., data = credit, method = "C5.0")
m

p <- predict(m, credit)
table(p, credit$default)

head(predict(m, credit))
head(predict(m, credit, type = "prob"))

# Customizing the tuning process
# . The oneSE function
# chooses the simplest candidate within one standard error of the best performance,
# and tolerance uses the simplest candidate within a user-specified percentage.
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "oneSE")
grid <- expand.grid(.model = "tree",
                    .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    .winnow = "FALSE")
grid

set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0",metric = "Kappa",
           trControl = ctrl,tuneGrid = grid)
m

# Bagging (Bootstrap aggregating)
library(ipred)
set.seed(300)
mybag <- bagging(default ~ ., data = credit, nbagg = 25) # 25 decision trees
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag",
      trControl = ctrl)

str(svmBag)
svmBag$fit

library(kernlab)
credit$default<-as.factor(credit$default)
bagctrl <- bagControl(fit = svmBag$fit,
                      predict = svmBag$pred,
                      aggregate = svmBag$aggregate)
set.seed(300)
# svmbag <- train(default ~ ., data = credit, "bag",
#                   trControl = ctrl, bagControl = bagctrl)
# svmbag

# Boosting

# Random Forests
# bagging with random featureselection

# Training
library(randomForest)
set.seed(300)
credit$default<-as.factor(credit$default)
rf <- randomForest(default ~ ., data = credit)
rf

# Evaluation
library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)
# mtry defines how many features are randomly selected at each split.
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))
set.seed(300)
m_rf <- train(default ~ ., data = credit, method = "rf",metric = "Kappa",
              trControl = ctrl, tuneGrid = grid_rf)

grid_c50 <- expand.grid(.model = "tree",
                        .trials = c(10, 20, 30, 40),
                        .winnow = "FALSE")
set.seed(300)
m_c50 <- train(default ~ ., data = credit, method = "C5.0",
               metric = "Kappa", trControl = ctrl,
               tuneGrid = grid_c50)
# Comparing RF and C50
m_rf
m_c50

# Chapter 12

library(RCurl)
webpage <- getURL("https://www.packtpub.com/")
str(webpage)

library(rjson)
sample_json <- '{"breakfast" : [ "milk", "fruit loops", "juice" ],"lunch" : [ "left over sushi" ]}'

r_object <- fromJSON(sample_json)
# #To convert from an R object to a JSON object:
json_string <- toJSON(r_object)

system.time(rnorm(1000000))
