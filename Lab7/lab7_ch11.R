# Chapter 11
# 171EC146: Sathvik S Prabhu

library(caret)
library(C50)
library(irr)

# Creating a simple tuned model
credit <- read.csv("/home/sathvik/EC8/ML/Lab/Lab5/credit.csv")
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
