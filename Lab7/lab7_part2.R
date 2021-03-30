# 171EC146
# Sathvik S Prabhu

# Q.Using this data develop a model based on random forest as done in the book by 
# Bret Lanz and report the results with an analysis.
# Also compare the results with that of using the Decision Tree 
# for classification on this data. Classification is the aim here.

set.seed(300)

# Loading the dataset into R
library(readxl)
credit_train_raw<-read_excel("/home/sathvik/EC8/ML/Lab/Lab7/Chapter 12 German Credit Rating.xlsx",sheet=1)
credit_val_raw<-read_excel("/home/sathvik/EC8/ML/Lab/Lab7/Chapter 12 German Credit Rating.xlsx",sheet=2)

# First col is removed as it has the serial no.s
# Last col is removed as it is based on the classification column
# Target: Credit.classification
credit_train<-data.frame(credit_train_raw[2:15])
credit_val<-data.frame(credit_val_raw[2:15])

credit<-rbind(credit_train,credit_val)

# Checking the structure
str(credit)

# Converting columns into factors
col_names<-c(1,3,5,6,7,8,9,11,12,13,14)
credit[col_names] <- lapply(credit[col_names] , factor)

# Checking the structure again
str(credit)
table(credit$Job)

credit_train<-credit[1:800,]
credit_val<-credit[801:1000,]

# Random Forests: bagging with random feature selection

# Selection of model
# Metric: Kappa
library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)

# 1. Random Forest
# mtry defines how many features are randomly selected at each split.
grid_rf <- expand.grid(.mtry = c(2, 4, 8))
m_rf <- train(Credit.classification ~ ., data = credit_train, method = "rf",metric = "Kappa",
              trControl = ctrl, tuneGrid = grid_rf)

# 2. Decision Tree
grid_c50 <- expand.grid(.model = "tree",
                        .trials = c(10, 20, 30),
                        .winnow = "FALSE")
m_c50 <- train(Credit.classification ~ ., data = credit_train, method = "C5.0",
               metric = "Kappa", trControl = ctrl, tuneGrid = grid_c50)

# Comparing RF and C50
m_rf # Best: mtry=8
m_c50 # Best: trials=20

library(randomForest)
# Best model: random forest with mtry=8 which gives the highest kappa
m1<-randomForest(Credit.classification ~ ., data = credit_train, mtry=8)
m1

library(C50)
m2<-C5.0(Credit.classification ~ ., data = credit_train, trials=20)
m2

# Evauluation
# Metric: Kappa
for( i in col_names){
  levels(credit_val[[i]]) <- levels(credit_train[[i]])
}
pred1<-predict(m1,credit_val)
confusionMatrix(pred1,credit_val$Credit.classification)
# Kappa: 0.33

pred2<-predict(m2,credit_val)
confusionMatrix(pred2,credit_val$Credit.classification)
# Kappa: 0.30
# Best model upon evaluation: random forest with mtry=8, which gives the highest kappa (0.33)