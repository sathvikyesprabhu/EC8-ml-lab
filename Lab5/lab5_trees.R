# install.packages("C50")

library(C50)
library(gmodels)
library(rpart.plot)

# 2 Exploring and preparing the data
credit <- read.csv("/home/sathvik/EC8/ML/Lab/Lab5/credit.csv")
str(credit)

table(credit$Liking)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default) # Default 1:no  2:yes

# Train and test data
set.seed(12345)
credit_rand <- credit[order(runif(1000)), ]
summary(credit$amount) # checking
summary(credit_rand$amount)
head(credit$amount)
head(credit_rand$amount)

credit_train <- credit_rand[1:900, ]
credit_test <- credit_rand[901:1000, ]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# 3 Training a model on the data - C5.0 algorithm

# covert target vector into a factor vector
credit_train$default<-as.factor(credit_train$default)
str(credit_train$default)

credit_model <- C5.0(credit_train[-17], credit_train$default)

credit_model  # 57 decisions deep
summary(credit_model)

# 4 Evaluating model performance
credit_pred <- predict(credit_model, credit_test)

CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
# Accuracy: 75% Error rate:25%

#5 Improving model performance

credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
# Accuracy: 79% Error rate:21%

# The penalties are designated in a cost matrix, which specifies how many times more costly each error is, relative to
# any other. Suppose we believe that a loan default costs the bank four times as much
# as a missed opportunity.
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
# 1:no 2:yes
# row: Predicted column: Actual
error_cost

credit_cost <- C5.0(credit_train[-17], credit_train$default,costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual default', 'predicted default'))

# Rpart
# Step 3 – training a model on the data
credit <- read.csv("/home/sathvik/EC8/ML/Lab/Lab5/Pie_Choice_1.csv")
str(credit)
m.rpart <- rpart(Liking ~ ., data = credit)
m.rpart

# Visualizing decision trees
rpart.plot(m.rpart, digits = 3)

rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,
           type = 3, extra = 101)

# Step 4 – evaluating model performance
p.rpart <- predict(m.rpart, credit_test)
summary(p.rpart)
summary(credit_test$default)

cor(p.rpart, credit_test$default)

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

MAE(p.rpart, credit_test$default)
mean(credit_train$default)
MAE(1.3, credit_test$default)
# our model performs better than mean