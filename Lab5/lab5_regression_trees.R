library(rpart)
library(rpart.plot)

# Step 2 – exploring and preparing the data
wine <- read.csv("/home/sathvik/EC8/ML/Lab/Lab5/whitewines.csv")
str(wine)

hist(wine$quality)
table(wine$quality)

# already randomized
# 75:25
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

# Step 3 – training a model on the data
m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart

# Visualizing decision trees
rpart.plot(m.rpart, digits = 3)

rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,
           type = 3, extra = 101)

# Step 4 – evaluating model performance
p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)

cor(p.rpart, wine_test$quality)

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

MAE(p.rpart, wine_test$quality)
mean(wine_train$quality)
MAE(5.87, wine_test$quality)

# Step 5 – improving model performance
# M5' or M5Prime  algo
# error