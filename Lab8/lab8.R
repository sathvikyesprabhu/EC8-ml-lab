# SVM
concrete<-read.csv("/home/sathvik/EC8/ML/Lab/Lab8/concrete.csv")
str(concrete)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm$strength)
summary(concrete$strength)

# Step 3 – training a model on the data
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

library(e1071)
library(kernlab)

model <- ksvm(strength ~ ., data = concrete_train,
                          kernel = "vanilladot")
model

# Step 4 – evaluating model performance
predicted_strength <- predict(model, concrete_test)
cor(predicted_strength, concrete_test$strength)
# 0.71

# ANN

letters <- read.csv("/home/sathvik/EC8/ML/Lab/Lab8/letterdata.csv")
str(letters)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# letters_norm <- as.data.frame(lapply(letters[2:17], normalize))
# summary(letters_norm$xbox)
# summary(letters$xbox)

library(varhandle)
letters$letter <- unfactor(letters$letter)
str(letters)

letters_train <- letters[1:16000, ]
letters_test<- letters[16001:20000, ]

library(neuralnet)
model <- neuralnet(letter ~ . , data = letters_train)
plot(model)

# Step 4 – evaluating model performance
# model_results <- compute(model, letters_test)
# predictions <- model_results$net.result
predictions <- predict(model,letters_test)
head(predictions)
# argmax(predictions[10:20,])
# letters_test$letter[10:20]
table(predictions, letters_test$letter)

agreement <- predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))