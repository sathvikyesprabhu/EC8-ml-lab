library(caret)
d <- read.csv("/home/sathvik/EC8/ML/Lab/Lab7/test.csv")
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)
# mtry defines how many features are randomly selected at each split.
grid_rf <- expand.grid(.mtry = 8)
set.seed(300)
m_rf <- train(smoker ~ ., data = d[1:1003,], method = "rf",metric = "Kappa",
              trControl = ctrl, tuneGrid = grid_rf)
m_rf
