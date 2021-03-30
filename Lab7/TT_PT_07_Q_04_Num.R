
library(randomForest)
d <- read.csv("/home/sathvik/EC8/ML/Lab/Lab7/test.csv")
set.seed(300)
library(caret)
0.75*length(d$expenses)
d[1003,]
rf <- randomForest(smoker ~ ., data = d[1:1003,])
rf

