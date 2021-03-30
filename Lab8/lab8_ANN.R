# Step 2 – exploring and preparing the data

concrete<-read.csv("/home/sathvik/EC8/ML/Lab/Lab8/concrete.csv")
str(concrete)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# denormalize <- function(x) {
#   return(x*(max(x) - min(x))+min(x))
# }

concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm$strength)
summary(concrete$strength)

# Step 3 – training a model on the data
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

library(neuralnet)
concrete_model <- neuralnet(strength ~ cement + slag +
                              ash + water + superplastic +
                              coarseagg + fineagg + age,
                            data = concrete_train)

plot(concrete_model)

# Step 4 – evaluating model performance
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
# p1<- predict(concrete_model,concrete_test[1:8])
# predicted_strength==p1
p1<-predict(concrete_model,concrete_test[10,1:8])
p1
# p1*range(as.vector(concrete[9])) + min(concrete[9])
cor(predicted_strength, concrete_test$strength)

# Step 5 – improving model performance
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic +
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)
plot(concrete_model2)
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

library(neuralnet)
for (x in t){
set.seed(x)
d<-read.csv("/home/sathvik/EC8/ML/Lab/Lab8/Data.csv")
str(d)
# d_norm <- as.data.frame(lapply(d, normalize))
m<-neuralnet(X31.25 ~ .,data=d, hidden=2)
plot(m)
}
d[5,]=c(3.4,0)
model_results <- compute(m, d[5,])
p <- model_results$net.result
p

m$weights[[1]][[2]][2,]
