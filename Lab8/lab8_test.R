d<-read.csv("/home/sathvik/EC8/ML/Lab/Lab8/Data.csv")
str(d)

library(neuralnet)

m<-neuralnet(X31.25 ~ .,data=d, hidden=2)
plot(m)

d[5,]=c(3.4,0)
model_results <- compute(m, d[5,])
p <- model_results$net.result
p


