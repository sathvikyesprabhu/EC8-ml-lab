
launch<- read.csv("/home/sathvik/EC8/ML/Lab/Lab4/challenger.csv")

# y=a+bx
b <- cov(launch$temperature, launch$distress_ct) /var(launch$temperature)
b
a <- mean(launch$distress_ct) - b * mean(launch$temperature)
a

cor(launch$temperature, launch$distress_ct)

# regression function
reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  solve(t(x) %*% x) %*% t(x) %*% y
}

str(launch)

reg(y = launch$distress_ct, x = launch[3])
reg(y = launch$distress_ct, x = launch[3:5])

# Step 1 – collecting data
# Step 2 – exploring and preparing the data
insurance <- read.csv("/home/sathvik/EC8/ML/Lab/Lab4/insurance.csv", stringsAsFactors = TRUE)
str(insurance)
summary(insurance$charges)
hist(insurance$charges)
table(insurance$region)
cor(insurance[c("age", "bmi", "children", "charges")])

# scatterplot matrices
pairs(insurance[c("age", "bmi", "children", "charges")])

library(psych)
# Enhanced scatterplot matrix
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

# Step 3 – training a model on the data

ins_model <- lm(charges ~ age + children + bmi + sex +smoker + region, data = insurance)
# same as the above 
ins_model <- lm(charges ~ ., data = insurance)
ins_model

# Step 4 – evaluating model performance
summary(ins_model)

# Step 5 – improving model performance
# Model specification – adding non-linear relationships
insurance$age2 <- insurance$age^2
# Transformation – converting a numeric variable to a binary indicator
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
# Model specification – adding interaction effects

# An improved model
# Added a non-linear term for age
# Created an indicator for obesity
# Specified an interaction between obesity and smoking
ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex +bmi30*smoker + region, data = insurance)
summary(ins_model2)

data("warpbreaks")
ins_model <- lm(breaks ~ wool + tension, data = warpbreaks)
summary(ins_model)

insurance <- read.csv("/home/sathvik/EC8/ML/Lab/Lab4/Test_04_DataFile.csv", stringsAsFactors = TRUE)
mean(insurance$expenses)
IQR(insurance$expenses)

hist(insurance$expenses)
hist(insurance$age)
hist(insurance$bmi)
hist(insurance$children)

cor(insurance$bmi,insurance$expenses)
ins_model<-lm(expenses ~ age + children + bmi + sex +smoker + region, data=insurance)
ins_model
summary(ins_model)
