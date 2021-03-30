# Managing data
x=c(1,2,3)
y=c("Jora","M","Gonda")
save(x,y,file="my_data.Rdata")

remove(x,y)
load("my_data.Rdata")

save.image()

usedcars<- read.csv(file.choose(), stringsAsFactors = F)
str(usedcars) # gives data structure
summary(usedcars)
summary(usedcars$year)
summary(usedcars[c("price","mileage")])
IQR(usedcars$price) #Q3-Q1

usedcars<- read.csv(file.choose(), stringsAsFactors = T)
summary(usedcars)
quantile(usedcars$price,probs = c(0.01,0.99))
quantile(usedcars$price, seq(from=0, to=1, by=0.2))
levels(usedcars$model)
levels(usedcars$color) # levels
factor(usedcars$color) # for each + levels

boxplot(usedcars$price,main="Boxplot for car prices",ylab="Prices($)")
IQR(usedcars$mileage)
hist(usedcars$mileage,main="Hist mileage",xlab="Miles")
hist(usedcars$price,main="Hist for car prices",xlab="Prices($)")
var(usedcars$price)
var(usedcars$mileage)
sd(usedcars$price)
table(usedcars$year) # for categorical variables
table(usedcars$model)
se_p= 78/150 # proportion
se_p

model_table<-table(usedcars$model) # convert to table first
prop.table(model_table)
model_table<-table(usedcars$color) # convert to table first
round(prop.table(model_table)*100)
table(usedcars$color)

plot(x=usedcars$mileage, y=usedcars$price, main="Scatterplot of Price vs Mileage", xlab=" Used car Odometer (Miles)", ylab="Used car price($)")
cor(usedcars$mileage,usedcars$price)

#install gmodels 
usedcars$conservative<-usedcars$color %in% c("Black","Gray","Silver","White")
usedcars$conservative
table(usedcars$conservative)
CrossTable(x=usedcars$model, y=usedcars$conservative)
