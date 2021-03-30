# Name: Sathvik S Prabhu
# Roll. No.: 171EC146

## Chapter 1

dieroll <- c(2,5,1,6,5,5,4,1)
dieroll
ls()

newdieroll <- dieroll/2
newdieroll
ls()

rm(newdieroll)
ls()

help(log)

log(100) # natural log
log2(16)
log(1000,base=10)

log2(c(1,2,3,4))
apropos("norm")

a <- c(1,2,3,4,5,6,7,8)
A <- matrix(a,nrow=2,ncol=4, byrow=FALSE)
A

# Exercises

#1
help(mean)
help(median)
#2
apropos("test")
#3
info <- c(21,181,8216341022)
info
#4
Ident <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)
Ident
#5
#Saved

## Chapter 2

# Basic math
2+3
3/2
2^3 # this also can be written as 2**3
4^2-3*2
(56-14)/6 - 4*7*10/(5^2-5) # this is more complicated

sqrt(2)
abs(2-4)
cos(4*pi)
log(0)
factorial(6)
choose(52,5) # 52C5

# Vector Arithmetic
x <- c(1,2,3,4)
y <- c(5,6,7,8)
x*y
y/x
y-x
x^y
cos(x*pi) + cos(y*pi)

s <- c(1,1,3,4,7,11)
length(s)
sum(s) # 1+1+3+4+7+11
prod(s) # 1*1*3*4*7*11
cumsum(s)
sort(s)
diff(s) # 1-1, 3-1, 4-3, 7-4, 11-7
diff(s, lag = 2) # 3-1, 4-1, 7-3, 11-4

# Matrix Operations
a <- c(1,2,3,4,5,6,7,8,9,10)
A <- matrix(a, nrow = 5, ncol = 2) # fill in by column
A
B <- matrix(a, nrow = 5, ncol = 2, byrow = TRUE) # fill in by row
B
C <- matrix(a, nrow = 2, ncol = 5, byrow = TRUE)
C

dim(C)
t(C) # this is the same as A
B%*%C

D <- C%*%B
D
det(D)
solve(D)
eigen(D) # gives eigen values and their associated eigen vectors

# Exercises
#1
abs(2**3 - 3**2)

#2
exp(1)^exp(1)

#3
(2.3)^8 + log(0.75) - cos(pi/sqrt(2))

#4
A=matrix(c(1,2,3,2,2,1,6,4,4,7,2,5),nrow=3,ncol=4,byrow = TRUE)
A
B=matrix(c(1,3,5,2,0,1,3,4,2,4,7,3,1,5,1,2),nrow=4,ncol=4,byrow = TRUE)
B
A%*%solve(B)
B%*%t(A)

#5
x<-matrix(c(2,5,6,7),nrow=4,ncol=1)
x
y<-matrix(c(-1,3,-1,-1),nrow=4,ncol=1)
y
t(x)%*%y

## Chapter 3

mykids <- c("Stephen", "Christopher")
mykids

# Sequences
1:9
1.5:10
c(1.5:10,10)
prod(1:8) # same as factorial(8)
seq(1,5)
seq(1,5,by=.5)
seq(1,5,length=7)

rep(10,10) # repeat the value 10 ten times
rep(c("A","B","C","D"),2) # repeat the string A,B,C,D twice
matrix(rep(0,16),nrow=4)

# Reading in data
#passengers<-scan()
#passengers
passengers<-scan("/home/sathvik/EC8/ML/Lab/Lab1/data.txt")
passengers

# Data frames
#new.data <- data.frame()
#new.data <- edit(new.data)

seatbelt <- c("Y","N","Y","Y","Y","Y","Y","Y","Y","Y", "N","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","N","Y","Y","Y","Y")
car.dat <- data.frame(passengers,seatbelt)
car.dat

data(trees)
trees
trees$Height
sum(trees$Height)
trees[4,3]
trees[4,]
attach(trees)
Height
search()
attributes(trees)
Height[Height > 75]

#smith <- read.table(file.choose(), header=T)
#smith
#attributes(smith)

# Exercises

#1a
rep(c(1,2,3),3)
#1b
seq(10,10.5,length=12)
#1c
rep(c(1,2,3,"banana"),2)
#2
blahblah <- scan() # 10 no.s between 1 and 100
blahblah
#3
coursenumber <- c(871,347,348)
coursedays <- c("MWT","MTF","WT")
grade <- c("AA","AB","AB")
schedule <- data.frame(coursenumber,coursedays,grade)
schedule
#4
data("stackloss")
attach(stackloss)
tempacid <- data.frame(Water.Temp,Acid.Conc.)
tempacid
