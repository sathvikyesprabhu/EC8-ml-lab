# 171EC146
# Sathvik S Prabhu

# 1. 6.1. Distribution Functions in R, Owen-The R Guide:
#   • Learn to get the distribution (d), cdf (p), quantile (q), and generate samples of the distribution (r).
# Try for as many distributions as you wish.
# x <- rnorm(100)
# w <- rexp(1000,rate=.1)
# dbinom(3,size=10,prob=.25) # P(X=3) for X ~ Bin(n=10, p=.25)
# dpois(0:2, lambda=4)
# pbinom(3,size=10,prob=.25) 
# dnorm(12,mean=10,sd=2)
# qnorm(.75,mean=10,sd=2)
# qchisq(.10,df=8)
# qt(.95,df=20)

# • Report for any three distributions
# Dist 1: norm
dnorm(-1:1,mean=0.5,sd=1)
pnorm(-1:1,mean=0.5,sd=1)
qnorm(0.9985,mean=0.5,sd=1)
rnorm(5,mean=0.5,sd=1)

# Dist 2: exp
dexp(0.5:3,rate=0.5)
pexp(0.5:3,rate=0.5)
qexp(0.92,rate=0.5)
rexp(4,rate=0.5)

# Dist 3: binom
dbinom(1:10,10,prob=0.5)
pbinom(1:10,10,prob=0.5)
qbinom(0.5,10,prob=0.5)
rbinom(2,10,prob=0.5)

# 2. 6.3 Graphing Distributions
# Discrete
x <- 0:10
y <- dbinom(x, size=10, prob=.25)
plot(x, y, type = "h", lwd = 30, main = "Binomial Probabilities w/ n= 10, p = .25", ylab = "p(x)", lend ="square")
plot(0:10, dbinom(x, size=10, prob=.25), type = "h", lwd = 30) # without other embellishments

# Continuous
curve(dnorm(x), from = -3, to = 3) # normal pdf
curve(pnorm(x, mean=10, sd=2), from = 4, to = 16) # normal cdf
qnorm(0.9985) #68 95 99.7 rule
qnorm(0.975)

simdata <- rexp(1000, rate=.1)
hist(simdata, prob = T, breaks = "FD", main="Exp(theta = 10) RVs")
curve(dexp(x, rate=.1), add = T)
     
# 3. 6.4 Random Sampling
# sample(x, size, replace = FALSE, prob = NULL)
sample(1:100, 1)
sample(1:6, 10, replace = T) # fair die
sample(1:6, 10, T, c(.6,.2,.1,.05,.03,.02)) # biased die
urn <- c(rep("red",8),rep("blue",4),rep("yellow",3))
sample(urn, 6, replace = F)

# 4. 6.5 Exercises covered here (6.2, 6.4, and 6.5).
#1
simdata<-rbinom(20,15,0.2)
simdata

#2
qgamma(0.2,shape=2,scale=10)

#3
curve(pt(x,8),from = 0,to=10)
1-pt(2,8)

#4
plot(dpois(0:15,lambda=4),type="h",lwd=10,lend="square")

#5
x <- runif(1000000, min=0, max=2)
a=2*mean(exp(x^3))
a
b=integrate(f = function(x) exp(x^3), lower = 0, upper = 2)
b$value

#6
simdata<-rnorm(100,mean=50,sd=4)
hist(simdata, prob = T, breaks = "FD", main="RVs")
curve(dnorm(x,mean=50,sd=4), add = T)

#7 
fair_coin <- c("heads","tails")
sample(fair_coin,25,replace=T)
