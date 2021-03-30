
set.seed(2351)
require(kohonen)
require(RColorBrewer)

#1
library(cluster)
d<-data.frame(x1=runif(100,0,60),x2=runif(100,0,15),x3=runif(100,0,25))

k<-kmeans(d, 5)
str(k)
k$centers
k$cluster[20]
k$cluster[30]
k$cluster[70]

#2
d<-data.frame(x1=runif(10000,0,1),x2=runif(10000,0,1),x3=runif(10000,0,1))

d.measures1=c("x1","x2","x3")
d.SOM1 <- som(scale(d[d.measures1]), grid = somgrid(100, 100, "rectangular"))
t<-d.SOM1$grid$pts
t[1999,]
t[2909,]
# plot(d.SOM1)