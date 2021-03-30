# Roll No.: 171EC146 
# Name: Sathvik S Prabhu

# Introduction to Graphics

# Plot function
data("trees")
attach(trees)
plot(Height, Volume)


# Curve function
curve(4*sin(x), from = 0, to= 2*pi)

#Embellishments
curve(4*sin(x), from = 0, to= 2*pi)
abline(a=2 , b=-1, v=3 ) # a=Slope, b=intercept, h= hor., v=vert.
arrows(x0=1,y0=0,x1=1.5,y1=0.5) # adds an arrow at a specified coordinate
#lines(x=c(0,1),y=c(0,-1),type="l") #adds lines between coordinates
points(x=c(3,0),y=c(0.5,0),type="p") #adds points at specified coordinates
#rug(x=c(2,0)) # adds a “rug” representation to one axis of the plot
#segments() # similar to lines() above
text(3.5,3, labels="f(x)") #adds text 
title(main="A plot", sub="Sine wave", xlab="x") # titles, subtitles, etc.

#Changing Graphics Parameters
par(mfrow = c(2, 2)) #gives a 2 x 2 layout of plots
par(mar = rep(2, 4)) #change margins to fit all 4 subplots
par(bg = "cornsilk") #plots drawn with this colored background
par(lend = 1) #gives "butt" line end caps for line plots 2
curve(sin(x), from = 0, to= 2*pi)
curve(cos(x), from = 0, to= 2*pi)
curve(sin(x)^2, from = 0, to= 2*pi)
curve(cos(x)^2, from = 0, to= 2*pi)
par(mfrow=c(1,1)) # reset mfrow

par(xlog = TRUE) #always plot x axis on a logarithmic scale
x=1:100
y= sqrt(x)
plot(x,y,log="x")
title(main = "X axis on a logarithmic scale")


#Exercises

#demo(graphics)
x <- stats::rnorm(50)
opar <- par(bg = "white")
plot(x, ann = FALSE, type = "n")
abline(h = 0, col = gray(.90))
lines(x, col = "green4", lty = "dotted")
points(x, bg = "limegreen", pch = 21)
title(main = "Simple Use of Color In a Plot",xlab = "Just a Whisper of a Label",col.main = "blue", col.lab = gray(.8),cex.main = 1.2, cex.lab = 1.0, font.main = 4, font.lab = 3)

par(bg = "gray")
pie(rep(1,24), col = rainbow(24), radius = 0.9)
title(main = "A Sample Color Wheel", cex.main = 1.4, font.main = 3)

pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry","Apple", "Boston Cream", "Other", "Vanilla Cream")
pie(pie.sales,col = c("purple","violetred1","green3","cornsilk","cyan","white"))
title(main = "January Pie Sales", cex.main = 1.8, font.main = 1)

n <- 10
g <- gl(n, 100, n*100)
x <- rnorm(n*100) + sqrt(as.numeric(g))
boxplot(split(x,g), col="lavender", notch=TRUE)
title(main="Notched Boxplots", xlab="Group", font.main=4, font.lab=1)

par(bg="white")
n <- 100
x <- c(0,cumsum(rnorm(n)))
y <- c(0,cumsum(rnorm(n)))
xx <- c(0:n, n:0)
yy <- c(x, rev(y))
plot(xx, yy, type="n", xlab="Time", ylab="Distance")
polygon(xx, yy, col="gray")
title("Distance Between Brownian Motions")

x <- rnorm(1000)
hist(x, xlim=range(-4, 4, x), col="lavender", main="")
title(main="1000 Normal Random Variates", font.main=3)

pairs(iris[1:4], main="Edgar Anderson's Iris Data", pch=21,bg = c("red", "green3", "blue")[unclass(iris$Species)])

#demo(persp)
x <- seq(-10, 10, length.out = 50)
y <- x

rotsinc <- function(x,y)
{
    sinc <- function(x) { y <- sin(x)/x ; y[is.na(y)] <- 1; y }
    10 * sinc( sqrt(x^2+y^2) )
}

sinc.exp <- expression(z == Sinc(sqrt(x^2 + y^2)))
z <- outer(x, y, rotsinc)
oldpar <- par(bg = "white")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
title(sub=".")## work around persp+plotmath bug
title(main = sinc.exp)

z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
persp(x, y, z, theta = 120, phi = 15, scale = FALSE, axes = FALSE)

##   We border the surface, to make it more "slice like"
##  and color the top and sides of the surface differently.

z0 <- min(z) - 20
z <- rbind(z0, cbind(z0, z, z0), z0)
x <- c(min(x) - 1e-10, x, max(x) + 1e-10)
y <- c(min(y) - 1e-10, y, max(y) + 1e-10)

fill <- matrix("green3", nrow = nrow(z)-1, ncol = ncol(z)-1)
fill[ , i2 <- c(1,ncol(fill))] <- "gray"
fill[i1 <- c(1,nrow(fill)) , ] <- "gray"
par(bg = "lightblue")

persp(x, y, z, theta = 120, phi = 15, col = fill, scale = FALSE, axes = FALSE)
title(main = "Maunga Whau\nOne of 50 Volcanoes in the Auckland Region.",font.main = 4)
par(bg = "slategray")
persp(x, y, z, theta = 135, phi = 30, col = fill, scale = FALSE,ltheta = -120, lphi = 15, shade = 0.65, axes = FALSE)

persp(x, y, z, theta = 135, phi = 30, col = "green3", scale = FALSE,ltheta = -120, shade = 0.75, border = NA, box = FALSE)

## `color gradient in the soil' :
fcol <- fill ; fcol[] <- terrain.colors(nrow(fcol))
persp(x, y, z, theta = 135, phi = 30, col = fcol, scale = FALSE,ltheta = -120, shade = 0.3, border = NA, box = FALSE)

## `image like' colors on top :
fcol <- fill
zi <- volcano[ -1,-1] + volcano[ -1,-61] + volcano[-87,-1] + volcano[-87,-61]  ## / 4
fcol[-i1,-i2] <-
terrain.colors(20)[cut(zi,stats::quantile(zi, seq(0,1, length.out = 21)),include.lowest = TRUE)]
persp(x, y, 2*z, theta = 110, phi = 40, col = fcol, scale = FALSE,ltheta = -120, shade = 0.4, border = NA, box = FALSE)
par(oldpar)
  
#demo(image)
x <- 10*(1:nrow(volcano)); x.at <- seq(100, 800, by=100)
y <- 10*(1:ncol(volcano)); y.at <- seq(100, 600, by=100)
image(x, y, volcano, col=terrain.colors(100),axes=FALSE)
contour(x, y, volcano, levels=seq(90, 200, by=5), add=TRUE, col="brown")
axis(1, at=x.at)
axis(2, at=y.at)
box()
title(main="Maunga Whau Volcano", sub = "col=terrain.colors(100)", font.main=4)

# Using Heat Colors
image(x, y, volcano, col=heat.colors(100), axes=FALSE)
contour(x, y, volcano, levels=seq(90, 200, by=5), add=TRUE, col="brown")
axis(1, at=x.at)
axis(2, at=y.at)
box()
title(main="Maunga Whau Volcano", sub = "col=heat.colors(100)", font.main=4)

# Using Gray Scale
image(x, y, volcano, col=gray(100:200/200), axes=FALSE)
contour(x, y, volcano, levels=seq(90, 200, by=5), add=TRUE, col="black")
axis(1, at=x.at)
axis(2, at=y.at)
box()
title(main="Maunga Whau Volcano \n col=gray(100:200/200)", font.main=4)

#Summarizing Data
data("mtcars")  #Load dataset
attach(mtcars)
mtcars

#Numerical Summaries
mean(hp)
var(mpg)
quantile(qsec, probs = c(.20, .80))
cor(wt,mpg)
table(cyl)
table(cyl)/length(cyl)

#Graphical Summaries
# Bar plot
barplot(table(cyl)/length(cyl))
# Pie chart
x <- c(21, 62, 10, 53)
labels <- c("A", "B", "C", "D")
pie(x,labels,main = "Pie chart")


data("faithful")
attach(faithful)
hist(eruptions, main = "Old Faithful data", prob = T)
hist(eruptions, main = "Old Faithful data", prob = T, breaks=18)
boxplot(faithful)
qqnorm(waiting)
qqline(waiting)

#Exercises

#Using the stackloss dataset that is available from within R :
#1. Compute the mean, variance, and 5 number summary of the variable stack.loss
data("stackloss")
attach(stackloss)
stack.loss

mean(stack.loss)
var(stack.loss)
fivenum(stack.loss) # (minimum, lower-hinge, median, upper-hinge, maximum)

#2. Create a histogram, boxplot, and normal probability plot for the variable stack.loss .
#Does an assumption of normality seem appropriate for this sample?
hist(stack.loss, main = "Stackloss data", prob = T,breaks=18)
boxplot(stack.loss)
qqnorm(stack.loss) # Plot NPP
qqline(stack.loss) # Plot the reference line
# No. The data appears to be skewed.