################
# Friday, October 16, 2009
# Introduction to R
################

# Where are we? 
getwd()
setwd("/Users/streier/Desktop/Rlab")
objects()

# Assignment 
x <- 2
x_3 <- 2 # This works, but do not use underscores in names
	     # Long story, but may cause problems in different versions, 
	     #   or in Splus
y = 3    # This works, but it just seems wrong to me ... 
		 # There are actually some scoping resrictions as well ... 
assign("z",4)
z
get("z")
# Example, using for loop 
#Create objects  'r.1', 'r.2', ... 'r.6'
for(i in 1:6) { 
 nam <- paste("r",i, sep=".")
 assign(nam, 1:i)
}

for(i in 1:6){
	print(get(paste("r.",i,sep="")))
	}

methods(print)

objects()
rm(x_3)
objects()
ls()
ls(pattern="^r..$")
ls(pattern='r*')
ls(pattern=glob2rx('r*'))
ls(pattern=glob2rx('r.?'))


args(rm)
rm(list=objects())
objects()

# Asking for help
help.start()
help(c)
help("<-")

# Vectors
x <- c(10.4,5.6,3.1,6.4,21.7)
1/x
exp(x)
log(x)
x*2
y <- c(x,0,x)
# Empty vector
z <- numeric()
z[5] <- 3
z[8] <- 10

# Calculate mean and standard deviation
mean.x <- sum(x)/length(x)
var.x  <- sum((x-mean.x)^2)/(length(x)-1)
sd.x   <- sqrt(var.x)
# Or ... 
mean(x)
var(x)
sd(x)
# median
median(x)
quantile(x,0.5)


# Generate sequences of numbers
args(seq)
help(seq)
seq(0,100,by=2)
seq(0,100,length=51)
seq(1,30)
1:30
30:1
rep(1:4,5)
rep(1:4,each=5)
rep(1:4,c(5,5,5,5))
rep(1:4,1:4)

# Character vectors
states <- c("Minnesota","Iowa","New Hampshire","Florida")
labels <- paste(c("X","Y"),1:10,sep="")
paste("Today is",date())
paste("The mean of x is", mean.x)

# Logical vectors 
w  <- (x > 10)
ww <- c(TRUE,FALSE,TRUE,FALSE)

# Factors 
vote <- rep(1:5,c(10,8,6,5,1))
vote <- factor(vote,levels=1:5)
levels(vote) <- c("McCain","Romney","Huckabee","Giuliani","Paul")
summary(vote)
table(vote)
table(as.numeric(vote))
levels(vote)

fvote <- rep(c("McCain","Romney","Huckabee","Giuliani","Paul"),
			 c(10,8,6,5,1))
fvote <- factor(fvote)

# Matrices
x <- 1:12
dim(x) <- c(3,4)

x <- matrix(1:12,nrow=3)
x <- matrix(1:12,ncol=4)
x <- matrix(1:12,ncol=4,byrow=T)
colnames(x) <- letters[1:4]
rownames(x) <- LETTERS[1:3]
dimnames(x)

# attach rows or columns
new.x <- cbind(x,13:16)
new.x <- rbind(x,17:21)

y <- matrix(NA,nrow=3,ncol=4)

# Arrays
z <- 1:30
dim(z) <- c(3,5,2)
w <- array(1:30,dim=c(3,5,2))
ww <- array(1:60,dim=c(3,5,2,2))



# Indexing
x <- runif(20)
x[3] <- NA
x[5] <- NA
is.na(x)
y <- x[!is.na(x)]
z <- (x+1)[(!is.na(x)) & x>0]
x[1:10]
x[-(1:5)]
names(x) <- letters[1:(length(x))]
x[c("a","c","d")]
x[is.na(x)] <- 0

# Attributes 
is.numeric(x)
is.character(x)
is.matrix(x)
is.vector(x)
is.data.frame(x)
as.character(x)

# data.frame
Year <- c(1800,1850,1900,1950,2000)
Carbon <- c(8,54,534,1630,2000)
fossilfuel <- data.frame(year=Year,carbon=Carbon)
fossilfuel
rm(Year,Carbon)
plot(carbon ~ year, data=fossilfuel)
summary(fossilfuel)
summary(fossilfuel[,2])
summary(fossilfuel[,"carbon"])
dimnames(fossilfuel)

# Open data.frame
library(DAAG)
Cars93.summary
Cars93.summary <- edit(Cars93.summary)
head(Cars93.summary,n=3)
Cars93.summary[1:3,]
summary(Min.passengers)
summary(Cars93.summary$Min.passengers)
attach(Cars93.summary)
summary(Min.passengers)
detach()

# Time series 
numjobs <- c(982,981,984,982,981,983,983,983,983,979,973,
			 979,974,981,985,987,986,980,983,983,988,994,
			 990,999)
numjobs <- ts(numjobs,start=1995,frequency=12)
window(numjobs,start=1995.25,end=1996.25)
plot(numjobs)

# Graphics 
args(plot.default)
help(par)

# Simple example
x <- runif(50,0,2)
y <- runif(50,0,2)
plot(x,y)
plot(y ~ x)
plot(x,y,main="Main title", sub="subtitle",
	 	 xlab="x-label", ylab="y-label")
text(0.6,0.6,"text at(0.6,0.6)")
abline(h=0.6,v=0.6)
for (side in 1:4){
	mtext(-1:4,side=side,at=0.7,line=-1:4)
	}
mtext(paste("side",1:4),line=-1,font=2)

# Add a regression line
xy.lm <- lm(y~x)
summary(xy.lm)
abline(xy.lm,lty=2,col="blue")

# Build graph piece by piece
plot(x, y, type="n",xlab="",ylab="",axes=F)
points(x,y)
axis(1)
axis(2,at=seq(0.2,1.8,0.2))
box()
title(main="Main title", sub="subtitle",
	  xlab="x-label",ylab="y-label")
	  
# Combine plots
x <- rnorm(100)
hist(x,freq=F)
curve(dnorm(x),add=T)

# Better approach
h <- hist(x,plot=F)
ylimits <- range(0,h$density,dnorm(0))
ylimits <- range(0,h$density,dnorm(x))
hist(x,freq=F,ylim=ylimits)
curve(dnorm(x),add=T)

# What if ... 
hist(x,freq=F,ylim=ylimits)
lines(x,dnorm(x))

hist(x,freq=F,ylim=ylimits)
lines(x[order(x)],dnorm(x[order(x)]))




