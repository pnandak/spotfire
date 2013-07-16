###################################################
### chunk number 1: X.5 Exercise from Lecture1
###################################################
my.mat<-matrix(NA, ncol= 3, nrow=6)
my.mat[,1]<-rep(1,6)
my.mat[,2]<-seq(0,4,2)
my.mat[,3]<-rep(1:2,each=3)
my.mat<-cbind(my.mat, c(11,15,19,10,15,20))
help(colnames)
colnames(my.mat) =c("index", "weeks", "id", "height")

# Even Rows
my.mat[seq(2,nrow(my.mat),2),]
my.mat[5,4] <-18

# Sort by weeks and heights
matOrder<-order(my.mat[,"weeks"], my.mat[,"height"])
my.mat.srt<-my.mat[matOrder,]


###################################################
### chunk number 2: R datasets
###################################################
data()


###################################################
### chunk number 3: R datasets: Women
###################################################
data(women)
ls()
ls(pattern="w")


###################################################
### chunk number 4: R libraries
###################################################
library()


###################################################
### chunk number 5: Load R library
###################################################
library(MASS)
## Or the alternative
require(MASS)


###################################################
### chunk number 6: Help on R library
###################################################
library(help=MASS)


###################################################
### chunk number 7: View library loaded into workspace
###################################################
search()
sessionInfo()


###################################################
### chunk number 8: Unload packages from workspace
###################################################
detach("package:MASS")
search()


###################################################
### chunk number 9: Reading Data from an Excel file using read.xls
###################################################

dir(pattern="Tooth")

# Excel
library(xlsReadWrite)
TGxls<-read.xls("ToothGrowth.xls")
TGxls[1:2,]


###################################################
### chunk number 10: Reading Data using read.table and read.csv- Getting Help
###################################################
dir(pattern="Tooth")
# Read the help file
help(read.table)


###################################################
### chunk number 11: Reading Data using read.table and read.csv
###################################################
# Tab Delimited
TG1<-read.table("ToothGrowth.txt", sep="\t", header=TRUE, as.is=TRUE)
TG1[1:2,]
summary(TG1)

# Comma Delimited
TG2<-read.csv("ToothGrowth.csv", header=TRUE)
TG2[1:2,]


###################################################
### chunk number 12: Reading/Writing Data- Create a path variable
###################################################
# Create a variable, myPath, your data directory
myPath <- file.path('C:\\Documents and Settings\\Aedin\\Desktop\\')
# Set myPath to be current directory
myPath<-file.path(getwd())
TG <- read.table(paste(myPath, "ToothGrowth.txt", sep="/"), sep="\t", header=T)
names(TG)
dim(TG)


###################################################
### chunk number 13: Reading Data into using scan
###################################################
myFile <- paste(myPath, 'try.dat', sep='')
cat("Some data", "1 5 3.4 8", "9 11 23", file=myFile,sep="\n")
tryScan <- scan(myFile, skip = 1)
tryScan


###################################################
### chunk number 14: Sending output to a file using sink
###################################################
sink(paste(myPath, "sinkTest.txt", sep="/"))
ls()
sin(1.5*pi)
sink()


###################################################
### chunk number 15: Writing tables using write.table
###################################################
myPath<-getwd()
setwd(myPath)
myResults <- matrix(0:99, nrow=20)
write.table(myResults, file='results.txt')
df1 <- data.frame(myResults)
names(df1) <- paste("col", 1:5, sep="")
write.table(df1, file="results2.txt", row.names=F, col.names=T)
read.table(file="results2.txt", head=T)[1:2,]


###################################################
### chunk number 16: Writing output using R2HTML
###################################################
# Write data directly to a new webpage
library(R2HTML)
HTML(df1,outdir=myPath, file="results2.html")
# Capture output to a webpage
HTMLStart(outdir=myPath, filename="output")
print("Capturing Output")
df1[1:2,]
HTMLStop()


###################################################
### chunk number 17: Viewing R functions - median
###################################################
median
args(median)  # See the input arguments to the function

methods(median)
median.default

xx<-c(rep(NA,2),sample(1:30, 10))
xx
median(xx)
median(xx, na.rm=TRUE)


###################################################
### chunk number 18: Our first function - two sam
###################################################
twosam <- function(y1, y2) {
  n1 <- length(y1); n2 <- length(y2)
  yb1 <- mean(y1); yb2 <- mean(y2)
  s1 <- var(y1); s2 <- var(y2)
  s <- ((n1-1)*s1 + (n2-1)*s2)/(n1+n2-2)
  tst <- (yb1 - yb2)/sqrt(s*(1/n1 + 1/n2))
  return(tst)
}


###################################################
### chunk number 19: Twosam analysis of weight data from ChickWeight populations
###################################################
data(ChickWeight)
attach(ChickWeight)
chickDiet1 <- weight[Time==0 & Diet==1]
chickDiet2 <- weight[Time==0 & Diet==2]
twosam( chickDiet1, chickDiet2 )
detach(2)   # 2 is the second object in the search path, see search()


###################################################
### chunk number 20: Functions: conditional statements- if
###################################################
x <- 9
if (x > 0) sqrt(x) else sqrt(-x)


###################################################
### chunk number 21: Functions: conditional statements- ifelse
###################################################
x <- seq(-25,25, by=5)
ifelse(x >= 0, x, -x)


###################################################
### chunk number 22: Functions: conditional statements- switch
###################################################
spread <- function(x, type) {
   switch(type,
          sd = sd(x),
          mad = mad(x),
          IQR = IQR(x)/1.349)
     }
samp <- rnorm(50)
spread(samp, 2)
spread(samp, 'IQR')


###################################################
### chunk number 23: Functions: For loop + conditional statements : For and switch
###################################################
 for(i in 1:5)
  print(i^2)

 ccc <- c("b","QQ","a","A","bb")
 for(ch in ccc)
   cat(ch,":",switch(EXPR = ch, a=1, b=2:3), "\n")


###################################################
### chunk number 24: Functions: statements- repeat
###################################################
z <- 1
prod.cons <- 1
y <- 1e7
repeat
  { z <- z + 1   # iterator count
    prod.cons <- prod.cons*z
    cat(z, prod.cons, '\n')
    if (prod.cons > y) break
  }


###################################################
### chunk number 25: Functions: statements- while loop
###################################################
x <- 1
y <- 284
while (x^2 < y)
  { cat(x,"squared is ", x^2, "\n")   # print x and sq(x)
    x <- x+1
  }


###################################################
### chunk number 26: Functions: Equivalent functions
###################################################
prob <- c(0.5, 0.9, 0.95, 0.975, 0.99)
args(qnorm)
qnorm(p=prob, mean=0, sd=1, lower.tail = TRUE, log.p= FALSE)
qnorm(prob, 0, 1, log=F, low=T)
qnorm(prob, 0, 1, T, F)


###################################################
### chunk number 27: Functions: parameters
###################################################
qnorm(prob)
qnorm(prob, 2)
qnorm(prob, mean=2, sd=1)


###################################################
### chunk number 28: Functions: formal local and free parameters
###################################################
fn <- function(x) {
     y <- 2*x
     print(x)
     print(y)
     print(z)
}

z <- 2
x <- 4

fn(x=2)


###################################################
### chunk number 29: Functions: parameters and lexical scope
###################################################
cube <- function(n) {
    sq <- function() n*n
              n*sq()
}
cube(2)
n <- 4
cube(2)


###################################################
### chunk number 30: Functions: Extending our function two sam
###################################################
twosam2 <- function(y1, y2, paired=F)
{
  n1 <- length(y1); n2 <- length(y2)
  yb1 <- mean(y1); yb2 <- mean(y2)
  s1 <- var(y1); s2 <- var(y2)
  if (paired==F)
    {  s <- ((n1-1)*s1 + (n2-1)*s2)/(n1+n2-2)
       tst <- (yb1 - yb2)/sqrt(s*(1/n1 + 1/n2))
       p.val <- 2*(1 - pt(abs(tst), (n1+n2-2)))
    }
  else
    { if (n1==n2)
       { s <- var(y1-y2)
         tst <- (yb1-yb2)/sqrt(s/n1)
         p.val <- 2*(1 - pt(abs(tst), (n1-1)))
       }
      else stop('unequal vector lengths')
    }
list(tst=tst,p.val=p.val,pair=paired)
}


###################################################
### chunk number 31: Functions: apply lapply sapply tapply
###################################################
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
# compute the list mean for each list element
lapply(x,mean)
sapply(x, mean)

# Apply, work on the rows or columns 
data(airquality)
airQ<-airquality[,1:4]  # Look at first 4 columns
apply(airQ, 2, mean)
colMeans(airQ)
apply(airQ, 1, mean)
rowMeans(airQ)

#tapply input a vector of data and a factor
attach(ChickWeight)
names(ChickWeight)
tapply(weight, Diet, mean)
apply(ChickWeight, 2, length)
apply(ChickWeight, 1, length)



###################################################
### chunk number 32: Appendix: Example- Load ChickWeight Data
###################################################
data(ChickWeight)
attach(ChickWeight)


###################################################
### chunk number 33: Appendix: Example-  Table
###################################################
names(ChickWeight)
mean(weight)
table(Time)
table(Time,Diet)   # Cross Tabulate
# How many chickens?
length(unique(Chick))


###################################################
### chunk number 34: Appendix: Example- More summary statistics
###################################################
# Can we get the average weight of all the chicks on Diet==1 at Time==0
# One way
mean(weight[Diet==1 & Time==0])
# What about all ``Diet''s at all ``Time''s
tapply(weight, list(Diet,Time), mean)


###################################################
### chunk number 35: Appendix: Example- Basic scatterplot and boxplot
###################################################
plot(Time, weight)
boxplot(weight~ Diet, col = 1:4)


###################################################
### chunk number 36: Appendix: Example- Basic linear Regression
###################################################
chick.lm <- lm(weight~Time, data=ChickWeight)
summary(chick.lm)
plot(chick.lm)


###################################################
### chunk number 37: Appendix: Example- More advanced Linear regression using weights- Load data
###################################################
attach(ChickWeight)


###################################################
### chunk number 38: Appendix: Example- More advanced Linear regression using weights
###################################################
time.wgt <- tapply(weight,Time, var)
weight.ave <- tapply(weight,Time, mean)
time.wgt.rep <-
    as.numeric(time.wgt[match(Time,as.numeric(names(time.wgt)))])
detach(2)
Chick.anl <- data.frame(ChickWeight, time.wgt.rep)
chick.lm.wgt <-lm(weight~Time,
                   data=Chick.anl,
                   weight=1/time.wgt.rep)
summary(chick.lm.wgt)
plot(chick.lm.wgt)


###################################################
### chunk number 39: Appendix: Example- Plot of fits comparing two linear regression models
###################################################
attach(Chick.anl)
plot(Time, weight)
lines(Time, chick.lm$fit, col="blue")
lines(Time, chick.lm.wgt$fit, col="red")
legend(0,350, legend=c("Naive","Weighted"),
  lty=c(1,1),col=c("blue","red"))
title("Linear regression fit")
detach(2)


###################################################
### chunk number 40: Appendix: Example - linear regression with addition factor attach data
###################################################
attach(Chick.anl)


###################################################
### chunk number 41: Appendix: Example- Linear regression with additional factors and ANOVA
###################################################

chick.lm.d <- lm(weight~Time+Diet,
                 data=Chick.anl,
                 weight=1/time.wgt.rep)
chick.lm.dt <- lm(weight~Time*Diet,
                  data=Chick.anl,
                  weight=1/time.wgt.rep)
anova(chick.lm.wgt, chick.lm.d, chick.lm.dt)


###################################################
### chunk number 42: Appendix: Example-  More Plots - boxplots bwplot and xyplot
###################################################
# Boxplot
boxplot(weight~Time, data=ChickWeight, col="bisque")

library(lattice)
bwplot(Time~weight|Diet, data=ChickWeight, col="bisque")
xyplot(weight~Time|Diet, 
          groups=Chick, 
          data=ChickWeight,
          type="b", 
          main="Chicken weight raw data")


