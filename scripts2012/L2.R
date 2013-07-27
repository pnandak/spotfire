### R code from vignette source 'C:/Users/aedin/Dropbox/Talks/Bio503/winter2012/L2/L2.rnw'

###################################################
### code chunk number 1: X.5 Exercise from Lecture1
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
### code chunk number 2: Reading Data using read.table
###################################################
# Tab Delimited
TG1<-read.table("ToothGrowth.txt", sep="\t", header=TRUE)
TG1[1:2,]
summary(TG1)
class(TG1$Treatment)


###################################################
### code chunk number 3: L2.rnw:179-183
###################################################
TG1<-read.table("ToothGrowth.txt", sep="\t", header=TRUE, as.is=TRUE)
TG1[1:2,]
summary(TG1)
class(TG1$Treatment)


###################################################
### code chunk number 4: Help on read.table
###################################################
# Read the help file
help(read.table)


###################################################
### code chunk number 5: read.csv
###################################################
# Comma Delimited
TG2<-read.csv("ToothGrowth.csv", header=TRUE)
TG2[1:2,]


###################################################
### code chunk number 6: Reading Data into using scan
###################################################
myFile <- "outfile.txt"
# Create a file
cat("Some data", "1 5 3.4 8", "9 11 23", file=myFile,sep="\n")
exampleScan <- scan(myFile, skip = 1)
print(exampleScan)


###################################################
### code chunk number 7: scan using n
###################################################
scan(myFile, what="some text", n=3)


###################################################
### code chunk number 8: Using RODBC (eval = FALSE)
###################################################
## library(RODBC)
## RShowDoc("RODBC", package="RODBC")


###################################################
### code chunk number 9: Reading/Writing Data- Create a path variable
###################################################
# Create a variable, myPath, your data directory
myPath <- file.path('C:/Aedin/')
# Set myPath to be current directory
myPath<-getwd()

# It is better to expand a path using \Rfunction{file.path()} rather than \Rfunction{paste()}
# as \Rfunction{file.path()} will expand the path with delimiting characters appropriate 
# to the operating system in use (eg \verb+/+ unix, \verb+\+, windows etc) 

myfile<-file.path(myPath, "ToothGrowth.txt")   


###################################################
### code chunk number 10: file.exists
###################################################
if (file.exists(myfile)) {
   TG<- read.table(myfile, sep="\t", header=TRUE)
   TG[1:2,]
}
if (!file.exists(myfile)) print(paste(myfile, "cannot be found"))



###################################################
### code chunk number 11: Startsink (eval = FALSE)
###################################################
## myPath<-getwd()
## sink(file.path(myPath, "sinkTest.txt"))
## print("This is a test of sink")
## ls()
## sin(1.5*pi)
## print(1:10)
## sink()


###################################################
### code chunk number 12: Writing tables using write.table
###################################################
myResults <- matrix(rnorm(100,mean=2), nrow=20)
write.table(myResults, file='results.txt')


###################################################
### code chunk number 13: Writing tables using write.table2
###################################################
df1 <- data.frame(myResults)
colnames(df1) <- paste("MyVar", 1:5, sep="")
write.table(df1, file="results2.txt", row.names=FALSE, col.names=TRUE)
read.table(file="results2.txt", head=TRUE)[1:2,]


###################################################
### code chunk number 14: Writing output using R2HTML
###################################################
# Write data directly to a new webpage
library(R2HTML)
HTML(df1,outdir=myPath, file="results.html")
# Capture output to a webpage
HTMLStart(outdir=myPath, filename="Web_Results", echo=TRUE)

print("Capturing Output")
df1[1:2,]
summary(df1)
print("hello and Goodbye")
HTMLStop()


###################################################
### code chunk number 15: w1
###################################################
library(XML)
worldPop<-readHTMLTable("http://en.wikipedia.org/wiki/World_population")
length(worldPop)


###################################################
### code chunk number 16: w2a (eval = FALSE)
###################################################
## names(worldPop)


###################################################
### code chunk number 17: w3
###################################################
worldPop<- readHTMLTable("http://en.wikipedia.org/wiki/World_population", which=12)
worldPop


###################################################
### code chunk number 18: worldPop
###################################################
worldPop$Region<-sub("\\[Note 1\\]", "", worldPop$Region)
rownames(worldPop)<-worldPop[,1]
worldPop
nchar(worldPop[,1])
substr(worldPop[,1],1 ,12)
rownames(worldPop)<-substr(worldPop[,1],1 ,12)
##colnames(worldPop)<-paste("yr", colnames(worldPop))
worldPop<-worldPop[,-1]
worldPop<-t(worldPop)
numeric.data.frame<-function(df, mar=1) {
  rN<-rownames(df) 
  cN<-colnames(df)
  df<- apply(df, mar, sub,pattern=",",replacement= "")
  df<- apply(df, mar, as.numeric)
  rownames(df)<-rN
  colnames(df)<-cN
  df<-as.data.frame(df)
  return(df)
}
worldPop<-numeric.data.frame(worldPop)


###################################################
### code chunk number 19: w4
###################################################
worldPop


###################################################
### code chunk number 20: worldPop2
###################################################
nCt= ncol(worldPop)
plot(worldPop[,1], type="n", ylab="Population (Billions", xlab="year",xaxt="n")
axis(1, 1:nrow(worldPop), rownames(worldPop), las=2)
for (i in 1:nCt) {lines(worldPop[,i], col=i, lwd=2)}
legend("topleft", colnames(worldPop), fill=1:nCt)


###################################################
### code chunk number 21: rnorm
###################################################
rnorm(5,0,1)
rnorm(10,6,2)


###################################################
### code chunk number 22: sample
###################################################
sample(1:10)
sample(1:10, replace=TRUE)


###################################################
### code chunk number 23: sample with weights
###################################################
weights <- c(rep(.25, 5), rep(.05, 5))
print(weights)
sample(10, replace=T, prob=weights)


###################################################
### code chunk number 24: myMean Function
###################################################
myMean<-function(y1) {
        mean<-sum(y1)/length(y1)
        return(mean)
		}


###################################################
### code chunk number 25: Test myMean Function
###################################################
testVec= rnorm(50, 20, 4)
mean(testVec)
myMean(testVec)


###################################################
### code chunk number 26: Our first function - two sam
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
### code chunk number 27: Viewing R functions IQR
###################################################
help(IQR)
IQR
args(IQR)
body(IQR)
xx<-sample(1:30, 10)
quantile(xx)
IQR(xx)


###################################################
### code chunk number 28: viewing mean
###################################################
mean
methods(mean)
mean.default


###################################################
### code chunk number 29: Functions: conditional statements- if
###################################################
x <- 9
if (x > 0) sqrt(x) else sqrt(-x)


###################################################
### code chunk number 30: Functions: conditional statements- ifelse
###################################################
ifelse(x >= 0, sqrt(x), sqrt(-x))


###################################################
### code chunk number 31: Functions: conditional statements- switch
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
### code chunk number 32: Functions: For loop
###################################################
for(i in 1:5) print(i^2)


###################################################
### code chunk number 33: Functions: statements- while loop
###################################################
x <- 1
y <- 16
while (x^2 < y)
  { cat(x,"squared is ", x^2, "\n")   # print x and sq(x)
    x <- x+1
  }


###################################################
### code chunk number 34: apply example
###################################################
summary(women)
colMeans(women)
apply(women, 2, mean)

testEq<-all(rowMeans(women)== apply(women,1,mean))
print(testEq)
if (testEq)  print("rowMeans is equivalent to apply(df, 1, mean)")


###################################################
### code chunk number 35: colSd
###################################################
colSd<-function(df) apply(df, 2, sd)
colSd(women)


###################################################
### code chunk number 36: tapply
###################################################
women$age<-sample(30:39, 15, replace=TRUE)
ageSplit<- ifelse(women$age<35, "under35", "over35")
print(ageSplit)
tapply(women$weigh, ageSplit, length)
tapply(women$weigh, ageSplit, summary)


###################################################
### code chunk number 37: Functions: apply lapply sapply tapply
###################################################
myList <- list(ToothGrowth=TG, WomenAge= women$age, beta = exp(-3:3), logicalVec = c(TRUE,FALSE,FALSE,TRUE))
# compute the list mean for each list element
res1=lapply(myList, length)
print(res1)
print(paste("Class of res1:", class(res1)))

res2=sapply(myList, length)
print(res2)
print(paste("Class of res2:", class(res2)))


###################################################
### code chunk number 38: Functions: parameters
###################################################
prob <- c(0.5, 0.9, 0.95, 0.975, 0.99)
args(qnorm)
qnorm(prob)
qnorm(prob, 2)
qnorm(prob, mean=2, sd=1)


###################################################
### code chunk number 39: Functions: Equivalent functions
###################################################
prob <- c(0.5, 0.9, 0.95, 0.975, 0.99)
args(qnorm)
qnorm(p=prob, mean=0, sd=1, lower.tail = TRUE, log.p= FALSE)
qnorm(prob, 0, 1, log=FALSE, low=TRUE)
qnorm(prob, 0, 1, TRUE, FALSE)


###################################################
### code chunk number 40: Functions: formal local and free parameters
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
### code chunk number 41: Functions: parameters and lexical scope
###################################################
cube <- function(n) {
    sq <- function() n*n
              n*sq()
}
cube(2)
n <- 4
cube(2)


###################################################
### code chunk number 42: RSession
###################################################
save(women, file="women.RData")
save.image(file="entireL2session.RData")


###################################################
### code chunk number 43: load RSession
###################################################
rm(women)
ls(pattern="women")
load("women.RData")
ls(pattern="women")


###################################################
### code chunk number 44: history (eval = FALSE)
###################################################
## history()
## help(history)
## history(100)


###################################################
### code chunk number 45: SearchHistory (eval = FALSE)
###################################################
## history(pattern="save")


###################################################
### code chunk number 46: savehistory (eval = FALSE)
###################################################
## savehistory(file="L2.Rhistory")


