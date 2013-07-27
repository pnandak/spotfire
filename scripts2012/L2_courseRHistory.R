## Quiz 

c(1:50, seq(1,11,2))

## Assignment Operator
a<-c(1:50, seq(1,11,2))
a=c(1:50, seq(1,11,2))
a = c(1:50, seq(1,11,2))
c(1:50, seq(1,11,2)) -> a
a
length(a)
a<-seq(5,100,5)
length(a)
a
a[4,8]


matrix(c(1:20), ncol=5)
matrix(c(1:20), ncol=5, byrow=TRUE)
myMat<-matrix(c(1:20), ncol=5)
myMata
myMat
a
myList<-list(a=a, myMatrix=myMat)
myList[[2]][2,-1]
myList
myList<-list(a, myMat)
myList
myList[[2]]
myList[[2]][2,]
myList[[2]][2,-1]
myList[[2]][2,-c(1,3)]
myList[[2]][2,-1:3]
myList[[2]][2,1:3]
myList[[2]][2,-c(1:3)]
a
a<=30
sum(a<=30)
table(a<=30)
(a<=30)*1
(a<=30)+1
LETTERS
letters
letters[1:3]
rep(letters[1:3], each=3)
rep(letters[1:3], 3)
myVec<-rep(LETTERS[1:3], each=3)
myVec
myVec[3]
myVec<-myVec[3]
myVec
myVec<-rep(LETTERS[1:3], each=3)
factor(myVec)

## Lecture Notes
## List objects workspace
ls()

## remove element from workspace
rm(a)

## Data within R
data(women)
women

## Reading data into R
ToothGrowth <- read.csv("P:/Bio503_L1/L2/ToothGrowth.csv")
View(ToothGrowth)
ToothGrowth <- read.delim("P:/Bio503_L1/L2/ToothGrowth.txt")
View(ToothGrowth)
?read.delim

## Libaries loaded into workspace
search()
sessionInfo()

## Library not loaded into workspace
clara
library(cluster)
clara
search()
sessionInfo()
detach("package:cluster")

## Browsing Data.Frame loaded by read.table()
ls()
ToothGrowth
summary(ToothGrowth)
str(ToothGrowth)
class(ToothGrowth$Tooth.Length)
class(ToothGrowth$Treatment)
class(ToothGrowth$Dose)
ToothGrowth$Treatment

## By default read.table will read vectors with repeated elements as Factors
plot(ToothGrowth$Tooth.Length~ToothGrowth$Treatment)
ToothGrowth <- read.table("P:/Bio503_L1/L2/ToothGrowth.txt", as.is=FALSE, sep="\t")
class(ToothGrowth$Treatment)
summary(ToothGrowth)
ToothGrowth <- read.table("P:/Bio503_L1/L2/ToothGrowth.txt", as.is=FALSE, sep="\t", header=TRUE)
summary(ToothGrowth)
str(ToothGrowth)
ToothGrowth <- read.table("P:/Bio503_L1/L2/ToothGrowth.txt", as.is=FALSE, sep="\t", header=TRUE)
ToothGrowth$Treatment
ToothGrowth <- read.table("P:/Bio503_L1/L2/ToothGrowth.txt", as.is=FALSE, sep="\t", header=TRUE)
ToothGrowth <- read.table("P:/Bio503_L1/L2/ToothGrowth.txt", as.is=FALSE, sep="\t", header=TRUE)
tt <- read.table("P:/Bio503_L1/L2/ToothGrowth.txt", as.is=FALSE, sep="\t", header=TRUE)
str(tt)
tt <- read.table("P:/Bio503_L1/L2/ToothGrowth.txt", as.is=FALSE, sep="\t")
str(tt)

## Using scan
tt <- scan("P:/Bio503_L1/L2/ToothGrowth.txt", sep="\t", what="text")
tt

## Using cat
##Create a vector
myVec<-c(LETTERS[1:20], seq(0,200,10))
?LETTERS

cat(myVec, file="myVec.txt")
dir()
cat(myVec, file='myVec.txt')
dir()
getwd()

## Using Scan to read the file
scan("myVec.txt",what="text")
scan("myVec.txt",what=1)
scan("myVec.txt",what=1576783463)
scan("myVec.txt",what="hjhfdjdg")
scan("myVec.txt",what="hjhfdjdg")
scan("myVec.txt",what=TRUE)
scan("myVec.txt",what=FALSE)
scan("myVec.txt",what=123)
sink("mySinkFIle.txt")

ls()
getwd()
dim(women)
women

## Writing data out, append
write.table(women, file="women.txt")
write.table(women, file="women.txt", append=TRUE)
cat("More about Women", file="women.txt", append=TRUE)
write(women[,1], file="women.txt", append=TRUE)


## Using R2HTML
install.packages("R2HTML")
library(R2HTML)
myPath=getwd()
HTML(df1,file="results.html")
# Capture output to a webpage
HTMLStart(outdir=myPath, filename="Web_Results", echo=TRUE)
print("Capturing Output")
df1[1:2,]
summary(df1)
print("hello and Goodbye")
HTMLStop()


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
mypath=getwd()
HTML(df1,file="results.html")
# Capture output to a webpage
HTMLStart(outdir=myPath, filename="Web_Results", echo=TRUE)
print("Capturing Output")
df1[1:2,]
summary(df1)
print("hello and Goodbye")
HTMLStop()
library(R2HTML)
myPath=getwd()
HTML(df1,file="results.html")
# Capture output to a webpage
HTMLStart(outdir=myPath, filename="Web_Results", echo=TRUE)
print("Capturing Output")
df1[1:2,]
summary(df1)
print("hello and Goodbye")
HTMLStop()

## Using HTMLStart
HTMLStart("myfile.html", echo=TRUE)
HTMLStart(filename="myfile.html", echo=TRUE)
ls()
women
dir()
?ls
HTMLStop()

## Another quick example
HTMLStart(outdir = getwd(),filename="myfile.html", echo=TRUE)
ls()
women
HTMLStop()



## Reading tables from Wikipedida
install.packages("XML")
library(XML)

## read page, how many tables are in the page
worldPop<-readHTMLTable("http://en.wikipedia.org/wiki/World_population")
worldPop
length(worldPop)
names(worldPop)

## Reading specific wiki table (table 12)
worldPop<- readHTMLTable("http://en.wikipedia.org/wiki/World_population", which=12)
worldPop
worldPop
worldPop<- readHTMLTable("http://en.wikipedia.org/wiki/World_population", which=12)
worldPop$Region<-sub("\\[Note 1\\]", "", worldPop$Region)
rownames(worldPop)<-worldPop
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
worldPop

## Plot population data
nCt= ncol(worldPop)
plot(worldPop[,1], type="n", ylab="Population (Billions", xlab="year",xaxt="n")
axis(1, 1:nrow(worldPop), rownames(worldPop), las=2)
for (i in 1:nCt) lines(worldPop[,i], col=i, lwd=2)
legend("topleft", colnames(worldPop), fill=1:nCt)
nCt= ncol(worldPop)
plot(worldPop[,1], type="n", ylab="Population (Billions", xlab="year",xaxt="n")
axis(1, 1:nrow(worldPop), rownames(worldPop), las=2)
for (i in 1:nCt) lines(worldPop[,i], col=i, lwd=2)
legend("topleft", colnames(worldPop), fill=1:nCt)


## Introduction to Functions

addXY<-function(x,y) {x+y}
addXY(x=12,y=13)
meanXY<-function(x,y) {(x+y)/2}
meanXY(x=12,y=13)
statXY<-function(x) {sum(x)/length(x)}
statsXY(1:20)
statXY(1:20)

## My new Function
statXY<-function(x) {
  muX<-sum(x)/length(x)
  sdX<-sd(x)
  minX<-min(x)
  maxX<-max(x)
  medX<-median(x)
  myRes<-data.frame(mean=muX, sd=sdX, min=minX, max=maxX, median=medX)
  print("summary of X")
  print(myRes)
}

statXY(1:10)
statXY(1:100)
statXY(rnorm(100))
statXY(rnorm(1000))


##  S a v i n g   R    s e  s s i o n ,   Q u i t i n g   R
q()
ls()
save.image("L2.RData")
savehistory("L2.Rhistory")
