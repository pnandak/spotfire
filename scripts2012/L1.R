### R code from vignette source 'L1.rnw'

###################################################
### code chunk number 1: Working Dir
###################################################
getwd()


###################################################
### code chunk number 2: Listing Files in your local working directory
###################################################
dir()
list.files()
dir(pattern=".txt")


###################################################
### code chunk number 3: files (eval = FALSE)
###################################################
## ## Create an empty file
## file.create("test.txt")
## 
## ## Check a file exists
## file.exists("test.txt")
## 
## # System Information on the file
## file.info("test.txt")
## 
## # Open a file
## file.show("test.txt")
## 
## # Basic method to read in a file, we will cover more in Lecture 2
## scan('test.txt')
## 
## # File.path is very useful to create a platform independent path
## tmpfile<-file.path(getwd(), "L1.R")
## 
## # Given a path, drop the directory or file information
## basename(tmpfile)
## dirname(tmpfile)


###################################################
### code chunk number 4: help (eval = FALSE)
###################################################
## help(lm)
## ?matrix
## apropos("mean")
## example(rep)
## demo(graphics)


###################################################
### code chunk number 5: Searching R help pages (eval = FALSE)
###################################################
## help.search("combination")
## help.start()
## 
## # Or search the R website
## RSiteSearch("combination")


###################################################
### code chunk number 6: View library loaded into workspace
###################################################
search()
sessionInfo()


###################################################
### code chunk number 7: R libraries
###################################################
library()


###################################################
### code chunk number 8: Install (eval = FALSE)
###################################################
## #Installing and updating R libraries
## install.packages("XML")
## update.packages("XML")


###################################################
### code chunk number 9: R libraries (packages) (eval = FALSE)
###################################################
## library(XML)  # Load a package
## ## Or the alternative
## require(XML)
## 
## sessionInfo() #List all packages loaded in the current R session
## library()  # List all installed packages
## data()      # List all available example datasets


###################################################
### code chunk number 10: Unload package (eval = FALSE)
###################################################
## detach(package:XML)
## search()


###################################################
### code chunk number 11: Help on R library
###################################################
library(help=XML)


###################################################
### code chunk number 12: R datasets
###################################################
data()


###################################################
### code chunk number 13: R datasets: Women
###################################################
data(women)
ls()
ls(pattern="w")


###################################################
### code chunk number 14: R as a calculator
###################################################
2+2
2*2
2*100/4
2*100/4+2
2*100/(4+2)
2^10
log(2)
tmpVal<-log(2)
tmpVal
exp(tmpVal)
rnorm(5)


###################################################
### code chunk number 15: hist (eval = FALSE)
###################################################
## history()


###################################################
### code chunk number 16: Comparision operators
###################################################
       1 == 1 


###################################################
### code chunk number 17: more on logical operators (boolean AND)
###################################################
   x <- 1:10; y <- 10:1 
   x > y & x > 5 


###################################################
### code chunk number 18: more on logical operators- boolean OR
###################################################
            x == y | x != y 


###################################################
### code chunk number 19: more on logical operators- boolean NOT
###################################################
            !x > y 


###################################################
### code chunk number 20: Basic operations on numeric vectors
###################################################
x.vec <- 1:10
x.vec
y.vec <- seq(1,10,by=1)
y.vec
# Element-wise multiplication
x.vec * y.vec

# Note if one vector is shorter it will recycle
x.vec * 2

# Element-wise power
x.vec^y.vec


###################################################
### code chunk number 21: Basic operations on matices
###################################################
xMat <- matrix(x.vec, nrow=2)
yMat <- matrix(rep(1:5,rep(2,5)), nrow=2)


###################################################
### code chunk number 22: Basic operations on matices: multiplication2
###################################################
# Not matrix multiplication
xMat * yMat

# matrix multiplication
xMat %*% t(yMat)
t(xMat) %*% yMat


###################################################
### code chunk number 23: dimMat
###################################################
dim(xMat)
dim(yMat)


###################################################
### code chunk number 24: Basic operations on matices: multiplication (eval = FALSE)
###################################################
## xMat %*% yMat


###################################################
### code chunk number 25: Listing objects within an R session or workspace. Objects in memory
###################################################
 objects()
 ls()


###################################################
### code chunk number 26: Vectors: Creating
###################################################
vec1<-1:10
vec2<-LETTERS[1:10]
vec3<-vec2=="D"
vec3


###################################################
### code chunk number 27: Matrices: Creating
###################################################
mat1<-matrix(vec1, ncol=2, nrow=5)
print(mat1)
dim(mat1)
colnames(mat1) = c("A","B")
rownames(mat1)= paste("N", 1:5, sep="")
print(mat1)


###################################################
### code chunk number 28: List: Creating
###################################################
a<-20
newList1<-list(a, vec1, mat1)
print(newList1)
newList1<-list(a=a, vec=vec1, mat=mat1)
print(newList1)


###################################################
### code chunk number 29: data.frame: Creating a data.frame from a matrix using as.data.frame
###################################################
df1<-as.data.frame(mat1)
df1


###################################################
### code chunk number 30: factor: Creating
###################################################
charVec<-rep(LETTERS[1:3],10)
print(charVec)
table(charVec) # Tabulate charVec
fac1<-factor(charVec)
print(fac1)
attributes(fac1)
levels(fac1)


###################################################
### code chunk number 31: All objects have attributes Mode and length
###################################################
	x<-3
	mode(x)   # Numeric
	x<-"apple"
	mode(x)   # Charachter
	x<-3.145
	x+2               # 5.145
	x==2              # FALSE, logical
	x <- x==2
	x 
	mode(x)
    x<-1:10
    mode(x)
    x<-LETTERS[1:5]
    mode(x)
    x<-matrix(rnorm(50), nrow=5, ncol=10)
    mode(x)


###################################################
### code chunk number 32: Attributes of a matrix
###################################################
	x <- matrix(5:14, nrow=2, ncol=5)
	x
	attributes(x)


###################################################
### code chunk number 33: Creating numerical data using rep seq. Using rbind and cbind to generate matrices
###################################################
# Vector
x.vec <- seq(1,7,by=2)

# The function seq is very useful, have a look at the help on seq (hint ?seq)

names(x.vec) <- letters[1:4]
# Matrices
xMat <- cbind(x.vec, rnorm(4), rep(5, 4))
yMat <- rbind(1:3, rep(1, 3))
z.mat <- rbind(xMat, yMat)
# Data frame
x.df <- as.data.frame(xMat)
names(x.df) <- c('ind', 'random', 'score')


###################################################
### code chunk number 34: Accessing elements in a vector or matrix. Subsetting
###################################################
# Access first element of 'x.vec'
x.vec[1]
# or if you know the name
x.vec['a']
# Access an element of 'xMat' in the second row, third column
xMat[2,3]
# Display the second and third columns of matrix 'xMat'
xMat[,c(2:3)]
# or
xMat[,-c(1)]
# What does this command do?
xMat[xMat[,1]>3,]
# Get the vector of 'ind' from 'x.df'
x.df$ind
x.df[,1]


###################################################
### code chunk number 35: Modifying or replacing elements in a matrix
###################################################
# Change the element of 'xMat' in the third row and first column to '6'
xMat[3,1] <- 6
# Replace the second column of 'z.mat' by 0's
z.mat[,2] <- 0


###################################################
### code chunk number 36: Sorting elements in a vector or matrix: Using sort order
###################################################
# Simplest 'sort'
z.vec <- c(5,3,8,2,3.2)
sort(z.vec)
order(z.vec)
# Sorting the rows of a matrix
# We will use an example dataset in R called ChickWeight
# First have a look at the ChickWeight documentation (help)
?ChickWeight
ChickWeight[1:2,]
chick.short <- ChickWeight[1:36,]
chick.srt <- chick.short[order(chick.short$Time,chick.short$weight),]
chick.srt[1:2,]
chickOrd<-chick.short[order(chick.short$weight),]


###################################################
### code chunk number 37: more on subsetting vectors
###################################################
x1 <- c(1.2, 3.4, 4.8, 6.7, 10, -2, -5)
x1[-2]
-x1[2]


###################################################
### code chunk number 38: Extract elements 2 and 5
###################################################
x1[c(2,5)]  # Extract elements 2 and 5


###################################################
### code chunk number 39: Extract all elements except 2 and 5
###################################################
x1[-c(2,5)]  # Extract all elements except 2 and 5


###################################################
### code chunk number 40: more on subsetting matrices
###################################################
xMat <- matrix(3:11, nrow=3)
# Vector of indices


###################################################
### code chunk number 41: Conditional subsetting
###################################################
xMat[,1] > 3   # Conditional subsetting. 
xMat[xMat[,1]>3,]


###################################################
### code chunk number 42: Matrix of indices
###################################################
# Matrix of indices
row(xMat)>col(xMat)
xMat[row(xMat)>col(xMat)] <- 0
xMat


###################################################
### code chunk number 43: Note on lists
###################################################
course.info <- list(title = 'Bio503:Intro to R',
                    first = c('Culhane', 'Aedin'),

                    lectures = c(4,6,11,13,18),
                    time= rep("1:30-4:30", 5),
                    days= c(rep(c("Wed", "Fri"),2),"Fri")
                   )


str(course.info)
course.info[[2]]
course.info[[4]][1]
course.info[["lectures"]][3]
course.info[["days"]][3]



###################################################
### code chunk number 44: MissingValues
###################################################
z <- c(1:3,NA)
z
ind <- is.na(z)
ind


###################################################
### code chunk number 45: RmMissingvalues
###################################################
print(z)
x<-z[!is.na(z)]
print(x)


###################################################
### code chunk number 46: Creating empty vector and matrices
###################################################
x1 <- numeric()
x2 <- numeric(5)
x1.mat <- matrix(0,nrow=10,ncol=3)


###################################################
### code chunk number 47: Introduction to Functions
###################################################
rnorm(50)
rnorm(n=50)
args(rnorm)  # See function arguments 
rnorm(50,10)


###################################################
### code chunk number 48: Function arguments
###################################################
mean
args(mean)
example(mean)


###################################################
### code chunk number 49: R functions: Viewing code
###################################################
 mean
 ## This doesn't show us the code, therefore use methods()
 methods(mean)
 ## Now we see that there are different mean methods depending on the class of the object
 mean.default


###################################################
### code chunk number 50: Assignment
###################################################
2*5^2
x <- 2*5^2
print(x)


###################################################
### code chunk number 51: Assignment operators
###################################################
  2*5^2
  y <- 2*5^2
  z<-2*5^2
  2*5^2  -> z
  print(y)
  x==y
  y==z      


