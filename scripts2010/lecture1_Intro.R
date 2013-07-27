###################################################
### chunk number 1: Working Dir
###################################################
getwd()


###################################################
### chunk number 2: Listing Files in your local working directory
###################################################
dir()
dir(pattern=".txt")


###################################################
### chunk number 3: Installing and Updating R packages eval=FALSE
###################################################
## #Installing and updating R libraries
## install.packages("MASS")
## update.packages("MASS")


###################################################
### chunk number 4: R libraries (packages) eval=FALSE
###################################################
## library()  # List all installed packages
## library(MASS)  # Load a package
## data()      # List all available example datasets


###################################################
### chunk number 5: R Help eval=FALSE
###################################################
## help(lm)
## ?matrix
## apropos("mean")
## example(rep)


###################################################
### chunk number 6: Searching R help pages eval=FALSE
###################################################
## help.search("combination")
## help.start()


###################################################
### chunk number 7: R as a calculator
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
history()


###################################################
### chunk number 8: Comparision operators
###################################################
       1 == 1 


###################################################
### chunk number 9: more on logical operators (boolean AND)
###################################################
   x <- 1:10; y <- 10:1 
   x > y & x > 5 


###################################################
### chunk number 10: more on logical operators- boolean OR
###################################################
            x == y | x != y 


###################################################
### chunk number 11: more on logical operators- boolean NOT
###################################################
            !x > y 


###################################################
### chunk number 12: Assignment
###################################################
2*5^2
x <- 2*5^2
print(x)


###################################################
### chunk number 13: Assignment operators
###################################################
  2*5^2
  y <- 2*5^2
  z<-2*5^2
  2*5^2  -> z
  print(y)
  x==y
  y==z      


###################################################
### chunk number 14: Comments
###################################################
  print(y) # Here is a comment


###################################################
### chunk number 15: Listing objects within an R session or workspace. Objects in memory
###################################################
 objects()
 ls()


###################################################
### chunk number 16: Vectors: Creating
###################################################
vec1<-1:10
vec2<-LETTERS[1:10]
vec3<-vec2=="D"
vec3


###################################################
### chunk number 17: Matrices: Creating
###################################################
mat1<-matrix(vec1, ncol=2, nrow=5)
print(mat1)
dim(mat1)
colnames(mat1) = c("A","B")
rownames(mat1)= paste("N", 1:5, sep="")
print(mat1)


###################################################
### chunk number 18: List: Creating
###################################################
a<-20
newList1<-list(a, vec1, mat1)
print(newList1)
newList1<-list(a=a, vec=vec1, mat=mat1)
print(newList1)


###################################################
### chunk number 19: data.frame: Creating a data.frame from a matrix using as.data.frame
###################################################
df1<-as.data.frame(mat1)
df1


###################################################
### chunk number 20: factor: Creating
###################################################
charVec<-rep(LETTERS[1:3],10)
print(charVec)
table(charVec) # Tabulate charVec
fac1<-factor(charVec)
print(fac1)
attributes(fac1)
levels(fac1)


###################################################
### chunk number 21: All objects have attributes Mode and length
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
### chunk number 22: Attributes of a matrix
###################################################
	x <- matrix(5:14, nrow=2, ncol=5)
	x
	attributes(x)


###################################################
### chunk number 23: Creating numerical data using rep seq. Using rbind and cbind to generate matrices
###################################################
# Vector
x.vec <- seq(1,7,by=2)
names(x.vec) <- letters[1:4]
# Matrices
xMat <- cbind(x.vec, rnorm(4), rep(5, 4))
yMat <- rbind(1:3, rep(1, 3))
z.mat <- rbind(xMat, yMat)
# Data frame
x.df <- as.data.frame(xMat)
names(x.df) <- c('ind', 'random', 'score')


###################################################
### chunk number 24: Accessing elements in a vector or matrix. Subsetting
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
### chunk number 25: Modifying or replacing elements in a matrix
###################################################
# Change the element of 'xMat' in the third row and first column to '6'
xMat[3,1] <- 6
# Replace the second column of 'z.mat' by 0's
z.mat[,2] <- 0


###################################################
### chunk number 26: Sorting elements in a vector or matrix: Using sort order
###################################################
# Simplest 'sort'
z.vec <- c(5,3,8,2,3.2)
sort(z.vec)
order(z.vec)
# Sorting the rows of a matrix
# We will use an example dataset in R called ChickWeight

ChickWeight[1:2,]
chick.short <- ChickWeight[1:36,]
chick.srt <- chick.short[order(chick.short$Time,chick.short$weight),]
chick.srt[1:2,]
chickOrd<-chick.short[order(chick.short$weight),]


###################################################
### chunk number 27: Note on lists
###################################################
course.info <- list(title = 'Bio503:Intro to R',
                    first = c('Culhane', 'Aedin'),
                    lectures = c(7,9,14,16,24),
                    time= rep("1:30-4:30", 5),
                    days= c(rep(c("Mon", "Wed"),2), "Thurs"),
                    assnt = c(9,16,24))

course.info
course.info[[2]]
course.info[[4]][1]
course.info[["lectures"]][3]
course.info[["days"]][3]



###################################################
### chunk number 28: Creating empty vector and matrices
###################################################
x1 <- numeric()
x2 <- numeric(5)
x1.mat <- matrix(0,nrow=10,ncol=3)


###################################################
### chunk number 29: more on subsetting vectors
###################################################
x1 <- c(1.2, 3.4, 4.8, 6.7, 10, -2, -5)
x1[-2]
-x1[2]
x1[c(2,5)]  # Extract elements 2 and 5
x1[-c(2,5)]  # Extract all elements except 2 and 5


###################################################
### chunk number 30: more on subsetting matrices
###################################################
xMat <- matrix(3:11, nrow=3)
# Vector of indices
xMat[,1] > 3   # Conditional subsetting. 
xMat[xMat[,1]>3,]
# Matrix of indices
row(xMat)>col(xMat)
xMat[row(xMat)>col(xMat)] <- 0
xMat


###################################################
### chunk number 31: Basic operations on numeric vectors
###################################################
x.vec <- seq(5,8,by=0.5)
y.vec <- (1:length(x.vec))/10
# Element-wise multiplication
x.vec * y.vec
# Element-wise power
x.vec^y.vec


###################################################
### chunk number 32: Basic operations on matices
###################################################
xMat <- matrix(-10:10, nrow=7)
yMat <- matrix(rep(1:7,rep(3,7)), nrow=3)


###################################################
### chunk number 33: Basic operations on matices: multiplication eval=FALSE
###################################################
## xMat * yMat
## xMat %*% yMat
## t(xMat) * yMat


###################################################
### chunk number 34: Introduction to Functions
###################################################
rnorm(50)
rnorm(n=50)
args(rnorm)  # See function arguments 
rnorm(50,10)


###################################################
### chunk number 35: Function arguments
###################################################
median
args(median)


###################################################
### chunk number 36: R functions: Viewing code
###################################################
 mean
 methods(mean)
 mean.default


