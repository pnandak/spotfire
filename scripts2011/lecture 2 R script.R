height <- c(1.75, 1.8, 1.65, 1.90,  1.74, 1.91)

weight <- c(60, 72, 57, 90, 95, 72)


pain.level = c("none", "mild", "medium", "severe")


tall = height >= 1.9

# & means "and"

avg = (height>1.7 & height <1.8)
avg
height

# ## pound sign means comment - don't run this line
# | means "or"

v1 = c(3,7,8)
v2 = c(9,10,11)
v1
v2
c(v1,v2,v1)

seq(1,10)
seq(1,100)
seq(1,10, by = 0.5)


1:10
50:40
25:1

rep(99,10)
rep(5,25)
rep(c(4,5), 7)

rep(1:3, 5)
99:70
1:5
rep(1,5)
c(1:5, rep(1,5))
rep(c(1:5, rep(1,5)),4)
v=seq(20,30,by=0.5)
v

which(v==24.0)
which(v==tall) ## vectors are different lengths
v==24.0
which(v==24.0)
tall
which(tall==TRUE)
which(tall)


mat <- 1:12
dim(mat)=c(3,4)
dim(mat)

mat2 = matrix(1:12, nrow=3, byrow=T)
mat2


my.mat = matrix(11:26, nrow=4)

my.mat + my.mat
my.mat - my.mat
my.mat - 1  

my.mat * my.mat
my.mat %*% my.mat
my.mat[1,3]
## to change the value of a my.mat[1,3] 
my.mat[1,3] = 100
my.mat[1,3]


v1 <- seq(10,5)
v2 <- v1^2
v2

mat1 = rbind(v1, v2)

mat1
v3 = v1^3
v4 = v1^4
mat2 <- rbind(mat1, v3, v4)

Q2 <- c(2,4,4,4,4,3,0,2)
Q2

fQ2 <- factor(Q2, levels=0:4)
levels(fQ2) <- c("never", "rarely", "sometimes", "often", "always")
fQ2

as.numeric(fQ2) # levelts recoded starting with 1

x=
# Print the 12th and 13th values as follows:
x[12:13]

# Print the 12th-20th values of x.
x[12:20]

###  Lecture 2 Lab

## Vectors

# Create a vector with values 0, 4, 8, 12, ..., 100; call it x
x=seq(0,100,by=4)
x

## retrieve the index for the value 52
which(x==52)

# print the 12th value of x.
x[12]

# Print the 12th and 13th values as follows: x[12:13]
 x[12:13]

# Print the 12th-20th values of x.
x[12:20]

# In one command, print the 5th, 10th, 15th, and 20th values of x.
x[c(5,10,15,20)] 

# or 
x[seq(5,20,by=5)]

## Matrices

# Create a matrix with values 10, 20, ..., 1000 with 10 rows, call it y
y=matrix(seq(10,1000,by=10),nrow=10)

# Display the third row

y[3,]

# Display the fourth and fifth columns
y[,4:5]

# Change the element in the 5th row, 5th column to 0.
y[5,5] = 0
y

# Type ?diagonal and try out this function.

# Display the diagonal elements of y
diag(y)

# Change the diagonal elements of y to be all 1's.
diag(y)=1
y


## Boolean variables

## Boolean variables contain TRUE and FALSE values.  R treats TRUE as 1 and FALSE as zero.
## Try this example: B = c(TRUE, TRUE, FALSE, FALSE, TRUE)

 B = c(TRUE, TRUE, FALSE, FALSE, TRUE)
 sum(B)

## sum(B)
## You can abbreviate TRUE with T and FALSE with F: B = c(T,T,F,F,T)
 B = c(T,T,F,F,T)
B

# Use rnorm to create a vector z, containing a sample of size 100 from a normal distribution with
# mean 20 and standard deviation 5
z=rnorm(100, 20, 5)

# Create a Boolean variable indicating which elements of z are greater than 23
bool1 = z>23

# Create a Boolean variable indicating which elements of z are less than 20
bool2 = x<20

# Create a Boolean variable called less.than.20 showing the indices for
#  the elements less than 20.
less.than.20 = which(bool2)

# How many elements are less than 20?
length(less.than.20)

