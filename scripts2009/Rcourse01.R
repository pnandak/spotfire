#
# Rcourse01.R
#
# Example code for lecture 2008-09-03 of "Modern Applied Statistics Using R"
#
# Alexander.Ploner@ki.se
#


#############################################################################
##                           Part 1: Concepts                              ##
#############################################################################

#-------- Evaluating numerical expressions ----------##

# Simple numerical expressions
17
17+26
3*14
210/5+0.42

# Mathematical functions
sqrt(27.04)
5.02^2
exp(2.303)
log(10)
log10(10)
log2(128)
# Combine ad lib
exp(abs(2.88*sin(12) + (tanh(-1))))
3*sin(12) + abs(tanh(-1))

# Extra: mathematical constants, complex numbers
exp(1)
pi
3+2i
Re(3+2i)
Im(2+3i)
exp(-pi*1i)

# Extra: special values
1/0
log(0)
log(0) + 17
log(0) + 1/0


##------- Character expressions -----------##

# Expressions
"a"
"this is text"

# Predefined constants
letters
LETTERS

# Extra: simple functions operating on characters
substr("abcd", start=2, stop=3)
paste("ab", "cd")           # Not quite: check ?paste
paste("ab", "cd", sep="")   # Now we're talking
nchar("abcd")

##------- Logical expressions -----------##

# Simple expressions
12 == 17
12 < 17
12 <= 17

# Composite logical expressions
(12 < 13) | (12 == 13)
(42 < 42) & (42 < 420)
!(12 == 12)
!(12 < 11)

##------- Storing expressions as objects --------##

# Assignment and evaluation
a <- 3
a
b = 7
b

# Using objects in expressions
a + b
2*a - b
a^b
first = "Alexander"
last  = "Ploner"
paste(first, last)
a < b
3*a < b

# Working with objects
ls()
rm(a)
ls()
a

# Extra: secret objects
.private = 23
ls()
ls(all=TRUE)
rm(.private)


##------- Vectors --------##

# Combing elements of the same mode
c(1,2,3,4)
c("a","b","c","d")
c(TRUE, FALSE, TRUE, TRUE)

# Vectors are objects, too
x = c(3.1, 4.2, 5.3, 6.4, 7.5, 8.6)
x
y = c(2.1, 2.2, 2.3, 8.2, 8.4, 8.6)
y
length(x)
length(y)

# Doing things with vectors
z = c(x, y)
z
sort(z)
sum(z)
sum(z)/length(z)


##---- Finally: some statistics -------##

# Numerical summaries
mean(x)
var(x)
sd(x)
summary(x)
min(y)
range(q)
quantile(y, 0.5)

# Simple plots
boxplot(x)
dotchart(y)
hist(z)

##-------- Generating regular vectors -------##

# Useful shorthands
1:50
-50:50
seq(from=1, to=100, by=2)
seq(from=1, by=2, length=50)
seq(from=100, by=-10, length=10)

# Repeating vectors
rep(1:4, 2)
rep(1:4, c(2,2,2,2))
rep(1:4, rep(2,4))

##------ Extracting elements of vectors -------##

# Simple integer indices
x[1]
x[-1]
third_element = y[3]
third_element
z[length(z)]

# Integer index vectors
x[c(1,4)]
y[1:3]
y[-(4:6)]

# Logical vector indices
ndx = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)
x[ndx]
y[ndx]

# Extra: using names/character vectors
xx = x
names(xx) = letters[1:6]
xx
xx[c("a","f")]

##------ Changing elements of vectors -------##

# Just move index to the left hand side
x[1] = 17
x
y[3] = -y[3]
ndx = seq(1,6,by=2)
ndx
x[ndx] = y[ndx]

##------ Vector arithmetic -------------##

# Vectors of same length: elementwise
first = 1:10
secnd = 101:110
first + secnd
first*secnd
first/secnd

# Vector and numbers: each element
first + 1
secnd*2
a = 13
secnd - a

# Vectors of different lengths: recycle
third = 11*(1:5)
third
first + third
secnd - third
# Even for non-multiple lengths
forth = seq(100, 300, by=100)
first + forth

# Crazily useful: vector valued functions
x = seq(0, 2*pi, length=100)
x
y = sin(x)
y
plot(x, y)
abs(y)
round(y)
exp(y)

# Extra: fun plots
z = cos(x)
plot(x, y, type="l")
lines(x, z, col="red")
# Or equivalently
curve(sin(x), 0, 2*pi, 100)
curve(cos(x), add=TRUE, col="red")

# Extra: vector magic
vec3 = 1:3
vec3
vec3[4]
vec3[4] = 123
vec3
vec3[6] = 321
vec3

##---------- Factors: dealing with categorical data ------##

# Unordered
GRP = rep(c("Treatment","Control"), c(10,10))
GRP
grp = factor(grp)
grp
table(grp)

# Extra: Ordered
grp2 = rep(c("Low","Medium","High"), rep(5,3))
ordered(grp2)  # Not quite
grp2 = ordered(grp2, levels=c("Low","Medium","High")) 
grp2           # Now we're talking

# Extra: some internals
unclass(grp)
unclass(grp2)


##---------- Matrices: vectors of the same mode ------##

# Rectangular arrangement of numbers
mat1 = matrix(1:16, nrow=4, ncol=4)
mat1

# Combining rows and columns
height = c(1.75, 1.83, 1.57, 1.74, 1.72, 1.89)
weight = c(67, 75, 54, 66, 66, 82)
mat2 = cbind(height, weight)
mat2
colnames(mat2)
rownames(mat2)
# Add an extra person 
mat2 = rbind(mat2, c(1.82, 75))

# Simple indexing of matrix elements
mat1[1,1]
mat1[4,4]
mat1[1,4]
mat1[ ,2]
mat1[1, ]

# Advanced indexing of matrix elements
mat1[1:3, ]
mat1[1:3, -4]
mat2[,1]
mat2[, "height"]
mat2[mat2[,1] < 1.75,]

# Assignment: just move it to the left hand side
mat1[1,1] = -1
mat1[,2] = rep(0.2, 4)
mat1


##---------- Data frames: combining vectors of different modes ----##

# Add sex to height, weight
sex = c(1,2,2,1,2,1)
sex = factor(sex, levels=1:2, labels=c("m","f"))
# Not really
cbind(height, weight, sex)
# That does it 
dd = data.frame(height, weight, sex)
dd

# Works like a matrix, almost
dd[1,1]
dd[1,2]
dd[1,3]
dd[,"weight"]
dd[-1, 1:2]

# ... but can do still more!
dd$weight
dd$height
dd$sex
dd$w
dd$s

##--------- Combining anything: lists -------------##

# Different modes
list1 = list(1:4, "a", rep(TRUE, 10))
list1

# Access elements
list1[[1]]
list1[[2]]
# As a list
list1[1]
list[1:2]

# NB. lists are recursive
list2 = list1
list2[[4]] = list(matrix(1:15, nrow=3), LETTERS[1:13], dd)
list2

# Names are useful
list3 = list(a=1:3, b=4:7, c=TRUE)
list3
list3[["c"]]
list3$c

##--------------- Classes and methods ------------##

# Same function for different objects
summary(z)
summary(grp)
# Depending on class
class(z)
class(grp)

# Extra methods:
summary
summary.default
summary.factor

##------------- Computation on the language ----------##

# Results are stored and processed just like data 
tab1 = table(grp)
tab1
length(tab1)
names(tab1)
tab1[1]
sum(tab1)

ss1 = summary(z)
ss1
ss1["Mean"]

