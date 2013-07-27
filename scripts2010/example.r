# Create Dataset

# Vectors

x<-c(2,4,6,8,10)
x
#[1]  2  4  6  8 10

y<-c(1,2,3,4,5)
y
#[1] 1 2 3 4 5

# Column Bind
new1<-cbind(x,y)
new1
#      x y
#[1,]  2 1
#[2,]  4 2
#[3,]  6 3
#[4,]  8 4
#[5,] 10 5

# Row Bind
new2<-rbind(x,y)
new2
#  [,1] [,2] [,3] [,4] [,5]
#x    2    4    6    8   10
#y    1    2    3    4    5

# Column Bind new1 and x
new3<-cbind(new1,x)
#      x y  x
#[1,]  2 1  2
#[2,]  4 2  4
#[3,]  6 3  6
#[4,]  8 4  8
#[5,] 10 5 10

# Create a vector with a sequence of 1 to 4 by 0.5
seq1<-seq(1,4,by=.5)
seq1
#[1] 1.0 1.5 2.0 2.5 3.0 3.5 4.0

# Create a vector with a sequence from 2 to 4 by 1
seq2<-seq(2,4)
seq2
#[1] 2 3 4

# Another way to create seq2
seq2<-c(2:4)
seq2
#[1] 2 3 4

# Create a vector that repeats seq2 4 times
rep1<-rep(seq2,4)
rep1
#[1] 2 3 4 2 3 4 2 3 4 2 3 4

# Create a matrix from rep1 with 4 columns and 3 rows
Mrep1<-matrix(rep1,ncol=4)
Mrep1
#     [,1] [,2] [,3] [,4]
#[1,]    2    2    2    2
#[2,]    3    3    3    3
#[3,]    4    4    4    4

# Create a list with Mrep1 and x
list1<-list(Mrep1=Mrep1,x=x)
list1
#$Mrep1
#     [,1] [,2] [,3] [,4]
#[1,]    2    2    2    2
#[2,]    3    3    3    3
#[3,]    4    4    4    4

#$x
#[1]  2  4  6  8 10

# Select second element of x
x[2]
#[1] 4

# Select 2 through 4 element of x
x[2:4]
#[1] 4 6 8

# Select first Row and column of new1
new1[1,1]
#[1] 2

# Select first Row of new1
new1[1,]
#x y
#2 1


# Select second Column of new1
new1[,2]
#[1] 1 2 3 4 5

new1$y
#[1] 1 2 3 4 5

# Select Column 1 and rows 2 to 3 of new1
new1[2:3,1]
#[1] 4 6

# Select Mrep1 from list1
list1$Mrep1
#     [,1] [,2] [,3] [,4]
#[1,]    2    2    2    2
#[2,]    3    3    3    3
#[3,]    4    4    4    4

# Sum first row of new1
sum(new1[1,])
#[1] 3

# Calculate mean of each row of new1
apply(new1,1,mean)
#[1] 1.5 3.0 4.5 6.0 7.5

# Calculate sum of each column of new1
apply(new1,2,sum)
# x  y
#30 15

# Calculate summaries of each column of new1
summary(new1)
#       x            y
# Min.   : 2   Min.   :1
# 1st Qu.: 4   1st Qu.:2
# Median : 6   Median :3
# Mean   : 6   Mean   :3
# 3rd Qu.: 8   3rd Qu.:4
# Max.   :10   Max.   :5

# Add, Subtract, Multiply and Divide Vectors element by element
x
#[1] 2 4  6  8 10
y
#[1] 1 2  3  4  5

x+y
#[1] 3 6  9 12 15

x-y
#[1] 1 2  3  4  5

x*y
#[1] 2 8 18 32 50

x/y
#[1] 2 2  2  2  2

# Each Element of x to the 1/3 power to 2 decimal places
round(x^(1/3),2)
#[1] 1.26 1.59 1.82 2.00 2.15

# Square Root of each element of y to 3 decimal places
round(sqrt(y),3)
#[1] 1.000 1.414 1.732 2.000 2.236


plot(x=new1[,1],y=new1[,2])
plot(x=new1[,1],y=new1[,2],type="l",main="X and Y Plot",xlab="X",ylab="Y")

# Import house_elections.dat from Lab 1
house<-read.table("C:/stat155/house_elections.dat",sep=" ",header=T)

write.table(x=new1,file="C:/stat155/new1.txt",sep=",",row.names=F)

# 5 obs from a normal with mean 2 and standard deviation 1
rnorm(5,2,1)
#[1] 1.8180562 0.8011290 1.3340688 0.8291908 1.6088192

# 2 obs from a poisson with mean 3
rpois(2,3)
#[1] 2 4

# 3 obs from a uniform over values 0 to 2
runif(3,0,2)
#[1] 0.926057 1.969269 1.038766

# 4 obs from a binomial with sample size 10 and probability of success 0.2
rbinom(4,10,0.2)
#[1] 1 1 2 4

# Subject ID for 5 Subjects observed 4 times each
Subject<-rep(c(1:5),4)
Subject
# [1] 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5

# Treatment for the subject at each time
trt<-rep(c(1,2),10)
trt
# [1] 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2

# Generate Random Outcomes from N(0,1)
# for all subject and treatments (Round to 2 decimals)
out1<-round(rnorm(20,0,1),2)
out1
# [1] -0.14  0.28  1.20 -1.46  0.74  0.25  0.91  0.33 -1.04 -0.53  2.56 -1.60
#[13] -0.96 -0.34 -0.57 -0.51 -0.37  1.84  0.94 -0.82

# Generate Random Outcomes from Binom(10,.2)
out2<-rbinom(20,10,.2)
out2
# [1] 0 2 1 3 3 1 2 2 3 3 1 2 2 3 3 1 1 3 4 2

data1<-cbind(Subject,trt,out1,out2)
data1
#      Subject trt  out1 out2
# [1,]       1   1 -0.14    0
# [2,]       2   2  0.28    2
# [3,]       3   1  1.20    1
# [4,]       4   2 -1.46    3
# [5,]       5   1  0.74    3
# [6,]       1   2  0.25    1
# [7,]       2   1  0.91    2
# [8,]       3   2  0.33    2
# [9,]       4   1 -1.04    3
#[10,]       5   2 -0.53    3
#[11,]       1   1  2.56    1
#[12,]       2   2 -1.60    2
#[13,]       3   1 -0.96    2
#[14,]       4   2 -0.34    3
#[15,]       5   1 -0.57    3
#[16,]       1   2 -0.51    1
#[17,]       2   1 -0.37    1
#[18,]       3   2  1.84    3
#[19,]       4   1  0.94    4
#[20,]       5   2 -0.82    2

# Calculate mean out1 and out2 for each subject and treatment combination
aggregate(data1[,3:4],by=list(Subject=Subject,trt=trt),mean)
#   Subject trt   out1 out2
#1        1   1  1.210  0.5
#2        2   1  0.270  1.5
#3        3   1  0.120  1.5
#4        4   1 -0.050  3.5
#5        5   1  0.085  3.0
#6        1   2 -0.130  1.0
#7        2   2 -0.660  2.0
#8        3   2  1.085  2.5
#9        4   2 -0.900  3.0
#10       5   2 -0.675  2.5

# Calculate sum out1 and out2 for each subject combination
aggregate(data1[,3:4],by=list(Subject=Subject),sum)
#  Subject  out1 out2
#1       1  2.16    3
#2       2 -0.78    7
#3       3  2.41    8
#4       4 -1.90   13
#5       5 -1.18   11


# Sorts the vector out1
sort(out1)
# [1] -1.60 -1.46 -1.04 -0.96 -0.82 -0.57 -0.53 -0.51 -0.37 -0.34 -0.14  0.25
#[13]  0.28  0.33  0.74  0.91  0.94  1.20  1.84  2.56

# Returns the a vector of the order of out1
order(out1)
# [1] 12  4  9 13 20 15 10 16 17 14  1  6  2  8  5  7 19  3 18 11

# Sort data1 by the ascending order of out1
data1[order(out1),]
#      Subject trt  out1 out2
# [1,]       2   2 -1.60    2
# [2,]       4   2 -1.46    3
# [3,]       4   1 -1.04    3
# [4,]       3   1 -0.96    2
# [5,]       5   2 -0.82    2
# [6,]       5   1 -0.57    3
# [7,]       5   2 -0.53    3
# [8,]       1   2 -0.51    1
# [9,]       2   1 -0.37    1
#[10,]       4   2 -0.34    3
#[11,]       1   1 -0.14    0
#[12,]       1   2  0.25    1
#[13,]       2   2  0.28    2
#[14,]       3   2  0.33    2
#[15,]       5   1  0.74    3
#[16,]       2   1  0.91    2
#[17,]       4   1  0.94    4
#[18,]       3   1  1.20    1
#[19,]       3   2  1.84    3
#[20,]       1   1  2.56    1
