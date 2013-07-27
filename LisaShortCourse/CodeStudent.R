#######################################################################
#                 Data Structures and Manipulation
#######################################################################
# 1. Object creation:
#a. Expression
3+3
exp(2)
2*3
10/4
log(1)
#b. Assignment
a=3+3
b=exp(2)
c=2*3
d=10/4
a
b
c
d
ls() # Lists all the elemnts of the workspace
#rm() removes element from the workspace
rm(a)

# 2. Vectors
# Sequences.
vec1=1:10
vec2=1:-10
vec3=seq(0,5,by=.5)
vec4=seq(0,5,length=15)
# Vectors with no pattern
#Expression
c(1,2,3,4,5)
c(2,-1,7)
#Assignment
x=c(1,2,3,4,5)
y=c(2,-1,7,0,1)
#Vectors of characters
char=c("aa","bb", "cc", "dd")
pets=c(Bolt = "dog", Garfield = "cat", Sebastian = "crab")
#repeating values
rep1=rep(1,5)
rep2=rep("a",4)
rep3=rep(c(1,2,3),5)
rep4=rep(c("a","b"),c(5,2))
#Arithmetic with vectors
x+1
2*x
x^2
2^x
x+y
x*y #Coordinate by coordinate product
x%*%y #Dot product
x<3 #Tells whether each value is less than 3
x==3 #Tells whether each value is exacty 3

length(x) # Gives the length of the vector. 
#Subsets
w=c(1:10)
w[3]
w[2:5]
w[c(2,4,6)]

# 3. Matrices
#Creation
mat1=matrix(c(1,2,3,4,5,6),nrow=2,ncol=3)
mat2=matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow=TRUE)
mat3=matrix(vec4,nrow=3,ncol=5)

#Functions
dim(mat1)
dim(mat2)
dim(mat3)

c1 = c(1, 1, 2)
c2 = c(2, 2, 2)
c3 = c(0, 1, 0)
mat4=cbind(c1,c2,c3)

r1=c(0,1,1)
r2=c(0,0,2)
r3=c(1,1,1)
mat5=rbind(c1,c2,c3)
I4=diag(4)
mat6=2*I4
           
# Operations
#addition
mat1+mat2
mat1+5
#Subtraction
mat1-mat2
mat2-5   
# Inverse
solve(I4)
solve(mat6)
# Transpose
t(mat1)
t(mat2)
#Element-wise multiplication
mat1*mat2
# Matrix multiplication
t(mat1)%*%mat1
t(mat2)%*%mat2
           
# Subsets
#referencing a cell: 
mat1[1,1]
mat1[1,2]          
mat1[2,1]
mat1[2,2]
#referencing a row:
mat1[1,]
mat1[2,]
#referencing a column:
mat1[,1]
mat1[,2]
mat1[,3]           
           
#######################################################################
#                         Data Import
#######################################################################           
setwd("C:/Users/FDI/Desktop/Rbasics")
mydatacsv<- read.table('Iris.csv', sep=',', header=T)
mydatatxt<- read.table('Iris.txt', sep='\t', header=T)       
mydatacsv
mydatatxt
           
#######################################################################
           #                      Exploratory Data Analysis
#######################################################################
#Exploratory Data Analysis: Numbers
PW=mydatacsv[,4]
mean(PW)
var(PW)
min(PW)
max(PW)
median(PW)
           
summary(PW) 
           
#Exploratory Data Analysis: Graphs
hist(PW, main="Histogram of Petal Width", col="dodgerblue", breaks=10)
boxplot(PW, main="Boxplot of Petal Width", col="khaki1", ylab="Petal Width")
boxplot(PW~mydatacsv[,5])
boxplot(PW~mydatacsv[,5],col="pink")
boxplot(PW~mydatacsv[,5],col=c("blue","red","yellow"))          
qqnorm(PW, main="Normal QQ Plot Petal Length")
           
#######################################################################
#                             Loops
#######################################################################           
#For Loops
for(i in 1:10){
   print(i)
}
           
for(i in 1:4){
  print(median(mydatacsv[,i]))
}      
           
#While loops
#Calculating factorial Example
x=3
f=1
t=x
while (t>1){
   f=f*t
   t=t-1
}
f
#Printing example
i=1           
while(i<=5){
   print(mydatacsv[i,])
   i=i+1
}          
           
#######################################################################
#                             If/Else Statements
#######################################################################           
           
# if statement:
# Even example           
x=1:10
for(i in 1:10){
  if(x[i]%%2==0){
    print(x[i])
    print(T)
             }
           } 
           
for(i in 1:dim(mydatacsv)[1]){
   if(mydatacsv[i,5]=="virginica"){
       print(mydatacsv[i,])
   }
}    
           
# if/else statement:           
# Even example           
x=1:10
for(i in 1:10){
  if(x[i]%%2==0)
    print(T)
  else
    print(F)
} 
#virginica example
for(i in 1:dim(mydatacsv)[1]){
   if(mydatacsv[i,5]=="virginica")
       print("virginica")
   else
       print("other")
}   
           
# if/else if/else statement:   
           
x=10
if(x<0){
  sign=-1
}else {
  if(x==0){
  sign=0
  }else{
  sign=1
  }  
}
sign   
           
#######################################################################
#                             Exporting data
#######################################################################                      
write.csv(mydatacsv,file="mydatacsv.csv")  
write.csv(vec2,file="vec2.csv") 
write.table(mydatacsv,file="mydatatxt.txt", sep=" ")
write.table(mydatacsv,file="mydatatxt2.txt", sep="\t")                 
           