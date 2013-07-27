#Sijeong Lim

#POLS/CSSS 510, Fall 2011
#Section 1 (Oct. 7th) 

#Continuing R refresher

# Clear memory of all objects
rm(list=ls())

#####################Types of Data Objects########################

# Create following vectors 

# vector.1 : 1,2,3,4,5,6,6,6,6,6

vector.1<-c(seq(1,5,1), rep(6,5))

# vector.2: 10 randomly drawn numbers from a normal distribution with a mean 10 and a s.d of 1

vector.2<-rnorm(10, 100, 1)

# vector.3: results of 10 single binominal trials with a probability of 0.4

vector.3<-rbinom(10, 1, 0.4)

# Check what type of data vector.2 is.

is.character(vector.2)
mode(vector.2)

# round up vector.2 to two decimal place

round(vector.2, 2)

# matrix.1: Create 5 by 5 matrix containing all NAs 

matrix.1<-matrix(NA, nrow=5, ncol=5)

# Assign matrix.1 the row names (a,b,c,d,e) and the column names (1,2,3,4,5)

rownames(matrix.1)<-c("a","b","c","d","e")
colnames(matrix.1)<-c(1,2,3,4,5) 

# Replace the NAs in the first columne of matrix.1 with Inf

matrix.1[,1]<-Inf

# Creat a list that contains vector.1, vector.2, and matrix.1

list.1<-list(vector.1, vector.2, vector.3, matrix.1)
names(list.1)<-c("vector.1", "vector.2", "vector.3", "matrix.1")

#locate vector.2 from the list

list.1[[2]]
list.1$vector.2

#data frame is a special type of list - list in which each row has same length. It is also a matrix like object, yet its elements -unlike elements in a matrix- doesn't have to be of same type. 

# Most of the data we use are in data frame. 
# Now load the data 

########################### Working with Sample Data##############

data<-read.csv("lab1_data.csv", header=T)  

is.data.frame(data) #Yes!
is.matrix(data) #No
names(data)
summary(data)
data<-na.omit(data)

plot(data$GDP.per.capita.PPP.current.international, data$polity2)

#Create a new variable called "democracy". Assign 0 to countries with negative value or zero polity2 score, and assign 1 to countries with positive score. 

data$democracy[data$polity2>0]<-1
data$democracy[data$polity2<0|data$polity2==0]<-0

#You can use a loop to do the same recoding

data$democracy.2<-rep(NA, length(data$polity2))

for (i in 1:length(data$polity2)) {
    if (data$polity2[i]>0) data$democracy.2[i]<-1
     else data$democracy.2[i]<-0
     }

cbind(data$democracy, data$democracy.2)

# creating a function

country.classification<-function(income, polity){

cat ("This country is a/an")
if (income >= 20000 & polity > 0) cat ("\ advanced democracy.")
if  (income<20000 & polity>0) cat ("\ developing democracy.")
if (income>=20000 & polity<=0) cat ("\ advanced authoritarian country.")
if (income<20000 & polity<=0) cat ("\ authoritarian developing country.")
}


## a country with gdp per capita 30000 and polity score 5

country.classification(30000, 5) 

#pick a random country from our data 

country.classification(sample(data$GDP.per.capita.PPP.current.international,1), sample(data$polity2,1))


























  


