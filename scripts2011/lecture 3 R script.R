## Lecture 3 R script

age <- c(18,20,21) 
major <- c("math", "biology", "history")

students <- data.frame(age,major)
students

students$age
students$GPA = c(3.0, 3.5, 2.9)

# Compute the mean GPA.
mean(students$GPA)

# Indexing Data Frames

students[1,3]
# gives the value in the first row, third column of the data frame “students”.

#If we want to retrieve a whole row, simply leave the column index blank:
students[1,] 

# To retrieve a specified column, leave the row index blank
students[,2]

data(swiss)

?swiss
summary(swiss)# displays summary statistics for each of the variables

dim(swiss)# gives the dimensions

# Display the education variable.  Do this in two different ways.  
swiss$Education
swiss[,4]

# Display the Catholic variable in two different ways.
swiss$Catholic
swiss[,5]

swiss[, c(3,5)]

# To display all columns except 3 and 5, type
swiss[, -c(3,5)]

#	Display all columns except column1.
swiss[,-1]

#	Compute the mean percent Catholic.
mean(swiss$Catholic)

#	Compute the mean and standard deviation of the Education variable.
mean(swiss$Education)
sd(swiss$Education)

attach(swiss)
mean(Education)


#	Detach the swiss data set.
detach(swiss)

#	Create an Education variable taking values from 1 to 10.
Education=1:10

#	Attach the swiss data set.
attach(swiss)

#	Now check which Education variable you get when you type Education.
Education

#	Now correct the problem by removing the Education variable from the workspace.  Check that this worked properly.
rm(Education)
Education

# Conditional Selection

swiss[Catholic<25,]

Catholic<25 # is a Boolean (true/false) variable

swiss[Examination>20,]

names(swiss)

swiss[Fertility>75 , c(1,4)]
# OR
swiss[Fertility>75 , c("Fertility","Education")]

# sub-setting
swiss.few.Catholics = swiss[Catholic<25,]
swiss.few.Catholics = subset(swiss, Catholic<25)

sort(Catholic)

order(Catholic)
# This output tells us that the smallest element in the Catholic variable
# is in position 19.  The second smallest is in position 13, 
# and the third smallest is in position 25, etc.  Let’s try it:

sort(Catholic)
Catholic[19]
Catholic[13]
Catholic[25]

# We can change the order of the elements in a vector
# by specifying the order of indices that we want.  
# Suppose we want to swap the first two elements of the Catholic vector.

Catholic
Catholic[1:length(Catholic)]
Catholic[c(2,1,3:length(Catholic))]

# Display the Catholic variable sorted from smallest to largest
# by using the order function and not the sort function.
Catholic[order(Catholic)]
sort(Catholic)

ordered.swiss = swiss[order(Catholic),]

ordered.swiss

# The rows of the data frame have been rearranged so that the
# Catholic percentage values are in order.

# Order the data set by Education.
swiss[order(Education),]

Order the data set by Fertility, from largest to smallest.  
swiss[order(Fertility,decreasing=T),]

## First download the Arrests data set from the class website to your computer.
Arrests = read.table(file.choose(), header=T)

summary(Arrests)

save(Arrests, file="C://temp//gail R work//arrests")

# You can also read files directly from the internet.
rm(Arrests)
Arrests

Arrests = read.table("http://students.washington.edu/gailp2/CSSS508/lectures/USArrests.txt", header=T)

# Lab 3 Intro to R
## before starting the lab, remove everything in the workspace
rm(list=ls())

#  Type help(read.csv).  This function reads in comma-separated files from Excel.

## Use read.csv to read in the cancer data set from the class website. 
## Call it "cancer"
cancer=read.csv("http://students.washington.edu//gailp2//CSSS508//lectures//cancer.csv")

# Some info on this data set:
# stime = survival or follow-up time in days. 
# status = dead or censored. 
# treat = treatment: standard or test. 
# age = patient's age in years. 
# Karn = Karnofsky score of patient's performance on a scale of 0 to 100. 
# diag.time = times since diagnosis in months at entry to trial. 
# cell = one of four cell types. 
# prior = prior therapy? 


#  Display summary statistics of all variables in the data frame.
summary(cancer)

#  Find its dimensions.
dim(cancer)

#  Attach the data set
attach(cancer)

# Display all columns except column 4
cancer[,-4]

# Create a Boolean (TRUE/FALSE) variable indicating which subjects survived more than 50 days.
surv = stime>50

# Display all data for subjects with survival time over 50.  
cancer[surv,]

# Display all ages for subjects with survival time over 50.
age[surv]

# Compute the mean age of subjects with survival time over 50.
mean(age[surv])

## Sorting

# Sort the survival time variable, stime.
sort(stime)

# Order the data frame so that stime is in increasing order.
cancer[order(stime),]

## MISSING DATA

## The function is.na() creates a Boolean variable indicating which values are 
## missing in a vector.  Try this example:

is.na(Karn)

## Now use the which() function to retrieve the indices for the subjects missing Karn.

which(is.na(Karn))

# Compute the mean of the Karnovsky score with mean(Karn) - what happens?
mean(Karn)

# Since some data is missing, you need to use the option na.rm=T to the mean() function
mean(Karn, na.rm=T)

## Now let's check whether any other variables are missing data.

## The function which() can be used not only on vectors, but also on matrices and data frames

## For matrices and data frames, use the optional parameter arr.ind=TRUE to return
## both row indices and column indices

## Now combine which() and is.na() functions to return the row and column indices
## of all missing values in the data frame.

which(is.na(cancer),arr.ind=T)

