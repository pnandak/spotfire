# This lab makes use of two Monte Carlo datasets (school1.dat and
# school2.dat) to illustrate some ways to summarize and visualize
# hierarchically organized data.
# 
# This example was done using S-PLUS 6.0 on an i686 running linux.
# All commands should work under S-PLUS 6.0 on Win95/98/2K machines.

# The first thing we need to do is to read our data in. To do this we
# can use the read.table() function. Let's look at the school1 data
# first.

> sch1 <- read.table("school1.dat", header=TRUE)

# this reads the data held in the file school1.dat into a data.frame
# called sch1. The header=TRUE argument specifies that the first row 
# of school1.dat contains the variable names.
# 
# The school1 data is a Monte Carlo dataset I created for illustrative
# purposes. It consists of 600 students nested within 20 schools.
# The variables in this dataset are: 
# school (school id number)
# sex (sex of student)
# private (indicator of whether school is private (1) or not (0) )
# pincome (parents' income level)
# mathscore (student's score on a standardized mathematics test
#
# If we want to look at the raw data matrix we can type the name of
# the data.frame:
# > sch1
# Let's just look at the first 10 rows for now:

> sch1[1:10,]

   school sex private pincome mathscore 
 1      1   1       1       7       585
 2      1   0       1       5       581
 3      1   1       1       8       608
 4      1   0       1      13       642
 5      1   1       1       4       603
 6      1   0       1       7       596
 7      1   1       1       5       595
 8      1   1       1       3       618
 9      1   0       1      11       631
10      1   1       1       9       615

# The next thing we want to do is to create a groupedData object 
# so that S-PLUS will sensibly deal with the nesting structure of the 
# data. To do this we'll need to make use of the nlme3 library of
# Pinheiro and Bates.
#
# Let's load the nlme3 library:

> library(nlme3)

# To turn sch1 into a groupedData object we type:

> sch1 <- groupedData(mathscore~pincome|school,
+ data=sch1,
+ labels=list(x="Parents' Income",
+ y="Mathematics Achievement Test Score"))

# the mathscore~pincome+sex|school argument to the groupedData
# function signifies that the default dependent variable in the new
# groupedData object is the mathscore variable, and that the primary 
# covariate is the pincome variable, and that school is the clustering
# variable (students are nested within schools). This formula
# determines the default plot and model specification for this 
# groupedData object. 
#
# The data=sch1 argument specifies that the dataset that is to be 
# copied into a groupedData object is called sch1. 
#
# The labels argument specifies descriptive labels for the x and y 
# axis of the default plot of this groupedData object. Recall that the
# mathscore~pincome|subject formula specified that the default y
# variable is mathscore and the default x variable is pincome.
#
# Finally, note that the assignment operator ( <- ) takes the output
# of the groupedData() function and puts it into a groupedData object
# called sch1. Note that we've overwritten our original, 
# non-groupedData data.frame with the new groupedData version.
#
# Let's take a look at the first 10 rows of the new sch1:
 
> sch1[1:10,]
Grouped Data: mathscore ~ pincome | school
   school sex private pincome mathscore 
 1      1   1       1       7       585
 2      1   0       1       5       581
 3      1   1       1       8       608
 4      1   0       1      13       642
 5      1   1       1       4       603
 6      1   0       1       7       596
 7      1   1       1       5       595
 8      1   1       1       3       618
 9      1   0       1      11       631
10      1   1       1       9       615

# Note that this looks exactly the same as the original 
# non-groupedData version except now a grouped data formula appears
# to let us know the grouping structure of this dataset. 
# 
# If we are just interesting in the grouping structure of a dataset we
# can use the formula() function:

> formula(sch1)
mathscore ~ pincome | school

# Since the new sch1 groupedData data.frame contains information about 
# the nesting structure, S-PLUS (in conjunction with the nlme
# functions) can create sensible plots by default. For instance,
# typing plot(sch1) will produce:

> plot(sch1)