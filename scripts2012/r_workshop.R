# IBS INTRODUCTION TO R WORKSHOP
# OCT 10, 2008
# CASEY BLALOCK
# Special thanks to Matt Keller and Ying Lu, I used materials from previous courses taught by them to get ideas for this script.

_________________________________________________________________
#ABOUT R - 	

#R is case sensitive

#Commands are separated by 1) a new line or 2) a ;

# The > in the console indicates R is ready to take commands

# The + indicates that R is waiting for a command to be finished - often happens when the syntax is incomplete (i.e forget to close a loop, parenthesis, or bracket)

# Hashmarks can come anywhere on a line and indicates that everything following is a comment

# You can't start the name of an object with numbers or most symbols

_________________________________________________________________
# R'S WORKING DIRECTORY / CHANGING OPTIONS
getwd()    # Get working dirctory; asking R, "where is the default folder?"
						       					       
setwd("/Users/caseyblalock/Desktop/Course Work/R Workshop")				
setwd("\Users\caseyblalock\Desktop\Course Work\R Workshop")

list.files() 	#Provides list of files in the working directory or specified path 


?options()			# these are the many default options for our R session.
options()				

pi
options(digits=15)
pi
options(digits=3)

options(stringsAsFactors=FALSE)  	 # this sets the default behavior of read.table and data.frame functions; 
									 # now the default is to read characters as characters 
									 # rather than to convert character columns into factors
									 # This may save you some heart ache if you don't want to use R's factors
									 
									 
_________________________________________________________________
# GETTING HELP & R PACKAGES

help(cbind)						#These are only helpful if you know an actual function that you want help on
?cbind							#Equivalent to the above
								#NOTE:  these won't work unless they refer to functions in loaded packages


help.search("simulate")				# This lets you search through all of your installed 
									# packages for these words in help files


#install.packages("MASS",dependencies=TRUE) 	 # You can install packages within the script 

library(MASS)							# Installing packages essentially saves packages to 
										# your computer...you have to load them to use them
									
_________________________________________________________________
#USE FOREIGN PACKAGE TO LOAD MIDUS

												
midus=read.dta(file="midus.dta",convert.factors=FALSE)    # I read STATA file into R using foreign package

library(foreign)								# I have to load the foreign package first
midus=read.dta(file="midus.dta",convert.factors=FALSE)

str(midus,give.attr=FALSE)						#This let's me look at the structure of the midus matrix
dim(midus)										#This gives dimension, 7108 obs (row), 41 variables (columns)

_________________________________________________________________
#SUBSCRIPTING MATRICES AND VECTORS

midus[9,]				# returns all columns for the 9th observation
midus[,4]				# returns all observations of the 4th column (age variable)
						# NOTE: both of these actually return vectors...
						
						
midus[9,4]				# Returns the 9th observation and the 10th column.
						# Notice this returns a single value

c(1,2,3,4,5)			#This is common function.  It combines the elements into a single vector
1,2,3,4,5,6				# R doesn't know what to do with commas outside of a function
1 2 3 4 5 6				# Works for MATLAB, not R


c(1:5)					#The the :, this does the same thing as c(1,2,3,4,5)
c(10:1)					# : tells R go from 1 to 5 (it'll also go in reverse)
						

midus[9,c(1,2,3,4)]		# We can use these lists to provide more complex subsets of data 
						# Here, the first four variables for the 9th observation

midus[1:10,1:10]		#We're going to keep only the first 10 observations 
						# and first 10 variables because they'll be easier to see
							
dim(midus)				#The midus matrix is the same as before.  We need to save over it



						
midus=midus[1:10,1:10]	#Applying the "=" will save over the current matrix
dim(midus)				#applying a "<-" would have done the same thing
						# "=" and "<-" are equivalent


				
age=midus[,4]			#We've just assigned the 4th column to "age".  Age is vector
						# of the age variable in the midus matrix




age[9]					#Subsetting vectors is easy.  There's only a single dimension,
						#so 9 here calls the 9th element of age.  This is the same
						#as using midus[9,4] above.  
						
midus[,4][9]			#Since midus[,4] returns a vector, we can subset it with a [9]
						#You'll rarely want to do subsetting like this, but it's 
						#important to think about what you're getting when you run
						#a function because you can subset those as well
						

c(1:5)[3]				#And we've just subset this function.
						#Several functions will return vectors or matrices, so when
						#you get the logic you can start bypassing the assignment phase
						#and directly go for what you want.

_________________________________________________________________
# SAVED OBJECTS

ls()					# Lists on the objects on the workspace.  We have midus and age
objects()				# The same as ls()

rm("age")					# Removes objects from the workshop.  Here we remove z.
ls()					# z is no longer there
age

rm(list=ls())			#This is useful becasue it returns all objects on workspace and removes them						#Essentially resets R, although packages are still loaded (more about this later)
						#I advise to remove all objects at beginning of a script so you know your script is 
						#working (i.e. not using a previously created object to keep going)
						
ls()					# midus is gone because we cleared everything
midus=read.dta(file="midus.dta",convert.factors=FALSE)
midus=midus[1:10,1:10]		#back to normal
midus

_________________________________________________________________
# TYPES OF DATA VECTORS 
# R places certain attributes on objects that determines how objects are displayed in the console
# and the way objects are treated in computations.

nrow(midus)				# returns the number of rows of a matrix
ncol(midus)				# returns the number of cols of a matrix

length(midus)			# returns the length, or number of elements, in a vector
						# returns the number of columsn when applied to matrix
						# Since midus isn't a vector, let's make one
						
x=c(1,2,3,4,5)			# It combines elements in a vector
						
length(x)				# We created a vector, x, with 5 elements	
							
mode(x)					# "mode" function tells you what type of object x is
mode(midus)				# Common modes are numeric, logical, character,list
						#Often times the mode and the class are the same.  In fact, they are
						#generally the same when we're dealing with vectors. However, a numeric vector 
class(x)				#can have factor class.  Often times, problems arise due to the mode and class of
class(midus)			#objects be careful.  There are more classes than modes.
					.
dim(x)					# Vectors have no dimensions in R
dim(midus)				

_________________________________________________________________
# LOGICAL OPERATORS
x <- c(T,T,F,T,T,F)		#"T","F","TRUE","FALSE" are reserved in R 
x						#These are given the mode "logical"
						#You can use these to subset data or for running loops
						#If logical conditions return a TRUE value, if statements will
						#continue or the subsetted item will be used (More on this later)


x*1						#Logical expressions have a numeric property
x+10					#TRUE = 1, FALSE=0


x=c(1:10)

x<5				# Logical operators ==, <, >, <=, >=, &, |
x==5			 # == function means "are they equal?" 
x!=5			 # != function means "are they unequal?" The ! can be used with 
x>=5			#certain functions to indicate the oppositie or NOT function.

x<3 & x>6			# The & is used to indicate the intersection of so that 
					#both conditions must be true
x<3 | x>6			#The | indicates that either condition must be true, but not both


#Let's try to create a dummy variable for gender using midus and then interact with age
midus
age=midus[,4]			#Let's create a separate vector for age

gender=midus[,5]		#Let's create a separate vector for gender

gender
gender==2				#1 are men, 2 are women - we want 2s to be zeros

log.vec=gender==2		#Instead of numbers, we can subset midus with a logical vector
log.vec
midus[log.vec,]						
					
midus[gender==2,]		#Notice we don't have to assign the logical vector to log.vec

midus[gender==2,5]		#Now that we've got the female subsample, we can change only their
midus[gender==2,5]=0	#scores to 0
midus


midus[,5]=gender 		#STARTING OVER:  gender is a copy of midus[,5] before we changed it
midus


gender==2				#There's a much easier way to create dummy variables

gender==2*1				#Be mindful of those of () and []s


(gender==2)*1			#Here's our dummy variable right here! 
						#Except in this case, females have 1s because our logical statement was selecting females

(gender!=2)*1			# The not equal sign now makes men the TRUEs and thus the 1s



#OPTIONS FOR WHERE TO SAVE THIS VARIABLE

midus[,5]=(gender!=2)*1		#We can assign this vector over the midus matrix variable for gender

						
						
ncol(midus)					#Create a column in midus and call it male
midus[,11]=(gender!=2)*1						
names(midus)[11]="male"		
midus


male=(gender!=2)*1		#or create a new vector called male
						
						
						#I favor creating new variables, but it's entirely up to you and how you like to work				

int=male*age			#This is a vector of interaction terms
						#We could assigne it to the matrix like we did with the female dummy variable

midus[,12]=male*age
names(midus)[12]="interaction"
midus

_________________________________________________________________
# ELEMENT BY ELEMENT CALCULATION

cbind(male,age,int)		#Notice female and age are both vectors - length 10
						#We could create the interaction term because
						# R performs element by element calculations.
							

male=male[1:5]			#What will R do if we reduce male to first 5 elements?

int=male*age			#R recycles male, and uses the vector twice

cbind(male,age,int)


male=male[1:4]			
int=male*age					#the larger vector has to be a multiple of smaller vector

cbind(male,age,int)
_________________________________________________________________
# VECTORS OF CHARACTERS

x=c("sociology","political science","geography")
length(x)
mode(x)
class(x)

summary(x)						# We can also get length, mode, and class from summary()

x=c("sociology","political science","geography",3)  #All elements of a vector have to be the same mode
x 												# 3 is coerced to a character

summary(x)

_________________________________________________________________
# BUILDING A MATRIX WITHIN R - DATA MANIPULATION
# Can build a matrix by putting vectors together


a=c("apple","orange","banana","kiwi")	#character vector
b=c(1,3,15,7)							#numeric vector
c=c(TRUE,FALSE,TRUE,TRUE)				#logical vector



d=cbind(a,b,c)			#All vectors coerced to character
summary(d)



e=rbind(a,b,c)			#Instead of building matrix through columns
summary(e)				#we add vectors together as rows
						#We're still coercing all vectors to character
						#rbind() and cbind() create matrices of the same mode!
						
						

str(midus)
temp=cbind(midus[,4],midus[,8])
summary(temp)						#I happens with MIDUS too
									#USE DATAFRAME FOR MIXED TYPES!

_________________________________________________________________
#DATA FRAMES

a=c("apple","orange","banana","kiwi")	#character vector
b=c(1,3,15,7)							#numeric vector
c=c(TRUE,FALSE,TRUE,TRUE)				#logical vector

x=cbind(a,b,c)					#Unlike a simple matrix a data frame came accomodate different 
y=data.frame(a,b,c)				#modes because they are types of lists (lists can hold 								#different modes).  
		
mode(x)						
mode(y)							#Similar to how we think of stored data

summary(x)
summary(y)

_________________________________________________________________
#INDEX DATA FRAME

y
y[,1]						# They can be indexed just like a regular matrix
y[2,2]



y$a							#You can also call up columsn (or variables) by name
y$a[3]						#Because y$a is a vector (column a), you can subset that just like							# a vector



midus$age					#midus is a dataframe



midus[,"age"]				#We can call up age with the variable name inside the bracket too
midus[,c("age","female","sample")]			#And if we make a vector of column names ie c("age,"female"...)
											# we can get just those columns
											
			
			
					
names(midus)								#This returns a list of variable names
names=names(midus)							#let's save these into a vector



		
names(midus)=c("THIS","IS","A","NEAT","CAPABILITY")
midus									#This reassigns the names of the first five variable




names(midus)=names						#Let's change the names back 
midus




names(midus)[2]="FAMILY ID"				#Since those names are a vector, it can be subset
midus


_________________________________________________________________
# MISSING DATA
# NA and NaN carry special meaning in R.  Like TRUE / FALSE these are R default values
# NA stands for not available - i.e. the data is missing
# NaN is "not a number"  - These are values that cannot be calculated
# Inf is infinity 

mean(midus$race)			#Let's calculate the mean of race
							#Certain functions like mean(), var() don't like missing values
						
							
mean(midus$race,na.rm=TRUE)	#Most functions have a remove NA option. The syntax differs by function


midus$race
is.na(midus$race)			#A useful function for returning TRUE when there's NA
!is.na(midus$race)			# Use ! to reverse that.  TRUE if NOT missing


	
midus[!is.na(midus$race),]	# You can use these logical vectors to subset your data			

temp=is.na(midus[,c("age","race","badl")]) #You can also use them to evaluate the missing data
temp


rowSums(temp)				#This sums up the missing values for these 3 variables for each observation

colSums(temp)				#This will give you the number of NAs for each variable


#TRY WITH BIG DATA
midus=read.dta(file="midus.dta",convert.factors=FALSE)
dim(midus)

#Create a count of missing variables per variable

colSums(is.na(midus))					#This is ugly

		
data.frame(colSums(is.na(midus)))	 	#data frame keeps and uses the variable labels



rowSums(is.na(midus))					#Lots of people here

ncol(midus)
midus[,42]=rowSums(is.na(midus))		#We can add this to midus data frame if wwe like
names(midus)[42]="missing"
midus[1:30,c(1:10,42)]					#Let's limit our data so we see only bits


mean(missing)		#Let's figure out the average missing values


hist(missing)

midus=midus[missing<=5,]				#Let's drop everybody that's missing more than five
sum(missing<=5)							#2464 people missing five or less variables
nrow(midus)								#And that's exactly how many people we have in the matrix now

_________________________________________________________________
# SAVING AND LOADING OBJECTS AND DATA
#1) Save your scripts and just rerun.  

#You can load objects by running another script that has already been saved
#Using the source() function.  This will create whatever objects that file creates

#Creating a log
sink("egsink")
cat("This is an example of a sink file.  It's like a log file") # cat() prints comments
cat("The default is to overwrite the same file")
cat("You can alternative provide the option append=TRUE")
cat("I generally rerun scripts whenever I work on a project again")
cat("But this may be a useful way to document output")
cat("Remember to write labels for the output because")
cat("R doesn't show the script commands, just output")
x
cat("This iw what happens when we divide x by 12")
print(x/12)
cat("Close the sink with sink()")
sink() 		


# Using RData files
save(x,file="wkshp_x.RData")				# This writes x to an R data file
save(list=ls(),file="wkshp_all.RData")		#This will save all objects in workspace
save.image("wkshp_all2.RData")				#This does the same exact thing as above
remove(list=ls())


ls()								#You may be just interested in a single set of objects
load("wkshp_x.RData")				#But if you're working on scripts that take a while
ls()								#It may be nice to save your workspace at a certain point
remove(list=ls())					#so you don't have to rerun stuff over once you come back
load("wkshp_all.RData")				#to it.
ls()
remove(list=ls())
load("wkshp_all2.RData")
ls()

#Writing to other formats
#write.table(x=,file=,col.names=,row.names=,sep=,na=,quote=)
#x= , this is the matrix or data that you want to save
#file= , this is the name / path of the file that you want to create
#col.names= , default=FALSE, if you have column names and want them saved
#row.names= , default=FALSE, if you have row names and want them saved
#sep= ,specified the delimiter. eg. "\t" - tab delimited "," - comma delimited 
#quote= , default=TRUE, puts quotes around character variables
#na= , tells R how to save NAs, eg "99"

write.table(x=x1,file="example.data")	
write.table(x="Matt Keller says 'Hi all, this is something I'm writing to help out in the R class. Lots of fun is R. Strong is the power. Like the wookies you do'", file="mattstext")
write.table(x=cbind(y,z),file="example2.data",sep="*",na="999",col.names=TRUE,row.names=FALSE)

read.table("example.data")   
read.table("mattstext")
read.table("example2.data",header=TRUE,na="999",sep="*")
cbind(y,z)

write.csv(x1,"example.csv")
read.csv("example.csv")

###################################################
############## OTHER USEFUL STUFF #################
###################################################

_________________________________________________________________
#MATH WITH MATRICES

x=matrix(1:16,4,4)

x+x							#Element by element operations
x-x							
x*x							
x/x							
x^2

y=c(100,10,-10,-100)
x*y							#Can do element by element operations with vectors as well							#It's the recycling principle.  This works so long as the number
							#of rows in the matrix is equal to the length of the vector

x=matrix(1:4,nrow=2,ncol=2)
y=matrix(1:6,nrow=2,ncol=3)
			
x%*%y			#Matrix multiplication - Rules of matrix multiplication apply

t(x)			# "t" is transpose function

diag(x)			# Returns the diagonall of x

solve(x)		#Gives you the inverse of x

rowSums(x)		#Creates a vector of the sum across columsn by row

colSums(x)		#Creates a vector of the sum across rows by columns

_________________________________________________________________
#Can create matrices through the matrix function
#matrix(data=,nrow=,ncol=,byrow=)


matrix(data=1:9,nrow=3,ncol=3)				#Divides the vector 1:9 into a 3x3 matrix
matrix(data=1:9,nrow=3,ncol=3,byrow=FALSE)	#byrow option - default = FALSE
											#This is the same as saying, it applies the 
											#numbers of the vector down column 1, down column 
											#2, and down column 3
matrix(data=1:9,nrow=3,ncol=3,byrow=TRUE)	#An alternative would be to fill in the matrix 											#across rows

matrix(data=1:9,nrow=6,ncol=3,byrow=TRUE)	#Oops, R's recycling again.
 											
x=c(1:9)
mat=matrix(data=x,nrow=3,ncol=3)			#The data can come from another object
y=3
z=3
matrix(data=x,nrow=y,ncol=z)			#Infact, all options of functions can be filled 
										#with other objects.  This makes R versatile and
										# easily automated
_________________________________________________________________
# FACTORS

x=c("red","blue","green","red","red","blue")
y=c("red","blue","green","red","red","blue")
summary(x)
summary(y)

x=rep(x,times=3)				# Notice we are duplicating x three times, and saving over x
x
summary(x)

x <- as.factor(x)	# We're changing these characters to categorical factors, which changes 
					#the mode to number and class to factor
levels(x)			#Tells you the different levels			
summary(x)			#Notice that summary now does a cross tab of the category
mode(x)
class(x)
x

contrasts(x)		# this is how R codes this factor in a linear model

y=c(1,1,2,3,1,2,3,2,3,3,3,1,4,4)
ord.numbers=ordered(y,levels=c(1,2,3,4),labels=c("Worst","Bad","Good","Best"))
summary(ord.numbers)
contrasts(ord.numbers)

_________________________________________________________________
#CHANGING AN OBJECT'S CLASS
# the as.xxx series of functions are used to change the classes of objects
# as.data.frame
# as.logical
# as.numeric
# as.character
# as.factor

y=as.data.frame(x)				#This is saving a character matrix to a dataframe
x								#Notice that these are displayed differently
y
str(x)							#But variables a, b, and c are characters
str(y)
		
x[,2]=as.numeric(x[,2])			#Let's try to change variable b to numeric in both
str(x)							#Doesn't change it because this is a character matrix
y[,2]=as.numeric(y[,2])			
str(y)							#Howver, y is a data frame so it can be a collection of modes

y[,3]=y[,3]*1					#Oops, can't multiply a character vector
y[,3]=as.logical(y[,3])			#Change variable to logical
str(y)
y[,3]=y[,3]*1					#Remember logical vectors are also 0 and 1
y

______________________________________________________
#ATTACHING / DETACHING DATA FRAMES & R WORKSPACE

search()					#R has different tiers of objects
							#It looks first for objects in tier 1, then keeps going until
							#it finds what you've asked for

ls()						#Remember we can list all object
ls(1)						#Actually, this is defaulted to tier 1, .GlobalEnv
ls(2)						#You can look at the objects on specific tiers
ls(3)

attach(y)					#Data frames can be attached so that you can type a varible name
							#rather than indexing it each time
							#But we run into a problem...a b c (our variables) are masked by
							#.GlobalEnv
search()					#But see how y is now position 2?
ls(2)						#It has the variables (i.e. columns) of y as objects

remove(a,b,c)				#We're removing a b and c from .GlobalEnv - Thus, they're no 
							#no longer masked
a ; b ; c
a<-NA
a							#We're saving a as NA
ls()						#a is created now in the .GlobalEnv, not y
							#I have to specific y$a to change it to NA
							#IMPORTANT: attaching essentially creates a copy
							#of a dataframe, that are not changed until you detach
							# and reattach
detach(y)							
search()					#Y is now gone, and we can't reference variables anymore
b				

x1=y						#
x2=y
x2$a=NA
x1 ; x2

remove(a)					#Get rid of the a in the global environment
attach(x1)					#Attaching x1 and x2
attach(x2)
search()					#Most recent dataframe attached is higher up...i.e. x2 is 
							#is position2 and x1 is position 3
							
a							#Notice that the NAs of x2 are masking the fruits associated
							#with a in x1.  This is because of position
							
detach(x2)
a							#Now they're back.




detach(x1)

ls(2)									# Packages are loaded and objects become available to you, ls(#) let's you
										# look at objects that are availab
anorexia								# anorexia appears to be sample data.  mvrnorm is a simulation function 
mvrnorm

ls(3)									# These are all the functions avaialble for foreign, which imports data
										# As with data frames, certain objects can be obcured by objects at a higher level
										# If you find that something isn't working properly, you'll likely want to check
										# To make sure that you haven't obscured the function you want with another one

_________________________________________________________________

#THE REP() FUNCTION

x <- c(2,2,2,2,2,2)
y <- rep(2,6)			# The "rep" (repeat) function; you'll use this a lot too
y

rep(x=3,times=4)		# Functions in R have a specific format.  You can name the elements needed for function.
rep(times=4,x=3)		# Or you can provide the parameters of the function in their proper order.
rep(3,4)
rep(4,3)

my.123.vector=c(1:3)	#As with most things, x can be a vector or matrix, not necessarily a scalar

rep(my.123.vector,4)    #Notice the vector is reproduced 4 times
rep(1:3,4)				#There's no need to save vectors for use in a function, they can be embedded						#Therefore, you can either assign an object or embed a expression within a function

rep(my.123.vector,5:3) #The times parameter can also be a vector.  The first element of x is reproduced
								# by the first element of times.  The second element of x by the second element
								# of times.
								
rep(1:3, each=5)		#In addition to times, there's also an each options
rep(1:3, each=5:3)		#Apparently you can't use a vector for each

_________________________________________________________________
#THE SEQ() FUNCTION

#seq(from=,to=,by=,length=)  #by=1, default
seq(1,30)
seq(1,30,by=2)			#By tells R to go from 1 to 30 in units of 2
seq(1,30,length=59)		#Length tells R to go from 1 to 30 and do it in steps that results in a 59 length vector
seq(1,30,by=.5)			
seq(1,30,by=.5,length=59) 	#Can't constraint both the length and by
				
temp=c(1:59)				#There's also an option along=, which allows you to name an assigned vector and along
seq(1,30,along=temp)		#essentially creates a sequence of the named vectors length.

_________________________________________________________________
#SAMPLING

#Creating a sample
x=1:200
sample(x,size=20,replace=TRUE) # Draws a random sample with size 20,
								# with the possibility of drawing the same number.
prob=seq(from=0,to=.98,length=200)	# Create an element probability of being drawn
sample(x,size=20,prob=prob)		# Now we've drawn a sample with probability weights

#LOOPS
for(i in 1:100) {				#Basic format of a loop
	print(i)
	}

#Let's creating a sampling distribution of means
iterations=100								# The number of samples to draw

means=rep(NA,iterations)					# Generates a vector of 100 lenght with NAs
for(i in 1:iterations){
	temp=(sample(x,size=50,replace=TRUE))	# Generates a temporary sample, size 50, from x
	means[i]=mean(temp)						# Assigns the mean of the iteration to the vector means
	}
	
mean(x) 					# We know the mean of x is 100.5
hist(means)					 
mean(means)					# Pulling random samples of size 50 100 
							# times gives us a mean of 99.3802

_________________________________________________________________
#SIMULATING DATA
n <- 500						#We're going to generate samples with size n (here n=500)

x <- rnorm(n=n,mean=10,sd=3)		# defaults of mean=0, sd=1, but you can change these if you like
length(x)
mean(x)
sd(x)								
hist(x)

x = rbinom(n=n,size=1,prob=.25)		#Generates binomial distribution - size=number of trials per obs..default is 1
length(x)
mean(x)
hist(x)

x = rpois(n=n,lambda=2)				# Generate from a poisson distribution
mean(x)								# Mean and variance = lambda of 2
(sd(x))^2
hist(x)

#Generating data from a multivariate normal distribution
library(MASS)

mean=c(0,0,0)									#This is a column of means
mean
cov=matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)	#This is the covariance matrix, notice there's 
cov												#no covariance between the three variables

temp=mvrnorm(n=n,mu=mean,Sigma=cov)
mean(temp[,1])									#All variables should have mean 0, sd 1
sd(temp[,2])

cor(temp[,1],temp[,2])							#All variables should have ~0 correlation


cov[1,2]=.75									#Let's add a correlation between X1 and X2
cov[2,1]=.75

temp=mvrnorm(n=n,mu=mean,Sigma=cov)
cor(temp[,1],temp[,2])							#X1 & X2 should be correlated at .75 correlation

