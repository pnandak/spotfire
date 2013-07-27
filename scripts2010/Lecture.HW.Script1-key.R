
###############Lecture/HW SCRIPT FOR R CLASS 1



#1 R'S WORKING DIRECTORY
getwd()					   # Get working dirctory; asking R, "where is the default folder?"
						       # This is where R expects to find files, and where it saves files, but this can be changed

#HW Problem 1
#-----------------------------------------------------------------------
#create a folder on your computer that will be your working directory for this session
#this script expects this folder to be "C:/temp" for windows users or /temp for mac users
#but you can change this if you wish
#-----------------------------------------------------------------------
						       
setwd("C:/temp")				# "C:/temp" is new default folder for saving and importing data for THIS SESSION
setwd("C:\temp")				# As in C, R wants the forward slash (/) or double slash (//) 
						       # NOT THE BACKWARD ONE (\) FROM DOS LANGUAGE!!
#setwd("/temp")				# for MAC users
getwd()
GETWD() 					    # R language is case sensitive


#HW Problem 2
#-----------------------------------------------------------------------
# (a) change the working directory to a random folder on your computer
setwd("/Users/matthewkeller/Documents/RESEARCH/Cascade")
# (b) using the function list.files(), make a list of the files in this folder
list.files()
# (c) change your working directory back to the folder you created above
setwd("/temp")
#----------------------------------------------------------------------



#2 LOADING OBJECTS THAT WE'VE ALREADY CREATED
load("OldObjects.Rdata") 	 # If we've made objects in a previous session and want them again in this session
                             # will return an error here; errors are your friends! Read the message.


#3 R AS A CALCULATOR
9+12*3			      # R can be used as a calculator
(9+12)*3
(sqrt(9)-6^2)/4
ls()					# Still nothing in memory; we haven't created any objects yet


#4 CREATING OBJECTS
x1 <- (sqrt(9)-6^2)/4		# "<-" Assigns objects ((sqrt(9)-6^2)/4) to new objects (x1)
x1
ls()
mode(x1)				# "mode" function tells you what type of object x1 is
class(x1)				# Although the same for vectors, mode and class are different
					   # There are many more classes than modes
		              # Function often need the class of an object in order to know how to deal with them
dim(x1)				# Vectors have no dimensions in R
length(x1)				# Only lengths
x1 <- 4				# Overrides old assignment - no warning given
x1


#5 CREATING VECTORS OF NUMERIC MODE
x1 <- 1,2,3,4,5,6			# R doesn't know what to do with commas outside of a function
x1 <- 1 2 3 4 5 6			# Works for MATLAB, not R
x1					       # Still is 4
x1 <- c(1,2,3,4,5,6)     # "c" is a function to combine; you'll use it ALL THE TIME!
x1
c(1,2,3,4,5,6)
1:6					        # The ":" operator does the same

mode(x1)
class(x1)
dim(x1)				    # x1 is now a numeric vector of length 6
length(x1)
info(x1)				    # Instead of typing "mode", "class", "dim" each time


#6 VECTORS OF CHARACTER MODE
x5 <- c("red","blue","green",3)	# Elements don't have to be numbers, but all elements have to be the same mode within a vector
x5
info(x5)
info(x5[4])


#7 VECTORS OF LOGICAL MODE
x5 <- c(T,T,F,T,T,F)		# The values "T","F","TRUE", "FALSE", and "NA" are reserved in R and have special meanings
x5
info(x5)
x5[3] <- NA
x5


#8 THE REP FUNCTION
x2 <- c(2,2,2,2,2,2)
x3 <- rep(2,6)			# The "rep" (repeat) function; you'll use this a lot too
x3
?rep					   # What is the format for rep? "?", the help function, tells us
rep(x=3,times=4)
rep(3,4)                # Same as above!
rep(1:3,4)
rep(1:3,times=4)
rep(1:3, each=4)
rep(4:8,times=c(1,2,4,0,1))   #reading the "times" entry under rep tells us that times can be a vector!


#9 REMOVING OBJECTS
LS()
x4 <- c(5,6,7)
LS()
remove(x4)				# Remove object x4
LS()


#10 VECTOR MANIPULATION & INDEXING
x1*x2					# R performs element by element arithmetic (matrix operators too; see below)
x1*c(2,2,2,2,2,2)
x1*2					# If the dimensions are nonequal, R recycles
x1*c(2,3)				# R recycles happily, so long as the longer is divisible by the shorter; BE CAREFUL!
x1*c(2,3,4,5)			# R still gives you an answer, but also a warning

x3[4]					# [4] returns the fourth element in x3
x3[4] <- 99			# The [4] indexes the fourth element in vector x3, and changes it to 99
x3[6] <- 5
x3;x2					# ";" operator allows you to put multiple commands on the same line


#11 LOGICALS
# logicals are used very often in R as a way of subsetting data (as you'll do in your hw)
x2<x3					# Logical operators ==, <, >, <=, >=, &, |
x2==x3				   # == function means "are they equal?" do ALL the elements of x2 = x3?
x2!=x3				   # != function means "are they unequal?" do ANY of the elements x2 not equal x3?
x2<= x3
x3<3 | x3>6
x3>2 & x3<50

truefalse <- x2==x3
info(truefalse)			     # truefalse is a vector of mode and class "logical"
truefalse
truefalse*1
truefalse <- truefalse*1	     # Arithmetic on a "logical" vector changes the TRUE elements=1 and FALSE=0
truefalse
info(truefalse)			     # truefalse is now a "numeric" rather than "logical" vector
(x2==x3)*5 + (x2 != x3)*8     # recoding is easy; use this trick for HW problem C
x3>3                          # logicals recycle too
new.vector  <- c(33,85,28,12,6,97,25,63,69,78)


#HW Problem 3
#-----------------------------------------------------------------------
# (a) using a logical operators & vector multiplication, make 
# another vector "a2" that is 1 if new.vector element <= 50 and 0 otherwise
a2 <- (new.vector<=50)*1
# (b) using logical operators & vector multiplication, make vector "a3" 
#whose elements = 5 if new.vector element is between 30 and 80 and -99 otherwise (challenging)
a3 <- (new.vector>=30 & new.vector<=80)*5 + (new.vector<30 | new.vector>80)*-99
index1 <- (new.vector>=30 & new.vector<=80)  #another way to do it; longer but useful to understand
index2 <- index1==FALSE
a3 <- new.vector
a3[index1] <- 5
a3[index2] <- -99
answer <- rbind(new.vector,a2,a3) # check your answer visually with "rbind" (row bind) function
# (c) look at answer to see if its right; also check what class & mode answer is
answer
info(answer)	
#-----------------------------------------------------------------------


#12 ANOTHER CLASS OF OBJECTS: THE MATRIX
?matrix
x1*c(1,3)		
matrix(x1*c(1,3), nrow=2, ncol=3,byrow=TRUE) #the "matrix" function creates matrices
mat1 <- matrix(x1*c(1,3), nrow=2, ncol=3,byrow=TRUE)
mat1
mat2 <- matrix(x1*c(1,3), nrow=2, ncol=3,byrow=FALSE)	
mat2
matrix(x1*c(1,3), nrow=2, ncol=3)	# Default is for byrow=FALSE, so we don't have to write it
mat1;mat2
mat1*mat2					# Element by element multiplication
mat1-mat2
mat1 %*% mat2				# %*% function is for matrix multlication; can't multiply 2*3 by a 2*3
t(mat2)					# "t" is transpose function
mat3 <- mat1 %*% t(mat2);mat3	 


#13 INDEXING MATRICES
mat1
mat1[1,3]					# Index specific values with []; [1,3] is 1st row, 3rd column
mat1[1,]					# The "[1,]" means row 1, all columns
mat1[1,]==mat1[1,1:3]	# All true
mat1[,2]					# 2nd column
mat1;mat1[c(2,1),c(3,1,2,2,2,2)]	# Rearrange columns and rows
mat1[,c(1,3)]				# Only columns 1 & 3
mat1[,c(TRUE,FALSE,TRUE)]       	# We can also index by a logical vector of the same length as the data row or column
my.awesome.index <- c(TRUE,FALSE,TRUE)
mat1[,my.awesome.index]         	# Same thing! Indexing my logical vectors is IMPORTANT to understand & use


#14 SIMULATING DATA
a <- 50
1:50;1:a					   		 	# Same thing; a is now an numerical object that equals 50
x <- rnorm(a)				    	 	# x is a vector of 50 random normal variables
?cbind                             # cbind = "column bind" - binds columns together
cbind(rnorm(a),x)					 	# rnorm(a) and x are not the same; R re-randomizes each time 
x[a-10]					    	 	# The 40th entry of x
y <- x*sqrt(.4) + rnorm(a)*sqrt(.6) # y is linear combination of 40% x & 60% random noise
z <- cbind(x,y)					 	# column bind x & y so that we can look at them side by side
z
sample(x, size=50, replace=FALSE)	# The "sample" function; useful for permutation testing (replace=FALSE) 
sample(x, size=10, replace=TRUE)	# or for bootstrapping (replace=TRUE)


#15 DATA FRAMES
dis <- rbinom(n=50,size=1,prob=.15)
intv <- sample(c("Gary","Lew","Chick"),50,replace=TRUE)
dataset <- data.frame(col1=x,col2=y,disease=dis,interviewer=intv)
names(dataset) <- c("var1","IQ","disorder","interviewer") # Rename the variables in dataset
dataset <- edit(dataset)		# Opens in an excel like spreadsheet. BE CAREFUL; changes made are made to dataset
summary(dataset)
plot(dataset) 				# default behavior of these two functions for class "data.frame"


#16 INDEXING DATA FRAMES
dataset[,4]					# Data.frames can be indexed like matrices
dataset$interviewer			# Or using the "$" notation
info(dataset$interviewer)


#17 CHANGING AN OBJECT'S CLASS
# the as.xxx series of functions are used to change the classes of objects
mat2 <- as.data.frame(mat2)		# Change the class of mat2 from matrix to data.frame
mat2						       # It is VERY COMMON to change between class matrix and class data.frame
info(mat2)					       # You will (and should) usually keep flat data in data.frames, but often
mat2 <- as.matrix(mat2)			# you might want to change data.frames to matrices
mat2;info(mat2)			       # in order to do (e.g.) matrix manipulation or to arrays to store 3+ dimensions
                               # Warning: be careful changing data.frames to matrices when you have data of mixed modes!


#HW Problem 4
#-----------------------------------------------------------------------
# (a) Create a 2500 x 3 data.frame that is named "dataset"
# The first columns is normally dist. data, mean=100, sd=15; see ?rnorm
# The second column is normally dist but 10% of them are outliers (add 30 to a random 10% of the data); see ?rbinom 
# The third column is binary, with overall prob(x=1) = .25
# Give each column an informative name
normal.dat <- rnorm(2500,mean=100,sd=15)
outlier.dat <- rnorm(2500,mean=100,sd=15)
adder <- rbinom(n=2500,size=1,prob=.1)*30
outlier.dat <- outlier.dat+adder
binary.dat <- rbinom(n=2500,size=1,prob=.25)
dataset <- as.data.frame(cbind(normal.dat,outlier.dat,binary.dat))
# (b) Plot a histogram & scatterplot of the first two variables; see ?hist and ?plot
hist(dataset[,1])
hist(dataset[,2])
plot(dataset[,1:2])
# (c) What is the mean, median, & var of the first two columns? What % of the third column is equal to 1?
# see ?mean ?median ?var ?summary
summary(dataset)
var(dataset[,1]);var(dataset[,2])
# (d) What is the mean, median, & var of the first two columns WHEN the third column is equal to 1?
# (hint: first try creating a vector that is TRUE when the third column is 1 and FALSE when 0,
# then use this vector as an index; see the last few lines of section #13)
# Place the mean, median, & var for each of these column into two 3 element vectors named col1 and col2
col1 <- c(mean(dataset[dataset[,3]==1,1]),median(dataset[dataset[,3]==1,1]),var(dataset[dataset[,3]==1,1]))
col2 <- c(mean(dataset[dataset[,3]==1,2]),median(dataset[dataset[,3]==1,2]),var(dataset[dataset[,3]==1,2]))
# (e) Take a random sample of 50 rows (without replacement) using the "sample" function. 
# What is the mean, median, & var of the first two columns for this subset of rows?
# Place this information into two 3 element vectors named col1.subset and col2.subset
dat2 <- dataset[sample(1:nrow(dataset),size=50,replace=FALSE),]
col1.subset <- c(mean(dat2[dat2[,3]==1,1]),median(dat2[dat2[,3]==1,1]),var(dat2[dat2[,3]==1,1]))
col2.subset <- c(mean(dat2[dat2[,3]==1,2]),median(dat2[dat2[,3]==1,2]),var(dat2[dat2[,3]==1,2]))
#-----------------------------------------------------------------------


#18 SAVING AND QUITTING
# there are three basic ways to save objects from an R session: 
# (1) save the syntax file so it can be rerun, thereby recreating all objects (my favorite method)
# (2) save all the objects you want to save in an .RData file
# or (3) write out the object into a format (e.g., tab delimitted) that can be read by other programs
LS()
write.table(dataset,file="our.data")  # Saves dataset as an object of class "data.frame"
remove(dataset)
LS()

help.search("save")                    # say that we don't know how to save. help.search searches through titles & keywords to match "save"
?save
save(truefalse,x1,x2,file="threeobjects.RData")  # saves these three objects in an .RData file
unlink("threeobjects.RData")                     # erases the .RData file
save.image("all.objects.RData")                  # saves all objects into an .RData file
remove(list=ls())								       # Remove everything from list
LS()
q('no')                                          # Quit R and don't save ('no')


#############################################
#THE END
#############################################










