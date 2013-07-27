# IBS INTRODUCTION TO R WORKSHOP
# CLASS EXAMPLE USING THE MIDUS DATA
# OCT 10, 2008
# CASEY BLALOCK

remove(list=ls())
library(foreign)

setwd("/Users/caseyblalock/Desktop/Course Work/R Workshop")

midus=read.dta(file="midus.dta",convert.factors=FALSE) #Factors can be difficult to work with
str(midus)

#We're going to use the MIDUS data to predict BMI with gender, race, and a spouse strain score that we're going to create. First we'll need to clean up our data, create our spouse strain scale, create some dummy variables, and then perform a linear regression. 

#1. The MIDUS data comes from four samples.  We only want to use sample 1, which is the main sample.  Otherwise, we'll have multiple observations from families.

#1.1 How many observations are from each sample? 


#1.2 Create a logical vector indicating true if the observation comes from sample 1.
 
 
#1.3 Apply the logical vector to the midus data to subset it. Save the subsetted matrix over the midus data. If you ever run into problems, i.e. find that you saved over midus with the wrong data, simply rerun the script up to the point where you make a mistake.  


#1.4 Double check yourself.  How many rows are in the "new" matrix?  How many observations are from each sample? 


#2. There are a lot of variables that we want to get rid of.  Put together a new data frame with only bmi, age, gender, race, and the spouse indicators...spouse_1, spouse_2...spouse_10.  Name the new matrix, midus.


#2.2 - Create a new data set with the desired variables and name it midus.  Hint:  What's going to happen if you use cbind() vs data.frame()?  


#2.3 - We're goign to make the spouse strain variable first.  We're goign to assume that people missing on all ten variables aren't married and those who have complete information on all questions are married.  Those trouble makers that don't answer all the questions are getting dropped. Tip - This will be easier if we deal with those blocks of variables as a matrix rather than 10 vectors.

#2.3a - Identify married and not married people.  How many of each do you have?


#2.3b - Now that we know who's married, who's not, and who didn't answer all the questions, we can sum the scores to create a measure of spouse strain.  Think about what sort of output you need and what it'll look like.  Who will have NAs?


#2.3c - Apply those scores to the midus matrix


#2.3d - Now subset the midus dataframe so that it's only married people.  Given our method of creating the spouse strain scale, there should be no missing values.  Are their missing values once you've subset the data?


#3 - Ok.  Let's make gender into a dummy variable.  2s are females.  I'm going to make men my reference category.


#4.  MIDUS is a primarily white sample. White=1.  Create a dummy variable with white vs other.  Use white as the reference.


#5.  Alright.  I think we've got it.  Run a linear regression using the lm() command.  Use ?lm to get help on the options available for lm().  Do we need to deal with missing data?


#######################################################################################
#######################################################################################
#################################### SOLUTIONS ########################################
#######################################################################################
#######################################################################################

remove(list=ls())
library(foreign)

setwd("/Users/caseyblalock/Desktop/Course Work/R Workshop")

midus=read.dta(file="midus.dta",convert.factors=FALSE) #Factors can be difficult to work with
str(midus)

#We're going to use the MIDUS data to predict BMI with gender, race, and a spouse strain score that we're going to create. First we'll need to clean up our data, create our spouse strain scale, create some dummy variables, and then perform a linear regression. 

#1. The MIDUS data comes from four samples.  We only want to use sample 1, which is the main sample.  Otherwise, we'll have multiple observations from families.

#1.1 How many observations are from each sample? 
summary(midus$sample)
table(midus$sample)

# 3487 people in the main sample

#1.2 Create a logical vector indicating true if the observation comes from sample 1.
 
log.vector=midus$sample==1
sum(log.vector*1) # The subsetted data will have 3487 obs and that's the right amount

#1.3 Apply the logical vector to the midus data to subset it. Save the subsetted matrix over the midus data. If you ever run into problems, i.e. find that you saved overed midus with the wrong data, simply rerun the script up to the point where you make a mistake.  

midus=midus[log.vector,]

#1.4 Double check yourself.  How many rows are in the "new" matrix?  How many observations are from each sample? 
nrow(midus)
table(midus$sample)

#2. There are a lot of variables that we want to get rid of.  Put together a new data frame with only bmi, age, gender, race, and the spouse indicators...spouse_1, spouse_2...spouse_10.  Name the new matrix, midus.


#2.2 - Create a new data set with the desired variables and name it midus.  Hint:  What's going to happen if you use cbind() vs data.frame()?  

midus=cbind(midus$bmi,midus$age,midus$gender,midus$race,midus$spouse_1,midus$spouse_2,midus$spouse_3,midus$spouse_4,midus$spouse_5,midus$spouse_6,midus$spouse_7,midus$spouse_8,midus$spouse_9,midus$spouse_10)

#The above coerces all of the variables into characters. Use a data.frame instead.

midus=data.frame(midus$bmi,midus$age,midus$gender,midus$race,midus$spouse_1,midus$spouse_2,midus$spouse_3,midus$spouse_4,midus$spouse_5,midus$spouse_6,midus$spouse_7,midus$spouse_8,midus$spouse_9,midus$spouse_10)

midus=midus[,c(4,5,6,11:21)]
str(midus,give.attr=FALSE)


#2.3 - We're goign to make the spouse strain variable first.  We're goign to assume that people missing on all ten variables aren't married and those who have complete information on all questions are married.  Those trouble makes that don't answer all the questions are getting dropped. Tip - This will be easier if we deal with those blocks of variables as a matrix rather than 10 vectors.
detach(midus)

#2.3a - Identify married and not married people.  How many of each do you have?

sstrain=midus[,5:14]					#These are in colums 5:14
notmarried=rowSums(is.na(sstrain))==10 #Creates a vector of notmarried folks
sum(notmarried)							 # 1346 nonmarried people

married=rowSums(!is.na(sstrain))==10 	# Creates a vector of married folks
sum(married) 							# 1587 married people

sum(married==FALSE & notmarried==FALSE) #These people didn't answer all of the questions 554
sum(married==TRUE & notmarried==TRUE)	 # This should be zero...a person can't be married and not married


#2.3b - Now that we know who's married, who's not, and who didn't answer all the questions, we can sum the scores to create a measure of spouse strain.  Think about what sort of output you need and what it'll look like?  Who will have NAs?

strain.score=rowSums(sstrain)
lenght(strain.score) 						# This is still the same number of people.  The NAs here don't matter 
											#because we won't be using those people anyway.

#2.3c - Apply those scores to the midus matrix

midus[,15]=strain.score  				#This creates a new column 
names(midus)[15]="strain.score" 		#This names the new column strain.score
midus[1:20,]							#This lets you take a peek.  Does everything look ok?

#2.3d - Now subset the midus dataframe so that it's only married people.  Given our method of creating the spouse strain scale, there should be no missing values.  Are their missing values once you've subset the data?

midus=midus[married,]				
sum(is.na(midus$strain.score))			# No missing values!

#3 - Ok.  Let's make gender into a dummy variable.  2s are females.  I'm going to make men my reference category.

midus[,16]=(midus$gender==2)*1
names(midus)[16]="female"

#4.  MIDUS is a primarily white sample. White=1.  Create a dummy variable with white vs other.  Use white as the reference.
midus[,17]=(midus$race!=1)*1
names(midus)[17]="other.race"

#5.  Alright.  I think we've got it.  Run a linear regression using the lm() command.  What parameters are here?  Do we need to deal with missing data?

summary(lm(bmi~age+female+other.race+strain.score,data=midus))

