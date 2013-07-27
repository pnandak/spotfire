# Lab 3 Intro to R

#  Type help(read.csv).  This function reads in comma-separated files from Excel.

## Use read.csv to read in the cancer data set from the class website. 
## Call it "cancer"


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

#  Find its dimensions.

#  Attach the data set

# Display all columns except column 4

# Create a Boolean (TRUE/FALSE) variable indicating which subjects survived more than 50 days.

# Display all data for subjects with survival time over 50.  

# Display all ages for subjects with survival time over 50.
# Compute the mean age of subjects with survival time over 50.


## Sorting

# Sort the survival time variable, stime.

# Order the data frame so that stime is in increasing order.


## MISSING DATA

## The function is.na() creates a Boolean variable indicating which values are 
## missing in a vector.  Try this example:

is.na(Karn)

## Now use the which() function to retrieve the indices for the subjects missing Karn.

# Compute the mean of the Karnovsky score with mean(Karn) - what happens?

# Since some data is missing, you need to use the option na.rm=T to the mean() function

## Now let's check whether any other variables are missing data.

## The function which() can be used not only on vectors, but also on matrices and data frames

## For matrices and data frames, use the optional parameter arr.ind=TRUE to return
## both row indices and column indices

## Now combine which() and is.na() functions to return the row and column indices
## of all missing values in the data frame.