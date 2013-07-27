
# R Program to Assign Missing Values.

setwd("/myRfolder")

# Read the data to see what it looks like.
mydataNA <- read.table("mydataNA.txt")
mydataNA

# Now read it so that ".", 9, 99 are 
# converted to missing.
mydataNA <- read.table("mydataNA.txt",
  na.strings=c(".", "9", "99") )
mydataNA

# Convert 9 and 99 manually
mydataNA <- read.table("mydataNA.txt",
  na.strings=".")
mydataNA[mydataNA==9 | mydataNA==99] <- NA
mydataNA

# Substitute the mean for missing values.
mydataNA$q1[is.na(mydataNA$q1)] <- 
  mean(mydataNA$q1, na.rm=TRUE)
mydataNA

# Eliminate observations with any NAs.
myNoMissing <- na.omit(mydataNA)
myNoMissing

# Test to see if each case is complete.
complete.cases(mydataNA)

# Use that result to select compete cases.
myNoMissing <- mydataNA[ complete.cases(mydataNA), ]
myNoMissing

# Use that result to select incomplete cases.
myIncomplete <- mydataNA[ !complete.cases(mydataNA), ]
myIncomplete

# When "99" Has Meaning...
# Now read it and set missing values 
# one variable at a time.
mydataNA <- read.table("mydataNA.txt", na.strings=".")
mydataNA
attach(mydataNA)

# Assign missing values for q variables.
mydataNA$q1[q1==9]  <- NA
mydataNA$q2[q2==9]  <- NA
mydataNA$q3[q3==99] <- NA
mydataNA$q4[q4==99] <- NA
mydataNA
detach(mydataNA)

# Read file again, this time use functions.
mydataNA <- read.table("mydataNA.txt",na.strings=".")
mydataNA
attach(mydataNA)

#Create a functions that replaces 9, 99 with NAs.
my9isNA   <- function(x) { x[x==9  ] <- NA; x}
my99isNA  <- function(x) { x[x==99 ] <- NA; x}

# Now apply our functions to the data frame using lapply.
mydataNA[3:4] <- lapply( mydataNA[3:4], my9isNA )
mydataNA[5:6] <- lapply( mydataNA[5:6], my99isNA )
mydataNA

