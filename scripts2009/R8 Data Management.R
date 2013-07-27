
# R PROGRAM FOR DATA MANAGEMENT

# Set our Working Directory, load & print mydata

setwd("myRworkshop")

load(file="mydata.RData")

mydata




# ---TRANSFORMING VARIABLES---

mydata$q4Log <- log(mydata$q4)

mydata

summary( mydata$q4Log )
 
summary( log( mydata$q4 ) )


# The transform function

mydata <- transform( mydata,
  score1=(q1+q2)/2,
  score2=(q3+q4)/2  )
mydata




# ---PROCEDURES OR FUNCTIONS?---
#    The apply Functions Decide

mean( mydata[3:6], na.rm=TRUE )

# apply works with a matrix, 
# places its results in a vector

apply( mydata[3:6], 2, mean, # Columns
    na.rm=TRUE )

apply( mydata[3:6], 1, mean, #Rows
  na.rm=TRUE)

# lapply works with components of a list (l is for list)
# (data frames are lists too)
# places its results in a list

lapply(mydata[3:6], mean, # List is the result
  na.rm=TRUE)

# sapply works with lists too
# results are simplified to a vector (s is for simplify)

sapply(mydata[3:6],mean, # Simplifies to vector
  na.rm=TRUE)




# ---COUNTING FUNCTIONS---

# Look at q3:
mydata

length(mydata$q3) #counts all values

sum( !is.na(mydata$q3) ) #counts non-missing

library("prettyR")

valid.n(mydata$q3)

valid.n 




# ---ADDING RESULTS TO A DATA FRAME---

mydata$meanQ <- apply( mydata[3:6], 1,
  mean, na.rm=TRUE )

library("prettyR")

mydata$nQ <- apply( mydata[3:6], 1,
  valid.n )

mydata




# ---CONDITIONAL TRANSFORMATIONS---

# Follows form: var <- ifelse( condition, yes, no)

mydata$gals <- 
  ifelse( gender=="f",  1,  0 )
mydata

mydata$satisfied <- 
  ifelse( q1 >= 4, 1, 0 )
mydata

mydata$score <- ifelse( gender=="f",
  (2*q1)+q2,
  (3*q1)+q2  )
mydata




# ---RECODING VARIABLES---

# (Could use indexing or ifelse too)

load(file="mydata.RData")
mydata

library("car")

mydata <- mydata  #make a fresh copy

mydata$qr1 <- recode( mydata$q1, "1=2; 5=4")
mydata$qr2 <- recode( mydata$q2, "1=2; 5=4")
mydata$qr3 <- recode( mydata$q3, "1=2; 5=4")
mydata$qr4 <- recode( mydata$q4, "1=2; 5=4")
mydata




# ---Missing Data---

myNoMissing <- na.omit(mydata)
myNoMissing

# Read messy data with no instructions for missing data.

mydataNA <- read.table("mydataNA.txt")

mydataNA

# Read again, excluding all three missing codes.

mydataNA <- 
  read.table("mydataNA.txt",
  na.strings=c(".", "9", "99") )

mydataNA

# Mean substitution for missing

mydataNA$q1[ is.na( mydataNA$q1 ) ] <- 
  mean( mydata$q1, na.rm=TRUE )

mydataNA




# ---RENAMING VARIABLES---

# Using Text Editor

fix(mydata)


# Renaming with names Function

# load a fresh copy of our data.

load(file="mydata.RData")
mydata

names(mydata)
 
names(mydata) <- c("group", "gender", "x1", "x2", "x3", "x4")
 
mydata


# Renaming Variables with rename Function

# load a fresh copy of our data.

load(file="mydata.RData")
mydata

library("reshape")

myChanges <- 
   c(q1="x1", q2="x2", q3="x3", q4="x4")

myChanges
 
mydata <- rename(mydata, myChanges)

mydata




# ---KEEPING AND DROPPING VARIABLES---

load(file="mydata.RData")
mydata


myleft <- mydata[ ,1:4] # Keeping.

myleft

#Or assign NULL to vars to drop:

myleft <- mydata 

myleft

myleft$q3 <- myleft$q4 <- NULL  # Dropping

myleft




# ---STACKING / ADDING CASES---

# Create female data frame.

females <- mydata[ which(gender=="f"), ]
females

# Create male data frame.

males <- mydata[ which(gender=="m"), ]
males

#Bind their rows together with the rbind function.

both <- rbind(females, males)
both

# Drop q2 to see what happens.

males$q2 <- NULL
males

# See that row bind will not work.

both <- rbind(females, males)

# Use reshape's rbind.fill function.

library("reshape")
both <- rbind.fill(females, males)
both

# Add a q2 variable to males.
males <- data.frame( males, q2=NA )
males

# Now rbind can handle it.
both <- rbind(females,males)
both




# ---MERGING / JOINING DATA FRAMES---

setwd("/myRfolder")

# Read data keeping ID as a variable.
mydata <- read.table("mydata.csv",
  header=TRUE,sep=",",na.strings=" ")
mydata

#Create a data frame keeping the left two q variables.
myleft <- mydata[ c("id","workshop","gender","q1","q2") ]
myleft

#Create a data frame keeping the right two q variables.
myright <- mydata[ c("id","workshop","q3","q4") ]
myright

# Merge the two dataframes on both id and workshop.
# all argument keeps them all like SAS/SPSS merges
both <- merge(myleft, myright, all) 
both

#Merge the two dataframes by ID only.
both <- merge(myleft, myright,
            by.x="id", by.y="id" )
both

#Merge dataframes specifying both ID and workshop.
both <- merge(myleft,myright,by=c("id","workshop"))
both

#Merge dataframes by both ID and workshop,
#while allowing them to have different names.
both <- merge(myleft,
            myright,
            by.x=c("id","workshop"),
            by.y=c("id","workshop") )
both




# ---AGGREGATING / SUMMARIZING DATA---

attach(mydata)

# Using aggregate Function
# ------------------------
# Results easy to read
# Results easy to program
# Accepts only functions with 1 result

myAgg <- aggregate(q1,
 by=data.frame(gender,workshop), 
 mean, na.rm=TRUE)

myAgg # A data frame.


# Using tapply Function
# ------------------------------------------------
# Results not easy to read for more than 2 factors
# Results easy to program
# Accepts any function

myAgg <- tapply(q1, 
   data.frame(workshop,gender), 
   mean, na.rm=TRUE) 
 
myAgg # A matrix or array.

# Counted Aggregates

table(workshop) 
 
table(gender,workshop)

myCounts <- 
  table(gender, workshop)

myCounts  # A table.

# Convert to data frame for ease of programming

myCountsDF <- as.data.frame(myCounts)
myCountsDF


# ---BY OR SPLIT FILE PROCESSING---

# Using by Function
# ----------------------------
# Results very easy to read
# Results hard to program
# Accepts any function

by(q1, 
   data.frame(workshop,gender), 
   mean, na.rm=TRUE
) 


# ---RESHAPING DATA---

mydata$subject <- 1:8
mydata

library("reshape")
 
mylong <- melt(mydata, id=c("subject","workshop","gender") )
 
mylong

# Return to Wide Format

mywide <- cast(mylong, subject+workshop+gender ~ variable)
 
mywide


# ---SORTING DATA---

# See that it's sorted by gender.

mydata

# Now sort by workshop.

myW   <- order( workshop )
mydata[ myW, ]

# rev function can reverse it
# with numeric vars, negative sign does too

mydata[ rev(myW), ] #reverses.

myWG <- order( workshop,gender )
mydata[ myWG, ]

# Keep a sorted copy

mySortedWG <- mydata[ myWG, ]
mySortedWG


















