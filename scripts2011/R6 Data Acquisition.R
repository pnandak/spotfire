
# DATA ACQUISITION

# Set our Working Directory
setwd("myRworkshop")

# ---THE R DATA EDITOR-CREATING---
#
# Limited functionality vs. SAS/SPSS/Stata
#
# Cannot create new data frame, nor factors
#
# Create an empty data frame with:

mydata <- edit( data.frame() )

# Click on variable names to change
#
# Enter values, using blanks for missing 


# ---THE R DATA EDITOR-EDITING---
#
# Edit an existing one with "Edit> Data editor" or
#
# This:     myPractice <- edit( mydata )
#
# Or this:  fix(mydata)
#
# This will lose your changes!   
#
# edit(mydata) # Evil!

# Print the data

myPractice

#Edit it again

fix( mydata )

# This is same as fix(mydata)

mydata <- edit( mydata )


# ---COMMA SEPARATED VALUES

# mydata.csv:
#
#   workshop,gender,q1,q2,q3,q4
# 1,1,f,1,1,5,1
# 2,2,f,2,1,4,1
# 3,1,f,2,2,4,3
# 4,2, ,3,1, ,3
# 5,1,m,4,5,2,4
# 6,2,m,5,4,5,5
# 7,1,m,5,3,4,4
# 8,2,m,4,5,5,5

# ---COMMA DELIMITED FILE DETAILS---
#
# ID is in leftmost column but without a name
#
# Values are separated by a single comma
#
# Missing Values: consecutive commas, Blanks or NA
#
# This reads NUMERIC ONLY data fine:

mydata <- read.csv("mydata.csv")

mydata

# When you have any character data, this strips the 
# blanks (white) out and sets the "nothing" to missing
# Now gender for observation 4 is missing:

mydata <- read.csv("mydata.csv",
  strip.white=TRUE, na.strings="")

mydata


# ---TAB DELIMITED FILES: mydata.tab---

# mydata.tab:
#
#    workshop	gender	q1	q2	q3	q4	
# 1   1	f	1	1	5	1
# 2   2	f	2	1	4	1
# 3   1	f	2	2	4	3
# 4   2	NA	3	1	NA	3
# 5   1	m	4	5	2	4
# 6   2	m	5	4	5	5
# 7   1	m	5	3	4	4
# 8   2	m	4	5	5	5


# ---TAB DELIMITED FILE DETAILS---
#
# ID in leftmost column but without name
#
# Values separated by 1 OR MORE tabs or spaces
#
# NA for missing values (tabs/blanks don’t work)
#
# Consecutive tabs can be NA if you specify 
#   sep="\t" ("\t" is a single tab)


mydata <- read.table("mydata.tab")

mydata


# ---READING CODED MISSING VALUES---
# First without fixing them

mydataNA <- read.table("mydataNA.tab")
mydataNA

# Now with na.strings to fix them
mydataNA <- 
  read.table("mydataNA.tab",
  na.strings=c(".", "9", "99") )
mydataNA


# ---WHAT IF ID IS NAMED?---
#
# If the firdst row contains names INCLUDING
# the name of an ID variable, e.g. myIDvar...
#
# Then add this to any example above:
#
#   "...header=TRUE, rownames=myIDvar )"


# ---READING DATA FROM SAS/SPSS/STATA---
#
# If you have SAS/IML or SPSS, use those to 
# translate your data sets to R
#
# Otherwise, or to read Stata files use:

Library("Hmisc")

mydata <- sasxport.get("mydata.xpt")

mydata <- spss.get("mydata.por",
  use.value.labels=TRUE)

mydata <- stata.get("mydata.dta")



# ---MISCELLANEOUS IMPORT/EXPORT---
#
# R Data Import/Export Manual in Help files for:
#   -ODBC access to relational database tables
#   -Read directly from web address, etc.
#
# If you have Excel, do import/export via the 
# Rexcel package
#
# To export data, use write.foreign function


# ===DATA ACQUISITION TOPIC FOR SELF STUDY===

# ---DATA WITHIN AN R PROGRAM---
# (cards,datalines/begin data/input automatic)

mydata <- read.csv( stdin() )
  workshop,gender,q1,q2,q3,q4
1,1,f,1,1,5,1
2,2,f,2,1,4,1
3,1,f,2,2,4,3
4,2, ,3,1, ,3
5,1,m,4,5,2,4
6,2,m,5,4,5,5
7,1,m,5,3,4,4
8,2,m,4,5,5,5

# Blank line above ends STandarD INPut (stdin)
mydata

# ---Fixed-Width Format Files---

# ---A Fixed-Width File (FWF)---
# 011f1151 #id is in cols 1-2
# 022f2141
# 031f2243
# 042 31 3
# 051m4524
# 062m5455
# 071m5344
# 082m4555

# ---FIXED-WIDTH FILE DETAILS---
#
# ID is in columns 1-2
#
# Other variables take 1 column each
#
# Names are NOT in the file
#
# Missing values are blank…
#
# So character data needs:
#
# …strip.white=TRUE, na.strings="")
#
# If width is a list, its N of components tells R 
#   the N of records per observation


# ---FIXED WIDTH TEXT FILES---

mydata <- read.fwf(
   file="mydataFWF.txt",
   width=c(2,-1,1,1,1,1,1),  #minus skips
   col.names=
     c("id","gender","q1","q2","q3","q4"),
   row.names="id",
   strip.white=TRUE,
   na.strings="",
   fill=TRUE #fills records that end early
)
mydata


# ---It's Easier with "Macro Substitution"---

myfile <- "mydataFWF.txt"

myVarNames  <- 
  c("id","gender","q1","q2","q3","q4")

myRecord <- c(2,-1,1,1,1,1,1)

mydata <- read.fwf(
   file=myfile,
   width=myRecord,  
   col.names=myVarNames,
   row.names="id",
   na.strings="",
   fill=TRUE,
   strip.white=TRUE )

mydata


# ---MORE THAN 1 RECORD PER CASE---

myfile <- "mydataFWF.txt"

myVarNames  <- 
  c("id","group", "gender",
    "q1","q2","q3","q4",
    "q5","q6","q7","q8")

myRecord1  <- c( 2, 1, 1, 1, 1, 1, 1)

myRecord2  <- c(-2,-1,-1, 1, 1, 1, 1)

# Two components in this list tell R, 
#   there are two records per case:

myRecords <- list( myRecord1, myRecord2 )

# Now plug them in and read the data.
# This code is an exact copy of the code for 1 record.
# Only the values of the arguments have changed.

mydata <- read.fwf(
   file=myfile,
   width=myRecords,  
   col.names=myVarNames,
   row.names="id",
   na.strings="",
   fill=TRUE,
   strip.white=TRUE )

mydata



