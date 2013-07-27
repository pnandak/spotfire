
# ---SELECTING VARIABLES AND OBSERVATIONS---

# Set Working Directory, load & print mydata.
setwd("myRworkshop")
load(file="mydata.RData")
mydata

# ---IT'S A VERY DIFFERENT WORLD!---
#
# SAS/SPSS/Stata select variables by name:
#   A, B, C, A--Z, A TO Z, q:, q*, etc.
#
# They select observations by logic:
#   IF, WHERE, SELECT IF, FILTER, var==“value”
#
# R uses *many* more methods to select
#
# Several work for variables AND observations!


# ---SELECTING VARIABLES IN R---
#
# In SAS/SPSS/Stata, all variables in an analysis 
#   must be in the same data set
#
# R can analyze variables from multiple data 
#   frames, vectors, etc. in a single analysis
#
# If variables are separate vectors in your 
#   workspace, R can find them by name, e.g. q1.
#
# But if the variables are only in a data frame 
#   (strongly recommended), you must tell it WHICH one.



# ---SELECTING VARS USING $ NOTATION---
#
# The form mydata$myvariable is referred to 
#   as "$ notation", "$ prefixing", etc.
#
# $ Notation works only for data frames & lists

summary( mydata$q1 )

summary( data.frame( mydata$q1, mydata$q2 ) )

# WARNING: this creates one long variable!

summary( c(mydata$q1,mydata$q2) )


# ---SELECTING VARIABLES WITH attach()---
#
# It dumps the variables into a temp space
#
# So they’re accessible as vector names
#
# But new variables do NOT go into the data frame

summary( q1 ) # No vector q1 found!

attach(mydata)

summary( q1 )
summary( data.frame(q1,q2) )

detach(mydata) #optional


# ---SELECTING VARIABLES WITH with()---
#
# Is like attach(mydata) and 
# detach(mydata) for every analysis

with( mydata, 
  summary(q1) 
)

with( mydata, 
  summary( data.frame(q1,q2,q3,q4) ) 
)


# ---SELECTING VARIABLES USING FORMULAS---
#

# Unlike SAS "DATA=" option, the R "data=" 
#   argument tells R functions which data to use 
#   only when using a formula. So this works:

t.test( q4~gender, data=mydata)

# But this does not (unless mydata is attached, 
#   or q1 and q2 are vectors outside of mydata):

t.test( q1, q2, paired=TRUE, data=mydata)

# ---Calling R from SAS/SPSS/Stata---
#
# If you call R from another package, 
#   use that package to select your observations, 
#   then pass the variables to R.
#
# The methods of selecting variables above will 
#   be sufficient for using R from another package. 
#
# But to use R as a stand-alone package, you 
#   must be able to also select observations in it…




# ---SELECTING VARS & OBS USING subset()
#
# You nest it within any other function to select
#
# The subset function is helpful but very odd.
#
# It’s the only R function to use q1:q4 form to 
# select contiguous variables.
#
# It uses c() function to combine variable 
# names rather than the data they contain.
#
# subset argument does logical selections 
#   == for equivalence, &=and, |=or, !=not

summary(
  subset( mydata,
    select=c(workshop, q1:q4),
    subset=gender=="f" )
)


# ---INDEXING OR SUBSCRIPTING---
#
# Data frames (and matrices) have row & 
# column index values accessed in the form:
#   mydata[rows,columns] or
#   mydata[observations,variables]
#
# Vectors have single index values accessed 
# in the form:
#   myvector[ element ]



# ---Subscripts Can Contain---
#
# Nothing, e.g. mydata[ , ] is mydata
#
# Numbers, e.g. mydata[ ,3] is q1
#
# Names, e.g. mydata[ ,“q1”] is q1
#
# Logic, e.g. mydata[gender==“f”, ]
#
# Vectors
#   mydata[ ,c(“q1”,”q2”,”q3”,”q4”) ]
#   mydata[ ,c( 3,4,5,6 ) ]
#   mydata[ ,3:6 ]

# ---Data Frame Subscripting Comma Rules---
#
# If comma is missing…
#   Subscripts refer to columns 
#     E.g. mydata[3] is variable 3
#   A single variable is a data frame
#   A set of variables is a data frame
#
# If comma is present…
#   A single variable is a vector, e.g. mydata[ ,3]
#   A set of variables is a data frame
#     e.g. mydata[ ,3:6]

# ---EQUIVALENT WAYS TO SAVE VARS & OBS---

myFqs <- subset( mydata, 
  select=q1:q4,
  subset=gender=="f" )
print(myFqs)


myFqs <- mydata[1:3,3:6]
print(myFqs)

myVars<-c("q1","q2","q3","q4")
myObs <- which(mydata$gender=="f")
myFqs <- mydata[myObs,myVars]
print(myFqs)


# ---TABLE OF LOGICAL COMPARISONS---
#
# Equals              ==
# Less than           <
# Greater than        >
# Less or equal       <=
# Greater or equal    >=
# Not equal           !=
# And                 &
# Or                  |
# 0<=x<=1             (x >= 0) & (x<=1)
# Missing value size  Just missing 
# Testing for missing is.na(x)


# ---DETERMINING AN OBJECT'S CLASS---
#
# Selection is one way, e.g.

class( mydata[  "q1"] )

class( mydata[ ,"q1"] )

# as.XXX functions convert from one class to
#   another, when possible:

class( as.data.frame( mydata[ ,"q1"] ) )

class( as.data.frame( myMatrix ) )

# ---print(mydata)---

print(mydata)

# ---print( as.list(mydata) )---

print( as.list(mydata) )


# See table in notes for many types of conversion.





