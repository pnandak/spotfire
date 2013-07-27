
# R Program for Selecting Variables.

# Uses many of the same methods as selecting observations.
setwd("/myRfolder")
load(file="myWorkspace.RData")

# This refers to no particular variables, 
# so all are printed.
print(mydata) 

# ---SELECTING VARIABLES BY INDEX NUMBER---

# These also select all variables by default.
print( mydata[ ] )
print( mydata[ , ] )

# Select just the 3rd variable, q1.
print( mydata[ ,3] ) #Passes q3 as a vector.
print( mydata[3] )   #Passes q3 as a data frame.

# These all select the variables q1,q2,q3 and q4 by indexes.
print( mydata[ c(3, 4, 5, 6) ] )
print( mydata[ 3:6 ] )

# These exclude variables q1,q2,q3,q4 by indexes.
print( mydata[ -c(3, 4, 5, 6) ] )
print( mydata[ -(3:6) ] )

# Using indexes in a numeric vector.
myQindexes <- c(3, 4, 5, 6)
myQindexes
print( mydata[myQindexes] ) 
print( mydata[-myQindexes] )

# This displays the indexes for all variables.
print( data.frame( names(mydata) ) )

# Using ncol to find the last index.
print( mydata[ 1:ncol(mydata) ] )
print( mydata[ 3:ncol(mydata) ] )

# ---SELECTING VARIABLES BY COLUMN NAME---

# Display all variable names.
names(mydata)

# Select one variable.
print( mydata["q1"] ) #Passes q1 as a data frame.
print( mydata[ ,"q1"] ) #Passes q1 as a vector.

# Selecting several.
print( mydata[ c("q1","q2","q3","q4") ] )

# Save a list of variable names to use.
myQnames <- c("q1","q2","q3","q4") 
myQnames
print( mydata[myQnames] )

# Generate a list of variable names.
myQnames <- paste( "q", 1:4, sep="")
myQnames
print( mydata[myQnames] )

# ---SELECTING VARIABLES USING LOGIC---

# Select q1 by entering TRUE/FALSE values.
print( mydata[ c(FALSE,FALSE,TRUE,FALSE,FALSE,FALSE) ] ) 

# Manually create a vector to get just q1.
print( mydata[ as.logical( c(0,0,1,0,0,0) ) ] )

# Automatically create a logical vector to get just q1.
print( mydata[ names(mydata)=="q1" ] )

# Exclude q1 using NOT operator "!".
print( mydata[ !names(mydata)=="q1" ] )

# Use the OR operator, "|" to select q1 through q4,
# and store the resulting logical vector in myqs. 
myQtf <- names(mydata)=="q1" |
         names(mydata)=="q2" |
         names(mydata)=="q3" |
         names(mydata)=="q4" 
myQtf
print( mydata[myQtf] )

# Use the %in% operator to select q1 through q4.
myQtf <- names(mydata) %in% c("q1","q2","q3","q4")
myQtf
print( mydata[myQtf] )

# ---SELECTING VARIABLES BY STRING SEARCH---.

# Use grep to save the q variable indexes.
myQindexes <- grep("^q", names(mydata), value=FALSE)
myQindexes
print( mydata[myQindexes] )

# Use grep to save the q variable names (value=TRUE now).
myQnames <- grep("^q", names(mydata), value=TRUE)
myQnames
print( mydata[myQnames] )

# Use %in% to create a logical vector 
# to select q variables.
myQtf <- names(mydata) %in% myQnames
myQtf
print( mydata[myQtf] )

# Repeat example above but searching for any 
# variable name that begins with q, followed 
# by one digit, followed by anything.
myQnames <- grep("^q[[:digit:]]\{1\}", 
   names(mydata), value=TRUE)
myQnames
myQtf <- names(mydata) %in% myQnames
myQtf
print( mydata[myQtf] )

# Example of how glob2rx converts q* to ^q.
glob2rx("q*")

# ---SELECTING VARIABLES USING $ NOTATION---

print( mydata$q1 )
print( data.frame(mydata$q1, mydata$q2) )

# ---SELECTING VARIABLES BY SIMPLE NAME---

# Using the "attach" function.
attach(mydata)
print(q1)
print( data.frame(q1, q2, q3, q4) )
detach(mydata)

# Using the "with" function.
with( mydata,
  summary( data.frame(q1, q2, q3, q4) ) 
)


# ---SELECTING VARIABLES WITH SUBSET FUNCTION---

print( subset(mydata, select=q1:q4) )
print( subset(mydata,
  select=c(workshop, q1:q4) 
) )


# ---SELECTING VARIABLES BY LIST INDEX---

print( mydata[[3]] )

# ---GENERATING INDEXES A TO Z FROM TWO VARIABLES---

myqA <- which( names(mydata)=="q1" )
myqA
myqZ <- which( names(mydata)=="q4" )
myqZ 
print( mydata[myqA:myqZ] )

#---CREATING A NEW DATA FRAME OF SELECTED VARIABLES

# Equivalent ways to create a data frame 
# of just the q vars.

myqs <- mydata[3:6]
myqs
myqs <- mydata[ c("q1","q2","q3","q4") ]
myqs
myqs <- data.frame(mydata$q1, mydata$q2,
                   mydata$q3, mydata$q4)
myqs
myqs <- data.frame(q1=mydata$q1, q2=mydata$q2,
                   q3=mydata$q3, q4=mydata$q4)
myqs

attach(mydata)
myqs <- data.frame(q1,q2,q3,q4)
myqs
detach(mydata)

myqs <- subset(mydata, select=q1:q4)
myqs


