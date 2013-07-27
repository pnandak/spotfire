
# SELECTING VARIABLES AND OBSERVATIONS

# Set Working Directory, load & print mydata.
setwd("myRworkshop")
load(file="mydata.RData")
mydata



# ---SELECTING VARIABLES WITH DATA$VAR---

print( mydata$q1 )

print( data.frame( mydata$q1, mydata$q2 ) )



# ---SELECTING VARIABLES WITH attach()---
# attach is not safe for creating variables!

print( q1 ) # No vector q1 found!

attach(mydata)

print( q1 )
print( data.frame(q1,q2) )

detach(mydata) #optional



# ---SELECTING VARIABLES WITH with()---

with( mydata, 
  print(q1) 
)

with( mydata, 
  print( data.frame(q1,q2,q3,q4) ) 
)




# ---SELECTING VARIABLES WITH FORMULAS


# data= tells R where to find q4 and gender

t.test( q4~gender, data=mydata)


# data= affects only formulas so this does not work.

t.test( q1, q2, data=mydata)


# Now the second t.test form works.

with( mydata,
  t.test( q1, q2, data=mydata)
)


#
# To use R from within SAS or SPSS, the above methods suffice.
# But the following approaches are more central to the R philosophy.
#



# ---SELECTING VARS & OBS USING subset

print(
  subset( mydata,
    select=c(workshop, q1:q4),
    subset=gender=="f" )
)




# ---SELECTING VARS & OBS BY INDEX NUMBER---


# These all select all variables by default.

print( mydata )

print( mydata[ ] )

print( mydata[ , ] )


# Using a single index value.

print( mydata[ ,3] ) # all obs, 3rd var

print( mydata[3, ] ) # 3rd obs, all vars

print( mydata[3] )   # 3rd var as a data frame


# These all select the variables q1,q2,q3 and q4 by indexes.

myVars<-c(3,4,5,6) #the q variables

myObs <-c(1,2,3,4) #the females

print( mydata[ myVars ] ) # all obs, only q1-q5

print( mydata[ myObs, ] ) # only females, all vars

print( mydata[ myObs,myVars ] ) #for just females

print( mydata[ -myObs, ] ) #not females, all vars


# Use 1:n as a shortcut.

myVars<-c(3:6) #the q variables

myObs <-c(1:4) #the females

print( mydata[ myVars ] ) #q1-q4 for all obs

print( mydata[ myObs,myVars ] ) #for just females

print( mydata[ -myObs, ] ) #not females, all vars


# Use ncol and nrow to find the ends.

myVars<-c( 3:ncol(mydata) )

print(myVars)

myObs <-c( 5:nrow(mydata) )

print(myObs)

print( mydata[myObs,myVars] )




# ---SELECTING VARIABLES BY LIST INDEX---

print( mydata[[3]] )

print( mydata[[3:6]] ) #No good.

# --------------------
# -----BREAK TIME-----
# --------------------

# ---SELECTING VARS & OBS BY NAME---


# Display all variable names.

names(mydata)

row.names(mydata)


# Create more interesting names.

myNames <-
  c("Ann","Cary","Sue","Carla",
    "Bob","Scott","Mike","Rich")

myNames


# Replace the row names.

row.names(mydata) <- myNames

print(mydata)


# Select 1 var or obs by name.

print( mydata[ ,"q1"] )     # all obs for q1.

print( mydata[ "Carla", ] ) # Only Carla, all vars.

print( mydata[ "q1" ] )     # all obs, q1 as a data frame.



# Select more vars and obs by name.

myVars <- c("q1","q2","q3","q4")

myObs  <- c("Ann","Cary","Sue","Carla")

print( mydata[myVars] )  # all obs, just q1 to q4

print( mydata[myObs, ] ) # Only females, all vars

print( mydata[myObs,myVars] ) # Only females, only q1 to q4

print( mydata[-myObs, ] ) # Cannot use negative names!


# Generate a list of variable names.

myVars <- paste( "q", 1:4, sep="")

myVars




# ---SELECTING VARS & OBS USING STRING SEARCH---

myVars <- grep("^q", names(mydata), value=TRUE)

myVars

print( mydata[myVars] )


myObs <- grep("^C", row.names(mydata), value=TRUE)

myObs

print( mydata[myObs, myVars ] )




# ---SELECTING OBSERVATIONS USING LOGIC---


# Using only logic is problematic with NA

myObs <- mydata$gender=="f"

myObs

print( mydata[ myObs, ] ) 


# Adding which function helps

myObs <- which( mydata$gender=="f" )

myObs

print( mydata[myObs, ] )


# ---SAVING VARS AND OBS TO A DATA FRAME---

# Using subset

myFqs <- subset(
  mydata, 
  select=q1:q4,
  subset=gender=="f"
)

print(myFqs)


# Again using index values
# Assuming Carla is female

myFqs <- mydata[1:4,3:6]

print(myFqs)


# Again using a mix of names for variables 
# and logical selection of observations

myVars<-c("q1","q2","q3","q4")

myObs <- which(mydata$gender=="f")

myFqs <- mydata[myObs,myVars]

print(myFqs)




# ---DETERMINING AN OBJECT'S CLASS---

class( mydata[  "q1"] )

class( mydata[ ,"q1"] )

class( as.data.frame( mydata[ ,"q1"] ) )


library("Hmisc")

mydata[3:6]

class( mydata[3:6] )

rcorr( mydata[3:6] ) # Doesn't work!

# ---CONVERTING DATA STRUCTURES---

rcorr( as.matrix( mydata[3:6] ) ) # Now it does.

print(mydata)

print( as.list(mydata) )

# See table in notes for many types of conversion.





