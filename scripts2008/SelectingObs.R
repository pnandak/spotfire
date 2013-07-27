
# R Program to Select Observations.

setwd("/myRfolder")
load(file="myWorkspace.RData")
print(mydata)

#---SELECTING OBSERVATIONS BY INDEX
# Print all rows.
print( mydata[ ] )
print( mydata[ , ] )
print( mydata[1:8, ] )

# Just observation 5.
print( mydata[5 , ] )

# Just the males:
print( mydata[ c(5,6,7,8) , ] )
print( mydata[ 5:8        , ] )

# Excluding the females with minus sign.
print( mydata[ -c(1,2,3,4), ] )
print( mydata[ -(1:4)    , ] )

# Saving the Male (M) indexes for reuse.
myMindexes <- c(5,6,7,8)
summary( mydata[myMindexes, ] ) 

# Print a list of index numbers for each observation.
data.frame(myindex=1:8,mydata)

# Select data using length as the end.
print( mydata[ 1:nrow(mydata),  ]  )
print( mydata[ 5:nrow(mydata),  ]  )

#---SELECTING OBSERVATIONS BY ROW NAME

# Display row names.
row.names(mydata)

# Select rows by their row name. 
print( mydata[ c("1","2","3","4"), ] )

# Assign more interesting names.
mynames <- c("Ann","Cary","Sue","Carla",
             "Bob","Scott","Mike","Rich")
mynames

# Store the new names in mydata.
row.names(mydata) <- mynames
mydata

# Print Ann's data.
print( mydata["Ann" , ] )
mydata["Ann" , ] 

# Select the females by row name.
print( mydata[ c("Ann","Cary","Sue","Carla"), ] )

# Save names of females to a character vector.
myFnames <- c("Ann","Cary","Sue","Carla")
myFnames

# Use character vector to select females.
print( mydata[ myFnames, ] )

# ---SELECTING OBSERVATIONS USING LOGIC---

#Selecting first four rows using TRUE/FALSE. 
myRows <- c(TRUE, TRUE, TRUE, TRUE,
  FALSE, FALSE, FALSE, FALSE)
print( mydata[myRows, ]  )

# Selecting first four rows using 1s and 0s.
myBinary <- c(1, 1, 1, 1, 0, 0, 0, 0)
print( mydata[myBinary, ] )
myRows <- as.logical(myBinary)
print( mydata[ myRows, ] )

# Use a logical comparison to select the females.
mydata$gender=="f"
print( mydata[ mydata$gender=="f", ] )
which( mydata$gender=="f" )
print( mydata[ which(mydata$gender=="f") , ] )

# Select females again, this time using a saved vector.
myFemales <- which( mydata$gender=="f" ) 
myFemales 
print( mydata[ myFemales , ] ) 

# Excluding the females using the "!" NOT symbol.
print( mydata[-myFemales , ] )

# Select the happy males.
HappyMales <- which(mydata$gender=="m" 
  & mydata$q4==5)
HappyMales
print( mydata[HappyMales , ] )

# Selecting observations using %in%.
myRspss <- 
  which( mydata$workshop %in% c("R","SPSS") )
myRspss
print( mydata[myRspss , ] )

# Equivalent selections using different 
# ways to refer to the variables.

print( subset(mydata, gender=='f') )

attach(mydata)
  print(  mydata[ gender=="f" , ] )
detach(mydata)

with(mydata,
  print ( mydata[ gender=="f" )
)

print( mydata[ mydata["gender"]=="f" , ] ) 

print( mydata[ mydata$gender=="f" , ] ) 


# ---SELECTING OBSERVATIONS BY STRING SEARCH---

# Search for row names that begin with "C".
myCindexes <- grep("^C", row.names(mydata), value=FALSE)
print( mydata[myCindexes , ] )

# Again, using wildcards.
myCindexes <- grep( glob2rx("C*") , 
  row.names(mydata), value=FALSE)
print( mydata[myCindexes , ] )

# ---SELECTING OBSERVATIONS BY subset Function---

subset(mydata,subset=gender=="f")

# ---GENERATING INDEXES A TO Z FROM TWO ROW NAMES---

myMaleA <- which( row.names(mydata)=="Bob" )
myMaleA
myMaleZ <- which( row.names(mydata)=="Rich" )
myMaleZ
print( mydata[myMaleA:myMaleZ , ] )

#---CREATING A NEW DATA FRAME OF SELECTED OBSERVATIONS

# Creating a new data frame of only males (all equivalent).
myMales <- mydata[5:8, ]
myMales
myMales <- mydata[ which( mydata$gender=="m" ) , ]
myMales
myMales <- subset( mydata, subset=gender=="m" )
myMales

# Creating a new data frame of only females (all equivalent).
myFemales <- mydata[1:3, ]
myFemales
myFemales <- mydata[ which( mydata$gender=="f" ) , ]
myFemales
myFemales <- subset( mydata, subset=gender=="f" )
myFemales


