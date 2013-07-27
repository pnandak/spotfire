
# MANAGING YOUR FILES AND WORKSPACE

# Set Working Directory, load data, list objects

setwd("myRworkshop")

load(file="mydata100.RData")

ls()

# ---EXAMINING OBJECTS---

print(mydata100)

head(mydata100)

tail(mydata100)

names(mydata100)

row.names(mydata100)

class(mydata100)

attributes(mydata100)

mode(mydata100)

str(mydata100)

str(lm)

ls.str()

library("Hmisc")

contents(mydata100)




# ---DELETING OBJECTS---
#       (not run)

# By name

# rm(q1,q2,q3,q4)


# By pattern
# myQs <- ls(pattern="q")

# rm( list=myQs )


# Removing all objects

rm( list=ls() )

ls()



# ---WORKING DIRECTORY---

# Getting it

getwd()

# Setting it

setwd( "c:/myRfolder" )




# ---SAVING YOUR WORKSPACE---

# Naming what to save

save(mydata,mylist,mymatrix,
  file="myWorkspace.RData")

# Saving all objects

save.image(file="myWorkspace.RData")

# Loading your workspace next time

load( "file=myWorkspace.RData" )



