
# R Program for Joining/Merging Data Sets.

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

#Merge the two dataframes by ID.
both <- merge(myleft,myright,by="id") 
both

#Merge the two dataframes by ID.
both <- merge(myleft,    myright,
            by.x="id", by.y="id" )

#Merge dataframes by both ID and workshop.
both <- merge(myleft,myright,by=c("id","workshop"))
both

#Merge dataframes by both ID and workshop,
#while allowing them to have different names.
both <- merge(myleft,
            myright,
            by.x=c("id","workshop"),
            by.y=c("id","workshop") )
both

