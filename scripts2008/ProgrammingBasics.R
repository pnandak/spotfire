workshop <- c(1,2,1,2,1,2,1,2)
gender <- c("f","f","f",NA,"m","m","m","m")
q1 <- c(1,2,2,3,4,5,5,4)
q2 <- c(1,1,2,1,5,4,3,5)
q3 <- c(5,4,4,NA,2,5,4,5)
q4 <- c(1,1,3,3,4,5,4,5)

workshop <- factor(
workshop,
levels=c(1,2,3,4),
labels=c("R","SAS","SPSS","STATA")
)

gender <- factor(gender)

> gender <- factor(
+ gender,
+ levels=c("m","f"),
+ labels=c("Male","Female")
+ )

mydata <- data.frame(workshop,gender,q1,q2,q3,q4)
mydata

# Example renaming gender to sex while creating
# a data frame.
# mydata <- data.frame(workshop, sex=gender, 
#  q1, q2, q3, q4)

mymatrix <- cbind(q1, q2, q3, q4)
dim(mymatrix)
mymatrix
table(mymatrix)
cor( mymatrix, use="pairwise")

mylist <- list(workshop, gender, 
  q1, q2, q3, q4, mymatrix)
mylist

save(workshop,gender,q1,q2,q3,q4,
  file="myVectors.RData")
save(mydata, file="mydata.RData")
save(mylist, file="myList.RData")

# Add a function to myAll.RData
# R program that creates a simple function.

mystats <- function(x) 
{
  myinput <- x
  mymean  <- mean(x, na.rm=TRUE) 
  mysd    <-   sd(x, na.rm=TRUE)     
  list(data=myinput, mean=mymean, sd=mysd) 
}
save(mydata,mymatrix,mylist,mystats,
  file="myAll.RData")