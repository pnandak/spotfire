## Gov 2001
## Problem set 1

###############################################################################
Problem 1
###############################################################################

## To estimate probabilities by simulation, we draw 10,000 samples of size 10
## from a population with 50 women (1's) and 50 men (0's) and store them in holder

set.seed(12345)
population <- c(rep(1, 100), rep(0, 100))
sims <- 10000
holder <- matrix(data = NA, nrow = 10000, ncol = 10)
for(i in 1:sims){
  holder[i,] <- sample(population, size = 10)
}

## Next we create a vector of the number of women in each of the 10,000 samples
women <- apply(holder, 1, sum)

## Now the probability of drawing exactly 7 women in a sample of 10 is:
sum(women==7)/length(women)

## Probability of drawing exactly 7 women or exactly 7 men:
sum(women==7 | women==3)/length(women)

## Probability of drawing 7 or more women or 7 or more men:
sum(women >= 7 | women <= 3)/ length(women)

## Women being 3x more likely to be drawn than men is equivalent to drawing 
## men and women with equal probability from a population with 3x more women than men

population2 <- c(rep(1, 150), rep(0, 50))
holder2 <- matrix(data = NA, nrow = 10000, ncol = 10)
for(i in 1:sims){
  holder2[i,] <- sample(population2, size = 10)
}
women2 <- apply(holder2, 1, sum)
sum(women2==7)/ length(women2)

##############################################################################
## Problem 2
##############################################################################

set.seed(12345)
sims <- 5000
alldays <- seq(1, 365, 1)  #A vector of birthdates
people <- 0                #A counter for the number of people in a room
tries <- 90                #An arbitrary upper bound on the number of people
for (i in 1:tries){
   sameday <- 0            #A counter for the number of duplicate birthdays
   for (j in 1:sims) {
     room <- sample(alldays, people, replace = TRUE)
     if ((length(unique(room)) < people -1) & (length(unique(room[duplicated(room)]))) < length(room[duplicated(room)]))
     sameday <- sameday+1  #### See explanation below
   }
  if (sameday/sims < .5)  #If the proportion of triple-birthdays is less than .5, 
                          # add a person to the room and try again
  people <- people +1
}
people

####The trick to this problem is figuring out a way to determine if three or more 
## people share a birthday. Note that simply altering the condition Gary used to test 
## if 2 or more people share a birthday, with something like 
## if(length(unique(room)) < people -1) isn't sufficient to know if 3 share a birthday.  
## This condition would also pick up the case of 2 pairs of people sharing a birthday.  
## Further specifying length(unique(room[unique(room)])) < length(room[-unique(room)])) 
## makes sure that at least 2 of the nonunique dates are the same date.  
## These two conditions together ensure 3 people share a birthday.  An alternate
## condition would be if(sum(table(room) >= 3) >0), which also checks for 
## 3 sharing a birthday.

## If birthdays in October, November and December are twice as likely, we can include each of
## these birthdays twice.  These birthdates are the last 92 days:

set.seed(12345)
sims <- 1000
alldaysWeighted <- c(seq(1, 365, 1), 274:365)  #Vector of birthdates with oct, nov and dec included twice
people <- 0                            #A counter for the number of people in a room
tries <- 90                            #An arbitrary upper bound on the number of people
for (i in 1:tries){
   sameday <- 0                        # A counter for the number of duplicate birthdays
   for (j in 1:sims) {
     room <- sample(alldaysWeighted, people, replace = TRUE)
     if ((length(unique(room)) < people -1) & (length(unique(room[duplicated(room)]))) < length(room[duplicated(room)]))
     sameday <- sameday+1  #### See explanation above
   }
  if (sameday/sims < .5)  #If the proportion of triple-birthdays is less than .5, 
                          # add a person to the room and try again
  people <- people +1
}
people

########################################################################################
## Problem 3
########################################################################################

##A function that performs OLS in matrix notation is:

OLS <- function(y,X){  #y is the dependent variable vector, X is the covariate marix
   X <- cbind(1,X)                            #add a column of 1's for the intercept
   colnames(X)[1] <- c("Intercept")           #label this column 'intercept'
   betas <- solve(t(X) %*% X) %*% t(X) %*% y  #compute the betas with matrix operations
   fitted <- X %*% as.matrix(betas)           #estiamate the fitted values
   resid <- fitted - y                        #calculate the residuals
   sigma.sq <- t(resid) %*% resid / (nrow(X) - ncol(X))  #calculate sigma squared
   output <- list(betas = betas, sigma.sq = sigma.sq)    #return a list of betas and sigma squared
   return(output)        
}

##Load the data into R
load("ps1_data.RData")

##Sepqrate the dependent and the independent variables for use in the function
y <- as.matrix(data[,1])
X <- as.matrix(data[,-1])

##Use our function to regress y on the covariates x1, x2 and x3
reg <- OLS(y = y, X = X)

##We can check our results with 
lm(y ~ x1 + x2 + x3, data = data)

##Now we store fitted values and residuals

X1 <- cbind(1, X)
fitted <- X1 %*% as.matrix(reg$betas)
resid <- fitted - y

##And plot the fitted values against the residuals

# pdf("hw1_plot1.pdf") #to save the plot as a pdf
plot(x = resid, y = fitted, xlab="residuals", ylab="fitted values")
# dev.off()

## Now we sample 100 datasets with 50 observations each

sims <- 100
mat <- matrix(NA, nrow = sims, ncol = ncol(data))

for(i in 1:sims){
   temp <- sample(1:1000, 50, replace=T)    #Select 50 numbers between 1 and 1000
   new <- data[temp,]                       #Collect the rows of data corresponding to the 50 numbers
   y <- as.matrix(new[,1])                  #label the dependent variable
   X <- as.matrix(new[,-1])                 #label the matrix of covariates
   output <- OLS(y = y, X = X)              #run OLS
   mat[i,] <- output$betas                  #store the betas in our matrix mat
}

## For the mean and standard deviation of each of the four betas, we use
means <- apply(mat, 2, mean)
sds <- apply(mat, 2, sd)

## And make a table
library(Hmisc)
latex(round(cbind(Mean = means,"Standard Deviation" = sds), digits=3), file="", "Table 1")
