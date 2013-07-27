## Section 7
## Jens Hainmueller and Jenn Larson

## Review of expected/predicted values from PS5
## This file includes useful R tidbits: subsetting, if statements, 
##	plotting tips, and R-generated latex tables
## Implementation of probit and ordered probit

################################################################################
## Expected/predicted values from PS5
################################################################################

## First we load the data
load("data_ps5.RData")

## To make the data numeric, we can use

X <- matrix(NA,23,3)

X[,1] <- 1
X[,2] <- as.numeric(data[,3])
X[,3] <- as.numeric(data[,4])

colnames(X) <- c("Intercept","Temperature","Pressure")

y <- as.numeric(data[,2])








## Here is the log likelihood of the logit model:

logit.ll <- function(par, X, y){#par is the parameter, X is the matrix of 
   #covariates with a column of 1s, Y is the observed dependent variable
   betas <- par[1:ncol(X)]
   mu <- X %*% betas 
   out <- -sum(log(1 + (exp((1-(2*y))*mu))))
  return(out)
}




## Maximize the log likelihood
optim.out <- optim(par = c(1,1,1), logit.ll, y = y, X = X, method="BFGS",
                   control=list(fnscale = -1), hessian = T)










## Calculate standard errors

se <- sqrt(diag(solve(-optim.out$hessian)))








## We can compare our MLEs with the results of a logit model in Zelig

library(Zelig)

data <- as.data.frame(cbind(y,X))
names(data) <- c("Incident", "Intercept", "Temperature", "Pressure")

out <- zelig(Incident ~ Temperature + Pressure, data = data, model = "logit")




optim.out$par
se
summary(out)

## Our MLE's are verified by the logit model 






## Now we calculate first differences
## First we set our x's

X.old <- X
X.new <- X
X.old[,2] <- 53
X.new[,2] <- 31




## And extract the var-covar matrix
VCV <- solve(-optim.out$hessian)






## To simulate first differences in expected probabilities, we use
sims <- 1000
temp <- rep(NA, sims)
set.seed(12345)

for(i in 1:sims){ 
    sim.coef <- mvrnorm(n = 1, mu = optim.out$par, Sigma = VCV)

    mu.old <- 1/ (1+ exp(-X.old %*% sim.coef))
    mu.new <- 1/ (1+ exp(-X.new %*% sim.coef)) 

    temp[i] <- mean(mu.new - mu.old)

}

mean(temp)
sd(temp)
quantile(temp, prob = c(.025, .975))






## Simulating expected values


sims <- 1000
start <- 31
stop <- 81
temp <- matrix(data = NA, nrow = sims, ncol = stop - start + 1)

X.new <- X

for(j in start : stop){
	X.new <- X
	X.new[,2] <- j
	for(i in 1:sims){ 
        	sim.coef <- mvrnorm(n = 1, mu = optim.out$par, Sigma = VCV)
		mu.new <- 1/ (1+ exp(-X.new %*% sim.coef)) 
        	temp[i,j-(31-1)] <- mean(mu.new)
	}
	cat("Simulating Temperature:",j, "degrees \n") #to entertain yourself during the iterations
}

pred <- apply(temp,2,mean)



## Construct .95 confidence intervals (illustrating another method of creating intervals)
up <- round(sims * .975)
low <- round(sims * .025)

bounds <- matrix(NA,51,2)
bounds[,1] <- apply(temp,2,sort)[low,]
bounds[,2] <- apply(temp,2,sort)[up,]



## Now we plot the expected probabilities

plot(31:81, pred, type="l", lwd=2, xlab="Temperature at Launch",
 ylab="Expected Probability of O-Ring Failure", ylim = c(0,1))

lines(31:81, bounds[,1], lwd=1.75, col="red", lty=2)
lines(31:81, bounds[,2], lwd=1.75, col="red", lty=2)

rug(X[,2], lwd=1)





## Now we simulate predicted values (1.5)
## A predicted value is a value of the outcome variable; in this case, either a 0 or a 1

sims <- 1000
start <- 31
stop <- 81
temp <- matrix(data = NA, nrow = stop - start + 1, ncol = 2)


for(j in start:stop){
	X.new <- X
	X.new[,2] <- j
      for(i in 1:sims){ 
	      sim.coef <- mvrnorm(n = 1, mu = optim.out$par, Sigma = VCV)
		mu.new <- 1/ (1+ exp(-X.new %*% sim.coef)) 
		mu <- mean(mu.new)
		y.new <- rbinom(n = 1, size = 1, prob = mu)
        		# adds fundamental uncertainty to our prediction
		temp[j - (start - 1), 1] <- y.new
		temp[j - (start - 1), 2] <- mu # we can store the probability to get
                                           # a measure of uncertainty
	}
	cat("Simulating Temperature:",j-30, "degrees \n")
}

temp




## Finding a measure of uncertainty for a point prediction of a dichotomous variable is 
## tricky.  One way we could think about this is to use the fact that each 
## predicted probability is drawn from a binomial distribution, so the variance 
## is mu(1-mu) and the standard deviation is sqrt(mu(1-mu))

mu.sd <- sqrt(temp[,2]*(1-temp[,2]))
temp <- cbind(temp, mu.sd)

###########
## Sidenote: Useful Plotting method:
###########


plot(31:81, temp[,1], lwd=2, xlab="Temperature at Launch",
 ylab="Predicted O-Ring Failure")


## So let's add a line indicating the standard deviation (estimated from the 
## bernoulli) for each predicted failure or non-failure

for(i in start:stop){
	if(temp[i - (start - 1),1] == 1){
		segments(x0 = i, y0 = temp[i - (start - 1),1], x1 = i, y1 = temp[i - (start - 1),1] - temp[i - (start - 1),3], col = "red")
	}
	if(temp[i - (start - 1),1] == 0){
		segments(x0 = i, y0 = temp[i - (start - 1),1], x1 = i, y1 = temp[i - (start - 1),1] + temp[i - (start - 1),3], col = "red")
	}
}

rug(X[,2], lwd=1)



## We can also simulate predicted probabilities (as opposed to predicted values)
## (This simulation features another way to set up the indices in the 
## for loops correctly)

sims <- 1000
r <- 51
temp <- matrix(NA, sims, r)

X.new <- X
X.new[,2] <- 30

for(j in 1:r)   {
    X.new[,2] <- 30 + j
    for(i in 1:sims)    { 
        sim.coef <- mvrnorm(n = 1, mu = optim.out$par, Sigma = VCV)
        mu.new <- 1/ (1+ exp(-X.new %*% sim.coef)) 

     	  y.new <- rbinom(n = 23, size = 1, prob = mu.new)
        # adds fundamental uncertainty to our prediction

     	  temp[i,j] <- mean(y.new)

    }
    cat("Simulating Temperature:",j+30, "degrees \n")
}

pred <- apply(temp,2,mean)


# construct .95 confidence intervals
up <- round(sims * .975)
low <- round(sims * .025)

bounds <- matrix(NA,51,2)
bounds[,1] <- apply(temp,2,sort)[low,]
bounds[,2] <- apply(temp,2,sort)[up,]



plot(31:81, pred, type="l", lwd=2, xlab="Temperature at Launch",
 ylab="Predicted Probability of O-Ring Failure")

lines(31:81, bounds[,1], lwd=1.75, col="red", lty=2)
lines(31:81, bounds[,2], lwd=1.75, col="red", lty=2)

rug(X[,2], lwd=1)


##########################################################################
## Implementing a Probit Model
##########################################################################

## Let's use some data from the 2004 NES to evaluate the effect of 
## different covariates on voter turnout

load("FinalData6.RData")

dim(final.data)
colnames(final.data)


# Organize data

X <- cbind(1,final.data[,1:11])
blackXcontact <-  X[,10] *  X[,4] # include interaction b/t black and contact
X <- cbind(X, blackXcontact)
X <- as.matrix(X[,-(11:12)]) # do not include attention
colnames(X)

y <- final.data[,10]


# Probit LL
probit.ll <- function(beta,X,y)     {
	phi <- pnorm(X %*% beta)
	out <- sum(y * log(phi) + (1-y) * log(1-phi))
}




# Maximize LL
optim.out <- optim(par = rep(0,11), probit.ll, y = y, X = X, method="BFGS",
                   control=list(fnscale = -1), hessian = T)


# Calculate standard errors
se <- sqrt(diag(solve(-optim.out$hessian)))
VCV <- solve(-optim.out$hessian)  



# check with canned function; almost identical
out <- glm(y ~ X, family=binomial(link="probit"))
summary(out)


############
## Side note: here's a useful way to construct Latex tables:
############

library(Design)                   
table1 <- matrix(NA,11,2)          
colnames(table1) <- c("estimate", "se")
rownames(table1) <- colnames(X)
rownames(table1)[1] <- "Intercept"
rownames(table1)[11] <- "black $\times$ contact"

table1[,2] <- se                           
table1[,1] <- optim.out$par       
                                  
table1 <- round(table1, digits=3) 
                                  
latex(table1, "Table 1", file="")

## Suppose we're interested in knowing the difference in turnout resulting from
## personal contact

## We then want to know the first difference between no contact and contact

###############
## Sidenote: Useful data examination tip:
###############

## If we can't remember how contact was coded, we can use
summary(X)

## Since our data are in a matrix, we can subset with colnames
colnames(X)
summary(X[,"contact"])

## Or, if we want to know which column corresponds to contact,
which(colnames(X) == "contact")

summary(X[,10])

## We can look at a histogram
hist(X[,10])

## And verify that every obesrvation is either a 0 or a 1
table(X[,10])

## Or a density
plot(density(X[,10]))

##{which is more useful here? The histogram or the density plot?)
summary(X[,"contact"])






# Define first differences:
X.old <- X
X.new <- X
X.old[,10] <- 0
X.new[,10] <- 1

# Don't forget that contact was interacted with black.
## We have to set contact in the interaction term too.
## (Do NOT leave interaction as is!)

X.old[,11] <- 0
which(colnames(X) == "black")
X.new[,11] <- X[,4]


library(MASS)
# simulate first differences in expected probabilities                                  
sims <- 1000                                                                           
temp <- rep(NA, sims)                                                                   
                                                                                        
                                                                                        
for(i in 1:sims)    {                                                                   
                                                                                        
    sim.coef <- mvrnorm(n = 1, mu = optim.out$par, Sigma = VCV)                         
                                                                                        
    mu.old <- pnorm(X.old %*% sim.coef)                                        
    mu.new <- pnorm(X.new %*% sim.coef)                                          
                                                                                        
    y.old <- rbinom(n = nrow(X), size = 10000, prob = mu.old)
    y.new <- rbinom(n = nrow(X), size = 10000, prob = mu.new)

    y.old <- y.old / 10000
    y.new <- y.new / 10000

    temp[i] <- mean(y.new - y.old)                                                    
                                                                                        
}                                                                           

mean(temp)                                                                              
sd(temp)                                                                                


                                                                                        
# construct .95 confidence intervals                                                    
up <- round(sims * .975)                                                                
low <- round(sims * .025)                                                               
CI <- c(sort(temp)[low], sort(temp)[up])                                                
                                                                                        
# confidence interval for increase in probability of failure                            
CI                                                                                      



###########################################################################
## Implementing an ordered Probit with Zelig
############################################################################

## Suppose now we're interested in the dependent variable of attention

## First we load Zelig
library(Zelig)

## And organize our data into a dataframe for Zelig, letting attention
## be the dependent variable

## Again, we can inspect the attention variable
head(final.data)
class(final.data)
summary(final.data$attention)
which(colnames(final.data) == "attention")


y <- final.data[,11]
data <- cbind(y,X)
data <- as.data.frame(data)
data$y  <- factor(data$y, ordered=T, levels=c(1,2,3,4))


z.out <- zelig(y ~ educ + income + dem + rep + lib + con + age +
black*contact, model = "oprobit", data = data)
summary(z.out)


table2 <- matrix(NA,13,3)          
colnames(table2) <- c("estimate", "se", "t-value")
rownames(table2) <- c(names(z.out$coeff),names(z.out$zeta))
rownames(table2)[10] <- "black $\\times$ contact"

table2[,1] <- summary(z.out)$coeff[,1]                          
table2[,2] <- summary(z.out)$coeff[,2]       
table2[,3] <- summary(z.out)$coeff[,3]       
                                  
table2 <- round(table2, digits=3) 
                                  
latex(table2, "Table 2", file="")

## Let's simulate first differences to answer the question
## 'What is the effect of changing from the lowest level of education
## 	to the highest level of education on attention paid to campaigns'?

## We define our first differences, setting education to the lowest and 
## highest level

X.low <- setx(z.out, educ = 1)
X.high <- setx(z.out, educ = 7)

s.out <- sim(z.out, x = X.low, x1 = X.high, num=10000)
summary(s.out)

## Zelig includes some illustrative plots                 

plot(s.out)

## We can extract the first differences with

table3 <- summary(s.out)$qi.stats$fd
rownames(table3) <- c("A Great Deal", "Quite a Bit","Some", "Very Little or Less")
                          
## And make our latex table with

table3 <- round(table3, digits=3) 
latex(table3, "Table 3", file="")