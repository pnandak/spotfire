## Gov 2001 Problem Set 5


## First we load the data
load("data_ps5.RData")

## To make the data numeric, we can use

X <- matrix(NA,23,3)

X[,1] <- 1
X[,2] <- as.numeric(data[,3])
X[,3] <- as.numeric(data[,4])

colnames(X) <- c("Intercept","Temperature","Pressure")

y <- as.numeric(data[,2])

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

    # no need to average over fundamental uncertainty since logit model;
    # we would just get our mu's back

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

pdf(file= "2001_ps5_plot1.pdf")

plot(31:81, pred, type="l", lwd=2, xlab="Temperature at Launch",
 ylab="Expected Probability of O-Ring Failure", ylim = c(0,1))

lines(31:81, bounds[,1], lwd=1.75, col="red", lty=2)
lines(31:81, bounds[,2], lwd=1.75, col="red", lty=2)

rug(X[,2], lwd=1)

dev.off()


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
## tricky.  One way we could think aboutthis is to use the fact that each 
## predicted probability is drawn from a binomial distribution, so the variance 
## is mu(1-mu) and the standard deviation is sqrt(mu(1-mu))

mu.sd <- sqrt(temp[,2]*(1-temp[,2]))
temp <- cbind(temp, mu.sd)

pdf(file= "2001_hw5_plot2.pdf")

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

dev.off()

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


pdf(file= "2001_hw5_plot3.pdf")

plot(31:81, pred, type="l", lwd=2, xlab="Temperature at Launch",
 ylab="Predicted Probability of O-Ring Failure")

lines(31:81, bounds[,1], lwd=1.75, col="red", lty=2)
lines(31:81, bounds[,2], lwd=1.75, col="red", lty=2)

rug(X[,2], lwd=1)

dev.off()

## One way to determine if age matters is to use mission number as a 
## rough measure of age of program:


X2 <- cbind(X, seq(1:23))
colnames(X2)[4] <- "Mission No."


## First we calculate MLE's under the restricted and full models

optim.res <- optim(par = c(1,1,1), logit.ll, y = y, X = X, method="BFGS",
                   control=list(fnscale = -1), hessian = T)
optim.full <- optim(par = c(1,1,1,1), logit.ll, y = y, X = X2, method="BFGS",
                   control=list(fnscale = -1), hessian = T)

## Then we evaluate the log likelihood at both
lik.res <- logit.ll(par = optim.res$par, X = X, y = y)
lik.full <- logit.ll(par = optim.full$par, X = X2, y = y)
lik.res
lik.full
optim.res$value
optim.full$value

## And retrieve our likelihood test statistic:
test.stat <- 2*(lik.full - lik.res)

## Our test statistic is distributed chi-square with 1 degree of freedom (since we had 1 restriction)
1 - pchisq(test.stat, df = 1)
## returns .82
## Using this measure of age, we could not reject the null that age does not matter




## You could also use the same method with a date variable:

Date <- as.Date(data[,1], format="%m/%d/%y")
#And create a measure ranging from oldest to youngest
days <- as.numeric(Date - min(Date))

