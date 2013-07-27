##########
#Section 6 Code
###########

##########
#Ordered probit
##########
##Load the data
library(Zelig)
data(sanction)
y <- sanction$coop
y0 <- sort(unique(y))
m = 4

##Make a matrix nrows = # observations, ncols = # categories
##with 1's or 0's if that observation is in that category
Z <- matrix(NA, nrow(sanction), m)
for (j in 1:m)
	Z[,j] <- y==y0[j]
X <- cbind(sanction$target, sanction$cost, sanction$mil)


##Code up the loglikelihood function
ll.oprobit <- function(par, Z, X){
	beta <- par[1:ncol(X)]
	tau <- par[(ncol(X)+1):length(par)]
	ystarmu <- X%*%beta
	m <- length(tau) + 1
	cprobs = matrix(nrow=length(ystarmu), ncol=m)
	probs <- matrix(nrow=nrow(sanction), ncol=m)
	for (j in 1:(m-1))
		cprobs[,j] <- pnorm(tau[j]- ystarmu)
	probs[,m] <- 1-cprobs[,m-1]
	probs[,1] <- cprobs[,1]
	for (j in 2:(m-1))
		probs[,j] <- cprobs[,j] - cprobs[,(j-1)]
	sum(log(probs[Z]))
	}
	
##Optimize
par <- c(rep(1,3),0,1,2)
out <- optim(par, ll.oprobit, Z=Z, X=X, method="BFGS", control=list(fnscale=-1))
out$par

##Let's use zelig to code the ordered probit
z.out <- zelig(factor(coop) ~ target + cost + mil, model="oprobit", data=sanction)

##Set the counterfactual
x.low <- setx(z.out, mil = 0) 
x.high <- setx(z.out, mil = 1)

##Simulate and plot using zelig
s.out <- sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
plot(s.out)

#################
#Zero-inflated logit
################

#Load the data
fish <- read.table("http://www.ats.ucla.edu/stat/R/dae/fish.csv", sep=",", header=T)

X <- fish[c("child", "persons")]
Z <- fish[c("persons")]
X <- as.matrix(cbind(1,X))
Z <- as.matrix(cbind(1,Z))
y <- ifelse(fish$count>0,1,0)

#Program the loglikelihood function
ll.zilogit <- function(par, X, Z, y){
	beta <- par[1:ncol(X)]
	gamma <- par[(ncol(X)+1):length(par)]
	phi <- 1/(1+exp(-Z%*%gamma))
	pie <- 1/(1+exp(-X%*%beta))
	sum(y*log((1-phi)*pie) + (1-y)*(log(phi + (1-phi)*(1-pie))))
	}


#Optimize
par <- rep(1,(ncol(X)+ncol(Z)))

out <- optim(par, ll.zilogit, Z=Z, X=X,y=y, method="BFGS", control=list(fnscale=-1), hessian=TRUE)

out$par


#Let's simulate the predicted probabilities of not having fished

#First, simulate the parameters from the model
varcv.par <- solve(-out$hessian)
library(mvtnorm)
sim.pars <- rmvnorm(10000, out$par, varcv.par)
sim.z <- sim.pars[,(ncol(X)+1):length(par)]


#Estimate the predicted probabilities for each possibility of the number of people in the group
person.vec <- seq(1,4)
Zcovariates <- cbind(1, person.vec)
exp.holder <- matrix(NA, ncol=4, nrow=10000)
for(i in 1:length(person.vec)){
	exp.holder[,i] <- 1/(1+exp(-Zcovariates[i,]%*%t(sim.z)))
	}

#Plot the predicted probabilities
plot(density(exp.holder[,4]), col="blue", xlim=c(0,1), main="Probability of a Structural Zero", xlab="Probability")
lines(density(exp.holder[,3]), col="red")
lines(density(exp.holder[,2]), col="green")
lines(density(exp.holder[,1]), col="black")
legend(.7,12, legend=c("One Person", "Two People", "Three People", "Four People"), col=c("black", "green", "red", "blue"), lty=1)