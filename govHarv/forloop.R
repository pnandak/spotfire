## HOW TO NOT USE FOR LOOPS


######################################
t <- 1:10
new.t <- c()
for (i in 1:length(t)){
  new.t[i] <- t[i]^2
}

t2 <- t^2  ## better way
######################################




library(pscl)
data(bioChemists)


######################################

for (i in 1:length(bioChemists$fem)){
	ifelse(bioChemists$fem[i]=="Women",bioChemists$fem2[i] <- 1, bioChemists$fem2[i] <- 0)
}
bioChemists$fem <- ifelse(bioChemists$fem == "Women", 1, 0) ## better way

######################################


bioChemists$mar <- ifelse(bioChemists$mar == "Married", 1, 0)
X <- cbind(1, bioChemists$mar, bioChemists$fem, bioChemists$kid5, bioChemists$phd, bioChemists$ment)
Z <- cbind(1, bioChemists$mar)
y <- bioChemists$art



######################################
ll <- function(par, y, x, z){

	betas <- c(par[1:ncol(x)])
	gammas <- c(par[(ncol(x)+1):(ncol(x)+ncol(z))])

	d <- c()
	for (i in 1:length(y)){
		ifelse(y[i]==0,d[i]<-1,d[i]<-0)
	}
	# d <- ifelse(y==0,1,0) # better way


	lambda <- exp(x%*%betas)
	pi <- 1/(1+exp(-z%*%gammas))

	yzero <- c()
	for (j in 1:length(y)){
		yzero[j] <- exp(-lambda[j])
	}
	# yzero <- exp(-lambda) # better way

	ynotzero <- c()
	for (j in 1:length(y)){
		ynotzero[j] <- (exp(-lambda[j])*lambda[j]^(y[j])) / factorial(y[i])
	}
	# ynotzero <- (exp(-lambda)*lambda[j]^(y)) / factorial(y) # better way
	

	loglik <- c()
	for (i in 1:length(y)){
		loglik[i] <- d[i]*log(pi[i] + (1-pi[i])*(yzero[i])) + (1-d[i])*log((1-pi[i])*(ynotzero[i]))
	}
	# loglik <- d*log(pi + (1-pi)*(yzero)) + (1-d)*log((1-pi)*(ynotzero)) # better way

	return(sum(loglik))
}
###############################################



system.time(zip.opt <- optim(par=c(0,0,0,0,0,0,0,0), fn=ll, y=y, x=X, z=Z,
method="BFGS", control=list(fnscale=-1), hessian=T))

zc <- c(1,0)
m <- 1000
par.sim <- mvrnorm(m,mu=zip.opt$par,Sigma=solve(-zip.opt$hessian))
gammas.sim <- par.sim[,7:8]



################################################
pies.s <- c()
for(i in 1:m){
pies.s[i] <- 1/(1+exp(-zc%*%gammas.sim[i,]))
}
# pies.s <- 1/(1+exp(-gammas.sim %*% as.matrix(zc))) # better way
################################################



## USING apply()

# Find the 95% quantiles for each column of the dataset
# Use apply() which ONLY works for 2-dimensional objects (matrices, dataframes)

quants <- matrix(NA, nrow=2, ncol=ncol(bioChemists))
for(i in 1:ncol(bioChemists)){
  quants[,i] <- quantile(bioChemists[,i], probs = c(.025,.975))
}
# apply(bioChemists, MARGIN = 2, FUN = quantile, probs = c(.025,.975))


## USING sapply or lapply (works for vectors and lists)
## sapply tries to return vectors or matrices if possible, whereas lapply returns lists

triangle.function <- function(x) {
  if (x >= 0 && x < 0.25)
    8 * x
  else if (x >= 0.25 && x <= 1)
    8/3 - 8 * x/3
  else 0
}
x <- seq(0, 1, length = 500)
px <- sapply(x, FUN = triangle.function)
