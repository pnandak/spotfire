# Maximum likelihood estimation in R
# Heteroskedastic normal example
# Chris Adolph  www.chrisadolph.com

rm(list = ls())
set.seed(123456)
library(MASS)
library(simcf)


#############################################
# Draw heteroskedastic normal data

# Generate 1500 observations
obs <- 1500

# Set the parameter vector for the mean
beta <- c(0, 5, 15)

# Set the parameter vector for the variance
gamma <- c(1, 0, 3)

# Create the constant and covariates
w0 <- rep(1,obs)
w1 <- runif(obs)
w2 <- runif(obs)
x <- cbind(w0,w1,w2)
z <- x

# Create the systematic component for the mean
mu <- x%*%beta

# Create the systematic component for the variance
sigma2 <- exp(z%*%gamma)

# Create the response variable
y <- rnorm(obs)*sqrt(sigma2) + mu

# Save the data to a datafram
data <- cbind(y,w1,w2)
data <- as.data.frame(data)
names(data) <- c("y","w1","w2")

# Plot the data
pdf("YvsW1.pdf")
plot(y=y,x=w1)
dev.off()

pdf("YvsW2.pdf")
plot(y=y,x=w2)
dev.off()

pdf("W1vsW2.pdf")
plot(y=w1,x=w2)
dev.off()

###############################
# From here, we pretend we don't know the true specification

# Fit least squares model using lm
ls.result <- lm(y~w1+w2)
print(summary(ls.result))

# Calculate and print the AIC
ls.aic <- AIC(ls.result)
print("AIC")
print(ls.aic)

########################################
# Scenario 1:  Vary w1

# Calculate predicted values using predict()

# Start by calculating P(Y|w1) for different w1 values
w1range <- seq(0:20)/20                                               # Set as necessary

# Set up a dataframe with the hypothetical scenarios (varied w1, all else equal)
baseline <- c(mean(w1), mean(w2))                                     # Set as necessary
xhypo <- matrix(baseline, nrow=length(w1range), ncol=2, byrow= TRUE)  # Set ncol to # of x's
xhypo <- as.data.frame(xhypo)
names(xhypo) <- c("w1","w2")                                          # Set by user
xhypo[,1] <- w1range                                                  # Change as necessary

# Calculate Predicted Y using predict()
simls.w1 <- predict(ls.result, newdata = xhypo, interval = "prediction", level = 0.95)

# Plot them
yplot <- simls.w1
xplot <- cbind(w1range,w1range,w1range)
pdf("homoYvsW1.pdf")
matplot(y=yplot,
        x=xplot,
        type="l",
        lty=c("solid","dashed","dashed"),
        col=c("black"),
        xlab = "w1",
        ylab = "Predicted Y")
dev.off()



########################################
# Scenario 2:  Vary w2

# Calculate predicted values using predict()

# Start by calculating P(Y|w1) for different w1 values
w2range <- seq(0:20)/20                                               # Set as necessary

# Set up a dataframe with the hypothetical scenarios (varied w1, all else equal)
baseline <- c(mean(w1), mean(w2))                                     # Set as necessary
xhypo <- matrix(baseline, nrow=length(w2range), ncol=2, byrow= TRUE)  # Set ncol to # of x's
xhypo <- as.data.frame(xhypo)
names(xhypo) <- c("w1","w2")                                          # Set by user
xhypo[,2] <- w1range                                                  # Change as necessary

# Calculate Predicted Y using predict()
simls.w2 <- predict(ls.result, newdata = xhypo, interval = "prediction", level = 0.95)

# Plot them
yplot <- simls.w2
xplot <- cbind(w2range,w2range,w2range)
pdf("homoYvsW2.pdf")
matplot(y=yplot,
        x=xplot,
        type="l",
        lty=c("solid","dashed","dashed"),
        col=c("black"),
        xlab = "w1",
        ylab = "Predicted Y")
dev.off()





#####################################################
# Fit ML heteroskedastic normal model using optim()

# Create input matrices
xcovariates <- cbind(w1,w2)
zcovariates <- cbind(w1,w2)

# initial guesses of beta0, beta1, ..., gamma0, gamma1, ...
# we need one entry per parameter, in order!
stval <- c(0,0,0,0,0,0)

######~~~~~~~~~ No need to edit in here ~~~~~~~~~######

# A likelihood function for ML heteroskedastic Normal
llk.hetnormlin <- function(param,y,x,z) {
    x <- as.matrix(x)
    z <- as.matrix(z)
    os <- rep(1,nrow(x))
    x <- cbind(os,x)
    z <- cbind(os,z)
    b <- param[ 1 : ncol(x) ]
    g <- param[ (ncol(x)+1) : (ncol(x) + ncol(z)) ]
    xb <- x%*%b
    s2 <- exp(z%*%g)
    sum(0.5*(log(s2)+(y-xb)^2/s2))  # optim is a minimizer, so min -ln L(param|y)
}

# Run ML, get the output we need
hetnorm.result <- optim(stval,llk.hetnormlin,method="BFGS",hessian=T,y=y,x=xcovariates,z=zcovariates)
                   # call minimizer procedure
pe <- hetnorm.result$par   # point estimates
vc <- solve(hetnorm.result$hessian)  # var-cov matrix
se <- sqrt(diag(vc))    # standard errors
ll <- -hetnorm.result$value  # likelihood at maximum
hetnorm.aic <- 2*length(stval) - 2*ll  # Lower is better

print("Point estimates")
print(pe)

print("Standard errors")
print(se)

print("Log Likelihood at its maximum")
print(ll)

print("AIC")
print(hetnorm.aic)


# Simulate results by drawing from the model predictive distribution
sims <- 10000
simparam <- mvrnorm(sims,pe,vc) # draw parameters

# Separate into the simulated betas and simulated gammas
simbetas <- simparam[,1:(ncol(xcovariates)+1)]
simgammas <- simparam[,(ncol(simbetas)+1):ncol(simparam)]


#########~~~~~~~~~~ You can edit after this  ~~~~~~~~~~~~#########


# Put our models in "formula" form
model <- (y ~ w1 + w2)
varmodel <- (y ~ w1 + w2)

########################################
# Scenario 1:  Vary w1

# Start by calculating P(Y|w1) for different w1 values
w1range <- seq(0:20)/20

# Set up a matrix with the hypothetical scenarios (varied w1, all else equal)
xhypo <- cfMake(model, data, nscen = length(w1range))
for (i in 1:length(w1range)) {
    xhypo <- cfChange(xhypo, "w1", x=w1range[i], scen=i) 
}

zhypo <- cfMake(varmodel, data, nscen = length(w1range))
for (i in 1:length(w1range)) {
    zhypo <- cfChange(zhypo, "w1", x=w1range[i], scen=i) 
}

# Simulate the predicted Y's and CI's
simres.w1 <- hetnormsimpv(xhypo,simbetas,
                          zhypo,simgammas,
                          ci=0.95,
                          constant=1,varconstant=1)

# Plot them
yplot <- cbind(simres.w1$pe, simres.w1$lower, simres.w1$upper)
xplot <- cbind(w1range,w1range,w1range)
pdf("heteroYvsW1.pdf")
matplot(y=yplot,
        x=xplot,
        type="l",
        lty=c("solid","dashed","dashed"),
        col=c("black"),
        xlab = "w1",
        ylab = "Predicted Y")
dev.off()


########################################
# Scenario 2:  Vary w2

# Start by calculating P(Y|w2) for different w1 values
w2range <- seq(0:20)/20

# Set up a matrix with the hypothetical scenarios (varied w1, all else equal)
xhypo <- cfMake(model, data, nscen = length(w2range))
for (i in 1:length(w2range)) {
    xhypo <- cfChange(xhypo, "w2", x=w2range[i], scen=i) 
}

zhypo <- cfMake(varmodel, data, nscen = length(w2range))
for (i in 1:length(w2range)) {
    zhypo <- cfChange(zhypo, "w2", x=w2range[i], scen=i) 
}

# Simulate the predicted Y's and CI's
simres.w2 <- hetnormsimpv(xhypo,simbetas,
                          zhypo,simgammas,
                          ci=0.95,
                          constant=1,varconstant=1)

# Plot them
yplot <- cbind(simres.w2$pe, simres.w2$lower, simres.w2$upper)
xplot <- cbind(w1range,w1range,w1range)
pdf("heteroYvsW2.pdf")
matplot(y=yplot,
        x=xplot,
        type="l",
        lty=c("solid","dashed","dashed"),
        col=c("black"),
        xlab = "w2",
        ylab = "Predicted Y")
dev.off()












