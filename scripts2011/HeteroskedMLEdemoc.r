# Maximum likelihood estimation in R
# Heteroskedastic normal example using democracy dataset
# Chris Adolph  www.chrisadolph.com

rm(list = ls())
library(MASS)
library(simcf)

# Load the data as a dataframe
data <- read.csv("democracy.csv", na.strings=".")

# Change GDP to GDP in thousands
data$GDPW <- data$GDPW/1000

# Now we want to listwise delete ONLY based on the variables we need
# First, create two formula objects listing all variables we need
allvars <- GDPW ~ EDT + OIL
extra <-  ~ COUNTRY + YEAR 

# Next, use  extractdata  from the simcf library to create a listwise deleted dataset 
lwdata <- extractdata(allvars, data, extra=extra, na.rm = TRUE)

# Attach the listwise deleted data
attach(lwdata)

# Fit least squares model using lm
ls.result <- lm(GDPW~EDT+OIL, data=lwdata)
print(summary(ls.result))

# Calculate and print the AIC
ls.aic <- AIC(ls.result)
print("AIC for LS model")
print(ls.aic)

########################################
# Scenario 1:  Vary EDT

# Calculate predicted values using predict()

# Start by calculating P(Y|w1) for different w1 values
EDTrange <- 0:13                                        # Set as necessary

# Set up a dataframe with the hypothetical scenarios (varied EDT, all else equal)
baseline <- c(mean(EDT), mean(OIL))                     # Set as necessary
xhypo <- matrix(baseline, nrow=length(EDTrange), ncol=length(baseline), byrow= TRUE)
                                                        # Set ncol to # of x's
xhypo <- as.data.frame(xhypo)
names(xhypo) <- c("EDT","OIL")                          # Set by user
xhypo[,1] <- EDTrange                                   # Change as necessary

# Calculate Predicted Y using predict()
simls.EDT <- predict(ls.result, newdata = xhypo, interval = "prediction", level = 0.95)

# Plot them
yplot <- simls.EDT
xplot <- cbind(EDTrange,EDTrange,EDTrange)
pdf("homoGDPWvsEDT.pdf")
matplot(y=yplot,
        x=xplot,
        type="l",
        lty=c("solid","dashed","dashed"),
        col=c("black"),
        xlab = "Avg Male Years of Education",
        ylab = "Predicted GDP per capita in 1000s")
dev.off()


########################################
# Scenario 2:  Vary OIL

# Calculate predicted values using predict()

# Start by calculating P(Y|w1) for different w1 values
OILrange <- 0:1                                         # Set as necessary

# Set up a dataframe with the hypothetical scenarios (varied EDT, all else equal)
baseline <- c(mean(EDT), mean(OIL))                     # Set as necessary
xhypo <- matrix(baseline, nrow=length(OILrange), ncol=length(baseline), byrow= TRUE)
                                                        # Set ncol to # of x's
xhypo <- as.data.frame(xhypo)
names(xhypo) <- c("EDT","OIL")                          # Set by user
xhypo[,2] <- OILrange                                   # Change as necessary

# Calculate Predicted Y using predict()
simls.OIL <- predict(ls.result, newdata = xhypo, interval = "prediction", level = 0.95)

# Plot them
yplot <- simls.OIL
xplot <- cbind(OILrange,OILrange,OILrange)
pdf("homoGDPWvsOIL.pdf")
matplot(y=yplot,
        x=xplot,
        type="l",
        lty=c("solid","dashed","dashed"),
        col=c("black"),
        xlab = "Non-Oil (0) vs Oil-Producing (1)",
        ylab = "Predicted GDP per capita in 1000s")
dev.off()



#####################################################
# Fit ML heteroskedastic normal model using optim()

# Create input matrices
y <- GDPW
xcovariates <- cbind(EDT,OIL)
zcovariates <- cbind(EDT,OIL)

# initial guesses of beta0, beta1, ..., gamma0, gamma1, ...
# we need one entry per parameter, in order!
stval <- c(-1,2,5,5,0,0)

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
hetnorm.result <- optim(stval,llk.hetnormlin,method="BFGS",hessian=TRUE,
                        y=y,x=xcovariates,z=zcovariates)
                   # call minimizer procedure
pe <- hetnorm.result$par   # point estimates
vc <- solve(hetnorm.result$hessian)  # var-cov matrix
se <- sqrt(diag(vc))    # standard errors
ll <- -hetnorm.result$value  # likelihood at maximum
hetnorm.aic <- 2*length(stval) - 2*ll  # Lower is better

print("Point estimates for hetero ML")
print(pe)

print("Standard errors for hetero ML")
print(se)

print("Log Likelihood at its maximum for hetero ML")
print(ll)

print("AIC for hetero ML model")
print(hetnorm.aic)


# Simulate results by drawing from the model predictive distribution
sims <- 10000
simparam <- mvrnorm(sims,pe,vc) # draw parameters

# Separate into the simulated betas and simulated gammas
simbetas <- simparam[,1:(ncol(xcovariates)+1)]
simgammas <- simparam[,(ncol(simbetas)+1):ncol(simparam)]


#########~~~~~~~~~~ You can edit after this  ~~~~~~~~~~~~#########


# Put our models in "formula" form
model <- (GDPW ~ EDT + OIL)
varmodel <- (GDPW ~ EDT + OIL)

########################################
# Scenario 1:  Vary w1

# Start by calculating P(Y|w1) for different w1 values
EDTrange <- 0:13

# Set up a matrix with the hypothetical scenarios (varied EDT, all else equal)
xhypo <- cfMake(model, lwdata, nscen = length(EDTrange))
for (i in 1:length(EDTrange)) {
    xhypo <- cfChange(xhypo, "EDT", x=EDTrange[i], scen=i) 
}

zhypo <- cfMake(varmodel, lwdata, nscen = length(EDTrange))
for (i in 1:length(EDTrange)) {
    zhypo <- cfChange(zhypo, "EDT", x=EDTrange[i], scen=i) 
}

# Simulate the predicted Y's and CI's
simres.EDT <- hetnormsimpv(xhypo,simbetas,
                           zhypo,simgammas,
                           ci=0.95,
                           constant=1,
                           varconstant=1)

# Plot them
yplot <- cbind(simres.EDT$pe, simres.EDT$lower, simres.EDT$upper)
xplot <- cbind(EDTrange,EDTrange,EDTrange)
pdf("heteroGDPWvsEDT.pdf")
matplot(y=yplot,
        x=xplot,
        type="l",
        lty=c("solid","dashed","dashed"),
        col=c("black"),
        xlab = "Avg Male Years of Education",
        ylab = "Predicted GDP per capita in 1000s")
dev.off()


########################################
# Scenario 2:  Vary OIL

# Start by calculating P(Y|OIL) for different w1 values
OILrange <- 0:1

# Set up a matrix with the hypothetical scenarios (varied w1, all else equal)
xhypo <- cfMake(model, lwdata, nscen = length(OILrange))
for (i in 1:length(OILrange)) {
    xhypo <- cfChange(xhypo, "OIL", x=OILrange[i], scen=i) 
}

zhypo <- cfMake(varmodel, lwdata, nscen = length(OILrange))
for (i in 1:length(OILrange)) {
    zhypo <- cfChange(zhypo, "OIL", x=OILrange[i], scen=i) 
}

# Simulate the predicted Y's and CI's
simres.OIL <- hetnormsimpv(xhypo,simbetas,
                           zhypo,simgammas,
                           ci=0.95,
                           constant=1,varconstant=1)

# Plot them
yplot <- cbind(simres.OIL$pe, simres.OIL$lower, simres.OIL$upper)
xplot <- cbind(OILrange,OILrange,OILrange)
pdf("heteroGDPWvsOIL.pdf")
matplot(y=yplot,
        x=xplot,
        type="l",
        lty=c("solid","dashed","dashed"),
        col=c("black"),
        xlab = "Non-Oil (0) vs Oil-Producing (1)",
        ylab = "Predicted GDP per capital in 1000s")
dev.off()












