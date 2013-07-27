# Mixed effects Panel AR(I)MA models
# Model Selection, Fitting, and Interpretation
#
# Chris Adolph

# Clear memory
rm(list=ls())

# Load libraries
library(nlme)      # Estimation of mixed effects models
library(lme4)      # Alternative package for mixed effects models
library(arm)       # Gelman & Hill code for mixed effects simulation
library(pcse)      # Calculate PCSEs for LS models (Beck & Katz)
library(tseries)   # For ADF unit root test
library(simcf)     # For panel functions and simulators

# Load Democracy data
# Variable names:
# COUNTRY	YEAR	BRITCOL 	CATH
# CIVLIB	EDT	ELF60	GDPW	MOSLEM
# NEWC    	OIL	POLLIB	REG	STRA
data <- read.csv("democracy.csv",header=TRUE,na.strings=".")

# Create lags and differences now to correctly listwise delete
GDPWlag0 <- lagpanel(data$GDPW,data$COUNTRY,data$YEAR,1)
GDPWdiff0 <- data$GDPW - GDPWlag0

# Listwise delete using only the data we need
selectdata <- na.omit(cbind(data$COUNTRY,
                            data$YEAR,
                            data$GDPW,
                            data$OIL,
                            data$REG,
                            data$EDT,
                            GDPWlag0,
                            GDPWdiff0))
selectdata <- as.data.frame(selectdata)
names(selectdata) <- c("COUNTRY","YEAR","GDPW","OIL",
                       "REG","EDT","GDPWdag","GDPWdiff")

# Now attach the listwise deleted data
attach(selectdata)

# Create a list of the unique country numbers
countrylist <- unique(COUNTRY)

# Create a matrix of fixed effect dummies (for convenience)
fe <- makeFEdummies(COUNTRY)

# Find the number of units
n <- length(countrylist)

# Diagnose time series
# Look at the time series for each country
for (i in 1:length(countrylist)) {
    currcty <- countrylist[i]
    filename <- paste("tsGDPWcty",currcty,".pdf",sep="")
    pdf(filename,width=6,height=3.25)
    plot(GDPW[COUNTRY==currcty],type="l",ylab="GDP",xlab="Time",
         main = paste("Country",currcty) )
    dev.off()
}

# Look at the ACF for each country
for (i in 1:length(countrylist)) {
    currcty <- countrylist[i]
    filename <- paste("acfGDPWcty",currcty,".pdf",sep="")
    pdf(filename,width=6,height=3.25)
    acf(GDPW[COUNTRY==currcty])
    dev.off()
}
   
# Look at the PACF
for (i in 1:length(countrylist)) {
    currcty <- countrylist[i]
    filename <- paste("pacfGDPWcty",currcty,".pdf",sep="")
    pdf(filename,width=6,height=3.25)
    pacf(GDPW[COUNTRY==currcty])
    dev.off()
}

# Check for a unit root in each country
PPtest.pvalues <- rep(0,n)
adftest.pvalues <- rep(0,n)
for (i in 1:length(countrylist)) {
    currcty <- countrylist[i]

    # Check PP unit root test, omitting errors due to short series
    curPP <- try(PP.test(GDPW[COUNTRY==currcty])$p.value)
    if (any(class(curPP)=="try-error")) curPP <- NA
    PPtest.pvalues[i] <- curPP

    curadf <- try(adf.test(GDPW[COUNTRY==currcty])$p.value)
    if (any(class(curadf)=="try-error")) curadf <- NA
    adftest.pvalues[i] <- curadf
  }


pdf("PPtest.pdf",width=6,height=3.25)
hist(PPtest.pvalues)          # Plot a histogram of the p-values
dev.off()

pdf("adftest.pdf",width=6,height=3.25)
hist(adftest.pvalues)         # Plot a histogram of the p-values
dev.off()

# Repeat the ACF, PACF, and unit root tests on the differences data

# Look at the time series for each country in DIFFERENCES
for (i in 1:length(countrylist)) {
    currcty <- countrylist[i]
    filename <- paste("tsGDPWdiffcty",currcty,".pdf",sep="")
    pdf(filename,width=6,height=3.25)
    plot(GDPWdiff[COUNTRY==currcty],type="l",ylab="GDP",xlab="Time",
         main = paste("Country",currcty) )
    dev.off()
}

# Look at the ACF for each country in DIFFERENCES
for (i in 1:length(countrylist)) {
    currcty <- countrylist[i]
    filename <- paste("acfGDPWdiffcty",currcty,".pdf",sep="")
    pdf(filename,width=6,height=3.25)
    acf(GDPWdiff[COUNTRY==currcty])
    dev.off()
}
   
# Look at the PACF in DIFFERENCES
for (i in 1:length(countrylist)) {
    currcty <- countrylist[i]
    filename <- paste("pacfGDPWdiffcty",currcty,".pdf",sep="")
    pdf(filename,width=6,height=3.25)
    pacf(GDPWdiff[COUNTRY==currcty])
    dev.off()
}

# Check for a unit root in each country, differenced
PPtestdiff.pvalues <- rep(0,n)
adftestdiff.pvalues <- rep(0,n)
for (i in 1:length(countrylist)) {
    currcty <- countrylist[i]

    # Check PP unit root test, omitting errors due to short series
    curPPdiff <- try(PP.test(GDPWdiff[COUNTRY==currcty])$p.value)
    if (any(class(curPPdiff)=="try-error")) curPPdiff <- NA
    PPtestdiff.pvalues[i] <- curPPdiff

    curadfdiff <- try(adf.test(GDPWdiff[COUNTRY==currcty])$p.value)
    if (any(class(curadfdiff)=="try-error")) curadfdiff <- NA
    adftestdiff.pvalues[i] <- curadfdiff
  }

pdf("PPtestdiff.pdf",width=6,height=3.25)
hist(PPtestdiff.pvalues)          # Plot a histogram of the p-values
dev.off()

pdf("adftestdiff.pdf",width=6,height=3.25)
hist(adftestdiff.pvalues)         # Plot a histogram of the p-values
dev.off()



#########################################################################
# Estimate a random effects AR(I)MA(p,q) model using lme (Restricted ML)
lme.res1 <- lme(# A formula object including the response,
                # the fixed covariates, and any grouping variables
                fixed = GDPWdiff ~ OIL + REG + EDT,

                # The random effects component
                random = ~ 1 | COUNTRY,

                # The TS dynamics: specify the time & group variables,
                # and the order of the ARMA(p,q) process
                correlation = corARMA(form = ~ YEAR | COUNTRY,
                                      p = 1,  # AR(p) order
                                      q = 0   # MA(q) order
                                      ) 
                )

# Extract model results
pe.res1 <- fixed.effects(lme.res1)        # Point estimates of fixed effects
vc.res1 <- vcov(lme.res1)                 # Var-cov matrix of fixed effects estimates
se.res1 <- sqrt(diag(vc.res1))            # std erros of fixed effects estimates
re.res1 <- random.effects(lme.res1)       # "Estimated" random effects by group 
ll.res1 <- logLik(lme.res1)               # Log-likelihood at maximum
resid.res1 <- resid(lme.res1)             # Residuals
aic.res1 <- AIC(lme.res1)                 # Akaike Information Criterion


# Interpret the model using custom code
sims <- 1000
simbetas <- mvrnorm(sims,pe.res1,vc.res1)

# Make matrix of hypothetical x's
formula <- GDPWdiff ~ OIL + REG + EDT
periods.out <- 50
xhyp <- cfMake(formula,selectdata,periods.out)
for (i in 1:periods.out) 
  xhyp <- cfChange(xhyp, "EDT", x=mean(EDT,na.rm=TRUE)+sd(EDT,na.rm=TRUE),
                                scen=i)

phi <- 0.25 # from model summary()
lagY <- mean(GDPWdiff) # Hypothetical previous change in Y for simulation
initialY <- mean(GDPW) # Hypothetical initial level of Y for simulation

# Simulate expected values of Y (on original level scale)
# out to periods.out given hypothetical future values of X,
# initial lags of the change in Y, and an initial level of Y
sim.ev1 <- ldvsimev(xhyp,               # The matrix of hypothetical x's
                    simbetas,           # The matrix of simulated betas
                    ci=0.95,            # Desired confidence interval
                    constant=1,         # Column containing the constant
                                        # set to NA for no constant
                    phi=phi,            # estimated AR parameters; length must match lagY 
                    lagY=lagY,          # lags of y, most recent last
                    transform="diff",   # "log" to undo log transformation,
                                        # "diff" to under first differencing
                                        # "difflog" to do both
                    initialY=initialY   # for differenced models, the lag of the level of y
                    )
    

# Simulate first differences in Y (on original level scale)
# out to periods.out given hypothetical future values of X, X0,
# and initial lags of the change in Y
sim.fd1 <- ldvsimfd(xhyp,               # The matrix of hypothetical x's
                    simbetas,           # The matrix of simulated betas
                    ci=0.95,            # Desired confidence interval
                    constant=1,         # Column containing the constant
                                        # set to NA for no constant
                    phi=phi,            # estimated AR parameters; length must match lagY 
                    lagY=lagY,          # lags of y, most recent last
                    transform="diff",   # "log" to undo log transformation,
                                        # "diff" to under first differencing
                                        # "difflog" to do both
                    )
    


#########################################################################
# Estimate a fixed effects AR(I)MA(p,q) model using ARIMA (MLE)
arima.res2 <- arima(GDPWdiff, order = c(1,0,0),
                    xreg=cbind(REG,EDT,fe), include.mean = FALSE)

# Extract model results
pe.res2 <- arima.res2$coef       # Point estimates of fixed effects
vc.res2 <- vcov(arima.res2)               # Var-cov matrix of fixed effects estimates
se.res2 <- sqrt(diag(vc.res2))           # std erros of fixed effects estimates
ll.res2 <- logLik(arima.res2)               # Log-likelihood at maximum
resid.res2 <- resid(arima.res2)             # Residuals
aic.res2 <- AIC(arima.res2)                 # Akaike Information Criterion

# Interpret the model using custom code
sims <- 1000
simparam <- mvrnorm(sims,pe.res2,vc.res2)
simphi <- simparam[,1]
simbetas <- simparam[,2:ncol(simparam)]

# Make matrix of hypothetical x's: drop OIL!
#                                  and now we need hypothetical countries!
#                                  and no intercept!

# Make matrix of hypothetical x's
formula <- "GDPWdiff ~ REG + EDT - 1"
selectdatafe <- cbind(selectdata,fe)
fenames <- NULL
for (i in 1:ncol(fe)) {
  formula <- paste(formula,"+ fe",i," ",sep="")
  fenames <- c(fenames,paste("fe",i,sep=""))
}
names(selectdatafe) <- c(names(selectdata),fenames)
formula <- as.formula(formula)
  
periods.out <- 50
xhyp <- cfMake(formula,selectdatafe,periods.out)
for (i in 1:periods.out) 
  xhyp <- cfChange(xhyp, "EDT", x=mean(EDT,na.rm=TRUE)+sd(EDT,na.rm=TRUE), scen=i)

phi <- mean(simphi) 
lagY <- mean(GDPWdiff) # Hypothetical previous change in Y for simulation
initialY <- mean(GDPW) # Hypothetical initial level of Y for simulation


# Simulate expected values of Y (on original level scale)
# out to periods.out given hypothetical future values of X,
# initial lags of the change in Y, and an initial level of Y
sim.ev2 <- ldvsimev(xhyp,               # The matrix of hypothetical x's
                    simbetas,           # The matrix of simulated betas
                    ci=0.95,            # Desired confidence interval
                    constant=NA,        # NA indicates no constant!
                    phi=phi,            # estimated AR parameters; length must match lagY 
                    lagY=lagY,          # lags of y, most recent last
                    transform="diff",   # "log" to undo log transformation,
                                        # "diff" to under first differencing
                                        # "difflog" to do both
                    initialY=initialY    # for differenced models, the lag of the level of y
                    )
    


# Simulate first differences in Y (on original level scale)
# out to periods.out given hypothetical future values of X, X0,
# and initial lags of the change in Y
sim.fd2 <- ldvsimfd(xhyp,               # The matrix of hypothetical x's
                    simbetas,           # The matrix of simulated betas
                    ci=0.95,            # Desired confidence interval
                    constant=NA,        # Column containing the constant
                                        # set to NA for no constant
                    phi=phi,            # estimated AR parameters; length must match lagY 
                    lagY=lagY,          # lags of y, most recent last
                    transform="diff"   # "log" to undo log transformation,
                                        # "diff" to under first differencing
                                        # "difflog" to do both
                    )
   




###########################################################################
# Estimate a fixed effects AR(I)MA(p,q) model using ARIMA 
GDPWbardiff <- REGbar <- EDTbar <- OILbar <- length(COUNTRY)
# Alternative method:  subtract out the fixed effects by country means
for (i in 1:n) {
    currcountry <- countrylist[i]
    for (j in 1:length(COUNTRY)) {
        if (COUNTRY[j]==currcountry) {
            GDPWbardiff[j] <- GDPWdiff[j] - mean(GDPWdiff[COUNTRY==currcountry],na.rm=TRUE)
            REGbar[j] <- REG[j] - mean(REG[COUNTRY==currcountry],na.rm=TRUE)
            EDTbar[j] <- EDT[j] - mean(EDT[COUNTRY==currcountry],na.rm=TRUE)
            OILbar[j] <- OIL[j] - mean(OIL[COUNTRY==currcountry],na.rm=TRUE)
        }
    }
}

# Check for time invariance:
print("Observed REG - mean(REG|COUNTRY)")
print(unique(REGbar))
print("Observed OIL - mean(OIL|COUNTRY)")
print(unique(OILbar))
print("Observed EDT - mean(EDT|COUNTRY)")
print(unique(EDTbar))
    
# Notice OIL is perfectly time invariant for all cases!

# note we do remove the constant in this case
arima.res2a <- arima(GDPWbardiff, order = c(1,0,0),
                    xreg=cbind(REGbar,EDTbar), include.mean = FALSE)


# Extract model results
pe.res2a <- arima.res2a$coef        # Point estimates of fixed effects
vc.res2a <- vcov(arima.res2a)                 # Var-cov matrix of fixed effects estimates
se.res2a <- sqrt(diag(vc.res2a))            # std erros of fixed effects estimates
ll.res2a <- logLik(arima.res2a)               # Log-likelihood at maximum
resid.res2a <- resid(arima.res2a)             # Residuals
aic.res2a <- AIC(arima.res2a)                 # Akaike Information Criterion


################################################################################      
# Estimate a mixed effects AR(I)MA(p,q) model using lme (Restricted ML)
lme.res3 <- lme(# A formula object including the response,
                # the fixed covariates, and the country fixed effects
                # (either as dummy variables or as a "factor" variable),
                # then remove the model intercept with - 1
                fixed = GDPWdiff ~ REG + EDT + fe - 1 ,
                             # NOTE:  I must drop OIL, which doesn't vary over
                             #        time for any country.
                             #        If I leave it in, I get a singularity error

                # The random effects component
                random = ~ 1 | COUNTRY,

                # The TS dynamics: specify the time & group variables,
                # and the order of the ARMA(p,q) process
                correlation = corARMA(form = ~ YEAR | COUNTRY,
                                      p = 1,  # AR(p) order
                                      q = 0   # MA(q) order
                                      ) 
                )

# Extract model results
pe.res3 <- fixed.effects(lme.res3)        # Point estimates of fixed effects
vc.res3 <- vcov(lme.res3)                 # Var-cov matrix of fixed effects estimates
se.res3 <- sqrt(diag(vc.res3))            # std erros of fixed effects estimates
re.res3 <- random.effects(lme.res3)       # "Estimated" random effects by group 
ll.res3 <- logLik(lme.res3)               # Log-likelihood at maximum
resid.res3 <- resid(lme.res3)             # Residuals
aic.res3 <- AIC(lme.res3)                 # Akaike Information Criterion


# Interpret the model using custom code (same as model 2, fixed effects)
sims <- 1000
simbetas <- mvrnorm(sims,pe.res3,vc.res3)

# Make matrix of hypothetical x's: drop OIL!
#                                  and now we need hypothetical countries!
#                                  and no intercept!

# Make matrix of hypothetical x's
formula <- "GDPWdiff ~ REG + EDT - 1"
selectdatafe <- cbind(selectdata,fe)
fenames <- NULL
for (i in 1:ncol(fe)) {
  formula <- paste(formula,"+ fe",i," ",sep="")
  fenames <- c(fenames,paste("fe",i,sep=""))
}
names(selectdatafe) <- c(names(selectdata),fenames)
formula <- as.formula(formula)
  
periods.out <- 50
xhyp <- cfMake(formula,selectdatafe,periods.out)
for (i in 1:periods.out) 
  xhyp <- cfChange(xhyp, "EDT", x=mean(EDT,na.rm=TRUE)+sd(EDT,na.rm=TRUE),
                                scen=i)

phi <- 0.25 # from model summary()
lagY <- mean(GDPWdiff) # Hypothetical previous change in Y for simulation
initialY <- mean(GDPW) # Hypothetical initial level of Y for simulation



# Simulate expected values of Y (on original level scale)
# out to periods.out given hypothetical future values of X,
# initial lags of the change in Y, and an initial level of Y
sim.ev3 <- ldvsimev(xhyp,               # The matrix of hypothetical x's
                    simbetas,           # The matrix of simulated betas
                    ci=0.95,            # Desired confidence interval
                    constant=NA,        # NA indicates no constant!
                    phi=phi,            # estimated AR parameters; length must match lagY 
                    lagY=lagY,          # lags of y, most recent last
                    transform="diff",   # "log" to undo log transformation,
                                        # "diff" to under first differencing
                                        # "difflog" to do both
                    initialY=initialY    # for differenced models, the lag of the level of y
                    )
    

# Simulate first differences in Y (on original level scale)
# out to periods.out given hypothetical future values of X, X0,
# and initial lags of the change in Y
sim.fd3 <- ldvsimfd(xhyp,               # The matrix of hypothetical x's
                    simbetas,           # The matrix of simulated betas
                    ci=0.95,            # Desired confidence interval
                    constant=NA,        # Column containing the constant
                                        # set to NA for no constant
                    phi=phi,            # estimated AR parameters; length must match lagY 
                    lagY=lagY,          # lags of y, most recent last
                    transform="diff"    # "log" to undo log transformation,
                                        # "diff" to under first differencing
                                        # "difflog" to do both
 
                    )
    


#####################################################################
# Make some graphs

# Random effects model of change in GDP given increase in EDT
pdf("simfdREupEDT.pdf",width=5,height=4.5)
plot.new()
par(usr=c(1,50,-15000,15000))
axis(1,at=seq(1,50,10))
axis(2,at=seq(-15000,15000,5000))
title(xlab="Time",ylab="Expected change in constant GDP ($ pc)",main="Random effects ARIMA(1,1,0)") 

# Make the x-coord of a confidence envelope polygon
xpoly <- c(1:periods.out,
           rev(1:periods.out),
           1)

# Make the y-coord of a confidence envelope polygon
ypoly <- c(sim.fd1$lower,
           rev(sim.fd1$upper),
           sim.fd1$lower[1])

# Choose the color of the polygon 
col <- "lightblue"

# Plot the polygon first, before the points & lines
polygon(x=xpoly,
        y=ypoly,
        col=col,
        border=FALSE
        )

# Plot the fitted line
lines(x=1:periods.out,y=sim.fd1$pe,col="blue")
lines(x=c(-10,1100),y=c(0,0),lty="solid")

dev.off()




# Fixed effects model of change in GDP given increase in EDT
pdf("simfdFEupEDT.pdf",width=5,height=4.5)
plot.new()
par(usr=c(1,50,-15000,15000))
axis(1,at=seq(1,50,10))
axis(2,at=seq(-15000,15000,5000))
title(xlab="Time",ylab="Expected change in constant GDP ($ pc)",main="Fixed effects ARIMA(1,1,0)") 

# Make the x-coord of a confidence envelope polygon
xpoly <- c(1:periods.out,
           rev(1:periods.out),
           1)

# Make the y-coord of a confidence envelope polygon
ypoly <- c(sim.fd2$lower,
           rev(sim.fd2$upper),
           sim.fd2$lower[1])

# Choose the color of the polygon 
col <- "lightgreen"

# Plot the polygon first, before the points & lines
polygon(x=xpoly,
        y=ypoly,
        col=col,
        border=FALSE
        )

# Plot the fitted line
lines(x=1:periods.out,y=sim.fd2$pe,col="green")
lines(x=c(-10,1100),y=c(0,0),lty="solid")

dev.off()



##########################
# Mixed effects model of change in GDP given increase in EDT
pdf("simfdMEupEDT.pdf",width=5,height=4.5)
plot.new()
par(usr=c(1,50,-15000,15000))
axis(1,at=seq(1,50,10))
axis(2,at=seq(-15000,15000,5000))
title(xlab="Time",ylab="Expected change in constant GDP ($ pc)",main="Mixed effects ARIMA(1,1,0)") 

# Make the x-coord of a confidence envelope polygon
xpoly <- c(1:periods.out,
           rev(1:periods.out),
           1)

# Make the y-coord of a confidence envelope polygon
ypoly <- c(sim.fd3$lower,
           rev(sim.fd3$upper),
           sim.fd3$lower[1])

# Choose the color of the polygon 
col <- "pink"

# Plot the polygon first, before the points & lines
polygon(x=xpoly,
        y=ypoly,
        col=col,
        border=FALSE
        )

# Plot the fitted line
lines(x=1:periods.out,y=sim.fd3$pe,col="red")
lines(x=c(-10,1100),y=c(0,0),lty="solid")

dev.off()
