# Testing the properties of least squares through Monte Carlo simulation
# Chris Adolph
# April 2010

# Example 2:  Omitted variable bias

# Load required libraries
library(MASS)

# Save graph as a pdf, or print to screen?
makepdf <- TRUE

# Filename of pdf
pdffile <- "ovbScen1.pdf"

# Main title of pdf
maintitle <- "LS with an Omitted Variable"

# Number of simulated datasets to generate
sims <- 1000

# Number of observations in each dataset
n <- 100

# True stadard deviation of the errors
sigma <- sqrt(2)

# True parameters of the model
b <- c(1,2,3,4)

# True means of the covariates
muX <- c(0,0,0)

# True variance-covariance matrix of the covariates
SigmaX <- c(1,  0.,  0,
            0,   1,  0,
            0,   0,  1)
SigmaX <- matrix(SigmaX,nrow=3,ncol=3,byrow=TRUE)

# Create storage matrices for results
allcoefs <- allses <- alltstats <- NULL

# Loop over the simulation runs
for (j in 1:sims) {

  # Draw the simulated covariates from their true
  # multivariate Normal distribution
  X <- mvrnorm(n,muX,SigmaX)

  # Create the simulated y by
  # adding together the systematic and stochastic
  # components, according to the true model
  os <- rep(1,n)
  mu <- cbind(os,X)%*%b
  y <- mu + rnorm(n)*sigma

  # Run a regression of the simulated y on the simulated X
  res <- lm(y~X[,c(1,3)])   # THIS IS CHANGED FROM mcls.r

  # Extract the estimated coefficients
  coefs <- res$coefficients

  # Extract the estimated standard errors of the coefficients
  ses <- sqrt(diag(vcov(res)))

  # Extract the t-statistics of these estimates
  tstats <- coefs/ses

  # Save these results as the next row in the storage matrices
  allcoefs <- rbind(allcoefs,coefs)
  allses <- rbind(allses,ses)
  alltstats <- rbind(alltstats,tstats)
}


# Calculate the average estimate, se, and t-stat
# for each parameter across the simulation runs
# (Note that we have to average the squared se's;
# we can't average sd's, only variances)
avgcoefs <- apply(allcoefs,2,mean)
avgses <- sqrt(apply(allses^2,2,mean))
avgtstats <- avgcoefs/avgses

# Calculate the "true" standard error and "true" t-stat
sdcoefs <- apply(allcoefs,2,sd)
truetstat <- b[c(1,2,4)]/sdcoefs

# Print the results to the screen
print("True parameters")
print(t(b))
print(paste("Average LS estimate across",sims,"simulation runs"))
print(avgcoefs)
print("")
print(paste("True standard errors across",sims,"simulation runs"))
print(sdcoefs)
print(paste("Average estimated standard errors across",sims,"simulation runs"))
print(avgses)
print("")
print(paste("True t-stat across",sims,"simulation runs"))
print(truetstat)
print(paste("Average estimated t-stat across",sims,"simulation runs"))
print(avgtstats)

# Minor helper function which lightens colors for polygons
lighten <- function (col, pct = 0.75, alpha = 1) 
{
    if (abs(pct) > 1) {
        print("Warning:  Error in Lighten; invalid pct")
        pcol <- col2rgb(col)/255
    }
    else {
        col <- col2rgb(col)/255
        if (pct > 0) {
            pcol <- col + pct * (1 - col)
        }
        else {
            pcol <- col * pct
        }
    }
    pcol <- rgb(pcol[1], pcol[2], pcol[3], alpha)
    pcol
}

# Plot smoothed histograms showing the distribution of
# estimate coefficients

if (makepdf) pdf(pdffile,width=7,height=4)

plot.new()
par(usr=c(0,8,0,4))
axis(1,at=seq(0,8,1))
axis(2,at=seq(0,4,1))
title(ylab="Density", main=maintitle)

# Pick some nice colors
cols <- c("#66C2A5", "#FC8D62", "#8DA0CB")

# Add distribution implied by the average se(beta)
xs <- seq(0,10,by=0.01)
polygon(xs,
      dnorm(xs, mean=avgcoefs[2], sd=avgses[2]),
      col=lighten(cols[1]), border=NA)
#polygon(xs,
#      dnorm(xs, mean=avgcoefs[3], sd=avgses[3]),
#      col=lighten(cols[2]), border=NA)
polygon(xs,
      dnorm(xs, mean=avgcoefs[3], sd=avgses[3]),
      col=lighten(cols[3]), border=NA)

# Mark the location of the true values of the parameters
lines(x=c(b[2],b[2]), y=c(0,1), col = cols[1])
#lines(x=c(b[3],b[3]), y=c(0,1), col = cols[2])
lines(x=c(b[4],b[4]), y=c(0,1), col = cols[3])

# Add labels of for the true values below the X axis
mtext(side=1,
      line=2,
      at=b[2],
      text=expression(paste("True",beta[1],sep=" ")),
      col=cols[1])
#mtext(side=1,
#      line=2,
#      at=b[3],
#      text=expression(paste("True",beta[2],sep=" ")),
#      col=cols[2])
mtext(side=1,
      line=2,
      at=b[4],
      text=expression(paste("True",beta[3],sep=" ")),
      col=cols[3])

# Add smoothed histograms of the estimated betas
lines(density(allcoefs[,2])$x,
      density(allcoefs[,2])$y,
      col=cols[1])
#lines(density(allcoefs[,3])$x,
#      density(allcoefs[,3])$y,
#      col=cols[2])
lines(density(allcoefs[,3])$x,
      density(allcoefs[,3])$y,
      col=cols[3])

# Label the smoothed histograms of estimated betas
text(x=avgcoefs[2],
     y=max(density(allcoefs[,2])$y)+0.2,
     label=expression(hat(beta)[1]),
     col=cols[1])
#text(x=avgcoefs[3],
#     y=max(density(allcoefs[,3])$y)+0.2,
#     label=expression(hat(beta)[2]),
#     col=cols[2])
text(x=avgcoefs[3],
     y=max(density(allcoefs[,3])$y)+0.2,
     label=expression(hat(beta)[3]),
     col=cols[3])

if (makepdf) dev.off()

