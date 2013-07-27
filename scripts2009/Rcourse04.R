##
## Rcourse04.R
##
## Example code for "Modern Applied Statistics Using R" - Programming
##
## Alexander.Ploner@ki.se 2007-09-24
##
########################################################################


##----------------- Script files -----------------------------##

# This an example for a typical script file
# We compare weights of chickens receiving different food additives
# See ?chickwts
# Same code as Chickwts.R

# Move to a prepared directory
olddir = getwd()
setwd("~/USERS/Rcourse/example/chickwts")

# We want to do a Tukey pairwise t-test later
require(multcomp)

# Set up a local copy of the data
# Generally, a load() or attach() or read.table() or similar
data(chickwts)
# Make variables generally available
attach(chickwts)

# Do a linear model
lm_cw = lm(weight ~ feed, chickwts)

# Do the Tukey test
tuk_cw = glht(lm_cw, mcp(feed="Tukey"))

# We store the actual results if we need to process them further
save(lm_cw, tuk_cw, file="chickwtsResult.RData")

# We redirect useful text output to a file
sink("chickwtsOutput.Rout")
print(table(feed))
print(tapply(weight, feed, summary))
print(summary(lm_cw))
print(summary(tuk_cw))
sink()

# We save useful graphs for further study/reporting
pdf("chickwtsDiag.pdf")
par(mfrow=c(2,2))
plot(lm_cw)
dev.off()
pdf("chickwtsSortedBoxplots.pdf", width=8, height=8)
meds  = tapply(weight, feed, median)
smeds = sort(meds)
feed2 = factor(feed, levels=names(smeds), ordered=TRUE)
boxplot(weight ~ feed2, xlab="Feed", ylab="Weight")
dev.off()

# Clean up
detach(chickwts)
setwd(olddir)

##--------------- Functions -----------------------------------##

# We take the code for producing a sorted boxplot from above
# and wrap it into a function
sboxplot = function(x, g)
{
    meds  = tapply(x, g, median)
    smeds = sort(meds)
    gsort = factor(g, names(smeds), ordered=TRUE)
    boxplot(x ~ gsort)
}

# This we can apply to our data
attach(chickwts)
sboxplot(weight, feed)

# We improve this version: 
# a) optional line to connect medians
# b) pass extra arguments to boxplot
# Note the default value for add.line
sboxplot2 = function(x, g, add.line=FALSE, ...)
{
    meds  = tapply(x, g, median)
    smeds = sort(meds)
    gsort = factor(g, names(smeds), ordered=TRUE)
    boxplot(x ~ gsort, ...)
    if (add.line) {
        lines(1:length(smeds), smeds, type="o")
    }   
}

# Call the new version
sboxplot2(weight, feed)  # same as before
sboxplot2(weight, feed, add=TRUE)   # plot line
sboxplot2(weight, feed, col="red")  # passed to boxplot
# Make use of multiple boxplot-arguments
sboxplot2(weight, feed, col="red", horizontal=TRUE, las=1)

# Another modification: use either classic or lattice graphs
sboxplot3 = function(x, g, lattice=FALSE, ...)
{
    # Identical
    meds  = tapply(x, g, median)
    smeds = sort(meds)
    gsort = factor(g, names(smeds), ordered=TRUE)
    # Switch
    if (!lattice) {
        boxplot(x ~ gsort, ...)
    } else {
        require(lattice)
        bwplot(x ~ gsort, ...)
    }
        
}
# Use it 
sboxplot3(weight, feed, lattice=FALSE)
sboxplot3(weight, feed, lattice=TRUE)
sboxplot3(weight, feed, lattice=TRUE, box.ratio=0.1)

# We extract estimates and confidence intervals from a model
coefTab = function(x, level=0.95, signif)
{
   est = coef(x)
   ci  = confint(x, level=level)
   # We bind the column vector of estimates to the matrix of 
   # confidence limts
   ret = cbind(Estimate=est, ci)
   # If number of significant dgits is specified, 
   # round ret accordingly
   if (!missing(signif)) {
       ret = signif(ret, digits=signif)
   }
   # Return the result
   ret
}
# Apply this to the chicken weight model
coefTab(lm_cw)
coefTab(lm_cw, level=0.99)
coefTab(lm_cw, signif=3)   # simplify
coefTab(lm_cw, signif=1)   # simplify radical
coef_cw = coefTab(lm_cw)   # store for later use

# Construct a simple bootstrap confidence interval for the mean
bootMean = function(x, level=0.95, B=3000)
{
    xbar = mean(x)
    boot = numeric(B)
    for (i in 1:B) {
        cursamp = sample(x, replace=TRUE)
        boot[i] = mean(cursamp)
    }
    # The confidence limits
    lim = c((1-level)/2, 0.5+level/2)
    ci = quantile(boot, lim)
    ci
}
# Apply as required
set.seed(20070924) # set fixed starting point for random number generation
x = rnorm(100, mean=1, sd=2)
bootMean(x)

# Compute the maximum likelihood estimate for zero-truncated 
# Poisson variable, using iteration
# This is taken from enables & Ripley, MASS, 3rd ed, p.95
#   x       data
#   eps     convergence tolerance
#   maxit   maximum number of iterations
mleTruncPois = function(x, eps=0.0001, maxit=25)
{
    # The mean
    xbar = mean(x)
    # Set up the iteration
    est  = xbar    # starting value for estimate
    corr = 1       # starting value for correction term
    it   = 0       # iteration conter
    
    while ((corr > eps) & (it <= maxit)) {
        corr = (est - xbar*(1 - exp(-est)))/(1 - xbar*exp(-est))
        est  = est - corr  
        it   = it + 1
    }
    
    # Check: convergence or out of iterations?
    if (corr > eps) {
        stop("No convergence")
    } else {
        est
    }
}
# We generate a zer-truncated Poisson sample
y = rpois(50, lambda=1)
table(y)
y = y[y>0]
table(y)
mleTruncPois(y)
mleTruncPois(y, eps=10^-7)


# We do some more reporting here
mleTruncPois2 = function(x, eps=0.0001, maxit=25, silent=TRUE)
{
    # The mean
    xbar = mean(x)
    # Set up the iteration
    est  = xbar    # starting value for estimate
    corr = 1       # starting value for correction term
    it   = 0       # iteration conter
    
    while ((corr > eps) & (it <= maxit)) {
        if (!silent) {
            cat("Iteration:", it, "\tEstimate:", est,"\n")
        }
        corr = (est - xbar*(1 - exp(-est)))/(1 - xbar*exp(-est))
        est  = est - corr  
        it   = it + 1
    }
    
    # Check: convergence or out of iterations?
    if (corr > eps) {
        stop("No convergence")
    } else {
        est
    }
}
mleTruncPois(y, silent=FALSE)



##---------------------- Classes and methods --------------------##

# This function generates an object of class bootCI
bootCI = function(x, level=0.95, B=3000)
{
    xbar = mean(x)
    boot = numeric(B)
    for (i in 1:B) {
        cursamp = sample(x, replace=TRUE)
        boot[i] = mean(cursamp)
    }
    # The confidence limits
    lim = c((1-level)/2, 0.5+level/2)
    ci = quantile(boot, lim)
    
    # Now we return all relevant information
    ret = list(statistic = xbar, statistic.name = "mean",
               level = level, ci = ci, B = B, boot = boot)
    # Set the class attribute
    class(ret) = "bootCI"
    
    # Return the resulting object
    ret
}

# Run a short example
x = rnorm(100)
bootCI(x, B=10)
# No methods for new class yet!
methods(class=bootCI)

# Let's start with a simple print method
print.bootCI = function(x)
{
    cat(round(x$level*100), "% Boostrap confidence interval for the ", sep="")
    cat(x$statistic.name,"\n\n")
    cat("Lower limit:", x$ci[1], "\n")
    cat("Upper limit:", x$ci[2], "\n\n")
    cat("Number of bootstrap iterations:", x$B, "\n")
    invisible(x)
}

# This works immediately!
bootCI(x, B=10)
b = bootCI(x, B=10)
b
print(b)

# A very cheap summary function
summary.bootCI = function(x) print(x)
summary(b)

# How do we get at the actual confidence interval?
confint.bootCI = function(x)
{
    x$ci
}
confint(b)

# A plot function
plot.bootCI = function(x)
{
    main = paste(round(x$level*100), "% Boostrap confidence interval for the ",
                 x$statistic.name,"\n\n", sep="")
    hist(x$boot, main=main, xlab="Bootstrap sample")
    abline(v=x$ci, lwd=2, col="red")
    invisible(x)
}
# For a serious sample now
c = bootCI(x)
summary(c)
plot(c)
# We have a nice set of methods now
methods(class=bootCI)


# We define now a more flexible function for generating boostrap samples
bootCI2 = function(x, level=0.95, B=3000, f=mean)
{
    xbar = mean(x)
    boot = numeric(B)
    for (i in 1:B) {
        cursamp = sample(x, replace=TRUE)
        boot[i] = f(cursamp)
    }
    # The confidence limits
    lim = c((1-level)/2, 0.5+level/2)
    ci = quantile(boot, lim)
    
    # Now we return all relevant information
    ret = list(statistic = xbar, statistic.name = deparse(substitute(f)),
               level = level, ci = ci, B = B, boot = boot)
    # Set the class attribute
    class(ret) = "bootCI"
    
    # Return the resulting object
    ret
}

# All our old functions still work!
x.mean = bootCI2(x)
x.med  = bootCI2(x, f=median)
x.var  = bootCI2(x, f=var)
x.max  = bootCI2(x, f=max)
par(mfrow=c(2,2))
plot(x.mean)
plot(x.med)
plot(x.var)
plot(x.max)


