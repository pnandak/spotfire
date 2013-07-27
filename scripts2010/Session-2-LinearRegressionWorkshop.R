# Linear Regression Workshop

# This line clears the environment.
rm(list=ls())

# We will use the lattice package for plotting, so we must load it first.
library(lattice)

# The Robey Data is in the car package.

library(car)

# The command data(Robey) loads the Robey data frame.
data(Robey)

# Try these two commands for the structure and for the first six rows.
str(Robey)
head(Robey)

# Since Robey comes from a package, there *must* be help for the data set.
# To see the help, you can either type ?Robey or help(Robey).  Try

?Robey

##############################################################################
# Plotting Robey.  Try each of these plots.

# Planels by the region factor.
xyplot(tfr ~ contraceptors|region,data=Robey,type=c('g','p','r'))

# A scatter plot without the factors.
xyplot(tfr ~ contraceptors,data=Robey,type=c('g','p','r'))

# A scatter plot with different colors with the regression lines for each group.
xyplot(tfr ~ contraceptors,data = Robey,groups=region,type=c('p','g','r'),
           auto.key = list(space="right"))


##############################################################################
# The first model: tfr as a response to the percent of contraception users.
lmRobey1 <- lm (tfr ~ contraceptors, data = Robey)


str(lmRobey1)

summary(lmRobey1)

###############################################################################
# Manipulating the model directly. Try both residuals(lmRobey1) and 
# lmRobey1$resid
residuals(lmRobey1)
lmRobey1$resid

# Extract the data frame from the model (of course, we don't really have to do 
# this because we already have the data frame).
modelData <- lmRobey1$model

# Extract the tfr values from the model's data frame.
tfr <- modelData[,tfr]

# This command has the same result as the last.
tfr <- modelData$tfr

# These commands both give the model fitted values.

lmRobey1$fitted.values
predict(lmRobey1)

###########################################################################
# Now it's time to try to  compute our F-statistic directly


# Here we define the SS function.  
SS <- function(x){sum( (x - mean(x))^2)}

# We could also have written this as
SS <- function(x)
          {
          xBar <- mean(x)
          sum( (x - xBar)^2)
          }
# or even         

SS <- function(x)
          {
          xBar <- mean(x)
          return(sum( (x - xBar)^2))
          }

# All three of these versions of SS are equivalent.   Try entering 
# SS at the R-prompt.


# Next define the F.statistic function.

F.statistic <- function(residuals,fittedValues)
    {
    p <- 2
    n <- 50
	((SS(fittedValues)/(p - 1)))/(SS(residuals)/(n - p))
    }


# Now try computing the F.statistic
F.statistic(residuals(lmRobey1),predict(lmRobey1))

# The improved F.statistic
F.statistic <- function(lmObject)
    {
    p <- length(lmObject$coef)
    n <- length(lmObject$resid)
    (SS(predict(lmObject))/(p - 1))/(SS(residuals(lmObject))/(n - p))
    }

# Now try computing again using the lmRobey1 object directly.
F.statistic(lmRobey1)


#########################################################################
# Is region a significant contributor to TFR?


# First build the new model.
lmRobey2 <- lm (tfr ~ contraceptors + region, data = Robey)

# Now try using the anova function to compare the models.

anova(lmRobey2,lmRobey1)

# and 

anova(lmRobey1,lmRobey2)


