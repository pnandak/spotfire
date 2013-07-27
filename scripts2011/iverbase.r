# Clear memory of all objects
rm(list=ls())

# Load data
file <- "iver.csv";
data <- read.csv(file,header=TRUE);
attach(data)

# A bivariate model
lm.result <- lm(povred~lnenp)
print(summary(lm.result))

# A new model with multiple regressors
lm.result2 <- lm(povred~lnenp+maj+pr)
print(summary(lm.result2))

# A new model with multiple regressors and no constant
lm.result3 <- lm(povred~lnenp+maj+pr+unam-1)
print(summary(lm.result3))

# A new model with multiple regressors and an interaction
lm.result4 <- lm(povred~lnenp+maj+pr+lnenp:maj)
print(summary(lm.result4))

# A different way to specify an interaction
lm.result5 <- lm(povred~pr+lnenp*maj)
print(summary(lm.result5))


# Make a plot of the data (automatic axes, etc)
plot(x=lnenp,
     y=povred,
     xlab="Log Effective Number of Parties",
     ylab="Poverty Reduction")

# One way to add a regression line to the plot
abline(lm.result$coefficients[1],    # Intercept
       lm.result$coefficients[2],    # Slope
       col="black")

# The above is easy for bivariate models
# For multivariate models, you need to calculate
# an appropriate intercept to take account
# of all the other covariates




# Generate expected values & CIs for povred at each lnenp
lnenp.hyp <- seq(min(lnenp),max(lnenp),0.1)
xnew <- list(lnenp=lnenp.hyp
             )
povred.pred <- predict(lm.result,
                       newdata=xnew,
                       interval="confidence",
                       level=0.95
                       )


# Open a pdf file for plotting
#pdf("redist.pdf",
#    height=5,
#    width=5)

# Create a new plot
plot.new()

# Set the plotting region limits
par(usr=c(0.5,2,0,100))

# Create the x-axis
x.ticks <- c(2,3,4,5,6,7)
axis(1,                 # Which axis to make (1 indicates x)
     at=log(x.ticks),   # Where to put the ticks
     labels=x.ticks     # How to label the ticks
     )

# Create the y-axis
axis(2,at=seq(0,100,10))

# Add plot titles
title(xlab="Effective Number of Parties",
      ylab="Poverty Reduction"
      )



# Plot ci for the regression line
# Make the x-coord of a confidence envelope polygon
xpoly <- c(lnenp.hyp,
           rev(lnenp.hyp),
           lnenp.hyp[1])

# Make the y-coord of a confidence envelope polygon
ypoly <- c(povred.pred[,2],
           rev(povred.pred[,3]),
           povred.pred[1,2])

# Choose the color of the polygon 
col <- "gray70"

# Plot the polygon first, before the points & lines
polygon(x=xpoly,
        y=ypoly,
        col=col,
        border=FALSE
        )


# Plot the expected values for the regression model
lines(x=lnenp.hyp,
      y=povred.pred[,1],
      col="black")

# Plot the data for the regression model
#points(x=lnenp,
#       y=povred,
#       col="black",   # see colors() for color names
#       pch=1)         # see example(points) for symbols

points(x=lnenp[maj==1],
       y=povred[maj==1],
       col="blue",     # see colors() for color names
       pch=17)         # see example(points) for symbols

points(x=lnenp[pr==1],
       y=povred[pr==1],
       col="green",    # see colors() for color names
       pch=15)         # see example(points) for symbols

points(x=lnenp[unam==1],
       y=povred[unam==1],
       col="red",      # see colors() for color names
       pch=16)         # see example(points) for symbols

text(x=lnenp,
     y=povred-3,
     labels=cty,
     col="black",
     cex=0.5
     )

# Finish drawing the box around the plot area
box()

# Close the device (ie, save the graph)
dev.off()
