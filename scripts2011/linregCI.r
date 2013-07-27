# Simple linear regression example using Duncan occupational prestige data
# with counterfactual predictions and confidence intervals
# Chris Adolph

# Clear workspace
rm(list=ls())

# Load the occupation data
library(car)
data(Duncan)
attach(Duncan)

# Regress prestige on education & income
lm.out <- lm(prestige~education+income)

# To get CIs for yhat given a set of hypothetical income & education
xhyp <- seq(min(income),max(income),1)
zhyp <- rep(mean(education),length(xhyp))
hypo <- data.frame(income=xhyp,education=zhyp)
pred <- predict(lm.out,
                newdata=hypo,
                interval="confidence",
                level=0.95)
yhat <- pred[,1]
yhat.lower <- pred[,2]
yhat.upper <- pred[,3]

pdf("yhatexample.pdf",width=5,height=4.5)
plot(y=prestige,x=income,type="n")

# Make the x-coord of a confidence envelope polygon
xpoly <- c(xhyp,
           rev(xhyp),
           xhyp[1])

# Make the y-coord of a confidence envelope polygon
ypoly <- c(yhat.lower,
           rev(yhat.upper),
           yhat.lower[1])

# Choose the color of the polygon 
col <- "gray"

# Plot the polygon first, before the points & lines
polygon(x=xpoly,
        y=ypoly,
        col=col,
        border=FALSE
        )

# Plot the fitted line
lines(x=xhyp,y=yhat)

dev.off()
