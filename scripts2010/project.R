
##############  Midterm Project Code and Solutions  #################

states <- read.csv("Election2008byState.csv") 
BOvote <- states$OBAMA/(states$OBAMA+states$MCCAIN)
regINC <- lm(BOvote ~ states$INC)
r <- rstudent(regINC)
par(mfrow=c(2,2))
plot(states$INC, BOvote ,col=0, xlab="INC", main="vote vs Income")
abline(regINC, lty=2, col=2)
text(states$INC, BOvote, labels=states$STATE)
plot(states$INC, regINC$residuals, col=0,
     xlab="INC", ylab = "e",
     main="residuals vs income")
abline(h=0, col=3, lty=2)
text(states$INC, regINC$residuals, labels=states$STATE)
hist(r, col=8)
qqnorm(r, main="Normal Q-Q plot for r")
abline(a=0, b=1, col=4, lty=2)

## DC is a big outlier (not really a state)
attach(states[states$STATE!="DC",])
BOvote <- OBAMA/(OBAMA+MCCAIN)
regINC <- lm(BOvote ~ INC)
r <- rstudent(regINC)
par(mfrow=c(2,2))
plot(INC, BOvote ,col=0, xlab="INC", main="vote vs Income")
abline(regINC, lty=2, col=2)
text(INC, BOvote, labels=STATE)
plot(INC, regINC$residuals, col=0,
     xlab="INC", ylab = "e",
     main="residuals vs income")
abline(h=0, col=3, lty=2)
text(INC, regINC$residuals, labels=STATE)
hist(r, col=8)
qqnorm(r, main="Normal Q-Q plot for r")
abline(a=0, b=1, col=4, lty=2)

# The residuals look much better without DC in there.
# there is still a bit of a 'curve' pattern, but
# we'll get rid of that by adding a variable

## look to see which variables are correlated with
# the residuals and with BOvote
notDC <- states$STATE!="DC"
pairs(cbind(regINC$resid,BOvote,states[notDC,4:9]), pch=20, col=6)
cor(cbind(e=regINC$resid,BOvote,INC), states[notDC,5:9])
# CHURCH and AGE both look like good candidates:
# they have similar correlation with residuals,
# but CHURCH has higher correlation with BOvote.
# I also took at look at correlation with INC,
# to check for multicolinearity; on this account,
# AGE might have been the better variable to add.
# For our purposes here, I choose CHURCH
fullreg <- lm(BOvote ~ INC + CHURCH)
r <- rstudent(fullreg)
par(mfrow=c(2,2))
plot(fullreg$fitted, BOvote, col=0, xlab="Yhat", main="vote vs fitted")
abline(0,1, lty=2, col=2)
text(fullreg$fitted, BOvote, labels=STATE)
plot(fullreg$fitted, fullreg$residuals, col=0,
     xlab="Yhat", ylab = "e",
     main="residuals vs fitted")
abline(h=0, col=3, lty=2)
text(fullreg$fitted, fullreg$residuals, labels=STATE)
hist(r, col=8)
qqnorm(r, main="Normal Q-Q plot for r")
abline(a=0, b=1, col=4, lty=2)
## The curvy residual pattern has completely disapeared.
# Wyoming turns out to be a big outlier, but I can't think of
# a good reason why so I keep it in there.
# you can check that it has low leverage anyways:
# R's hat.values function gives you leverage...
par(mfrow=c(1,1))
plot(STATE, hatvalues(fullreg), xlab="state", ylab="leverage")

### At the end of the day, it appears that BO's vote share
# relative to McCain increased about a third of a percent
# for every $1000 increase in a states median wage
fullreg$coef[2]*1000

######  Compare to income dynamics ########
income <- read.csv("Income-ObamaVoteShare.csv")
minc <- income$INCOME
plot(income$USA, type="l", lwd=2, xlim=c(1.25,8.25), main="Vote share by income group",
     xlab="Income in $1000", ylab="BOvote", ylim=c(0,100), xaxt="n")
for(i in 3:5) lines(income[,i], col=i-1, lwd=2)
text(x=8.25, y=income[8,2:5], labels=names(income)[2:5])
axis(1, at=1:8, labels=minc)
## We get the opposite result of the slope from our state
# income regression model; this is an instance of "simpson's paradox".
# To see the paradox in action, consider this example of
# within group positive slope and between group negative slope:
x <- 1:5
y1 <- 10 + x
y2 <- -2 + 3*x
plot(x, y1, pch=20, col=2, xlim=c(1,5), ylim=c(0,16), ylab="y")
lines(x, y1, col=2)
points(x, y2, pch=20, col=4)
lines(x, y2, col=4)
btwgroup.slope <- mean(y2)-mean(y1)
abline(a = mean(y1)-btwgroup.slope*mean(x), b= btwgroup.slope, lty=2)
# Andrew Gelman's book "Red State, Blue State, Rich State, Poor State"
# discusses the problem in specific context of income and voting patterns.
