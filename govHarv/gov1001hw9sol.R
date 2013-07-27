
## Problem 2

library(car)
data(Leinhardt)
attach(Leinhardt)

## Part a: leaving oil and income:oil out of the first model is
## equivalent to constraining them to be zero, which is what we
## want to test.

lmincome <- lm(infant ~ income)
lminteract <- lm(infant ~ income + oil + income:oil)

#  The anova() function carries out the F test.
anova(lmincome, lminteract)

#  Part b
summary(lminteract)
confint(lminteract)

#  Part c
plot(infant ~ income, type = "n", main="Infant mortality vs. Income per capita", 
     xlab = "$ per capita", ylab = "Infant deaths per 1000")

points(infant[oil=="yes"] ~ income[oil=="yes"], col="red", pch="O")
points(infant[oil=="no"] ~ income[oil=="no"], col="blue", pch="N")
abline(lminteract, col = "blue")
abline(lminteract$coef[1] + lminteract$coef[3], lminteract$coef[2] + lminteract$coef[4], col = "red")
legend(3700, 400, c("Oil Exporters", "Non Exporters"), pch=c("O", "N"), col=c("red", "blue")) 

## Problem 3

#  Load the data

elect <- read.csv("H:/election00new.csv")
attach(elect)
#  make some nice row names
rownames(elect) <- paste(stcode, cd)

# Part (a)

# Create the bivariate scatterplots; put them all on one graph
par(mfrow=c(3,3))
scatter.smooth(gorepct ~ pvtschool)
scatter.smooth(gorepct ~ pctwhite)
scatter.smooth(gorepct ~ medinc)
scatter.smooth(gorepct ~ noenglish)
scatter.smooth(gorepct ~ baorhigher)
scatter.smooth(gorepct ~ pcturban)
scatter.smooth(gorepct ~ party00, span = 2)
scatter.smooth(gorepct ~ south, span = 2)
scatter.smooth(gorepct ~ pctpoverty)

# (b-c)
par(mfrow=c(1,1))
lm1 <- lm(gorepct ~ pvtschool)
cr.plots(lm1)
summary(lm1)
confint(lm1)

# (d)

lm2 <- lm(gorepct ~ pvtschool + pctwhite)
summary(lm2)
cr.plots(lm2)

lm3 <- lm(gorepct ~ pvtschool + pctwhite + south + party00)
summary(lm3)
cr.plots(lm3)

lm4 <- lm(gorepct ~ pvtschool + pctwhite + south + party00 + medinc)
summary(lm4)
cr.plots(lm4)

lm5 <- lm(gorepct ~ pvtschool + pctwhite + south + party00 + baorhigher)
summary(lm5)
cr.plots(lm5)
6

lm6 <- lm(gorepct ~ pvtschool + pctwhite + south + party00 + pcturban )
summary(lm6)
cr.plots(lm6)

par(mfrow=c(2, 3))
cr.plot(lm6, "pvtschool", main = "CR plot for Private School")
cr.plot(lm6, "pctwhite", main = "CR plot for Pct. White")
cr.plot(lm6, "pcturban", main = "CR plot for Pct. Urban")
cr.plot(lm6, "south", main = "CR plot for South")
cr.plot(lm6, "party00", main = "CR plot for Incumbent Congress")



par(mfrow = c(1, 2))
plot(lm6, which=3, main = "Final Model: SL Plot")
qq.plot(lm6, main = "Final Model: QQ Plot")

summary(lm6)
confint(lm6)

round(cbind(lm6$coef, confint(lm6)) , 2)

par(mfrow=c(1,2))
av.plots(lm6, labels = rownames(elect)[which(gorepct!="NA")])
plot(lm6, which = 5, main = "Final Model: Influence Plot")

