setwd("C:/Documents and Settings/Jank/My Documents/Uniraps/Gov 2000 TF/My_PS/PS_4")


# Problem 1
set.seed(12345)               # Only have to set seed once in this case

# Part A
sample.1 <- rnorm(10)
sample.2 <- rnorm(1000)

pdf("fig1.pdf")
par(mfrow = c(1,2))           # Creates two plots in a single row

qqnorm(sample.1, pch = 19,    # Note: when pch = 19, small filled circles are plotted
       main = "Small Sample", ylab = "Draws from the Standard Normal Distribution")
qqline(sample.1, col = "red") # Adds 45 degree line to qqnorm plot

qqnorm(sample.2, pch = 19,
       main = "Large Sample", ylab = "Draws from the Standard Normal Distribution")
qqline(sample.2, col = "red")
dev.off()

# Part B
sample.3 <- runif(length(sample.2), min = -2, max = 2 )

pdf("fig2.pdf")
par(mfrow = c(1,1))           # Resets to single plot mode

qqplot(x = sample.2, y = sample.3, ylim = c(-4, 4), xlim = c(-4, 4), # good to have same limits
       main = "Contrast of Two Distributions", pch = 19,
       xlab = "Draws from a Standard Normal Distribution",
       ylab = "Draws from a Uniform Distribution over [-2,2]")
abline(a = 0, b = 1, col = "red")
dev.off()

# Problme 2
dat <- read.csv("pakistan.csv")

# problem 2a
# transform vars
dat$lincpp <- dat$hhincome/(dat$nfadult + dat$nmadult +dat$nfkids + dat$nmkids)
dat$pctfood <- dat$food/dat$totexp
out1 <- lm(pctfood~lincpp,data=dat)

png("fig3.png")
plot(dat$lincpp,dat$pctfood,main="Engel's Law Plot",
xlab="Log(Household Income per capita)",
ylab="Share of Food Expenditures on Total Household Expenditures")
abline(out1,col="red")
dev.off()

library(Hmisc)

# summary table
critVal <- qt(p = .025, df = nrow(dat) - length(out1$coeff)) # close to 1.96; not surprising, given sample size
cIb1 <- summary(out1)$coeff[2,1] + c(critVal * summary(out1)$coeff[2,2], -critVal*summary(out1)$coeff[2,2]) # confidence interval
cIb0 <- summary(out1)$coeff[1,1] + c(critVal * summary(out1)$coeff[1,2], -critVal*summary(out1)$coeff[1,2])

tab1 <- cbind(summary(out1)$coeff,rbind(cIb0,cIb1))
colnames(tab1)[5:6] <- c(".95 LB",".95 UB")
latex(round(tab1,3),file="",caption="Share of Food expenditures as Function of log of household income per capita")

# sum of residulas
sum(out1$residuals)+sum(out1$residuals*out1$fitted)+sum(out1$res*dat$lincpp)

# qq norm plot of residuals
png("fig4.png")
qqnorm(out1$residuals, pch = 19,
       main = "Residuals Normal QQ plot", ylab = "Residuals from Engel's Law Regression")
qqline(out1$residuals, col = "red")
dev.off()

plot(dat$lincpp,out1$residuals)
lines(lowess(dat$lincpp,out1$residuals),col="red")

#
out2 <- lm(pctfood~womshare,data=dat)
plot(dat$womshare,dat$pctfood)
abline(out2)

tab2 <- summary(out2)$coeff
latex(round(tab2,3),file="",caption="Share of Food expenditures as Function of Women share of household income")


# OLS estimator
ols <- function(x, y) {
  estimates <- matrix(NA, nrow = 2, ncol = 2)
  rownames(estimates) <- c("intercept", "slope")
  colnames(estimates) <- c("coefficient", "std error")

  xMinXBar <- x - mean(x)
  yMinYBar <- y - mean(y)

  b1Hat <- sum(xMinXBar * yMinYBar) / sum(xMinXBar^2)
  b0Hat <- mean(y) - b1Hat * mean(x)

  resids <- y - (b0Hat + b1Hat * x)
  sigHat <- sqrt(sum(resids^2) / (length(y)-2))

  seB1 <- sigHat / sqrt(sum(xMinXBar^2))
  seB0 <- sqrt((sigHat^2 * sum(x^2)) / (length(y) * sum(xMinXBar^2)))

  estimates[, 'coefficient'] <- c(b0Hat, b1Hat)
  estimates[, 'std error'] <- c(seB0, seB1)
  return(estimates)
}

myLmMat <- ols(x = dat$pctfood, y = dat$womshare)

critVal <- qt(p = .025, df = nrow(dat) - nrow(myLmMat)) # close to 1.96; not surprising, given sample size
cIb1 <- myLmMat[2,1] + c(critVal * myLmMat[2,2], -critVal*myLmMat[2,2]) # confidence interval

gMat <- matrix(NA, nrow = 2, ncol= 6)
colnames(gMat) <- c("Point estimate", "Std error", "t-stat", "p-value (2-tailed)", "lower .025", "upper .025")
rownames(gMat) <- c("Intercept", "Slope")

gMat[,c("Point estimate", "Std error")] <- myLmMat
gMat[,"t-stat"] <- gMat[,"Point estimate"] / gMat[,"Std error"]
gMat[,"p-value (2-tailed)"] <- 2 * pt(gMat[,"t-stat"], df = nrow(dat) - nrow(myLmMat), lower.tail = FALSE)
gMat[,"lower .025"] <- gMat[,"Point estimate"] + critVal * gMat[,"Std error"] # Note critVal is negative
gMat[,"upper .025"] <- gMat[,'Point estimate'] - critVal * gMat[,"Std error"]


#problem 3
dat <- read.table("USA_CEO.txt")
