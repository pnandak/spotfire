# Econometrics: Lecture 1

# Read data:
 # use "http://www.thiloklein.de/R/capm.dta", clear
 library(foreign)
 # getwd()
 # setwd("C:/Dokumente und Einstellungen/Thilo/Desktop")
 data <- read.dta("http://www.thiloklein.de/R/capm.dta")

 str(data)
 attach(data)


# Summarize the data:
 # sum macs, detail
 # sum market, detail
 summary(macs)
 summary(market)


# Plot histograms
 # hist macs
 # hist market
 hist(macs)
 hist(market)
 # try: par(mfrow=c(1,2))


# Two-way scatterplot:
 # twoway  (scatter macs market)
 par(mfrow=c(1,1))
 plot(macs ~ market)


# Linear regression:
 # reg macs market
 lm1 <- lm(macs ~ market)
 summary(lm1)


# Add the regression line to the scatterplot:
 # twoway (lfit macs market ) (scatter macs market)
 abline(lm1, col="red")


# Obtain fitted values and residuals:
 # predict macshat 
 # predict r, resid
 str(lm1)
 lm1$fitted
 lm1$resid


# Plot residuals against independent and dependent variable:
 # twoway  (scatter r market)
 # twoway  (scatter r macshat)
 par(mfrow=c(2,1))
 plot(lm1$resid ~ market)
 plot(lm1$resid ~ lm1$fitted)


# Visual test for normality of residuals:
 # hist r
 # qnorm r 
 hist(lm1$resid) 
 qqnorm(lm1$resid)


# Plot fitted values and observations of macs against independent variable:
 # twoway  (scatter macs macshat market)
 plot(macs ~ market)
 points(lm1$fitted ~ market, col="red")


# Summary statistics of regression residuals:
 # reg macs market
 # sum r
 summary(lm1$resid)


# Summary statistics of variables:
 # sum macs market
 summary(macs); summary(market)
 sd(macs); sd(market)


# Variance
 # dis 6.232106^2
 # dis 3.119853^2
 sd(macs)^2
 sd(market)^2


# 
 # twoway  (scatter macs macshat market)
 plot(macs ~ market)
 points(lm1$fitted ~ market, col="red")


# R^2:
 # reg macs market
 # dis 2711.90756/5087.92822
 summary(lm1)
 RSS <- sum(lm1$resid^2)
 ESS <- sum( (mean(macs) - lm1$fitted)^2 ) 
 TSS <- RSS + ESS # sum(macs^2)
 ESS/TSS	# 2711.90756/5087.92822


# Standard deviation:
 # sum macshat
 # sum macs
 # sum r
 sd(lm1$fitted)
 sd(macs)
 sd(lm1$resid)


# Variance:
 # dis 6.232106^2 
 # dis 4.549899^2 
 # dis  4.258822^2 
 var(lm1$fitted)
 var(macs)
 var(lm1$resid)

# 
 # dis 20.70+18.14
 var(lm1$fitted) + var(lm1$resid)


# Correlation structure:
 # corr(macshat macs)
 # corr(macshat r)
 # corr(macshat market)
 C <- data.frame(lm1$fitted, lm1$resid, macs)
 cor(C)

# --- Digression: Graphical Representation of Correlation Matrix (PLOTCORR) ---
 library(ellipse)
 corC <- cor(C, use="complete")
 colors <- c("#A50F15","#DE2D26","#FB6A4A","#FCAE91","#FEE5D9","white",
            "#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C")   
 plotcorr(corC, col=colors[5*corC + 6], type = "lower")


# Scatterplot residuals against independent variable:
 # scatter r market
 plot(lm1$resid ~ market)


# Regression with intercept forced to be 0:
 # reg macs market, noconstant
 # predict macshat2 
 # predict r2, resid
 lm2 <- lm(macs ~ -1 + market)
 summary(lm2)


# Scatterplot of residuals for model 1 and 2:
 # scatter  r2 market
 # scatter r r2 market
 par(mfrow=c(1,1))
 plot(lm1$resid ~ market, col="blue", ylab="Residuals") 
 points(lm2$resid ~  market, col="red")
 legend("topleft", c("Intercept","No intercept"), fill=c("blue","red"))
 ?legend

 

