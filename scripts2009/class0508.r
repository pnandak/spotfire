## commands from class, May 8 2006

## Verzani, section 8.6, pp237ff
## example 8.9, AZT dose-response

y300 <- c(284,279,289,292,287,295,285,279,306,298)
y600 <- c(298,307,297,279,291,335,299,300,306,291)

## two-sample t-test
t.test(y300,y600,
       var.equal=TRUE)

yDiff <- mean(y300) - mean(y600)

################################################
## an example for difference in proportions
## example 8.3 on p219
phat <- c(.121, .117)    ## 2002 vs 2001
n <- c(60000, 50000)     ## sample sizes
pooled <- n*phat

## using prop.test to get 95% confidence interval
prop.test(x=pooled,n=n)  



