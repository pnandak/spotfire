### this is simply an example to illustrate what it looks like to input "real" data into the optim command using a created function.

library(foreign)
library(MASS)

articles <- read.dta("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/long data/couart2.dta")

attach(articles)

#dataset borrowed from J. Scott Long at Indiana University.
#used in his book: Regression Models for Categorical Outcomes Using Stata and found at his website. 
#the datatset measures the productivity of some Biochem graduate students based upon the number of articles published 

#art: Articles in last 3 years of PhD
#fem: Gender, where 1=female
#mar: Married, where 1=married
#kid5: Number of children <6
#phd: PhD prestige
#ment: Article by mentor in last 3 years

llk.poisson <- function (param, y, x) {
     os <- rep(1, nrow(x))
     x <- cbind(os, x)
     b <- param[1 : ncol(x)]
     xb <- x%*%b
     sum( - y*xb + exp(xb) )                # see King 89 eq 5.20
     }
     
X <- cbind(fem, mar, kid5, ment, phd)       # matrix of explanatory variables 
Y <- art                                    # the dependent variable 

stval <- c(0,0,0,0,0,0)

poisson.result <- optim(stval, llk.poisson, method="BFGS", hessian=T, y=Y, x=X)

coeff <- poisson.result$par            # first coef is constant, rest will go in same order as entered above
vc <- solve(poisson.result$hessian)  
se <- sqrt(diag(vc))   
zscore <- coeff/se 
pvalue <- 2.0 * (1.0-pnorm(abs(zscore)))
tab <- cbind(coeff,se,zscore,pvalue)    # produces a table of the coefficients, standard errors, zscores, and p-values 
                                        # similar to what we're often used to seeing in journal articles
ll <- -poisson.result$value         


###### running glm on the same model
poisson.glm <- glm(art ~ fem + mar + kid5 + ment + phd, family = poisson(link = "log"))

## evidence that coefficients do actually turn out identically!
cbind(coeff, poisson.glm$coeff)

##### a few diagnostics
hats <- hatvalues(poisson.glm)/mean(hatvalues(poisson.glm))
stresid <- rstudent(poisson.glm)

plot(hats, stresid)
out <- identify(hats, stresid, rownames(articles))
articles[out,]





# Simulate results
sims <- 1000
simbetas <- mvrnorm(sims,coeff,vc) # draw parameters
female <- 1
male <- 0
married <- 1
unmarried <- 0
kidsmin <- 0
kidsmax <- 3
phdm <- mean(phd)
mentm <- mean(ment)


# for a married female with max children and average of other variables
  simlambdaf <- exp(simbetas%*%rbind(1, female, married, kidsmax, mentm, phdm))
  simyfmk <- rpois(sims, simlambdaf)

# for an unmarried male with no children and average of other variables
  simlambdam <- exp(simbetas%*%rbind(1, male, unmarried, kidsmin, mentm, phdm))
  simym <- rpois(sims, simlambdam)
