##------------------------------------##
## Script for Part 4: S Programming   ##
##    John Fox                        ##
## An Introduction to R               ##
##    UCLA Feb. 2005                  ##
##------------------------------------##


# pre-example: "influence plot"

library(car)
data(Duncan)
mod.duncan <- lm(prestige ~ income + education, data=Duncan)
summary(mod.duncan)

plot(hatvalues(mod.duncan), rstudent(mod.duncan), type='n')
cook <- sqrt(cookd(mod.duncan))
points(hatvalues(mod.duncan), rstudent(mod.duncan), 
    cex=10*cook/max(cook))
abline(h=c(-2, 0, 2), lty=2)
abline(v=c(2, 3)*3/45, lty=2)
identify(hatvalues(mod.duncan), rstudent(mod.duncan), 
    row.names(Duncan))

    # writing functions

influence.plot <- function(model, scale=10, col=c(1,2),
    labels=names(rstud), ...){
    hatval <- hatvalues(model)
    rstud <- rstudent(model)
    cook <- sqrt(cookd(model))
    scale <- scale/max(cook)
    p <- length(coef(model))
    n <- length(rstud)
    cutoff <- sqrt(4/(n - p))
    plot(hatval, rstud, xlab='Hat-Values',
        ylab='Studentized Residuals', type='n', ...)
    abline(v=c(2, 3)*p/n, lty=2)
    abline(h=c(-2, 0, 2), lty=2)
    points(hatval, rstud, cex=scale*cook, 
            col=ifelse(cook > cutoff, col[2], col[1]))
    if (labels[1] != FALSE) identify(hatval, rstud, labels)
    }

influence.plot(mod.duncan, col=c('green', 'red'))

influence.plot(mod.duncan, ylim=c(-3, 4), col=c('green', 'red'))

# control structures
    
    # conditionals

        # if

abs(-3:3)

abs.1 <- function(x) if (x < 0) -x else x
abs.1(-5)
abs.1(5)

abs.1(-3:3)  # the first element, -3, controls the result

        # ifelse (vectorized)

abs.2 <- function(x) ifelse(x < 0, -x, x)
abs.2(-3:3)

    # cascading ifs and ifelses

sign(c(-5, 0, 10))

sign.1 <- function(x) {
    if (x < 0) -1
        else if (x > 0) 1
            else 0
    }
sign.1(-5)

sign.2 <- function(x) {
    ifelse (x < 0, -1, 
        ifelse(x > 0, 1, 0))
    }    
sign.2(c(-5, 0, 10))

# loops (iteration)  

    # for

prod(1:5)     # factorial
gamma(5 + 1)  # alternative

fact.1 <- function (x){
    if (x <= 1) return(1)
    f <- 1
    for (i in 1:x) f <- f * i
    f
    }
fact.1(5)

    # while

fact.2 <- function (x){
    i <- f <- 1
    while (i <= x) {
        f <- f * i
        i <- i + 1
        }
    f
    }
fact.2(5)


# an extended example: logistic regression (time permitting)

    # by Newton-Raphson
    
lreg <- function(X, y, max.iter=10, tol=1E-6){
    # X is the model matrix
    # y is the response vector of 0s and 1s
    # max.iter is the maximum number of iterations
    # tol is a convergence criterion
    X <- cbind(1, X)  # add constant
    b <- b.last <- rep(0, ncol(X))  # initialize
    it <- 1  # iteration index
    while (it <= max.iter){
        p <- as.vector(1/(1 + exp(-X %*% b)))
        V <- diag(p * (1 - p))
        var.b <- solve(t(X) %*% V %*% X)
        b <- b + var.b %*% t(X) %*% (y - p)
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) 
            break
        b.last <- b
        it <- it + 1  # increment index
        }
    if (it > max.iter) warning('maximum iterations exceeded')
    list(coefficients=as.vector(b), var=var.b, iterations=it)
    }  
 
data(Mroz)
attach(Mroz)
Mroz[1:5,]

lfp <- recode(lfp, " 'yes'=1; 'no'=0 ", as.factor=FALSE)
wc <- recode(wc, " 'yes'=1; 'no'=0 ", as.factor=FALSE)
hc <- recode(hc, " 'yes'=1; 'no'=0 ", as.factor=FALSE)
hc[1:20]

system.time(mod.mroz <- lreg(cbind(k5, k618, age, wc, hc, lwg, inc),
    lfp))

mod.mroz$coefficients
sqrt(diag(mod.mroz$var))  # coef. standard errors

system.time(
    mod.mroz.glm <- glm(lfp ~ k5 + k618 + age +wc + hc + lwg + inc,
    family=binomial))  # check!
coefficients(mod.mroz.glm)
sqrt(diag(vcov(mod.mroz.glm)))

    # using optim to maximize the likelihood

lreg.2 <- function(X, y, method='BFGS'){
    X <- cbind(1, X)
    negLogL <- function(b, X, y) {
        p <- as.vector(1/(1 + exp(-X %*% b)))
        - sum(y*log(p) + (1 - y)*log(1 - p))
        }
    grad <- function(b, X, y){
        p <- as.vector(1/(1 + exp(-X %*% b)))
#        - apply(((y - p)*X), 2, sum) # version in text
        - colSums((y - p)*X)
            }
    result <- optim(rep(0, ncol(X)), negLogL, gr=grad,
        hessian=TRUE, method=method, X=X, y=y)
    list(coefficients=result$par, var=solve(result$hessian), 
        deviance=2*result$value, converged=result$convergence == 0)
    }

system.time(mod.mroz.2 <- lreg.2(cbind(k5, k618, age, wc, hc, lwg, inc),
    lfp))
mod.mroz.2$coefficient
sqrt(diag(mod.mroz.2$var))
mod.mroz.2$converged

# S3 classes and methods

data(Prestige)

mod.prestige <- lm(prestige ~ income + education + women,
    data=Prestige)
attributes(mod.prestige)
class(mod.prestige)

print
methods("print")
print.lm

mod.prestige
print(mod.prestige)
print.lm(mod.prestige)

mod.mroz <- glm(lfp ~ ., family=binomial, data=Mroz)
class(mod.mroz)
mod.mroz

summary
methods("summary")

summary(mod.mroz)

