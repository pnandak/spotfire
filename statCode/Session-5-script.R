##---------------------------------------##
## Script for Session 5: Programming 2   ##
##    John Fox                           ##
## Statistical Computing in R/S          ##
##    ICPSR Summer Program 2008          ##
##---------------------------------------##


# classes & object-orinted programming (S3 objects)

library(car) # for data

mod.prestige <- lm(prestige ~ income + education + women, 
    data=Prestige)
attributes(mod.prestige)
class(mod.prestige)
str(mod.prestige)

print              # generic function
methods("print")   # all methods
print.lm           # method for "lm" objects

mod.prestige             # print() invoked implicitly
print(mod.prestige)      # equivalent
print.lm(mod.prestige)   # equivalent but bad form

methods(class="lm")

mod.mroz <- glm(lfp ~ ., family=binomial, data=Mroz)
class(mod.mroz)
mod.mroz

summary
methods("summary")

    # writing method functions
    
lreg.3 <- function(X, y, predictors=colnames(X), max.iter=10,
        tol=1E-6, constant=TRUE){
    if (!is.numeric(X) || !is.matrix(X)) 
        stop('X must be a numeric matrix')
    if (!is.numeric(y) || !all(y == 0 | y == 1)) 
        stop('y must contain only 0s and 1s')
    if (nrow(X) != length(y)) 
        stop('X and y contain different numbers of observations')
    if (constant) {
        X <- cbind(1, X)
        colnames(X)[1] <- 'Constant'
        }
    b <- b.last <- rep(0, ncol(X))
    it <- 1
    while (it <= max.iter){
        p <- as.vector(1/(1 + exp(-X %*% b)))
        V <- diag(p * (1 - p))
        var.b <- solve(t(X) %*% V %*% X)
        b <- b + var.b %*% t(X) %*% (y - p)
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) 
            break
        b.last <- b
        it <- it + 1
        }
        dev <- -2*sum(y*log(p) + (1 - y)*log(1 - p))
    if (it > max.iter) warning('maximum iterations exceeded')
    result <- list(coefficients=as.vector(b), var=var.b, 
        deviance=dev, converged= it <= max.iter,
        predictors=predictors)
    class(result) <- 'lreg'
    result
    }

attach(Mroz)
some(Mroz)

lfp <- ifelse(lfp == "yes", 1, 0)
wc <- ifelse(wc == "yes", 1, 0)
hc <- ifelse(hc == "yes", 1, 0)

mod.mroz.3 <- lreg.3(cbind(k5, k618, age, wc, hc, lwg, inc), lfp)
class(mod.mroz.3)

mod.mroz.3

print.lreg <- function(x) {  # print method for class "lreg"
    coef <- x$coefficients
    names(coef) <- x$predictors
    print(coef)
    if (!x$converged) cat('\n *** lreg did not converge ***\n')
    invisible(x)
    }

summary.lreg <- function(object) {  # summary method for class "lreg"
    b <- object$coefficients
    se <- sqrt(diag(object$var))
    z <- b/se
    table <- cbind(b, se, z, 2*(1-pnorm(abs(z))))
    colnames(table) <- c('Coefficient', 'Std.Error', 'z', 'p')
    rownames(table) <- object$predictors
    print(table)
    cat('\nDeviance =', object$deviance,'\n')
    if (!object$converged) 
        cat('\n Note: *** lreg did not converge ***\n')
    }

    # Note: It's conventional for a summary method to produce an object
    #  to be printed (here, it would be of class print.summary.lreg),
    #  but I opted for a simpler approach.

mod.mroz.3  # invoke print method
summary(mod.mroz.3)  # invoke summary method

    # writing a generic function
    
Rsq <- function(model, ...) UseMethod("Rsq")  # generic

Rsq.lm <- function(model, ...){  # method for class "lm"
    summary(model)$r.squared
    }
    
Rsq.glm <- function(model, ...){  # method for class "glm"
    sumry <- summary(model)
    1 - sumry$deviance/sumry$null.deviance
    }

Rsq(mod.prestige)
Rsq(mod.mroz)
Rsq(Mroz$lfp)

Rsq.default <- function(model, ...){  # default method
    stop(paste("Rsq not available for objects of class", class(model)))
    }

Rsq(Mroz$lfp)   


# debugging

lreg.4 <- function(X, y, predictors=colnames(X), max.iter=10,   # bugged!
        tol=1E-6, constant=TRUE){
    if (!is.numeric(X) || !is.matrix(X)) 
        stop('X must be a numeric matrix')
    if (!is.numeric(y) || !all(y == 0 | y == 1)) 
        stop('y must contain only 0s and 1s')
    if (nrow(X) != length(y)) 
        stop('X and y contain different numbers of observations')
    if (constant) {
        X <- cbind(1, X)
        colnames(X)[1] <- 'Constant'
        }
    b <- b.last <- rep(0, ncol(X))
    it <- 1
    while (it <= max.iter){
        p <- 1/(1 + exp(-X %*% b))
        V <- diag(p * (1 - p))
        var.b <- solve(t(X) %*% V %*% X)
        b <- b + var.b %*% t(X) %*% (y - p)
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) 
            break
        b.last <- b
        it <- it + 1
        }
        dev <- -2*sum(y*log(p) + (1 - y)*log(1 - p))
    if (it > max.iter) warning('maximum iterations exceeded')
    result <- list(coefficients=as.vector(b), var=var.b, 
        deviance=dev, converged= it <= max.iter,
        predictors=predictors)
    class(result) <- 'lreg'
    result
    }

mod.mroz.4 <- lreg.4(cbind(k5, k618, age, wc, hc, lwg, inc), lfp)

    # locating the error with traceback()
    
traceback()

    # stopping execution with browser()
        #   for details: ?browser
        # <ENTER> or c: continue execution
        # n: execute next line and enter debugger
        # Q: quit
    
lreg.4 <- function(X, y, predictors=colnames(X), max.iter=10,   # bugged!
        tol=1E-6, constant=TRUE){
    if (!is.numeric(X) || !is.matrix(X)) 
        stop('X must be a numeric matrix')
    if (!is.numeric(y) || !all(y == 0 | y == 1)) 
        stop('y must contain only 0s and 1s')
    if (nrow(X) != length(y)) 
        stop('X and y contain different numbers of observations')
    if (constant) {
        X <- cbind(1, X)
        colnames(X)[1] <- 'Constant'
        }
    b <- b.last <- rep(0, ncol(X))
    it <- 1
    while (it <= max.iter){
        p <- 1/(1 + exp(-X %*% b))
        V <- diag(p * (1 - p))
      browser()
        var.b <- solve(t(X) %*% V %*% X)
        b <- b + var.b %*% t(X) %*% (y - p)
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) 
            break
        b.last <- b
        it <- it + 1
        }
        dev <- -2*sum(y*log(p) + (1 - y)*log(1 - p))
    if (it > max.iter) warning('maximum iterations exceeded')
    result <- list(coefficients=as.vector(b), var=var.b, 
        deviance=dev, converged= it <= max.iter,
        predictors=predictors)
    class(result) <- 'lreg'
    result
    }
mod.mroz.4 <- lreg.4(cbind(k5, k618, age, wc, hc, lwg, inc), lfp)

    # stepping through the function with debug()
        #   for details: ?debug
        # <ENTER> or n: execute next line
        # c: continue execution
        # Q: quit

debug(lreg.4)   # after reentering original bugged function!
mod.mroz <- lreg.4(cbind(k5, k618, age, wc, hc, lwg, inc), lfp)

undebug(lreg.4)
