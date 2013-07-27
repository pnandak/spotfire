##---------------------------------------##
## Script for Session 4: Programming 1   ##
##    John Fox                           ##
## Statistical Computing in R/S          ##
##    ICPSR Summer Program 2008          ##
##---------------------------------------##


# pre-example: "influence plot"

library(car)
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

influence.plot <- function(model, scale=10, col=palette()[1:2],
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


influence.plot(mod.duncan)

influence.plot(mod.duncan, ylim=c(-3, 4), col=c('blue', 'red'))

influence.plot()

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

    # switch

sign.3 <- function (x) {  # note: not vectorized
    switch(which(c(x < 0, x == 0, x > 0)),
        -1, 0, 1)
    }    
sign.3(-5)


# loops (iteration)  

    # for

prod(1:5)     # factorial
gamma(5 + 1)  # better alternative
factorial(5)

factorial

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

    # repeat

fact.3 <- function(x){
    if ((!is.numeric(x)) || (x != floor(x))
         || (x < 0) || (length(x) > 1))
        stop('argument must be a non-negative integer')
    i <- f <- 1
    repeat {
        f <- f * i
        i <- i + 1
        if (i > x) break
        }
    f
    }
fact.3(5)
fact.3(1.5)

# recursive functions

fact.4 <- function(x){
    if (x <= 1) 1
    else x * fact.4(x - 1)
    }
fact.4(5)


    # a nontrivial example of recursion

string <- "To be/or not to be/that is the question"
strsplit(string, "/")

strings <- c("To be/or not to be/that is the question",
             "Whether t'is nobler,in the mind",
             "to suffer the slings:and arrows:of outrageous fortune",
             "or to take arms;against;a sea of troubles")            
strsplit(strings, "[/,:;]")   

Strsplit <- function(x, split){
    if (length(x) > 1) {
        return(lapply(x, Strsplit, split))  # vectorization
        }
    result <- character(0)
    if (nchar(x) == 0) return(result)
    posn <- regexpr(split, x)
    if (posn <= 0) return(x)
    c(result, substring(x, 1, posn - 1), 
        Recall(substring(x, posn+1, nchar(x)), split))  # recursion
    }


Strsplit(string, "/")

Strsplit(strings, "[/,:;]")       

# to loop or not to loop?
    
    # Example: summing a list of matrices
        
matrices <- vector(length=10000, mode="list")   # reasonable
system.time(for (i in 1:10000) matrices[[i]] <- matrix(rnorm(100), 10, 10))

matrices <- list()    # problematic
system.time(for (i in 1:10000) matrices <- c(matrices,
                                    list(matrix(rnorm(100), 10, 10))))

S <- matrix(0, 10, 10)  # simple
system.time(for (i in 1:length(matrices)) S <- S + matrices[[i]])
S

system.time(S2 <- apply(array(unlist(matrices),  # "clever"
    dim = c(10, 10, 10000)), 1:2, sum))
range(S - S2)

system.time(S3 <- Reduce("+", matrices))  # really clever
range(S - S3)

# simple matrix operations

A <- matrix(c(1, 2, -4, 3, 5, 0), 2, 3)
B <- matrix(1:6, 2, 3)
C <- matrix(c(2, -2, 0, 1, -1, 1, 4 ,4, -4), 3, 3, byrow=TRUE)
A; B; C

A + B
A - B
A + C  # not the same order!
2*A
-A

A %*% C  # matrix product

a <- rep(1, 3)
b <- c(1, 5, 3)
C %*% a
a %*% C
a %*% b

outer(a, b)  # outer product

t(B)  #transpose

solve(C)  # inverse

attach(Duncan)
X <- cbind(1, income, education)
y <- prestige
head(X)
y

solve(t(X) %*% X) %*% t(X) %*% y  # not a computationally good approach!

solve(crossprod(X)) %*% crossprod(X, y)  # still not good

lm(prestige ~ income + education)

R <- cor(cbind(education, income, prestige))
R

eigen(R) # eigenvalues and eigenvectors

det(R)  # determinant

diag(R)  # extract diagonal
diag(R) <- NA  # set diagonal
R
diag(1:3)  # make diagonal matrix
diag(3)  # order-3 identity matrix

detach(Duncan)

# an extended example: logistic regression

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
 
attach(Mroz)
some(Mroz)

lfp <- ifelse(lfp == "yes", 1, 0)
wc <- ifelse(wc == "yes", 1, 0)
hc <- ifelse(hc == "yes", 1, 0)
hc[1:20]

system.time(mod.mroz <- lreg(cbind(k5, k618, age, wc, hc, lwg, inc), 
    lfp))

mod.mroz$coefficients
sqrt(diag(mod.mroz$var))  # coef. standard errors

system.time(mod.mroz.glm <-     # check! 
    glm(lfp ~ k5 + k618 + age +wc + hc + lwg + inc, family=binomial))  
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
