# Some functions to debug
# John Fox 13 June 2008

lregIWLS <- function(X, y, n=rep(1,length(y)), maxIter=10, tol=1E-6){ # bugged!
    # X is the model matrix
    # y is the response vector of observed proportion
    # n is the vector of binomial counts
    # maxIter is the maximum number of iterations
    # tol is a convergence criterion
    X <- cbind(1, X)  # add constant
    b <- bLast <- rep(0, ncol(X))  # initialize
    it <- 1  # iteration index
    while (it <= maxIter){
        if (max(abs(b - bLast)/(abs(bLast) + 0.01*tol)) < tol)
            break
        eta <- X %*% b
        mu <- 1/(1 + exp(-eta))
        nu <- as.vector(mu*(1 - mu))
        w <- n*nu
        z <- eta + (y - mu)/nu
        b <- lsfit(X, z, w, intercept=FALSE)$coef
        bLast <- b
        it <- it + 1  # increment index
        }
    if (it > maxIter) warning('maximum iterations exceeded')
    Vb <- solve(t(X) %*% diag(w) %*% X)
    list(coefficients=b, var=Vb, iterations=it)
    }  
    
reducedRowEchelonForm <- function(A){  # bugged!
    n <- nrow(A)
    m <- ncol(A)
    i <- j <- 1
    while (i <= n && j <= m){
        while (j <= m){
            currentColumn <- A[,j]
            currentColumn[1:n < i] <- 0
            # find maximum pivot in current column at or below current row
            which <- which.max(abs(currentColumn))
            pivot <- currentColumn[which]
            if (pivot == 0) { # check for 0 pivot
                j <- j + 1
                next
                }
            if (which > i) A[c(i, which),] <- A[c(which, i),]  # exchange rows
            A[i,] <- A[i,]/pivot            # pivot
            row <- A[i,]
            A <- A - outer(A[,j], row)      # sweep
            A[i,] <- row                    # restore current row
            j <- j + 1
            break
            }
        i <- i + 1
        }
     # 0 rows to bottom
    zeros <- which(apply(A[,1:m], 1, function(x) all(x == 0)))
    if (length(zeros) > 0){
        zeroRows <- A[zeros,]
        A <- A[-zeros,]
        A <- rbind(A, zeroRows)
        rownames(A) <- NULL
        }
    A
    }

runningMedian <- function(x, length=3){   # bugged!
#   x: a numeric vector
#   length: the number of values for each running median, defaults to 3
    n <- length(x)
    X <- matrix(x, n, length)
    for (i in 1:length) X[1:(n - i + 1), i] <- x[-(1:(i - 1))]
    apply(X, 1, median)[1:(n - length + 1)]
    }
        
