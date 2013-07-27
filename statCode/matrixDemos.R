# Last modified 9 July 2007 by J. Fox

GaussianElimination <- function(A, B, tol=sqrt(.Machine$double.eps), 
    verbose=FALSE, fractions=FALSE){
    # A: coefficient matrix
    # B: right-hand side vector or matrix
    # tol: tolerance for checking for 0 pivot
    # verbose: if TRUE, print intermediate steps
    # fractions: try to express nonintegers as rational numbers
    # If B is absent returns the reduced row-echelon form of A.
    # If B is present, reduces A to RREF carrying B along.
    if (fractions) {
        mass <- require(MASS)
        if (!mass) stop("fractions=TRUE needs MASS package")
        }
    if ((!is.matrix(A)) || (!is.numeric(A)))
        stop("argument must be a numeric matrix")
    n <- nrow(A)
    m <- ncol(A)
    if (!missing(B)){
        B <- as.matrix(B)
        if (!(nrow(B) == nrow(A)) || !is.numeric(B))
          stop("argument must be numeric and must match the number of row of A")
        A <- cbind(A, B)
        }
    i <- j <- 1
    while (i <= n && j <= m){
        while (j <= m){
            currentColumn <- A[,j]
            currentColumn[1:n < i] <- 0
            # find maximum pivot in current column at or below current row
            which <- which.max(abs(currentColumn))
            pivot <- currentColumn[which]
            if (abs(pivot) <= tol) { # check for 0 pivot
                j <- j + 1
                next
                }     
            if (which > i) A[c(i, which),] <- A[c(which, i),]  # exchange rows
            A[i,] <- A[i,]/pivot            # pivot
            row <- A[i,]
            A <- A - outer(A[,j], row)      # sweep
            A[i,] <- row                    # restore current row
            if (verbose) if (fractions) print(fractions(A))
                else print(round(A, round(abs(log(tol,10)))))
            j <- j + 1
            break
            }
        i <- i + 1
        }
     # 0 rows to bottom
    zeros <- which(apply(A[,1:m], 1, function(x) max(abs(x)) <= tol))
    if (length(zeros) > 0){
        zeroRows <- A[zeros,]
        A <- A[-zeros,]
        A <- rbind(A, zeroRows)
        }
    rownames(A) <- NULL
    if (fractions) fractions (A) else round(A, round(abs(log(tol, 10))))
    }

matrixInverse <- function(X, tol=sqrt(.Machine$double.eps), ...){
    # returns the inverse of nonsingular X
    if ((!is.matrix(X)) || (nrow(X) != ncol(X)) || (!is.numeric(X))) 
        stop("X must be a square numeric matrix")
    n <- nrow(X)
    X <- GaussianElimination(X, diag(n), tol=tol, ...) # append identity matrix
        # check for 0 rows in the RREF of X:
    if (any(apply(abs(X[,1:n]) <= sqrt(.Machine$double.eps), 1, all)))
        stop ("X is numerically singular")
    X[,(n + 1):(2*n)]  # return inverse
    }

RREF <- function(X, ...) GaussianElimination(X, ...)
    # returns the reduced row-echelon form of X
    
Ginv <- function(A, tol=sqrt(.Machine$double.eps), verbose=FALSE, 
        fractions=FALSE){
    # return an arbitrary generalized inverse of the matrix A
    # A: a matrix
    # tol: tolerance for checking for 0 pivot
    # verbose: if TRUE, print intermediate steps
    # fractions: try to express nonintegers as rational numbers
    m <- nrow(A)
    n <- ncol(A)
    B <- GaussianElimination(A, diag(m), tol=tol, verbose=verbose, 
        fractions=fractions)
    L <- B[,-(1:n)]
    AR <- B[,1:n]
    C <- GaussianElimination(t(AR), diag(n), tol=tol, verbose=verbose, 
        fractions=fractions)
    R <- t(C[,-(1:m)])
    AC <- t(C[,1:m])
    ginv <- R %*% t(AC) %*% L
    if (fractions) fractions (ginv) else round(ginv, round(abs(log(tol, 10))))
    }
    
cholesky <- function(X, tol=sqrt(.Machine$double.eps)){
    # returns the Cholesky square root of the nonsingular, symmetric matrix X
    # tol: tolerance for checking for 0 pivot
    # algorithm from Kennedy & Gentle (1980)
    if (!is.numeric(X)) stop("argument is not numeric")
    if (!is.matrix(X)) stop("argument is not a matrix")
    n <- nrow(X)
    if (ncol(X) != n) stop("matrix is not square")
    if (max(abs(X - t(X))) > tol) stop("matrix is not symmetric")
    D <- rep(0, n)
    L <- diag(n)
    i <- 2:n
    D[1] <- X[1, 1]
    if (abs(D[1]) < tol) stop("matrix is numerically singular")
    L[i, 1] <- X[i, 1]/D[1]
    for (j in 2:(n - 1)){
        k <- 1:(j - 1)
        D[j] <- X[j, j] - sum((L[j, k]^2) * D[k])
        if (abs(D[j]) < tol) stop("matrix is numerically singular")
        i <- (j + 1):n
        L[i, j] <- (X[i, j] -
                        colSums(L[j, k] * t(L[i, k, drop=FALSE]) * D[k]))/D[j]
        }
    k <- 1:(n - 1)
    D[n] <- X[n, n] - sum((L[n, k]^2) * D[k])
    if (abs(D[n]) < tol) stop("matrix is numerically singular")
    L %*% diag(sqrt(D))
    }


