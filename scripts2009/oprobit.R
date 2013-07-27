#################################################################
## ordered probit functions
##
## simon jackman, dept of political science, stanford university
## march 2000
## added option to trace iterations, april 17, 2000
## turned this all into a class, april 2003
## fixed Defunct print.coefmat, march 2005
#################################################################

## a wrapper function, stealing the setup and argument parsing from lm
oprobit <- function(formula, data = list(),
                    subset=NULL, weights=NULL,
                    na.action=na.fail,
                    singular.ok = TRUE,
                    method = "BFGS", model = TRUE, x = FALSE, y = FALSE,
                    contrasts = NULL, trace = FALSE,
                    link = "probit",
                    offset = NULL, maxit=50000,
                    ... )
{
  if (method=="Nelder-Mead")
    control <- list(maxit=maxit,trace=trace,REPORT=1)
  if(method=="BFGS")
    control <- list(trace=trace,maxit=maxit)
  if(method=="L-BFGS-B")
    control <- list(trace=trace,maxit=maxit)
  cat("\n")
  cat("        Ordered Probit Analysis (Copyright Simon Jackman, 2000-2005)\n")
  cat("        -----------------------------------------------------------\n")
  cat(paste("optimization of log-likelihood by",method,"method\n\n"))

  linkfn <- pnorm
  if(link=="logit"){
    cat("performing ordered logit analysis...\n")
    linkfn <- plogis
  }
  if(link !="logit" & link !="probit")
    stop("Invalid link function specified, terminating.\n")
  
  ret.x <- x
  ret.y <- y
  mf <- cl <- match.call(expand.dots=FALSE)
  mf$singular.ok <- mf$model <- mf$method <- mf$trace <- NULL
  mf$x <- mf$y <- mf$qr <- mf$contrasts <- mf$link <- NULL
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, sys.frame(sys.parent()))
  if (method == "model.frame")
    return(mf)
  mt <- attr(mf,"terms")
  xvars <- as.character(attr(mt, "variables"))[-1]
  if((yvar <- attr(mt, "response")) > 0) xvars <- xvars[-yvar]
  xlev <-
    if(length(xvars) > 0) {
      xlev <- lapply(mf[xvars], levels)
      xlev[!sapply(xlev, is.null)]
    }
  if (length(list(...)))
    warning(paste("Extra arguments", deparse(substitute(...)),
                  "are just disregarded."))
  if (!singular.ok)
    warning("only `singular.ok = TRUE' is currently implemented.")
  y <- model.response(mf, "numeric")
  w <- model.weights(mf)
  offset <- model.offset(mf)
  if(!is.null(offset) && length(offset) != NROW(y))
    stop(paste("Number of offsets is", length(offset),
               ", should equal", NROW(y), "(number of observations)"))
  
  if (is.empty.model(mt)) {
    x <- NULL
  }
  else {
    x <- model.matrix(mt, mf, contrasts)
  }
  
  if(is.null(y))
    stop("No observations in y")

  n <- length(y)
  tab <- table(y,exclude=NULL)
  cat("dependent variable y:\n")
  print(tab)
  y0 <- sort(unique(y))                        # unique values of y
  m <- length(y0)                              # number of unique values
  if(m<3)
    stop("Not enough unique levels in y for ordered probit\n")
  
  k <- dim(x)[2]
  b0 <- solve(crossprod(x))%*%crossprod(x,y)   # OLS for start values
  p <- c(b0,seq(from=0,by=.1,length=m-2))       # starting values

  cat("Start values for maximum likelihood estimation\n")
  print(p)
  
  Z <- matrix(NA,n,m)
  for (j in 1:m)
    Z[,j] <- y == y0[j]                        # Z_{ij} = 1 if y_i = j, else 0


  ## a function that will calculate the ordered probit log-likelihood function
  oprobitllh.exp <- function(parms)
    {
      b <- parms[1:k]              # number of regression parameters
      lambda <- exp(parms[-(1:k)]) # remaining parameters are thresholds
      lambda <- c(0,lambda)        # first threshold is zero
      lambda <- cumsum(lambda)
      probs <- oprobitprobs(x,n,m,b,lambda,linkfn)
      llh <- log(probs[Z])         # Z knocks out the probs we don't need
      if (any(is.nan(llh))){
        cat("found bad probabilities with current parameter values, continuing...\n")
      }
      llh <- -1 * sum(llh)         # return negative log-likelihood, since optim is a minimizer
      llh                          # this is what actually gets returned to the parent function
    }
  
  oprobitllh <- function(parms)
    {
      b <- parms[1:k]             # number of regression parameters
      lambda <- parms[-(1:k)]     # remaining parameters are thresholds
      lambda <- c(0,lambda)       # first threshold is zero
      probs <- oprobitprobs(x,n,m,b,lambda,linkfn)
      llh <- log(probs[Z])        # Z knocks out the probs we don't need
      if (any(is.nan(llh))){
        cat("found bad probabilities with current parameter values, continuing...\n")
      }
      llh <- -1 * sum(llh)        # return negative log-likelihood, since optim is a minimizer
      llh                         # this is what actually gets returned to the parent function
    }
  
  
  ## finally, call the optimization routine
  poo <- optim(par=p,fn=oprobitllh.exp,
               method=method,
               hessian=T,
               control=control)
  if (poo$convergence != 0){
    stop("failed to converge, hmmm....")
  }

  p <- c(poo$p[1:k],
         cumsum(exp(poo$p[-(1:k)])))
  poo <- optim(par=p,fn=oprobitllh,
               method=method,
               hessian=T,
               control=control)
  if (poo$convergence != 0){
    stop("failed to converge, hmmm....")
  }

  llh <- -1.0 * poo$value

  out <- list()
  out$call <- cl

  out$beta <- poo$p[1:k]
  names(out$beta) <- dimnames(x)[[2]]
  
  out$lambda <- c(0,poo$p[(k+1):(length(poo$p))])
  names(out$lambda) <- paste("Thres",1:length(out$lambda),sep="")
  
  out$hessian <- poo$hessian
  out$y <- y
  out$method <- method
  out$Z <- Z
  out$x <- x
  out$n <- n
  out$k <- k
  out$y0 <- y0
  out$m <- m
  out$llh <- llh
  out$contrasts <- attr(x,"contrasts")
  out$offset <- offset
  out$xlevels <- xlev
  out$terms <- mt
  out$link <- link
  out$linkfn <- linkfn
  class(out) <- "oprobit"
  out
}

print.oprobit <- function(x,digits=max(3,getOption("digits")-3),...)
{
  cat("\nCall:\n",deparse(x$call), "\n\n", sep="")
  cat("Coefficients:\n")
  print.default(format(coef(x), digits = digits), print.gap = 2, 
                quote = FALSE)
  cat("\n")
  invisible(x)
}

coef.oprobit <- function(object)
{
  c(object$b,object$lambda)
}

oprobitprobs <- function(x,n,m,b,lambda,linkfn){
  ystar <- x%*%b              # latent variable
  cprobs <- matrix(NA,n,m)
  probs <- matrix(NA,n,m)
  for (j in 1:(m-1))          # loop to get category-specific probabilities
    cprobs[,j] <- linkfn(lambda[j]-ystar)  # cumulative probabilities
  probs[,m] <- 1-cprobs[,m-1] # top category is simple
  probs[,1] <- cprobs[,1]     # bottom category is simple too
  for (j in 2:(m-1))          # middle categories are actually differences of cumulatives
    probs[,j] <- cprobs[,j]-cprobs[,j-1]
  probs
}

## a function that generates predicted probabilities
predict.oprobit <- function(obj,newdata=NULL,type="response")
{
  b <- obj$beta
  lambda <- obj$lambda
  tt <- terms(obj)

  indexmax <- function(x){
    n <- length(x)
    imax <- (1:n)[(order(x))[n]]
    imax
  }
  
  if(missing(newdata) || is.null(newdata)){
    if(type=="response"){   ## simply return matrix of predicted probabilities
      out <- oprobitprobs(x=obj$x,n=obj$n,m=obj$m,b=obj$beta,
                          lambda=obj$lambda,linkfn=obj$linkfn)
    }
    if(type=="link"){
      out <- obj$x%*%obj$beta
    }
    if(type=="discrete"){
     foo <- oprobitprobs(x=obj$x,n=obj$n,m=obj$m,b=obj$beta,
                         lambda=obj$lambda,linkfn=obj$linkfn)
     out <- apply(foo,1,indexmax)
   }
  }
  else{
    X <- model.matrix(delete.response(tt),
                      newdata,
                      contrasts = obj$contrasts, 
                      xlev = obj$xlevels)
    yhat <- X%*%obj$beta
    if(type=="response"){
      out <- oprobitprobs(x=X,n=dim(X)[1],m=obj$m,b=obj$beta,
                          lambda=obj$lambda,linkfn=obj$linkfn)
    }
    if(type=="link"){
      out <- yhat
    }
    if(type=="discrete"){
      foo <- oprobitprobs(x=X,n=dim(X)[1],m=obj$m,b=obj$beta,
                          lambda=obj$lambda,linkfn=obj$linkfn)
      out <- apply(foo,1,indexmax)
   }
  }
  out
}

summary.oprobit <- function(obj,table=T,digits=4){
  out <- list()
  out$call <- obj$call
  out$y <- obj$y
  out$y0 <- obj$y0
  out$n <- obj$n
  out$m <- obj$m
  out$beta <- obj$beta
  out$se <- sqrt(diag(solve(obj$hessian)))
  out$lambda <- obj$lambda
  out$coefficients <- cbind(obj$beta,
                            out$se[1:obj$k])
  tstat <- out$coefficients[,1]/out$coefficients[,2]
  pval <- 2*pnorm(-abs(tstat))
  out$coefficients <- cbind(out$coefficients,tstat,pval)
  dimnames(out$coefficients) <- list(names(out$beta),
                                     c("Estimate","Std. Error","t value","Pr(>|t|)"))
  out$lambda.coefficients <- cbind(out$lambda,
                                   c(NA,out$se[-(1:obj$k)]))
  dimnames(out$lambda.coefficients) <- list(names(out$lambda),
                                            c("Estimate","Std. Error"))
  out$llh <- obj$llh
  out$method <- obj$method
  out$link <- obj$link
  out$table <- NULL
  if(table){
    phat <- predict.oprobit(obj,type="response")
    out$yhat <- apply(phat,1,function(x)rev(order(x))[1])   ## largest category
    out$yhat <- out$y0[out$yhat]                            ## matching y value
    out$tab <- table(out$yhat,obj$y)
   }
  class(out) <- "summary.oprobit"
  out
}

print.summary.oprobit <- function(x,
                                  digits = max(3, getOption("digits") - 3),
                                  na.print = "", 
                                  symbolic.cor = p > 4,
                                  signif.stars = getOption("show.signif.stars"), 
                                  ...)
{
  cat("        Ordered Probit Analysis (Copyright Simon Jackman, 2000-2005)\n")
  cat("        ------------------------------------------------------------\n")
  
  cat("\nCall:\n")
  cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")

  if(x$link=="logit")
    cat("Ordered Logit Model was fit to the data\n")
  
  cat("\nCoefficients:\n")

  print.matrix(x$coefficients, digits = digits, ...)
  cat("\nThreshold parameters:\n")
  print(signif(x$lambda.coefficients),digits)
  
  cat(paste("\noptimization of log-likelihood by",x$method,"method\n"))
  cat(paste("log-likelihood at convergence:",
            round(x$llh,2),
            "\n\n"))
  
  if(!is.null(x$tab)){
    cat("Table of Predicted Outcomes Against Actual Outcomes\n")
    print(x$tab)
    ok <- rep(0,x$m)
    for(j in x$y0){
      ok[j] <- sum(x$yhat==j & x$y==j)
    }
    cat(paste(signif(sum(ok)/sum(x$tab)*100,3),
        "% correctly predicted\n",sep=""))
    cat(paste(signif(max(table(x$y))/x$n*100,3),
              "% correctly predicted by null model (modal outcome)\n",
              sep=""))
    cat("prediction rule assigns to category with largest predicted probability\n")
  }
  invisible(NULL)
}
