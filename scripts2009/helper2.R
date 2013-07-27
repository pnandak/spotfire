## 3 files of function definitions from pscl version 0.90
## to use these functions, just source this file into R:
## > source("helper.R")
##
## help for these functions is in library(pscl),
## available from CRAN approx May 1 2007
##
## simon jackman, dept of political science, stanford univ
###########################################################

## hitmiss methods
hitmiss <- function(obj,
                    digits=max(3,getOption("digits")-3),
                    ...){
  UseMethod("hitmiss")
}

hitmiss.glm <- function(obj,
                        digits=max(3,getOption("digits")-3),
                        ...,
                        k=.5){
  if(!inherits(glmobj,"glm"))
    stop("hitmiss.glm only defined for objects of class glm\n")

  ## check to make sure if of class binomial
  if(family(glmobj)$family!="binomial")
    stop("hitmiss.glm only defined for binomial family glm objects\n")
  if(is.null(glmobj$y))
    y <- update(glmobj,y=TRUE)$y
  else
    y <- glmobj$y
  
  if(!all(y %in% c(0,1)))
    stop("hitmiss.glm only defined for models with binary responses")
  
  p <- predict(glmobj,type="response")
  tab <- matrix(NA,2,2)
  tab[1,1] <- sum(p<k & y==0,na.rm=T)   ## true negative
  tab[1,2] <- sum(p<k & y==1,na.rm=T)   ## false negative
  tab[2,1] <- sum(p>k & y==0,na.rm=T)   ## false positive
  tab[2,2] <- sum(p>k & y==1,na.rm=T)   ## true positive
  pcp <- sum(diag(tab))/sum(tab) * 100  ## overall PCP measure
  pcp0 <- tab[1,1]/sum(tab[1:2,1])*100  ## PCP | y = 0
  pcp1 <- tab[2,2]/sum(tab[1:2,2])*100  ## PCP | y = 1
  
  dimnames(tab) <- list(c("yhat=0","yhat=1"),
                        c("y=0","y=1"))
  cat(paste("Classification Threshold =",k,"\n"))
  print(tab)
  cat(paste("Percent Correctly Predicted = ",
            format(pcp,digits=digits),
            "%\n",
            sep=""))
  cat(paste("Percent Correctly Predicted = ",
            format(pcp0,digits=digits),
            "%, for y = 0\n",
            sep=""))
  cat(paste("Percent Correctly Predicted = ",
            format(pcp1,digits=digits),
            "%  for y = 1\n",
            sep=""))
  nullmodel <- max(c(sum(y==0),sum(y==1)))/sum(tab) * 100
  cat(paste("Null Model Correctly Predicts ",
            format(nullmodel,digits=digits),
            "%\n",
            sep=""))
  out <- c(pcp,pcp0,pcp1)
  out
}

hitmiss.polr <- function(obj,
                         digits=max(3,getOption("digits")-3),
                         ...){
  tmp <- obj
  p <- predict(tmp)

  if(is.null(tmp$model)){
    cat("refitting the model to extract responses...\n")
    tmp <- update(tmp,model=TRUE)
    cat("\n")
  }
  y <- tmp$model[,1]
  uniqueY <- sort(unique(y))
  J <- length(uniqueY)
  p <- factor(p,levels=uniqueY)
  y <- factor(y,levels=uniqueY)
  cat("Table of Actual (y) Against Predicted (p)\n")
  cat("Classification rule: outcome with highest probability.\n")
  tab <- table(y,p)
  dimnames(tab)[[1]] <- paste("y=",dimnames(tab)[[1]],sep="")
  dimnames(tab)[[2]] <- paste("p=",dimnames(tab)[[2]],sep="")
  n <- length(y)
  pcp <- sum(diag(tab))/n * 100
  tabY <- table(y)
  pcp0 <- tabY[which.max(tabY)]/n * 100

  pcpByRow <- rep(NA,J)
  for(i in 1:J){
    pcpByRow[i] <- tab[i,i]/sum(tab[i,]) * 100
  }
  tab <- cbind(tab,pcpByRow)
  dimnames(tab)[[2]][J+1] <- "Row PCP"

  print(tab,digits=digits)
  cat("\n")

  cat("Percent Correctly Predicted, Fitted Model: ",
      format(pcp,digits=digits),
      "%\n",sep="")
  cat("Percent Correctly Predicted, Null Model  : ",
      format(pcp0,digits=digits),
      "%\n",sep="")
  invisible(NULL)
}

hitmiss.multinom <- function(obj,
                             digits=max(3,getOption("digits")-3),
                             ...){
  hitmiss.polr(obj)
}

## logLikelihood for polr objects
logLik.polr <- function(object, ...){
  if(class(object)!="polr")
    stop("logLik.polr only defined for objects of class polr\n")

  tmp <- object
  if(is.null(tmp$model)){
    stop("no model component found in polr object, try refitting\n")
  }

  y <- tmp$model[,1]
  yNum <- as.numeric(y)
  p <- tmp$fitted.values
  n <- length(y)
  logLike <- rep(NA,n)
  weights <- rep(1,n)
  if(!is.null(object$call$weights))
    weights <- object$model[,"(weights)"]
  for(i in 1:n){
    logLike[i] <- log(p[i,yNum[i]])*weights[i]
  }
  sum(logLike)
}


## pseudo r2 for binary, ordinal and multinomial models

pR2 <- function(object,...){
  UseMethod("pR2")
}

pR2Work <- function(llh,llhNull,n){
  McFadden <- 1 - llh/llhNull
  G2 <- -2*(llhNull-llh)
  r2ML <- 1 - exp(-G2/n)
  r2ML.max <- 1 - exp(llhNull*2/n)
  r2CU <- r2ML/r2ML.max
  out <- c(llh=llh,
           llhNull=llhNull,
           G2=G2,
           McFadden=McFadden,
           r2ML=r2ML,
           r2CU=r2CU)
  out      
}

pR2.glm <- function(object,...){
  llh <- logLik(object)
  objectNull <- update(object, ~ 1)
  llhNull <- logLik(objectNull)
  n <- dim(object$model)[1]
  pR2Work(llh,llhNull,n,k)
}

pR2.polr <- function(object,...){
  llh <- logLik(object)
  objectNull <- update(object, ~ 1)
  llhNull <- logLik(objectNull)
  n <- object$nobs
  pR2Work(llh,llhNull,n)
}

pR2.multinom <- function(object,...){
  llh <- logLik(object)
  cat("fitting null model for pseudo-r2\n")
  objectNull <- update(object, ~ 1)
  llhNull <- logLik(objectNull)
  n <- dim(object$fitted.values)[1]
  pR2Work(llh,llhNull,n)
}


