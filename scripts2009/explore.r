# ==============================================================================
#
# EXPLORE  set of functions to facilitate explorative data analyses
#
# ==============================================================================


# ==============================================================================
#
# pairs.lin.loess <- function(data,span=0.75,...)
#
# ==============================================================================
#
# Plot a scatter plot with linear regression, nonparametric regression (loess),
# and histograms.
#
# Arguments:
#   data:  Data frame of data to be plotted.
#   span:  Argument passed to loess to determine degree of smoothing.
#   ...:   Further arguments passed to the plotting routine "pairs".
#
# ==============================================================================
#
# Last revision: 05.07.2006 Peter Reichert

library(stats)

span.global <- 0.75

panel.lin.loess <- function(x,y,...)
{
   ind.avail <- ifelse(is.na(x)|is.na(y),F,T)
   x.tmp <- x[ind.avail]
   y.tmp <- y[ind.avail]
   ind <- order(x.tmp)
   x.tmp <- x.tmp[ind]
   y.tmp <- y.tmp[ind]
   points(x.tmp,y.tmp,...)
   res.lm    <- lm   (formula=y.tmp~x.tmp)
   res.loess <- loess(formula=y.tmp~x.tmp,span=span.global)
   y.lin <- predict(res.lm,interval="confidence")
   lines(x.tmp,y.lin[,1],lty="solid",lwd=2)
   lines(x.tmp,y.lin[,2],lty="dashed",lwd=1)
   lines(x.tmp,y.lin[,3],lty="dashed",lwd=1)
   lines(x.tmp,predict(res.loess),lty="solid",lwd=2)
}

panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, ...)
}

pairs.lin.loess <- function(data,span=0.75,...)
{
   assign("span.global",span,.GlobalEnv)
   pairs(data,diag.panel=panel.hist,panel=panel.lin.loess,...)
}


# ==============================================================================
#
# comb(n,p)
#
# ==============================================================================
#
# Calculate all combinations of subsets of length p out of n indices.
#
# Arguments:
#   n:  Number of indices.
#   p:  Subset size.
#
# Return Value:
#   Matrix with subsets of length p as rows.
#
# ==============================================================================
#
# Last revision: 29.12.2002 Peter Reichert

comb <- function(n,p)
{
   # check input:
   if ( p > n ) stop("comb: illeal arguments (p>n)")

   # initialize array and auxiliary variables:
   num.comb <- choose(n,p)
   comb <- matrix(nrow=num.comb,ncol=p)
   ind <- 1:p
   pointer <- p

   # calculate index combinations:
   for ( i in 1:num.comb )
   {
      comb[i,] <- ind
      ind[pointer] <- ind[pointer]+1
      if ( ind[pointer] > n )
      {
         while ( pointer > 1 )
         {
            pointer <- pointer-1
            if ( ind[pointer] < n-(p-pointer) )
            {
               ind[pointer] <- ind[pointer]+1
               for ( j in (pointer+1):p )
               {
                  ind[j] <- ind[pointer]+j-pointer
               }
               pointer <- p
               break
            }
         }
      }
   }

   # return results:
   return(comb)
}


# ==============================================================================


contains.no.na <- function(x)
{
   !any(is.na(x))
}

contains.only.na <- function(x)
{
   !any(!is.na(x))
}


# ==============================================================================
#
# explore.lin(data,name.dep,levels=1,interactions=F,file=NA,sort.by="BIC",...)
#
# ==============================================================================
#
# Input:
#   data:         Data frame of independent and dependent variables.
#   name.dep:     Name of dependent variable (one of the column names of "data")
#   levels:       Vector of numbers of arguments of functions to be tested.
#   interactions: Linear regression with or without interactions (T or F).
#   file:         Optional output file name.
#   sort.by:      Sort output according to "BIC", "AIC" or "R2".
#
# ==============================================================================
#
# Last revision: 18.11.2006 Peter Reichert and Renata Hari

explore.lin <- function(data,name.dep,levels=1,interactions=F,file=NA,sort.by="BIC",...)
{
   # check input and find dependent variable:
   
   if ( (nrow(data) < 1) || ncol(data) < 2 ) return(NULL)
   names.x <- names(data)
   ind.dep <- match(name.dep,names.x)
   if ( is.na(ind.dep) ) stop("dependent variable not found")
   if ( length(ind.dep) != 1 ) stop("dependent variable not unique")
   names.x <- names.x[-ind.dep]
   
   # perform analyses:
   
   collapse.char <- "+"
   if ( interactions ) collapse.char <- "*"
   res <- list()
   for ( i in 1:length(levels) )
   {
      # analyses for a given level:

      ind.col = comb(length(names.x),levels[i])
      if ( nrow(ind.col) > 0 )
      {
         print(paste("Fitting",
                     nrow(ind.col),
                     "models with",
                     levels[i],
                     "influence factors"))
         flush.console()
         num.par <- levels[i]+1
         if ( interactions ) num.par <- 2^levels[i]
         for ( j in 1:nrow(ind.col) )
         {
            # model string for single analysis:

            data.local <- data[,c(name.dep,names.x[ind.col[j,]])]
            ind.row <- apply(data.local,1,contains.no.na)
            data.local <- data.local[ind.row,]
            model <- paste(name.dep,
                           "~",
                           paste(names.x[ind.col[j,]],collapse=collapse.char))

            # check x-data for spanning of space:

            nrow.x = nrow(data.local)
            ncol.x = ncol(data.local)-1
            data.ok <- FALSE
            if ( nrow.x > 1 )
            {
               data.local.x <- data.local[,-1]
               if ( ncol.x < 2 ) dim(data.local.x) <- c(nrow.x,ncol.x)
               vectors.x <- data.local.x[-1,]-
                            matrix(rep(as.numeric(data.local.x[1,]),nrow.x-1),
                                   nrow=nrow.x-1,ncol=ncol.x,byrow=T)
               rank <- qr(vectors.x)$rank
               if ( rank == levels[i] ) data.ok <- TRUE
            }

            # single analysis:

            if ( !data.ok )
            {
               # insufficient data, result = NA:

               if ( j == 1 )
               {
                  res[[i]] <- matrix(c(nrow(data.local),
                                       rep(NA,3+num.par)),nrow=1)
                  rownames(res[[i]]) <- model
               }
               else
               {
                  res[[i]] <- rbind(res[[i]],
                                    c(nrow(data.local),rep(NA,3+num.par)))
               }
            }
            else
            {
               # perform single analysis:

               res.lm <- lm(as.formula(model),data=data.local)
               r2  <- 1 - var(residuals(res.lm))/var(data.local[,name.dep])
               aic <- AIC(res.lm)
               bic <- AIC(res.lm,k=log(nrow(data.local)))
               coef <- coefficients(res.lm)
               if ( j == 1 )
               {
                  res[[i]] <- matrix(c(nrow(data.local),r2,aic,bic,coef),nrow=1)
                  rownames(res[[i]]) <- model
               }
               else
               {
                  res[[i]] <- rbind(res[[i]],c(nrow(data.local),r2,aic,bic,coef))
               }
            }
            colnames(res[[i]]) <- c("n","R2","AIC","BIC",
                                    paste("par.",1:num.par,sep=""))
            row.names <- rownames(res[[i]])
            row.names[nrow(res[[i]])] <- model
            rownames(res[[i]]) <- row.names
         }
         ind.row <- order(res[[i]][,"BIC"],decreasing=F)
         if ( sort.by == "R2" )
         {
            ind.row <- order(res[[i]][,"R2"],decreasing=T)
         }
         else
         {
            if ( sort.by == "AIC" )
            {
               ind.row <- order(res[[i]][,"AIC"],decreasing=F)
            }
            else
            {
               if ( sort.by != "BIC" ) stop(paste("illegal value of argument sort:",sort))
            }
         }
         res[[i]] <- res[[i]][ind.row,]
         if ( ! is.na(file) )
         {
            file.split <- unlist(strsplit(file,"\\."))
            l <- length(file.split)
            file.split[l+1] <- file.split[l]
            file.split[l] <- levels[i]
            file.i <- paste(file.split,collapse=".")
            write.table(res[[i]],file=file.i,col.names=NA,sep="\t")
         }
      }
   }
   return(res)
}
