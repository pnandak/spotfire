## commands from class, May 17 2006

## Verzani, Example 9.2, p261

library(UsingR)
data(samhda)
attach(samhda)

tbl <- xtabs(~ gender + amt.smoke,   ## formula 
             subset = amt.smoke < 98 & gender !=7,  ## subset out missing data
             data=samhda)
chisq.test(tbl)

tbl <- xtabs(~ gender + alcohol,
             subset = alcohol != 9 & gender !=7,  ## subset out missing data
             data=samhda)
chisq.test(tbl)


###########################################################
## gammaGK function
##
## Goodman Kruskal gamma measure of association
## for ordinal variables
##
## simon jackman
## department of political science
## stanford university
##########################################################
gammaGK <- function(x,y=NULL){
  concordant <- function(x){ 
    ## get sum(matrix values > r AND > c) 
    ## for each matrix[r, c] 
    mat.lr <- function(r,c){ 
      lr <- x[(r.x > r) & (c.x > c)] 
      sum(lr) 
    } 
    
    ## get row and column index for each 
    ## matrix element 
    r.x <- row(x) 
    c.x <- col(x) 
    
    ## return the sum of each matrix[r, c] * sums 
    ## using mapply to sequence thru each matrix[r, c] 
    sum(x * mapply(mat.lr, r = r.x, c = c.x)) 
  } 
  
  discordant <- function(x){ 
    ## get sum(matrix values > r AND < c) 
    ## for each matrix[r, c] 
    mat.ll <- function(r,c){ 
      ll <- x[(r.x > r) & (c.x < c)] 
      sum(ll) 
    } 
    
    ## get row and column index for each 
    ## matrix element 
    r.x <- row(x) 
    c.x <- col(x) 
    
    ## return the sum of each matrix[r, c] * sums 
    ## using mapply to sequence thru each matrix[r, c] 
    sum(x * mapply(mat.ll, r = r.x, c = c.x)) 
  } 
 
  if(is.table(x) | is.matrix(x)){
    c <- concordant(x) 
    d <- discordant(x)
    n <- sum(x)
  }
  else{
    tab <- table(x,y)
    c <- concordant(tab) 
    d <- discordant(tab)
    n <- sum(tab)
  }
  gamma <- (c - d) / (c + d)
  
  arg <- (c+d)/(n*(1-(gamma^2)))
  stdError <- 1/sqrt(arg)
  z <- gamma/stdError
  
  cat("Goodman-Kruskal gamma statistic:\n")
  cat(paste("Concordant Pairs",c,"\n"))
  cat(paste("Discordant Pairs",d,"\n\n"))
  cat(paste("Estimate of gamma:",
            signif(gamma,.Options$digits),
            "Standard error:",
            signif(stdError,.Options$digits),
            "\n\n"))
  
  cat(paste("H0: gamma = 0 vs HA: two-sided\n"))
  cat(paste("z:",
            signif(z, .Options$digits),
            "p-value:",
            signif(2*(1-pnorm(abs(z))), .Options$digits),
            "\n\n"))
  if(c<51 | d<51){
    cat("Warning: p-values are based on a normal approximation to the\n")
    cat("sampling distribution of the z test statistic, which is commonly\n")
    cat("considered to be good only if C and D are both > 50.\n")
  }
  
  invisible(NULL)
}

## example of computing gamma
x <- c(0,0,1,1,2,2,3,3,2,2,3,3)
y <- c(0,1,1,2,1,1,2,4,3,3,1,0)

## can do it this way
gammaGK(x,y)

## or first compute a table
## and then feed to gammaGK
tbl <- table(x,y)
gammaGK(x,y)

## and yet another way, but no p-values etc
library(Hmisc)          ## contains the rcorr.cens function
rcorr.cens(x,y,outx=T)  ## Dxy is gamma

