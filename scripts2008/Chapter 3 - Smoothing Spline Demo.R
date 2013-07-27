#Now A Handrolled Cubic Smoothing Spline
#Read In The Data
library(foreign)

#Set the Working Directory
jacob <- read.dta("jacob.dta")
attach(jacob)

#Rescale Ind. Variable
perot <-  perotvote- min(perotvote)
perot <- perot/max(perot)

#Write a Function R(x,z) to For the Cubic Spline Basis
rk <- function(x,z){
  ((z-0.5)^2-1/12)*((x-0.5)^2-1/12)/4-
  ((abs(x-z)-0.5)^4-(abs(x-z)-0.5)^2/2 + 7/240)/24
  }
  
#Model Matrix
spl.X <- function(x, xk){
   # set up model matrix for cubic penalized regression spline
   q <- length(xk)+2 # number of parameters
   n <- length(x) #amount of data
   X <- matrix(1,n,q) #initialized model matrix with constant in first column
   X[,2] <- x #set second column to x
   X[,3:q] <- outer(x,xk,FUN=rk) # add in remaining to R(x,xk)
   X
   }
#Penalty Function
spl.S <- function(xk)
         {#penalty spline matrix for given knot sequence
         q <- length(xk)+2
         S <- matrix(0,q,q) #Initial Matrix is All 0's
         S[3:q,3:q] <- outer(xk,xk,FUN=rk) # fill in nonzero part
         S
         }
         
#Matrix Square Root Function Could Also use Cholesky Decomposition
mat.sqrt <- function(S){
            d <- eigen(S,symmetric=TRUE)  
            rS <- d$vectors%*%diag(d$values^0.5)%*%t(d$vectors)
            }
#Smoothing Spline Function
prs.fit <- function(y,x,xk,lambda){
           q <- length(xk)+2 #dimension basis
           n <- length(x)    #Data dimension
           #Create augmented data matrix
           X.a <- rbind(spl.X(x,xk),mat.sqrt(spl.S(xk))*sqrt(lambda))
           y[(n+1):(n+q)] <- 0 #augment y vector
           lm(y ~ X.a-1) #Fit and return spline coefficients.
           }

#Now Choose Number of Knots - Here 4 Knots
xk <- 1:9/10
#Create Model Matrix
mod <- prs.fit(chal.vote, perot, xk, 0.01)

#X values for prediction
xp <- 0:100/100

#Matrix for Predictions
Xp <- spl.X(xp,xk)

plot(perot, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot", bty="l")
points(perot, chal.vote, pch=".", cex=1.75)
lines(xp,Xp%*%coef(mod))
