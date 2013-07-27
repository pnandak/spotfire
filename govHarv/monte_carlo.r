# This code performs a simple Monte Carlo experiment
# to illustrate the coverage of CI intervals for single and joint tests
# Jens Hainmueller 11/28/2007

library(MASS)
M <- 5000         # number of Monte Carlo iterations
N <- 100          # sample size

# the following are the true values of beta that generate the data
b0.true <- 0        # the intercept
b1.true <- 25       # the coefficient on x1
b2.true <- -5       # the coefficient on x2
sigma2     <- 10    # variance of the disturbances

# X assumed fixed; drawn from multivariate normal
X <- mvrnorm(n = N, c(0,0), matrix(c(1,0.5,0.5,1),2,2))

# check ?mvrnorm for more info. here you can manipulate the variances
# and covarainces of x1 and x2.

#####################################
# Monte Carlo Experiment Starts here:

# function that runs regressions
myreg <- function(e,Xmat,b2,b1,b0)
  {
     # run regression in simulated dataset
     lm.out <- lm(I(b0+b1*Xmat[,1]+b2*Xmat[,2]+e)~X)

     # store betas and 95% CI
     out <- c(lm.out$coef[2],confint(lm.out)[2,],lm.out$coef[3],confint(lm.out)[3,])
}

# draw M error vectors
e.mat <- replicate(n=M,expr = rnorm(nrow(X),mean=0,sd=5))

# run M regresions in simulated datasets
res <- apply(e.mat,2, myreg , Xmat=X , b2=b2.true, b1=b1.true , b0=b0.true)

# check coverage
# true b1 in b1 CI?
cat("True Beta 1 covered by 95 confidence intervals in",
sum( (b1.true > res[2,]) & (b1.true < res[3,]))/M *100, "percent of the samples \n")

# true b2 in b2 CI?
cat("True Beta 2 covered by 95 confidence intervals in",
sum( (b2.true > res[5,]) & (b2.true < res[6,]))/M *100, "percent of the samples \n")

# both true b1 and true b2 in their respective CIs?
cat("True Beta 1 and True Beta 2 BOTH covered by their\n","95 confidence intervals in",
sum( ((b2.true > res[5,]) & (b2.true < res[6,])) &
     ((b1.true > res[2,]) & (b1.true < res[3,]))
 )/M *100, "percent of the samples \n")

## Plot results
# Coverage beta 1
plot(1:1000,ylim=c(min(res[2,]),max(res[3,])),type="n",xlab="Sample",ylab="b1.value",
main="How often is True Beta 1 covered?")
thin <- seq(1,1000,by=10)
colc <- (b1.true > res[2,]) & (b1.true < res[3,])
colc[colc==TRUE] <- "green"
colc[colc==FALSE] <- "red"
segments(col(res)[2,thin],res[2,thin],
         col(res)[3,thin],res[3,thin],
         col=colc[thin])
abline(h=b1.true,col="black",lty="dotted")
legend("bottomright",legend=c("True Beta 1","covered by CI","not covered by CI"),
lty=c("dotted","solid","solid"),col=c("black","green","red"))

## Plot results
# Coverage beta 2
plot(1:1000,ylim=c(min(res[5,]),max(res[6,])),type="n",xlab="Sample",ylab="b2.value",
main="How often is True Beta 2 covered?")
colc <- (b2.true > res[5,]) & (b2.true < res[6,])
colc[colc==TRUE] <- "green"
colc[colc==FALSE] <- "red"
segments(col(res)[5,thin],res[5,thin],
         col(res)[6,thin],res[6,thin],
         col=colc[thin])
abline(h=b2.true,col="black",lty="dotted")
legend("bottomright",legend=c("True Beta 2","covered by CI","not covered by CI"),
lty=c("dotted","solid","solid"),col=c("black","green","red"))

## Plot results
plot(1:1000,ylim=c(min(res[5,]),max(res[6,])),xlim=c(min(res[2,]),max(res[3,])),
     type="n",xlab="b1.value",ylab="b2.value",
main="How often are BOTH True Beta 1 and True Beta 2 covered?")
cov.both <-  (((b2.true > res[5,]) & (b2.true < res[6,])) &
             ((b1.true > res[2,]) & (b1.true < res[3,])))
cov.b1 <-  ((b1.true > res[2,]) & (b1.true < res[3,]))
cov.b2 <-  ((b2.true > res[5,]) & (b2.true < res[6,]))

points(res[1,covered],res[4,covered],pch=1,col="green")
#points(res[1,covered==F],res[4,covered==F],pch=19,col="red")
points(res[1,cov.b1==F & cov.b2==F],res[4,cov.b1==F & cov.b2==F],pch=19,col="red")
points(res[1,cov.b1==T & cov.b2==F],res[4,cov.b1==T & cov.b2==F],pch=6,col="blue")
points(res[1,cov.b1==F & cov.b2==T],res[4,cov.b1==F & cov.b2==T],pch=2,col="orange")
points(b1.true,b2.true,pch=15,col="black",cex=1)
legend("topright",legend=c(
"True (Beta 1,Beta 2)",
paste("covered by both CI: ",sum(cov.both)/M*100,"Percent"),
paste("b2 not covered by its CI: ",sum(cov.b1==T & cov.b2==F)/M*100,"Percent"),
paste("b1 not covered by its CI: ",sum(cov.b1==F & cov.b2==T)/M*100,"Percent"),
paste("both b1 and b2 not covered: ",sum(cov.b1==F & cov.b2==F)/M*100,"Percent")
),
pch=c(15,1,6,2,19),col=c("black","green","blue","orange","red"))
