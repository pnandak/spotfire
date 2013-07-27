##### CSSS 536, Homework 3 code 
### Shauna Fisher

library(nnet)
library(MASS)

nes1992 <- read.table("http://students.washington.edu/fishes/CSSS536/data/nes1992.csv", header=TRUE, sep=",")

nes1992.na <- na.omit(nes1992)

attach(nes1992.na)


###########
#Problem 1#
###########

## part (a)
# using Likelihood function for 4 category ordered probit

llk.oprobit4 <- function(param, x, y) {     # preliminaries
  os <- rep(1, nrow(x))
  x <- cbind(os, x)  
  b <- param[1:ncol(x)]
  t2 <- param[(ncol(x)+1)]
  t3 <- param[(ncol(x)+2)]  

  # probabilities and penalty function

  xb <- x%*%b
  p1 <- log(pnorm(-xb))
  if (t2<=0)  p2 <- -(abs(t2)*10000)    # penalty function to keep t2>0
  else p2 <- log(pnorm(t2-xb)-pnorm(-xb))
  if (t3<=t2) p3 <- -((t2-t3)*10000)    # penalty to keep t3>t2
  else p3 <- log(pnorm(t3-xb)-pnorm(t2-xb))     
  p4 <- log(1-pnorm(t3-xb)) 

  # -1 * log likelihood (optim is a minimizer)

  -sum(cbind(y==0,y==1,y==2,y==3) * cbind(p1,p2,p3,p4))
}

library(MASS)

y <- bushapp

x <- cbind(milforce,rbdist,yrsofed,partyid,econ)

# Use optim directly

ls.result <- lm(y~x)                    # use ls estimates as starting values
stval <- c(ls.result$coefficients,1,2)  # initial guesses

oprobit.result <- optim(stval, llk.oprobit4, method="BFGS", x=x, y=y, hess=T)

pe2 <- oprobit.result$par                # point estimates
vc <- solve(oprobit.result$hessian)     # var-cov matrix
se <- sqrt(diag(vc))                    # standard errors
ll <- -oprobit.result$value          

# pe2
# (Intercept)    xmilforce      xrbdist     xyrsofed     xpartyid        xecon 
# 3.332292677 -0.332329015 -0.254291822 -0.008054712  0.247622426 -0.273330407  0.787865527  2.107488003

# ll
#[1] -476.893


## part (b)
# calculating xb across preferences for use of military force for a Strong Democrat based upon function estimates
milhat <- seq(1,5,1)
xbetad <- rep(0, length(milhat))
for (i in 1:length(milhat)){ 
xbetad[i] <- pe2[1]*1 + pe2[2]*milhat[i] + pe2[3]*mean(rbdist) + pe2[4]*mean(yrsofed) + 
            pe2[5]*(-3) + pe2[6]*mean(econ)
    }

#expected probabilities for Democrat approval of Bush
pi1hatd <- pnorm(0,xbetad)                              #Strongly Disapprove
pi2hatd <- pnorm(pe2[7],xbetad) - pnorm(0,xbetad)       #Disapprove
pi3hatd <- pnorm(pe2[8],xbetad) - pnorm(pe2[7],xbetad)  #Approve
pi4hatd <- 1 - pi1hatd - pi2hatd - pi3hatd              #Strongly Approve

par(mfrow=c(1,2))
plot(milhat, pi1hatd, pch=15, col="blue", xlab="opposition to military force", main="Democrat approval of Bush", ylab="expected probability", ylim=c(0,.85))
points(pi2hatd, pch=16, col="green")
points(pi3hatd, pch=17, col="purple")
points(pi4hatd, pch=18, col="red")


# calculating xb across preferences for use of military force for a Strong Republican based upon function estimates
milhat <- seq(1,5,1)
xbetar <- rep(0, length(milhat))
for (i in 1:length(milhat)){ 
xbetar[i] <- pe2[1]*1 + pe2[2]*milhat[i] + pe2[3]*mean(rbdist) + pe2[4]*mean(yrsofed) + 
            pe2[5]*(3) + pe2[6]*mean(econ)
    }

#expected probabilities for Republicans
pi1hatr <- pnorm(0,xbetar)                              # Strongly Disapprove
pi2hatr <- pnorm(pe2[7],xbetar) - pnorm(0,xbetar)       #Disapprove
pi3hatr <- pnorm(pe2[8],xbetar) - pnorm(pe2[7],xbetar)  #Approve
pi4hatr <- 1 - pi1hatr - pi2hatr - pi3hatr           #Strongly Approve

plot(milhat, pi1hatr, pch=15, col="blue", xlab="opposition to military force", main="Republican approval of Bush", ylab="expected probability", ylim=c(0,1))
points(pi2hatr, pch=16, col="green")
points(pi3hatr, pch=17, col="purple")
points(pi4hatr, pch=18, col="red")

legend(x=4,y=.8,bty="n",
       xjust=1,
       legend=c("SD","D","A","SA"),col=c("blue", "green", "purple", "red"),pch=c(15, 16, 17, 18))
       

## part (c)       
# democrats simulation
sims <- 1000
simbetas<-mvrnorm(sims,pe2,vc)
milhat <- seq(1,5,1)
simx <- NULL
simmud <- NULL
sim.xbetad <- NULL 
sim.pi1hatd0 <- NULL; sim.pi1hatd <- NULL
sim.pi2hatd0 <- NULL; sim.pi2hatd <- NULL
sim.pi3hatd0 <- NULL; sim.pi3hatd <- NULL
sim.pi4hatd0 <- NULL; sim.pi4hatd <- NULL
for (i in 1:length(milhat)) { 
simmud <- rbind(1, milhat[i], mean(rbdist), mean(yrsofed), -3, mean(econ))
sim.xbetad <- simbetas[,1:6]%*%simmud
simx <- rbind(simx, t(t(rep(milhat[i],sims))))  

sim.pi1hatd0 <- pnorm(0,sim.xbetad)
sim.pi1hatd <- rbind(sim.pi1hatd, sim.pi1hatd0)    

sim.pi2hatd0 <- pnorm(simbetas[,7],sim.xbetad) - pnorm(0,sim.xbetad)                         
sim.pi2hatd <- rbind(sim.pi2hatd, sim.pi2hatd0)    

#sim.pi3hatd0 <- pnorm(simbetas[,8],sim.xbetad) - pnorm(simbetas[,7],sim.xbetad)
sim.pi3hatd0 <- as.matrix(pnorm(simbetas[,8],sim.xbetad) - pnorm(simbetas[,7],sim.xbetad))   #this was coming in as row vector; as.matrix fixed to column vector
sim.pi3hatd <- rbind(sim.pi3hatd, sim.pi3hatd0)
 
sim.pi4hatd0 <- 1 - sim.pi1hatd0 - sim.pi2hatd0 - sim.pi3hatd0 
sim.pi4hatd <- rbind(sim.pi4hatd, sim.pi4hatd0)      
} 

# republican simulations
sims <- 1000
simbetas<-mvrnorm(sims,pe2,vc)
milhat <- seq(1,5,1)
simxr <- NULL
simmur <- NULL
sim.xbetar <- NULL 
sim.pi1hatr0 <- NULL; sim.pi1hatr <- NULL
sim.pi2hatr0 <- NULL; sim.pi2hatr <- NULL
sim.pi3hatr0 <- NULL; sim.pi3hatr <- NULL
sim.pi4hatr0 <- NULL; sim.pi4hatr <- NULL
for (i in 1:length(milhat)) { 
simmur <- rbind(1, milhat[i], mean(rbdist), mean(yrsofed), 3, mean(econ))
sim.xbetar <- simbetas[,1:6]%*%simmur
simxr <- rbind(simxr, t(t(rep(milhat[i],sims))))  

sim.pi1hatr0 <- pnorm(0,sim.xbetar)
sim.pi1hatr <- rbind(sim.pi1hatr, sim.pi1hatr0)    

sim.pi2hatr0 <- pnorm(simbetas[,7],sim.xbetar) - pnorm(0,sim.xbetar)                         
sim.pi2hatr <- rbind(sim.pi2hatr, sim.pi2hatr0)    

sim.pi3hatr0 <- as.matrix(pnorm(simbetas[,8],sim.xbetar) - pnorm(simbetas[,7],sim.xbetar))   
sim.pi3hatr <- rbind(sim.pi3hatr, sim.pi3hatr0)
 
sim.pi4hatr0 <- 1 - sim.pi1hatr0 - sim.pi2hatr0 - sim.pi3hatr0 
sim.pi4hatr <- rbind(sim.pi4hatr, sim.pi4hatr0)      
} 

source("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/chris' code/plot.simulates.r")
par(mfrow=c(1,2))
plot.simulates(simx,
               sim.pi1hatd,
               ci.range=c(0.67),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(1,5,0,1),
               closeplot=FALSE,
               collist="blue"
               )
               
plot.simulates(simx,
               sim.pi2hatd,
               ci.range=c(0.67),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(1,5,0,1),
               closeplot=FALSE,
               collist="green",
               addtoplot=TRUE
               )

plot.simulates(simx,
               sim.pi3hatd,
               ci.range=c(0.67),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(1,5,0,1),
               closeplot=FALSE,
               collist="purple",
               addtoplot=TRUE
               )
               
plot.simulates(simx,
               sim.pi4hatd,
               ci.range=c(0.67),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(1,5,0,1),
               closeplot=TRUE,
               collist="red",
               addtoplot=TRUE
               )

#plotting republicans               
plot.simulates(simxr,
               sim.pi1hatr,
               ci.range=c(0.67),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(1,5,0,1),
               closeplot=FALSE,
               collist="blue"
               )
               
plot.simulates(simxr,
               sim.pi2hatr,
               ci.range=c(0.67),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(1,5,0,1),
               closeplot=FALSE,
               collist="green",
               addtoplot=TRUE
               )

plot.simulates(simxr,
               sim.pi3hatr,
               ci.range=c(0.67),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(1,5,0,1),
               closeplot=FALSE,
               collist="purple",
               addtoplot=TRUE
               )
               
plot.simulates(simxr,
               sim.pi4hatr,
               ci.range=c(0.67),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(1,5,0,1),
               closeplot=TRUE,
               collist="red",
               addtoplot=TRUE
               )


## part (e)
# rewriting likelihood function for 3 categories

llk.oprobit3 <- function(param, x, y) {     # preliminaries
  os <- rep(1, nrow(x))
  x <- cbind(os, x)  
  b <- param[1:ncol(x)]
  t2 <- param[(ncol(x)+1)] 

  # probabilities and penalty function

  xb <- x%*%b
  p1 <- log(pnorm(-xb))
  if (t2<=0)  p2 <- -(abs(t2)*10000)    # penalty function to keep t2>0
  else p2 <- log(pnorm(t2-xb)-pnorm(-xb))     
  p3 <- log(1-pnorm(t2-xb)) 

  # -1 * log likelihood (optim is a minimizer)

  -sum(cbind(y==1,y==2,y==3) * cbind(p1,p2,p3))
}

# and fitting a model to econ3 with partyid as covariate
y2 <- econ3

x2 <- cbind(partyid)

ls.result2 <- lm(y2~x2)                    # use ls estimates as starting values
stval2 <- c(ls.result2$coefficients,1)  # initial guesses

oprobit.result2 <- optim(stval2, llk.oprobit3, method="BFGS", x=x2, y=y2, hess=T)

pe <- oprobit.result2$par                # point estimates
vc2 <- solve(oprobit.result2$hessian)     # var-cov matrix
se2 <- sqrt(diag(vc2))                    # standard errors
ll2 <- -oprobit.result2$value 
 
 
detach(nes1992.na)



###########
#Problem 2#
###########
attach(nes1992)

vote92.f <- factor(vote92, labels=c("Bush", "Clinton", "Perot"))       

## part (a)
ml.1 <- multinom(vote92 ~ rlibcon + rbdist + econ + gulfwar + nonwhite, na.action=na.omit, Hess=TRUE)
ml.1

vc <- solve(ml.1$Hessian)

p <- ml.1$wts
p
# [1]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
# [7]  0.00000000  0.00000000 -1.13404015 -0.52504754  0.79202914  0.59698668
#[13] -0.76152563  0.75370220  0.00000000 -2.14624671 -0.07646878  0.68184604
#[19]  0.29185753 -0.31141399 -0.99457561
pe <- c(p[9], p[10], p[11], p[12], p[13], p[14], p[16], p[17], p[18], p[19], p[20], p[21])
pe
# [1] -1.13404015 -0.52504754  0.79202914  0.59698668 -0.76152563  0.75370220
# [7] -2.14624671 -0.07646878  0.68184604  0.29185753 -0.31141399 -0.99457561

pe.1 <- as.vector(t(coef(ml.1)))


## part (b)
## calculating xbetas for whites
# Clinton
libconhat <- seq(1,7,1)
xbeta1w <- rep(0, length(libconhat))
xbeta2w <- rep(0, length(libconhat))
for (i in 1:length(libconhat)) {
xbeta1w[i] <- pe[1]*1 + pe[2]*libconhat[i] + pe[3]*mean(rbdist, na.rm=T) + pe[4]*mean(econ, na.rm=T) + pe[5]*mean(gulfwar, na.rm=T) + pe[6]*0
}

# Perot
libconhat <- seq(1,7,1)
xbeta2w <- rep(0, length(libconhat))
for (i in 1:length(libconhat)) {
xbeta2w[i] <- pe[7]*1 + pe[8]*libconhat[i] + pe[9]*mean(rbdist, na.rm=T) + pe[10]*mean(econ, na.rm=T) + pe[11]*mean(gulfwar, na.rm=T) + pe[12]*0
}


## calculating xbetas for nonwhites
# Clinton
libconhat <- seq(1,7,1)
xbeta1nw <- rep(0, length(libconhat))
xbeta2nw <- rep(0, length(libconhat))
for (i in 1:length(libconhat)) {
xbeta1nw[i] <- pe[1]*1 + pe[2]*libconhat[i] + pe[3]*mean(rbdist, na.rm=T) + pe[4]*mean(econ, na.rm=T) + pe[5]*mean(gulfwar, na.rm=T) + pe[6]*1
}

# Perot
libconhat <- seq(1,7,1)
xbeta2nw <- rep(0, length(libconhat))
for (i in 1:length(libconhat)) {
xbeta2nw[i] <- pe[7]*1 + pe[8]*libconhat[i] + pe[9]*mean(rbdist, na.rm=T) + pe[10]*mean(econ, na.rm=T) + pe[11]*mean(gulfwar, na.rm=T) + pe[12]*1
}


## calculating probabilities for white voters
wbpihat <- 1/(1 + exp(xbeta1w) + exp(xbeta2w))              #bush
wcpihat <- exp(xbeta1w)/(1 + exp(xbeta1w) + exp(xbeta2w))   #clinton
wppihat <- exp(xbeta2w)/(1 + exp(xbeta1w) + exp(xbeta2w))   #perot

## for nonwhite voters
nwbpihat <- 1/(1 + exp(xbeta1nw) + exp(xbeta2nw))               #bush
nwcpihat <- exp(xbeta1nw)/(1 + exp(xbeta1nw) + exp(xbeta2nw))   #clinton
nwppihat <- exp(xbeta2nw)/(1 + exp(xbeta1nw) + exp(xbeta2nw))   #perot

par(mfcol=c(1,2))
#plot the predicted probabilities for white respondents
plot(libconhat, wbpihat, ylim=c(0,.8), pch=15, col="blue", xlab="liberal --> conservative", ylab="probability that respondent votes for . . . ",
         main="votes of white respondents")
lines(libconhat, wbpihat, col="blue")
points(libconhat, wcpihat, pch=16, col="red")
lines(libconhat, wcpihat, col="red")
points(libconhat, wppihat, pch=17, col="green")
lines(libconhat, wppihat, col="green")
legend(x=6,y=.8,bty="n",
       xjust=1,
       legend=c("Bush","Clinton","Perot"),col=c("blue", "red", "green"),pch=c(15, 16, 17))

#plot the predicted probabilities for nonwhite resondents
plot(libconhat, nwbpihat, ylim=c(0,1), pch=15, col="blue", xlab="liberal --> conservative", ylab="probability that respondent votes for . . . ",
         main="votes of nonwhite respondents")
lines(libconhat, nwbpihat, col="blue")
points(libconhat, nwcpihat, pch=16, col="red")
lines(libconhat, nwcpihat, col="red")
points(libconhat, nwppihat, pch=17, col="green")
lines(libconhat, nwppihat, col="green")
legend(x=6,y=1,bty="n",
       xjust=1,
       legend=c("Bush","Clinton","Perot"),col=c("blue", "red", "green"),pch=c(15, 16, 17))


## part (c)
## simulate probabilities for whites
sims <- 1000
simbetas <- mvrnorm(sims, pe, vc)
libconhat <- seq(1, 7, 1)
simy1 <- NULL; simy2 <- NULL; simy3 <- NULL; simx <- NULL
for (i in 1:length(libconhat)) {
    simx0 <- rbind(1, libconhat[i], mean(rbdist, na.rm=T), mean(econ, na.rm=T), mean(gulfwar, na.rm=T), 0)
    simmuden <- (1 + exp(simbetas[,1:6]%*%simx0) + exp(simbetas[,7:12]%*%simx0))
    
    simy0 <- 1/simmuden
    simy1 <- rbind(simy1, simy0)
    
    simy0 <- exp(simbetas[,1:6]%*%simx0)/simmuden
    simy2 <- rbind(simy2, simy0)
    
    simy0 <- exp(simbetas[,7:12]%*%simx0)/simmuden
    simy3 <- rbind(simy3, simy0)
    
    simx <- rbind(simx, t(t(rep(libconhat[i], sims))))
    }
    
## simulate probabilities for nonwhites
sims <- 1000
simbetas <- mvrnorm(sims, pe, vc)
libconhat <- seq(1, 7, 1)
simy1n <- NULL; simy2n <- NULL; simy3n <- NULL; simxn <- NULL
for (i in 1:length(libconhat)) {
    simx0 <- rbind(1, libconhat[i], mean(rbdist, na.rm=T), mean(econ, na.rm=T), mean(gulfwar, na.rm=T), 1)
    simmuden <- (1 + exp(simbetas[,1:6]%*%simx0) + exp(simbetas[,7:12]%*%simx0))
    
    simy0 <- 1/simmuden
    simy1n <- rbind(simy1n, simy0)
    
    simy0 <- exp(simbetas[,1:6]%*%simx0)/simmuden
    simy2n <- rbind(simy2n, simy0)
    
    simy0 <- exp(simbetas[,7:12]%*%simx0)/simmuden
    simy3n <- rbind(simy3n, simy0)
    
    simxn <- rbind(simxn, t(t(rep(libconhat[i], sims))))
    }    
    
    
source("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/chris' code/plot.simulates.r")

# Plot simulated results for white voters (all on same graph - but it's pretty messy)
par(mfrow=c(1,2))
plot.simulates(simx,
               simy1,
               ci.range=c(0.67),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(1,7,0,1),
               closeplot=FALSE,
               collist="light blue"
               )

plot.simulates(simx,
               simy2,
               ci.range=c(0.67),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(1,7,0,1),
               addtoplot=TRUE,
               collist="gray",
               closeplot=FALSE
               )

plot.simulates(simx,
               simy3,
               ci.range=c(0.67),
               ci.mark=c("dashed"),
               thin=0.0,
               usr=c(1,7,0,1),
               lcollist="purple",
               addtoplot=TRUE
               )   
legend(5, .6, bty="n", xjust=1, legend=c("Bush - blue", "Clinton - gray", "Perot - purple"))

# Plot simulated results for nonwhite voters (all on same graph - but it's pretty messy)
plot.simulates(simxn,
               simy1n,
               ci.range=c(0.67),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(1,7,0,1),
               closeplot=FALSE,
               collist="light blue"
               )

plot.simulates(simxn,
               simy2n,
               ci.range=c(0.67),
               ci.mark=c("poly"),
               thin=0.0,
               usr=c(1,7,0,1),
               addtoplot=TRUE,
               collist="gray",
               closeplot=FALSE
               )

plot.simulates(simxn,
               simy3n,
               ci.range=c(0.67),
               ci.mark=c("dashed"),
               thin=0.0,
               usr=c(1,7,0,1),
               lcollist="purple",
               addtoplot=TRUE
               )   
legend(5, .6, bty="n", xjust=1, legend=c("Bush - blue", "Clinton - gray", "Perot - purple"))
