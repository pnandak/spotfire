# Gov PS 2 Solution Code
# Jens Hainmueller

# Problem 2
rm(list=ls()) # clear workspace
house <- read.csv("ps2data.csv")
library(MASS)
library(Hmisc)


# load data
#house <-read.csv(header = TRUE,
#file = url("http://www.people.fas.harvard.edu/~jhainm/housedems06.csv"))

# create log var
house$logreceipts <- log(house$receipts)

# obtain true population parameters
truth <- apply(house[,2:3],2,mean)


# Part A define estimators
sample.mean <-
 function(x){
 est.out <- sum(x)/length(x)
 return(est.out)
}

sample.median <-
 function(x){
 p <- trunc((length(x)+1)*0.5)
 est.out <- sum(sort(x)[c(p,length(x)+1-p)])*0.5
 return(est.out)
}

deletion.estimator.1 <-
 function(x){
 est.out <- sum(x[x!=min(x)])/(length(x)-1)
 return(est.out)
}

deletion.estimator.2 <-
 function(x){
 #est.out <- sum(x[x!=min(x) & x!=max(x)])/length(x[x!=min(x) & x!=max(x)])
 est.out <- sum(sort(x)[-c(1,2,length(x)-1,length(x))])/(length(x)-4)
 return(est.out)
}

# evaluate estimators for toy data
x <- c(1, 2, 2, 3, 3, 4, 100)
print(c(sample.mean(x),sample.median(x),deletion.estimator.1(x),deletion.estimator.2(x)))

# Part B1
set.seed(1234)

# set sample size and no of simulations
m <- 10000
n <- 100

# set up empty matyrix to store the samples
store <- matrix(NA,m,n)

# draw random samples and store them)
for(i in 1:m){
 store[i,] <- sample(house$logreceipts,size=n,replace=FALSE)
}

# get the sampling distributions by for all four estimators
all.at.once <- function(x){
  out <- c(sample.mean(x),sample.median(x),deletion.estimator.1(x),deletion.estimator.2(x))
}

samp.dist <- apply(store,1,all.at.once)
rownames(samp.dist) <- c("sample.mean","sample.median","deletion.estimator.1","deletion.estimator.2")

boundary <- c(13.55,13.85)

# plot them
pdf("plot_log_100.pdf")
par(mfrow=c(2,2))
for(i in rownames(samp.dist)){
truehist(samp.dist[i,],main=i,xlim=boundary,xlab="log(receipts)",prob=T)
abline(v=truth[2],col="red")
}
dev.off()

# produce summary table
all.at.once.2 <- function(x){
  out <- c(mean(x),sd(x))
}

# table 1
tab1 <- apply(samp.dist,1,all.at.once.2)
rownames(tab1) <- c("Mean","SD")
tab1

# Part B2

# n = 40, 15, 5

tab2 <- matrix(NA,0,4)
nn <- c(40,15,7)
for(i in nn){
  sub.store <- store[,1:i]
  samp.dist <- apply(sub.store,1,all.at.once)
  rownames(samp.dist) <- c("sample.mean","sample.median","deletion.estimator.1","deletion.estimator.2")

  pdf(file=paste("plot_log",i,".pdf",sep=""))
  par(mfrow=c(2,2))
   for(i in rownames(samp.dist)){
   truehist(samp.dist[i,],xlim=boundary,main=i,xlab="log(receipts)",prob=T)
   abline(v=truth[2],col="red")
   }
  dev.off()

  tab <- apply(samp.dist,1,all.at.once.2)
  rownames(tab) <- c("Mean","SD")
  tab2 <- rbind(tab2,tab)
}

round(tab2,3)

# Part C

# set up empty matyrix to store the samples
store <- matrix(NA,m,n)

# draw random samples and store them)
for(i in 1:m){
 store[i,] <- sample(house$receipts,size=n,replace=FALSE)
}

# apply estimators to get sampling distribution
samp.dist <- apply(store,1,all.at.once)
rownames(samp.dist) <- c("sample.mean","sample.median","deletion.estimator.1","deletion.estimator.2")

boundary <- c(850000,1250000)

# plot them
pdf("plot_nolog_100.pdf")
par(mfrow=c(2,2))
for(i in rownames(samp.dist)){
truehist(samp.dist[i,],xlim=boundary,main=i,xlab="receipts",prob=T)
abline(v=truth[1],col="red")
}
dev.off()

# table 3
tab3 <- apply(samp.dist,1,all.at.once.2)
rownames(tab3) <- c("Mean","SD")
round(tab3,3)

# Part C2

# n = 40, 15, 7
tab4 <- matrix(NA,0,4)
nn <- c(40,15,5)
for(i in nn){
  sub.store <- store[,1:i]
  samp.dist <- apply(sub.store,1,all.at.once)
  rownames(samp.dist) <- c("sample.mean","sample.median","deletion.estimator.1","deletion.estimator.2")

  pdf(file=paste("plot_nolog",i,".pdf",sep=""))
  par(mfrow=c(2,2))
   for(i in rownames(samp.dist)){
   truehist(samp.dist[i,],xlim=boundary,main=i,xlab="receipts",prob=T)
   abline(v=truth[1],col="red")
   }
  dev.off()

  tab <- apply(samp.dist,1,all.at.once.2)
  rownames(tab) <- c("Mean","SD")
  tab4 <- rbind(tab4,tab)
}

round(tab4,3)


# Part E MSE

# set sample size and no of simulations
m <- 10000
n <- 100

# set up empty matyrix to store the samples
store <- matrix(NA,m,n)

# draw random samples and store them)
for(i in 1:m){
 store[i,] <- sample(house$logreceipts,size=n,replace=FALSE)
}

# get the sampling distributions by for all four estimators
all.at.once <- function(x){
  out <- c(sample.mean(x),sample.median(x),deletion.estimator.1(x),deletion.estimator.2(x))
}

samp.dist <- apply(store,1,all.at.once)
rownames(samp.dist) <- c("sample.mean","sample.median","deletion.estimator.1","deletion.estimator.2")

# MSE
apply((samp.dist-truth[2])^2,1,mean)

latex(apply((samp.dist-truth[2])^2,1,mean),file="")
