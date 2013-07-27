
# ------------------------------------- #
# Weighted Least Squares
# ------------------------------------- #

#---- Example of estimating sd from the data:

manager=read.table("Manager.txt",header=T)
attach(manager)
manager.lm=lm(Y~X,data=manager)
manager.absres = abs(rstandard(manager.lm))

plot(manager$X,manager$Y, xlab="Number of workers", ylab="Number of managers")
abline(manager.lm)

plot(manager$X,manager.absres)
stdres.fit = lm(manager.absres~manager$X)
abline(stdres.fit)
sthat = fitted(stdres.fit)
manager.lm2 = lm(Y~X,data=manager,weights=1/(sthat^2))

jpeg("manager_fits.jpg",width=600,height=600)
plot(manager$X,manager$Y,pch=15)
abline(manager.lm2,col="red",lwd=2)
abline(manager.lm,col="blue",lwd=2)
dev.off()

#---- Example of a data set with multiple Y's at each X:
X= c(1:10)
sdx = c(6, 8, 4, 6, 3, 4 ,8, 1, 2, 1)
nx = c(20,10,10,15, 12,40,10,15,20,18)
beta = c(1,1,1,1,1,1,1,0.5, 0.5, 0.5)
x=matrix(0,1,0)
y=matrix(0,1,0)
for(i in 1:length(X)){
    y = cbind(y, matrix(rnorm(nx[i],beta[i]*X[i],sdx[i]),1,nx[i]))
    x = cbind(x, matrix(rep(X[i],nx[i]),1,nx[i]))
}
y=t(y)
x=t(x)


# Estimate the variance in each region.
sdhatx = matrix(0,1,length(sdx))
for(i in 1:length(X)){
    sdhatx [i]= sd(y[x==i])
}

w = matrix(0,1,0)
for (i in  1:length(X)){
    w = cbind(w,matrix(rep(sdhatx[i],nx[i]),1,nx[i]))
}

w=1/w
w=t(w)

# fit two models
lm1=lm(y~x)
lm2=lm(y~x,weights=w)



#jpeg("groupedls.jpg",height=600,width=600)
plot(x,y,pch=15)
abline(lm1,col="red",lwd=2)
abline(lm2,col="blue",lwd=2)
#dev.off()


# ------------------------------------- #
# PCA 
# ------------------------------------- #

# European jobs data.

jobs = read.table("EuropeanJobs.txt", header=T,sep="\t")

#jpeg("jobs_pairs.jpg",height=600,width=600)
pairs(jobs,col="blue")
#dev.off()
jobs.dim = dim(jobs)

jobs.pc = princomp(jobs[,2:10])

# Look at what princomp returns:
names(jobs.pc)

# The important parts are:
# sdev[i] is the standard deviation of the data projected on the i-th component.
# loadings[,j] is the j-th principal vector.
# scores[i,j] are the entires in the matrix U of SVD X=UDV

# When you produce plots of projections of the dat, you use scores.
# For example, look at what the first two pc's  give you
#jpeg("jobs_pc12.jpg",height=600,width=1000)
plot(jobs.pc$scores[,1],jobs.pc$scores[,2],col="orange",xlab="principal component 1", ylab="principal component 2",xlim=c(-30,60))

for(i in c(1:jobs.dim[1])){
    text(jobs.pc$scores[i,1],jobs.pc$scores[i,2], jobs$Country[i],cex=1)
}
#Adev.off()
jobs.pc$loadings

# Do scree plot
screeplot(jobs.pc,main="Scree plot for European jobs data")
# PCA is a dimension reduction technique.  You choose to summarize
# the p=9 predictors using a fewer number of dimensions.  How many
# principal components to use?  Look at screeplot to see where the 
# cut off should be.  This can be quite subjective.


# compute the multicollinearity cscore
kappa=jobs.pc$sdev[1]/jobs.pc$sdev[9]

# ----- if you use correlation matrix instead of covariance matrix what do you get? ---

# do pc on correlation matrix
# The R function "scale" is very useful, see ?scale.
jobs.pc2=princomp(scale(jobs[,2:10]))
jobs.pc2$loadings

# Look at what the first two pc's  give you for correlation matrix
plot(-jobs.pc2$scores[,1],jobs.pc2$scores[,2],col="orange",xlab="principal component 1", ylab="principal component 2",xlim=c(-4,7))

for(i in c(1:jobs.dim[1])){
    text(-jobs.pc2$scores[i,1],jobs.pc2$scores[i,2], jobs$Country[i],cex=1)
}

# From this you see that pc is not scale invariant.
