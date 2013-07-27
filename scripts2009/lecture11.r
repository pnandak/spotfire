
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


