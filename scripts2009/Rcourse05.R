##
## Rcourse05.R
##
## Demonstration code for "Modern Applied Statistics Using R"
##
## Alexander.Ploner@ki.se 2007-10-08
##
##########################################################################

## Solution

##---------------- Bioconductor ----------------------------##

# This is a short demonstration of some basic capabilities of Bioconductor
# in regard to gene expression microarrays. It also establishes the objects
# ALL.exp and ALL.phe that we will use below; if you do not want to or cannot
# run this section on your machine, you can find the objects in the file 
# simpleALL.RData at the course website

# Parts of this section have been commented out to protect the innocent
# Proceed with care

# Installation; depending on the speed of your connection, this may take a bit
#source("http://bioconductor.org/biocLite.R") ## get the installation script
#biocLite()                                   ## install base packages
#biocLite("ALL")                              ## add the ALL data package

# Load the example data set; is of class exprSet
require(ALL)
data(ALL)
class(ALL)
# Short summary
ALL
# ExprSet have information on expression and phenotypic data
exprs(ALL)
pData(ALL)
# ExprSet allow easy extraction of samples and genes
ALL[,1:10] ## First ten patients
ALL[1:10,] ## First ten genes
ALL[1:10, 1:10]
# ExprSet have abundant additional information
description(ALL)
abstract(ALL)

# Extra annotation for ALL: Affymetrix chip hgu95av2
require(hgu95av2)
ls(2)
# Names of first ten genes
featureNames(ALL)[1:10]
# Get symbol names
mget(featureNames(ALL)[1:10], hgu95av2SYMBOL)
# Chromosomal position
mget(featureNames(ALL)[1:10], hgu95av2CHRLOC)

# Some more annotation: plotting the distribution of gene reporters
# on hgu95av2 chips across the chromosome
require(geneplotter)
example(cColor)
# Retrieve the pubmed abstracts associated with the ALL data set
require(annotate)
# This is commented out because...
# REPEATED/AUTOMATED ACCESS (<2s) TO PUBMED CAN GET YOU BANNED!!!
## pubmed(pubMedIds(ALL), disp="browser")

# Create the simple R objects for our convenience
ALL.exp = exprs(ALL)
ALL.phe = pData(ALL)
save(ALL.exp, ALL.phe, file="~/simpleALL.RData") # NB, to home directory ~


##-------------------- Clustering -------------------------------##

# Attach the prepared data set to the search path
attach("~/simpleALL.RData")

# Basic distances and clustering
ALL.dist = dist(t(ALL.exp))
ALL.clus1 = hclust(ALL.dist)
ALL.clus1
plot(ALL.clus1, ann=FALSE, cex=0.8)
# Extract groups
grp1 = cutree(ALL.clus1, k=2)   # two clusters
grp1 = cutree(ALL.clus1, h=110) # equivalent: at d=110
table(grp1)
table(grp1, ALL.phe$BT)

# Vary the clustering procedure
ALL.clus2 = hclust(ALL.dist, method="single")
ALL.clus3 = hclust(ALL.dist, method="average")
ALL.clus4 = hclust(ALL.dist, method="ward")
# Plot it
par(mfrow=c(2,2), mar=c(1,2,3,1))
plot(ALL.clus1, main="Complete linkage", sub="", xlab="", ylab="",labels=FALSE)
plot(ALL.clus2, main="Single linkage", sub="", xlab="", ylab="",labels=FALSE)
plot(ALL.clus3, main="Average linkage", sub="", xlab="", ylab="",labels=FALSE)
plot(ALL.clus4, main="Ward's method", sub="", xlab="", ylab="",labels=FALSE)

# Now vary the distance
ALL.dist2 = dist(t(scale(ALL.exp)))
ALL.dist3 = dist(t(ALL.exp), "manhattan")
ALL.dist4 = as.dist(1 - cor(ALL.exp))
# Default clustering
ALL.clus2 = hclust(ALL.dist2)
ALL.clus3 = hclust(ALL.dist3)
ALL.clus4 = hclust(ALL.dist4)
# Plot
par(mfrow=c(2,2), mar=c(1,2,3,1))
plot(ALL.clus1, main="Euclidean", sub="", xlab="", ylab="",labels=FALSE)
plot(ALL.clus2, main="Euclidean/scaled", sub="", xlab="", ylab="",labels=FALSE)
plot(ALL.clus3, main="Manhattan", sub="", xlab="", ylab="",labels=FALSE)
plot(ALL.clus4, main="Correlation", sub="", xlab="", ylab="",labels=FALSE)

## Now partitioning methods

# Kmeans
ALL.kmeans = kmeans(t(ALL.exp), 2) # only Euclidean distance
ALL.kmeans
names(ALL.kmeans)
# Almost the same as before
table(ALL.kmeans$cluster, grp1)

# Partioning around medoids
require(cluster)
ALL.pam = pam(ALL.dist, k=2) # any distance
ALL.pam
# A silhouette plot: not so great quality overall
# Three misclassifications (negative)
plot(silhouette(ALL.pam), col="red")
# Quite different
table(ALL.pam$cluster, grp1)
# Identifies T-cells
table(ALL.pam$cluster, ALL.phe$BT)

# Show approximation to distance matrix, with different grouping
# clusplot is part of package cluster
par(mfrow=c(1,2))
clusplot(ALL.dist, ALL.kmeans$cluster, diss=TRUE) 
clusplot(ALL.dist, ALL.pam$cluster, diss=TRUE) 



##-------------------- Prediction -------------------------------##

# Select B cells that are classified as either BCR/ABL or NEG
ndx = substr(ALL.phe$BT, 1, 1) =="B" & ALL.phe$mol.bio %in% c("BCR/ABL","NEG")
Exp = ALL.exp[,ndx]
Phe = ALL.phe[ndx, ]
mol = factor(Phe$mol.bio)

# Split into training and test set
set.seed(1220)
ndxTr = sort(sample(ncol(Exp), size=ncol(Exp)*2/3))
ndxTe = setdiff(1:ncol(Exp), ndxTr)
Exp_tr = Exp[,ndxTr]
Exp_te = Exp[,ndxTe]
mol_tr = mol[ndxTr]
mol_te = mol[ndxTe]
# Are these balanced? Not really
# This could be improved using balanced sampling
table(mol_tr) 
table(mol_te)

# Show the geometry of knn using two specific sequences
xy = t(Exp_tr[c("41457_at","649_s_at"),])
gn = 100
xx = seq(min(xy[,1]), max(xy[,1]), length=gn)
yy = seq(min(xy[,2]), max(xy[,2]), length=gn)
# We define a 100x100 grid
ta = expand.grid(xx, yy)
require(class)
# Prediction on the grid
pp = knn(xy, ta, mol_tr, k=1)
# Plot the data
plot(xy, pch=tolower(substr(mol_tr, 1, 1)))
# Add the predictions as color; by having add=TRUE and specifying 
# transparent colors shows both predictions and data
image(xx, yy, matrix(as.numeric(pp), nrow=gn), add=TRUE, 
      col=c(rgb(0,0,0,0.4), rgb(1,0,0,0.4)))


# Compute t-statistics
# This is very slow; Bioconductor has the nice genefilter package
trTstat = apply(Exp_tr, 1, function(x) t.test(x~mol_tr)$statistic)
# 200 absolutely largest t-statistics
ndxFe = rev(order(abs(trTstat)))[1:200]
# Let's look at them
heatmap(Exp_tr[ndxFe,])

# Function that does leave-1-out for specified data, parameter of k
l1o.err = function(data, grp, k)
{
    n = length(grp)
    pred = grp
    for (i in 1:n) {
        pred[i] = knn(data[-i,], data[i,], grp[-i], k=k)
    }
    tab = table(grp, pred)
    1-sum(diag(tab))/sum(tab)
}
# Now run this for several values; seems like k=1 is best
l1o.err(t(Exp_tr[ndxFe,]), mol_tr, k=1)
l1o.err(t(Exp_tr[ndxFe,]), mol_tr, k=3)
l1o.err(t(Exp_tr[ndxFe,]), mol_tr, k=5)
# How does this performon the test data?
pred = knn(t(Exp_tr[ndxFe,]), t(Exp_te[ndxFe,]), cl=mol_tr, k=1)
table(pred, mol_te)

# A more systematice exploration here
kk = 1:30
ee = rep(NA, 30)
for (i in 1:30) {
    ee[i] = l1o.err(t(Exp_tr[ndxFe,]), mol_tr, k=kk[i])
}
plot(kk, ee, type="l")



