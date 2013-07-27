## Solutions to Homework 1
## Problem 1 

# 1A
mammals <- read.table("mammals.txt", header=T, row.names=1, sep="\t")
# 1B
class(mammals)

# 1C 
mammals.mat <- as.matrix(mammals)

# 1D: mean body size = 199kg, mean brain size = 283g. 
apply(mammals.mat, 2, mean, na.rm=T)

# 1E 
rm(mammals)

# 1F
# method 1 
calcBBR <- function(x){ 
		bbr <- x[,2]/x[,1]
		return(bbr)
}
brain.to.body <- calcBBR(mammals.mat)

# method 2
calcBBR <- function(x,y){ 
		bbr <- x/y 
		return(bbr)
}
brain <- mammals.mat[,2] 
body <- mammals.mat[,1] 
brain.to.body <- calcBBR(brain, body)
brain.to.body <- calcBBR(mammals.mals[,2], mammals.mat[,1])

# method 3
calcBBR <- function(x,y){ 
		bbr <- x/y
		return(bbr)
}

brain.to.body <- NULL 
for( i in 1:nrow(mammals.mat) ){
	b <- calcBBR(mammals.mat[i,2], mammals.mat[i,1])
	brain.to.body <- c(brain.to.body, b)
}
brain.to.body <- apply(mammals.mat, 1, calcBBR)

# 1G
sort(brain.to.body) 

# Problem 2
rep(0:4, each=5)
rep(1:5, 5)

# Problem 3 
for( i in 0:10){ print(2^i) }
