
# elementary data structures and operations:
# ==========================================

# scalar variables:
# -----------------
a <- 2
print(a)
a
b <- 3
b
c <- a*b
c

# sequences / vectors:
# --------------------
1:5
x <- 1:5
x
length(x)
5*x
y <- c(3,8,9,100,200)
y
help(c)
names(y) <- c("a","b","c","d","e")
y
y[1:3]
y[c(1,4)]
y["c"]
x*y
sum(x*y)
t(x) %*% y
help("%*%")
help(t)
x %*% t(y)
t <- seq(0,0.2,length=21)
t
help(seq)


# matrices:
# ---------
x <- 1:5
as.matrix(x)
as.matrix(t(x))
m <- matrix(nrow=10,ncol=2)
print(m)
m
help(matrix)
nrow(m)
ncol(m)
dim(m)
m[,1] <- 1:10
m
m[,2] <- c(1,1,4,6,4,6,8,10,9,9)
m
plot(m[,1],m[,2])
colnames(m) <- c("x","y")
rownames(m) <- paste("row",1:10,sep=".")
m
m[3,2]
m[1:5,]
m[,2]
m[6:10,"y"]
A <- matrix(c(1:8,10),nrow=3,ncol=3)
help(solve)
B <- solve(A)
A*B
A%*%B
is.matrix(A)
is.matrix(x)


# data frames:
# ------------
A <- matrix(c(1:8,10),nrow=3,ncol=3)
A
a <- data.frame(A)
a
names(a) <- c("x","y","z")
a
a$y
a[,2]
a[,"y"]
is.data.frame(a)
is.data.frame(A)
str(a)

# lists (combinations of elements of different type):
# ---------------------------------------------------
x <- list(a=1,b=1:10,c=matrix(1:9,nrow=3,ncol=3))
x
help(list)
names(x)
x$a
x[["b"]]
x[[3]]
x[[3]][,1]

# elementary statistics functions:
# --------------------------------
A <- matrix(c(1:8,10),nrow=3,ncol=3)
a <- data.frame(A)
mean(A)
mean(a)
sd(a)
summary(a)
cor(a)

# definition of functions:
# ------------------------
square <- function(x)
{
   return(x*x)
}
a <- 1:10
square(a)

help("function")

topower <- function(x,y=2)
{
   z <- x^y
   return(z)
}
topower(2,3)
topower(y=3,x=2)
topower(2)

# plotting
# --------
A <- matrix(nrow=20,ncol=3)
A[,1] <- 1:20
A[,2] <- A[,1] + rnorm(20,sd=1)
A[,3] <- 21 - A[,1] + rnorm(20,sd=5)
a <- data.frame(A)
names(a) <- c("x","y","z")
pairs(a)
help(plot)
help(par)
plot(a$x,a$y)
plot(a$x,a$z,xlab="x",ylab="z",main="z(x)",xlim=c(0,20),ylim=c(0,1.1*max(a$z)))
par(mfrow=c(1,2),xaxs="i",yaxs="i",mar=c(4.5,4.5,2,1.5)+0.1)
for ( i in 2:3 )
{
   plot(numeric(0),numeric(0),xlim=c(0,max(a$x)),ylim=c(0,1.1*max(a[,i])),
        main=paste(names(a)[i],"(x)",sep=""),
        xlab=names(a)[1],ylab=names(a)[i],type="n")
   lines(a$x,a[,i])
   points(a$x,a[,i])
}


# miscellaneous:
# --------------
ls()
rm(list=ls())
ls()

