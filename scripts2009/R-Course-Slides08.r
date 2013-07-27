################################################################################
# Create Data Set
################################################################################

# Create data set 1:
# ------------------

dim      <- 3
corr     <- 0.4
sampsize <- 1000
mean     <- 1:dim
sd       <- rep(1,dim)
R <- diag(1,dim)
for ( i in 1:(dim-1) )
{
   for ( j in (i+1):dim )
   {
      s <- 1;    if ( as.integer((i+j)/2)*2 == i+j ) s <- -1
      c <- corr; if ( as.integer((i+j)/3)*3 == i+j ) c <- 0
      R[i,j] <- s*c
      R[j,i] <- R[i,j]
   }
}
samp <- runif(sampsize*dim); dim(samp) <- c(sampsize,dim)
sigma <- diag(sd) %*% R %*% diag(sd)
sigma.inv = solve(sigma)
det.sigma = det(sigma)
A <- t(chol(sigma))
data01 <- matrix(nrow=sampsize,ncol=dim)
for ( i in 1:sampsize )
{
   data01[i,]  <- A %*% qnorm(samp[i,]) + mean
}
write.table(data01,"R-Course-Dataset01.dat",col.names=T,row.names=F,sep="\t")
save(data01,file="R-Course-Dataset01.RData")

# Create data set 2:
# ------------------

len <- 101
x = seq(0,100,length=len)
y <- 5.0 + 0.100*x+rnorm(len,0,1)
z <- 0.001*x^2 + rnorm(len,0,1)
data02 <- data.frame(x=x,y=y,z=z)
write.table(data02,"R-Course-Dataset02.dat",col.names=T,row.names=F,sep="\t")
save(data02,file="R-Course-Dataset02.RData")

# Data set 3 consists of NADUF data
# ---------------------------------


################################################################################
# Examples
################################################################################


# =================
# Data Manipulation
# =================

# Variables
# ---------

a <- 2; print(a); a; ls(); rm(list=ls())

a <- 2; b <- 3.78; c <- 1.2 + 3
d <- TRUE; e <- F; f <- 5>3
g <- "blue"; h <- "C:/Documents and Settings"
i <- 1 + 1i; j <- 1 + 5*1i
k <- factor(c("a","b","c"))

a <- c(1,3,7.5); b <-c("a","b","c"); c <- c(x=5,y=3)
names(a) <- c("x","y","z"); names(a)
d <- 0:10; e <- seq(0,10,by=2); f <- rep(0,5)
g <- c(0:10,20); g[5] <- 99
length(a); a[2]; a["y"]; b[1]; c["y"]

a <- matrix(0,nrow=3,ncol=4); b <- diag(rep(1,5))
c <- matrix(1:16,nrow=4); c
d <- matrix(1:16,nrow=4,byrow=T); d
rownames(d) <- 1:4; colnames(d) <- c("A","B","C","D")
d; rownames(d); d[1,2]; d["1","B"]; d[1,]; d[1,][2]

a <- list(1,1:3,c("a","b","c"))
b <- list(a=1,b=1:3,c=c("a","b","c"))
a; b; a[[2]]; b[[3]]; b[["c"]]; b$c
names(b); names(b) <- c("A","B","C"); b
b[[3]][2]; b$C[2]

a <- data.frame(a=rep(1,3),b=1:3,c=c("a","b","c"))
a; a$b; a[[2]]; a[["b"]]
a$b[2]; a[[2]][2]
names(a); names(a) <- c("A","B","C"); a

# Missing Values
# --------------

a <- NA; is.na(a)
b <- c(1,2,NA); mean(b); mean(b,na.rm=T)

# Data Selection
# --------------

a <- matrix(1:50,nrow=10,ncol=5)
colnames(a) <- c("A","B","C","D","E")
a[,c("B","C")]
a[1:3,c("A","C")]
a[1:5,c(F,T,T,F,F)]
a[a[,"A"]>5,]; a[,-c(1:2)]
b <- c(1,2,3,4,5); c <- c(5,4,3,2,1)
ifelse(b>c,b,c)

# Operations
# ----------

a <- 1:10; b <- rep(5,10)
a > b; a==b; a!=b; a<=b; a < b | a==b
a <- matrix(c(1,2,3,4),nrow=2); a %*% solve(a)
a <- paste("a","b",sep="."); substr(a,1,1); strsplit(a,"\\.")

# Searching
# ---------

a <- c("A","B","A","A","B")
match("B",a)
grep("B",a)
gsub("B","xxx",a)
unique(a)
b <- as.factor(a); levels(b)

# Data Sorting
# ------------

a <- c(1,5,3,7,9,8,2,6,4,10)
rev(a)
sort(a)
ind <- order(a); a[ind]

# Data Combination
# ----------------

a <- 1:10; b <- rep(5,10); c <- matrix(1:20,ncol=2)
d <- c(a,b); e <- rbind(c,c); f <- cbind(c,a,b)

# Data Aggregation
# ----------------

a <- data.frame(category=c("A","A","B","B","A","B"),
                x       = 1:6,
                y       = 2:7,
                z       = rep(2,6))
apply(a[,-1],2,sum)
tapply(a$x,a$category,mean)
aggregate(a[,-1],list(a[,1]),mean)

# Flow Control
# ------------

for(i in 1:10) { print(i); print(sqrt(i)) }

# Dates
# -----

a <- as.Date("31.01.2006",format="%d.%m.%Y") + 1
a; format(a,"%d.%m.%Y"); as.numeric(a)
b <- as.POSIXct("06.11.2008 09:00",format="%d.%m.%Y %H:%M",tz="CET")
c <- as.POSIXct("06.11.2008 15:30",format="%d.%m.%Y %H:%M",tz="GMT")
format(c,tz="CET")
difftime(c,b)

# Working Directory
# -----------------

#setwd("C:/reichert/R-course/theory")
#setwd("../examples")

# Data Import
# -----------

a <- read.table("R-Course-Dataset01.dat",header=T)
b <- scan("R-Course-Dataset01.dat",what=list(0),skip=1)
load(file="R-Course-Dataset01.RData")

# Data Export
# -----------

write.table(a,"test.dat",col.names=T,row.names=F)
save(a,file="test.Rdata")


# ========
# Graphics
# ========


x <- c(0,1,2,3,4,5,6,7,8,9,10)
y <- c(3,5,2,6,4,2,7,4,3,3,4)
plot(x,y)
hist(y)
boxplot(y)
pairs(cbind(x,y))


x <- c(0,1,2,3,4,5,6,7,8,9,10)
y <- c(3,5,2,6,4,2,7,4,3,3,4)
plot(x,y,pch=1)
lines(c(0,10),c(mean(y),mean(y)),lty="solid")
legend("topright",legend=c("data","mean"),
       lty=c(NA,"solid"),pch=c(1,NA))


x <- c(0,1,2,3,4,5,6,7,8,9,10)
y <- c(3,5,2,6,4,2,7,4,3,3,4)
par.default <- par(no.readonly=T)
par(mfrow=c(2,2))
plot(x,y)
plot(x,y,type="l")
points(x,y,col=ifelse(y>=5, "red", "black"),pch= ifelse(y<=2 | y>=5,19,1)) 
hist(y)
boxplot(y)
par(par.default)


x <- c(0,1,2,3,4,5,6,7,8,9,10)
y <- c(3,5,2,6,4,2,7,4,3,3,4)
pdf("test.pdf")
plot(x,y)
dev.off()


# ======================
# Statistical Techniques
# ======================


y <- c(3,5,2,6,4,2,7,4,3,3,4)
summary(y); range(y); mean(y); sd(y); var(y)
quantile(y,seq(0,1,by=0.05))


x <- rnorm(1000,0,1); hist(x,freq=F)
lines(seq(-3,3,by=0.1),dnorm(seq(-3,3,by=0.1),0,1))


x <- c(0,1,2,3,4,5,6,7,8,9,10)
y <- c(3,5,2,6,4,2,7,4,3,3,4)
res.lm <- lm(y ~ x); summary(res.lm)
residuals(res.lm)
predict(res.lm,interval="confidence")
coefficients(res.lm)
summary(res.lm)$coefficients


# ================
# Programming in R
# ================


square <- function(x) { return(x*x) }
a <- 1:10; square(a)
topower <- function(x,y=2)
{
   z <- x^y
   return(z)
}
topower(2,3); topower(y=3,x=2); topower(2)

