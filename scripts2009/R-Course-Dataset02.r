################################################################################
#
# R-Course: Exercises with data set 2
#
################################################################################


# =======================================
# Part A: Data Manipulation and Graphics:
# =======================================


# Read data set 2:
# ----------------

data02 <- read.table("R-Course-Dataset02.dat",header=T)


# Get size and variable names:
# ----------------------------

dim(data02)
names(data02)


# Produce scatterplot matrix:
# ---------------------------

pairs(data02)


# ===============================
# Part B: Statistical Techniques:
# ===============================


# Read data set 2:
# ----------------

data02 <- read.table("R-Course-Dataset02.dat",header=T)


# Get summary statistics:
# -----------------------

summary(data02)


# Linear regression of y and z against x:
# ---------------------------------------

res.lm.y <- lm(y ~ x,data=data02)
summary(res.lm.y)
par.default <- par(no.readonly=T)
par(mfrow=c(2,1),xaxs="i",yaxs="i")
plot(data02$x,data02$y,main="Fit",xlab="x",ylab="y")
lines(data02$x,predict(res.lm.y))
plot(data02$x,resid(res.lm.y),
     main="Residuals",xlab="x",ylab="y.meas-y.pred")
abline(h=0)
par(par.default)

res.lm.z <- lm(z ~ x,data=data02)
summary(res.lm.z)
par.default <- par(no.readonly=T)
par(mfrow=c(2,1),xaxs="i",yaxs="i")
plot(data02$x,data02$z,main="Fit",xlab="x",ylab="z")
lines(data02$x,predict(res.lm.z))
plot(data02$x,resid(res.lm.z),
     main="Residuals",xlab="x",ylab="z.meas-z.pred")
abline(h=0)
par(par.default)


# Nonlinear regression of y and z against x (in fact, the regression is linear):
# ------------------------------------------------------------------------------

res.nls.y <- nls(y~a+b*x+c*x^2,start=list(a=1,b=1,c=1),data=data02)
summary(res.nls.y)

res.nls.z <- nls(z~a+b*x+c*x^2,start=list(a=1,b=1,c=1),data=data02)
summary(res.nls.z)


# =================================
# Part C: Packages and Programming:
# =================================


# Read data set 2:
# ----------------

data02 <- read.table("R-Course-Dataset02.dat",header=T)


# Load "Explore"-library:
# -----------------------

source("explore.r")


# Produce scatterplot matrix with linear and nonparametric fit:
# -------------------------------------------------------------

pairs.lin.loess(data02)


# Search for best polynomial model in x for y and z (up to degree 3):
# -------------------------------------------------------------------

explore.lin(data     = data.frame(y  = data02$y,
                                  x  = data02$x,
                                  x2 = data02$x^2,
                                  x3 = data02$x^3),
            name.dep = "y",
            levels   = c(1,2,3))
explore.lin(data     = data.frame(z  = data02$z,
                                  x  = data02$x,
                                  x2 = data02$x^2,
                                  x3 = data02$x^3),
            name.dep = "z",
            levels   = c(1,2,3))
            

