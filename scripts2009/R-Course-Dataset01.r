################################################################################
#
# R-Course: Exercises with data set 1
#
################################################################################


# =======================================
# Part A: Data Manipulation and Graphics:
# =======================================


# Read data set 1:
# ----------------

data01 <- read.table("R-Course-Dataset01.dat",header=T)


# Get size and variable names:
# ----------------------------

dim(data01)
names(data01)


# Produce scatterplot matrix:
# ---------------------------

pairs(data01)


# Produce histograms and box plot:
# --------------------------------

par.default <- par(no.readonly=T)
par(mfrow=c(2,2),xaxs="i",yaxs="i")
hist(data01[,1],main=names(data01)[1],xlab=names(data01)[1])
hist(data01[,2],main=names(data01)[2],xlab=names(data01)[2])
hist(data01[,3],main=names(data01)[3],xlab=names(data01)[3])
boxplot(data01)
par(par.default)


# ===============================
# Part B: Statistical Techniques:
# ===============================


# Read data set 1:
# ----------------

data01 <- read.table("R-Course-Dataset01.dat",header=T)


# Get summary statistics:
# -----------------------

summary(data01)


# Calculate correlation matrix:
# -----------------------------

cor(data01)


