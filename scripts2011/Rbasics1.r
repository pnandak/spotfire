## have to install first: there are newer versions, I am working with 2.7.2.
## http://www.stats.bris.ac.uk/R/

## get an editor to write R codes: winedt is one option, not free though: http://www.winedt.com/

## then use RWinEdt package in R: we can install from online or go local: http://cran.r-project.org/contrib/extra/winedt/

## hope there is no trouble to get things working. 


## R basic commands ##
## everything before regression analysis. 
## Tuesday, 7.July.2009. 


# 1. some very basic stuff: 
# An introduction to R: http://cran.r-project.org/doc/manuals/R-intro.pdf
# 1.a. but basically about defining variable: scalar, vector, matrix, array etc ...
a<-1.5
x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
y <- c(x, 0, x)
y*a

a<-seq(-5, 5, by=.2)
a

b <- rep(x, times=5)
b

# character, factor, numeric values:
labs <- paste(c("X","Y"), 1:10, sep="")
labs

as.character(b)
as.factor(b)
as.numeric(as.factor(b))

# add a missing value to x:
X<-c(x, NA)

# selecting and modifying subsets of a data set
x2<-X[!is.na(X)] # get rid of NA value here;
x3<-x2[x2>=10]
x3<-X[!is.na(X) & X>=10]
x3 > 10



# define a matrix:
matrix(c(1:12), ncol=3, nrow=4, byrow=TRUE) # see how 1:12 are put into the matrix, it is "by column" as default. 
M<-matrix(c(1:12), ncol=3, nrow=4)
M[,1]
M[,1]<-as.character(M[,1])
M # so all the elements in matrix have to be the same nature: character, numeric, factor ...

M<-as.data.frame(matrix(c(1:12), ncol=3, nrow=4))
M[,1]<-as.character(M[,1])
M
is.numeric(M[1,2])
is.character(M[1,1]) # so for data frame, columns can be of different nature. 


# some probability function: the ones we often use ...
# rnorm()
rnorm(100)
plot(density(rnorm(100)))

par(mfrow=c(2,2))
plot(density(rnorm(10)))
plot(density(rnorm(1000)))
plot(density(rnorm(100, 0, 10)))
plot(density(rnorm(100, 0, 100)))


?rbinom()
rbinom(100, 10, .5)

rpois(100, exp(.5+rnorm(1))) 

# also want to draw random number between 1 and 10
runif(1, 1, 10)




# 1.b. then some very simple function: mean, var, median, min, max, quantile, sum 
# we will use the data from POLS 501:


mulsel87<-read.table("http://privatewww.essex.ac.uk/~caox/teaching/Day%201/mulsel87.asc",header=TRUE)


dim(mulsel87) # take a look of the dimensions of the data
mulsel87[1:10, ]
attach(mulsel87) # set the dataset as the default to save sometime for typing;
                 # this might cause trouble when working with multiple data set;
                 
# just take a look at one independent variable:
mulsel87$dpm70ln; 
par(mfrow=c(1,1))
plot(density(dpm70ln, na.rm=T), ylab="", main="")
mtext("dpm70ln: deaths per million from political violence 1968-1972",side=3)


mean(dpm70ln)
# well, R refuses to calculate, b/c there are missing values; try to ignore them by "na.rm=T":
mean(dpm70ln, na.rm=T)

# but is it always OK to just ignore the missing data?
# maybe we should fill the vacancy by some arbitrary but reasonable values,
# say, the mean or...

median(dpm70ln, na.rm=T); var(dpm70ln, na.rm=T) # calculate the median and variance of the vector
max(dpm70ln, na.rm=T); min(dpm70ln, na.rm=T)    # calculate the maximum and minimum of the vector;
summary(dpm70ln)                                # we could also use summary() as well.

# sometimes, we want to know, say 95% or 90% quantile, then we use quantile():
quantile(dpm70ln, na.rm=T, c(.025, .975)) # gives you a 95% quantile; 

objects() # see the variables defined;
remove(a) # remove the variable;
objects()



# 1.c. then how to read in and save the data: dput, dump, write.table etc ...
# there are different ways to read in data and this depends on the type of data that need to import
# however, once we've fixed one data set and we can simply save it as the way it is in R by "dput" or "dump"
M
dump("M", "Msaved") # this goes into the working directory.
m<-dget("Msaved")   # we can get it back next time we want to use it;
dput(m, "Msaved")   # slightly different syntax for dput;
m<-dget("Msaved")



# 1.d. for and if function, we use these very often when manipulating the data. 
rpl<-mean(dpm70ln, na.rm=T)
for (i in 1:137){if (is.na(dpm70ln[i])){dpm70ln[i]<-rpl}
                             else {}
                             }
is.na(dpm70ln)

dpm70ln[is.na(dpm70ln)]<-rpl


# continue with the same data:
par(mfrow=c(2,2))
plot(mulsel87$dpm70,mulsel87$t20apsr)
# not very helpful graphically. So try some transformation of the data:

plot(mulsel87$dpm70ln,mulsel87$t20apsr)
hist(mulsel87$dpm70ln); 
plot(density(dpm70ln, na.rm=T))


# the plots, especially the second one, show that there are some extreme cases,
# or outliers; most of time, we are very interested in who they are;
# we might want to run and compare some regression analyseses with and without them:

# first, let's find them:
par(mfrow=c(1,1)) # plot a big picture;
plot(dpm70ln,t20apsr, main="Find the Outliers", xlab="dpm70ln: Death per million (log), 1968-72", ylab="t20apsr: Wealth Share of top 20%")
identify(dpm70ln,t20apsr,rownames(mulsel87))

rownames(mulsel87)[c(98,120,123)] # notice here that dpm70ln[c(98, 120, 123)]
abline(lm(t20apsr~dpm70ln), col="blue",lwd=2)
summary(lm(t20apsr~dpm70ln))

abline(lm(t20apsr[-98][-119][-121]~dpm70ln[-98][-119][-121]), col="red", lwd=2)
lm(t20apsr[-98][-119][-121]~dpm70ln[-98][-119][-121])






###################### stop here ###################################
#### if we have time we can play around with some simulation stuff: 
#### otherwise, feel free to play with after class. 

# Multilevel sampling: simulation example
# Two stages: 
# Normal(2,5) for macro units "a"
# Normal(a,1) for micro units "b"

#
# (A) sampling 3 macro units and 10 macro units
#

nmacro <- 3
a <- rnorm(nmacro, mean=2, sd=5)

#sampling 10 micro units from each macro unit
nmicro <- 10
b <- matrix(0,nrow=nmacro,ncol=nmicro)
for (i in 1:nmacro)
    { b[i,] <- rnorm(nmicro, mean=a[i], sd=1) }

# check the simulations by typing
round(b,2)
round(a,2)

#find the mean and standard deviation of samples in b
round(mean(b),2)
round(sd(as.vector(b)),2)

#
# (B) repeat similar simulations with 10 macro and 3 micro units
#

nmacro <- 10
a <- rnorm(nmacro, mean=2, sd=5)
nmicro <- 3
b <- matrix(0,nrow=nmacro,ncol=nmicro)
for (i in 1:nmacro)
    { b[i,] <- rnorm(nmicro, mean=a[i], sd=1) }

# find the mean and standard deviation of samples in b
round(mean(b),2)
round(sd(as.vector(b)),2)

# Repeat (A) and (B) sampling sevaral time to get an idea about variability
# What can we say about estimated means and standard errors?  



### simulation: not from regression parameters though ###

# Given a a group of 18 randomly selected folks, how likely is it that at least two have the same birthday?
# simulation to estimate the probability of having two identical birthdays
# in a set of n.folks individuals
# assumes that birthdays are distributed uniformly across year.
n.sims<- 100
n.folks<-18
days<-seq(1,365,1)
sameday<-0
for (i in 1:n.sims){
    room<-sample(days, n.folks,replace=T)
    if(length(unique(room))<n.folks)
    sameday<-sameday+1
    }
cat("Prob of at least 2 folks with same birthday:",sameday/n.sims,"\n")


# simulate a survey of democrats and republicans
# n.sims > s.size
dems<-rep(1,7000)
reps<-rep(0,3000)
pop<-c(dems,reps)
table(pop)
mean(pop)
n.sims<- 2500
s.size<- 159
dem.sim<-rep(9,s.size)
for (i in 1:n.sims){
    dem.sim[i]<-mean(sample(pop,s.size))
    }
plot(density(dem.sim),xlab="Estimated Proportion of Democrats",main="")
abline(v=mean(dem.sim))


# also from Gelmam and Hill page 20
# using simulation to find confidence intervals
# survey setup: 1000 respondents (500 men and 500 women), 75% men support death pnalty and the number for women is 65%
# like to know the ratio of support between men and women; also the confidence interval.
n.mem<-500
p.hat.men<-.75
se.men<-sqrt(p.hat.men*(1-p.hat.men)/n.mem) # the formula for standard errors for proportions is on the bottom of page 17. 
n.womem<-500
p.hat.women<-.65
se.women<-sqrt(p.hat.women*(1-p.hat.women)/n.womem)  
n.sims<-1000
p.men<-rnorm(n.sims, p.hat.men, se.men)
p.women<-rnorm(n.sims, p.hat.women, se.women)
ratio<-p.men/p.women
int.95<-quantile(ratio, c(.025, .975))
int.95
