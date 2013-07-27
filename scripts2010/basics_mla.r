library(R2WinBUGS)  # we use this package to run WinBUGS from R. 
library(arm)        # this is the package for multilevel data. 


# dd <- c("C:/XunCao/Teaching/Method_Stat/multilevel/Lec/lec4") 

dd <- c("C:/temp")  # this is the working directory if you saved the data in the temp folder on the C drive. 
setwd(dd)           # this sets the working directory as what has been specified in "dd" for R. 
                    # we can also set the working directory by hand though. 

## 1: simulating some multilevel data:
## something for fun: changing parameters. 
## what the data look like if this is the true data generating process? 

 J<-8                    # number of groups. 
 x<-seq(-10, 10, by=.5)  # x
 n.j<-length(x)          # number of obs for each group: set to be the same for every group, but we can let it vary as well. 
 n<-n.j*J                # total number of obs. 
 
 mu.a<-5                 # group level variations: mean of varying intercept
 s.a<-5                  # sd of varying intercept: across-group variation/heterogeneity. 
 
 s.y<-3                  # data-level variation or within-group variation: sigma_y
 
 m.b<-.5                 # group level variations: mean of varying slope for x              
 s.b<-0                  # sd of varying intercept: across-group variation/heterogeneity: for now, fixed it at zero: contant slope.  
                          
 
 b<-rnorm(J, m.b, s.b)   # so no correlation between the intercept and slope by this setup
 a<-rnorm(J, mu.a, s.a)  # but we can definitely do this using a multivariate distribution. 
 
 Y<-NULL
 for (j in 1:J){y.j<-a[j]+b[j]*x + rnorm(length(x), 0, s.y)
 Y<-c(Y, y.j)
 }
 
plot(rep(x, J), Y); abline(lm(Y~rep(x, J)), lwd=2)
for (j in 1:J){points(x, Y[(n.j*(j-1)+1):(n.j*j)], col=j)
               abline(lm(Y[(n.j*(j-1)+1):(n.j*j)]~x), col=j)}



## try another multilevel structure simulation:

# Multilevel sampling: simulation example
# Two stages: 
# Normal(2,5) for macro units "a"
# Normal(a,1) for micro units "b"

# (a) sampling 5 macro units and 10 macro units
nmacro <- 5
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

# (b) repeat similar simulations with 10 macro and 3 micro units
nmacro <- 10
a <- rnorm(nmacro, mean=2, sd=5)
nmicro <- 3
b <- matrix(0,nrow=nmacro,ncol=nmicro)
for (i in 1:nmacro)
    { b[i,] <- rnorm(nmicro, mean=a[i], sd=1) }

# find the mean and standard deviation of samples in b
round(mean(b),2)
round(sd(as.vector(b)),2)

# Repeat (a) and (b) sampling sevaral time to get an idea about variability
# What can we say about estimated means and standard errors? think about it in the extreme cases:
# when there is only one micro unit for each macro unit? 


#
# (c) simulations to demonstrate different types of multilevel model: here I am cheating by setting sigma_y = 0 for a better visual effect. 
#
x<-1:10 # we add a predictor here:
bta<-.5 # add a coefficient here for x
nmacro <- 5
a <- rnorm(nmacro, mean=2, sd=5)

#sampling 10 micro units from each macro unit
nmicro <- 10
b <- matrix(0,nrow=nmacro,ncol=nmicro)
for (i in 1:nmacro)
    { b[i,] <- bta*x+rnorm(nmicro, mean=a[i], sd=0) # this is equivalent to: a[i]+ bta*x+rnorm(nmicro, mean=0, sd=0)
    }                                               # I am cheating by setting the sd=0; this is for illustrative purposes. 

Coef<-NULL
for (i in 1:nmacro){fit.1<-lm(b[i, ]~x); Coef<-rbind(Coef, coef(fit.1))}

# postscript ("varyinginter.ps", horizontal=FALSE, height=6, width=6.5)
plot(x, b[1, ], type="n", ylim=c(min(b), max(b)), xlim=c(1, 13), ylab="y", main="Varying Intercepts")
for (i in 1:nmacro){curve(Coef[i, 1]+Coef[i,2]*x, lwd=1, add=TRUE)
                    text(11, max(Coef[i, 1]+Coef[i,2]*x), paste("group ", as.character(i), sep=""))
                    }
# dev.off ()


# now varying slope with constant intercept: but when does this model makes sense? think about the meaning of intercept: this is when x=0
x<-0:9 # we add a predictor here:
bta<-rnorm(nmacro, mean=2, sd=3) # add a coefficient here for x
nmacro <- 5
a <- 1 # common 

#sampling 10 micro units from each macro unit
nmicro <- 10
b <- matrix(0,nrow=nmacro,ncol=nmicro)
for (i in 1:nmacro)
    { b[i,] <- bta[i]*x+rnorm(nmicro, mean=a, sd=0) 
    }                                                

Coef<-NULL
for (i in 1:nmacro){fit.1<-lm(b[i, ]~x); Coef<-rbind(Coef, coef(fit.1))}

# postscript ("varyingslope.ps", horizontal=FALSE, height=6, width=6.5)
plot(x, b[1, ], type="n", ylim=c(min(b), max(b)), xlim=c(0, 13), ylab="y", main="Varying slopes with constant intercept")
for (i in 1:nmacro){curve(Coef[i, 1]+Coef[i,2]*x, lwd=1, add=TRUE)
                    text(11, max(Coef[i, 1]+Coef[i,2]*x), paste("group ", as.character(i), sep=""))
                    }
# dev.off ()


# now varying slope with varying intercept: let's them covary even though a simpler model might assume a 0 correlation.
nmacro <- 5
x<--4:5 
Sigma <- matrix(c(2,3,3,10),2,2) # so the correlation is 3/(10^.5*2^.5)=.67
Sigma                            # var(mvrnorm(n=1000, rep(0, 2), Sigma))
Beta<-mvrnorm(n=5, c(1, 2), Sigma)
var(Beta)

#sampling 10 micro units from each macro unit
nmicro <- 10
b <- matrix(0,nrow=nmacro,ncol=nmicro)
for (i in 1:nmacro)
    { b[i,] <- Beta[i, 1]+Beta[i, 2]*x
    }                                                

Coef<-NULL
for (i in 1:nmacro){fit.1<-lm(b[i, ]~x); Coef<-rbind(Coef, coef(fit.1))}

# postscript ("varyingboth.ps", horizontal=FALSE, height=6, width=6.5)
plot(x, b[1, ], type="n", ylim=c(min(b), max(b)), xlim=c(-5, 5), ylab="y", main="Varying slopes and intercepts with a rho of .67")
for (i in 1:nmacro){curve(Coef[i, 1]+Coef[i,2]*x, lwd=1, add=TRUE)
                    text(4, max(Coef[i, 1]+Coef[i,2]*x), paste("group ", as.character(i), sep=""))
                    }
# dev.off ()





####### now, getting serious for 
####### basics of multilevel modeling (in R and a little in Bugs): 

##
## data preparation: the radon example. 
srrs2 <- read.table ("srrs2.dat", header=T, sep=",")
mn <- srrs2$state=="MN"        # so we pick up a state "Minnesota": the grouping will be by county.
radon <- srrs2$activity[mn]    # radon level: DV. 
log.radon <- log (ifelse (radon==0, .1, radon))
floor <- srrs2$floor[mn]       # 0 for basement, 1 for first floor
n <- length(radon)
y <- log.radon
x <- floor

# get county index variable
county.name <- as.vector(srrs2$county[mn])
uniq <- unique(county.name)    # there are 85 counties. 
J <- length(uniq)
county <- rep (NA, J)
for (i in 1:J){
  county[county.name==uniq[i]] <- i
}

dim(cbind(y, x, county))
cbind(y, x, county)[1:10, ]
tapply(y, county, length)           # but the number of house in each county varies a lot
summary(tapply(y, county, length))  # from having only one to 116. 


# get the county-level predictor: county-level uranium level. 
srrs2.fips <- srrs2$stfips*1000 + srrs2$cntyfips
cty <- read.table ("cty.dat", header=T, sep=",")
usa.fips <- 1000*cty[,"stfips"] + cty[,"ctfips"]
usa.rows <- match (unique(srrs2.fips[mn]), usa.fips)
uranium <- cty[usa.rows,"Uppm"]
u <- log (uranium)
u.full<-u[county]

dim(cbind(y, x, county, u.full))
cbind(y, x, county, u.full)[1:10, ]



# 1.  

###  some basic stats for different counties: 
ybarbar = mean(y)
cty.mns = tapply(y,county,mean)
cty.vars = tapply(y,county,var)
sample.size <- as.vector (table (county))
cty.sds = mean(sqrt(cty.vars[!is.na(cty.vars)]))/sqrt(sample.size)


### varying-intercept model, no predictors: we are running Bugs here. 
# radon.data <- list ("n", "J", "y", "county")
# radon.inits <- function (){
#  list (a=rnorm(J), mu.a=rnorm(1),
#        sigma.y=runif(1), sigma.a=runif(1))
#}
#radon.parameters <- c ("a", "mu.a", "sigma.y", "sigma.a")
#mlm.radon.nopred <- bugs (radon.data, radon.inits, radon.parameters, "radon.multilevel.nopred.bug", n.chains=3, n.iter=2000)
#mlm.radon.nopred <- bugs (radon.data, radon.inits, radon.parameters, "radon.multilevel.nopred.bug", n.chains=3, n.iter=2000, debug=TRUE)
#plot(mlm.radon.nopred)
#
####



#sample.size <- as.vector (table (county))
#sample.size.jittered <- sample.size*exp (runif (J, -.1, .1))
#
## postscript ("radon.nopred.ps", horizontal=T,height=3.5, width=7)
#par(mfrow=c(1,2))
## pooled var ests
#plot (sample.size.jittered, cty.mns, cex.lab=.9, cex.axis=1,
#      xlab="sample size in county j",
#      ylab="avg. log radon in county j",
#      pch=20, log="x", cex=.3, mgp=c(1.5,.5,0),
#      ylim=c(0,3.2), yaxt="n", xaxt="n")
#axis (1, c(1,3,10,30,100), cex.axis=.9, mgp=c(1.5,.5,0))
#axis (2, seq(0,3), cex.axis=.9, mgp=c(1.5,.5,0))
#for (j in 1:J){
#  lines (rep(sample.size.jittered[j],2),
#         cty.mns[j] + c(-1,1)*cty.sds[j], lwd=.5)
#         cty.mns[j] + c(-1,1)*mean(cty.sds[!is.na(cty.sds)]), lwd=.5)
#}
#abline(h=mlm.radon.nopred$median$mu.a)
#title("No pooling",cex.main=.9, line=1)
##abline(h=ybarbar)
#points(sample.size.jittered[36],cty.mns[36],cex=4)#,col="red")
## mlm var ests
#
#plot (sample.size.jittered, mlm.radon.nopred$median$a, cex.lab=.9, cex.axis=1,
#      xlab="sample size in county j",
#      ylab="avg. log radon in county j",
#      pch=20, log="x", cex=.3, mgp=c(1.5,.5,0),
#      ylim=c(0,3.2), yaxt="n", xaxt="n")
#axis (1, c(1,3,10,30,100), cex.axis=.9, mgp=c(1.5,.5,0))
#axis (2, seq(0,3), cex.axis=.9, mgp=c(1.5,.5,0))
#for (j in 1:J){
#  lines (rep(sample.size.jittered[j],2),
#         mlm.radon.nopred$median$a[j] + c(-1,1)*mlm.radon.nopred$sd$a[j],
#      lwd=.5)
#}
#abline(h=mlm.radon.nopred$median$mu.a)
#points(sample.size.jittered[36],mlm.radon.nopred$median$a[36],cex=4)#,col="red")
#title("Multilevel model",cex.main=.9, line=1)
## dev.off()
#



# 2.  No-pooling and complete-pooling models

# fit the models
lm.pooled <- lm (y ~ x)
display (lm.pooled)

lm.unpooled.0 <- lm (y ~ x + factor(county))
display (lm.unpooled.0)

lm.unpooled <- lm (y ~ x + factor(county) - 1)
display (lm.unpooled)

# graph the data and fitted lines
x.jitter <- x + runif(n,-.05,.05)
display8 <- c (36, 1, 35, 21, 14, 71, 61, 70)
y.range <- range (y[!is.na(match(county,display8))])

sample.size <- as.vector (table (county))
sample.size.jittered <- sample.size*exp (runif (J, -.1, .1))

# postscript ("radon1.ps", height=4.5, horizontal=T)
par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
        xlab="floor", ylab="log radon level", cex.lab=1.6, cex.axis=1.5,
        pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1.3,
        main=uniq[j])
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.5)
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.5)
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lwd=.5, lty=2, add=TRUE) # add in the complete pooling regression line --- this is the same for every group
  curve (coef(lm.unpooled)[j+1] + coef(lm.unpooled)[1]*x, lwd=.5, add=TRUE)  # add in the no pooling regression line --- this is unique for every group
}
# dev.off()


# graph no-pooling ests vs. sample size
# postscript ("radon2.ps", horizontal=T)
par (mfrow=c(1,1), mar=c(5,5,4,2)+.1)
plot (sample.size.jittered, coef(lm.unpooled)[-1], cex.lab=2.5, cex.axis=2.5,
  xlab="sample size in county j", ylab=expression (paste
  ("est. intercept, ", alpha[j], "   (no pooling)")),
  pch=20, log="x", ylim=c(.15,3.5), yaxt="n", xaxt="n")
axis (1, c(1,3,10,30,100), cex.axis=2.5)
axis (2, seq(0,3), cex.axis=2.5)
for (j in 1:J){
  lines (rep(sample.size.jittered[j],2),
    coef(lm.unpooled)[j+1] + c(-1,1)*se.coef(lm.unpooled)[j+1], lwd=.5)
}
# dev.off()



##### this is important here: ####

# 3.  Multilevel model

# simplest mlm
M0 <- lmer (y ~ 1 + (1 | county))
display (M0)

# including x as a predictor
M1 <- lmer (y ~ x + (1 | county))
display (M1)

options (digits=2)

fixef(M1)
ranef(M1)
coef(M1)

a.hat.M1 <- fixef(M1)[1] + ranef(M1)$county #coef(M1)[[1]]["(Intercept)"]
b.hat.M1 <- fixef(M1)[2]



# make the graphs
# postscript ("radon3.ps", height=4.5, horizontal=T)
par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
    xlab="floor", ylab="log radon level", cex.lab=1.6, cex.axis=1.5,
    pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1.3, main=uniq[j])
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.5)
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.5)
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lwd=.5, lty=2, col="gray10", add=TRUE)
  curve (coef(lm.unpooled)[j+1] + coef(lm.unpooled)[1]*x, lwd=.5, col="gray10", add=TRUE)
  curve (a.hat.M1[j,] + b.hat.M1*x, lwd=1, col="black", add=TRUE)
}
# dev.off()


# plot of ests & se's vs. sample size
a.se.M1 <- se.coef(M1)$county

# postscript ("radon4.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1, mfrow=c(1,1))
plot (sample.size.jittered, t(a.hat.M1), cex.lab=2.5, cex.axis=2.5,
  xlab="sample size in county j", ylab=expression (paste
  ("est. intercept, ", alpha[j], "   (multilevel model)")),
  pch=20, log="x", ylim=c(.15,3.5), yaxt="n", xaxt="n")
axis (1, c(1,3,10,30,100), cex.axis=2.5)
axis (2, seq(0,3), cex.axis=2.5)
for (j in 1:J){
  lines (rep(sample.size.jittered[j],2),
    a.hat.M1[j,] + c(-1,1)*a.se.M1[j], lwd=.5, col="gray10")
}
abline (coef(lm.pooled)[1], 0, lwd=.5)
# dev.off()




# 4.  Multilevel model including uranium as a county-level predictor
u.full <- u[county]
M2 <- lmer (y ~ x + u.full + (1 | county))
display (M2)
coef(M2)

a.hat.M2 <- fixef(M2)[1] + fixef(M2)[3]*u + as.vector(ranef(M2)$county)
b.hat.M2 <- fixef(M2)[2]


# plot of ests & se's vs. county uranium
a.se.M2 <- se.coef(M2)$county
# postscript ("radon6.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1, mfrow=c(1,1))
plot (u, t(a.hat.M2), cex.lab=2.5, cex.axis=2.5,
      xlab="county-level uranium measure", ylab="est. regression intercept", pch=20,
      ylim=c(0,2), yaxt="n", xaxt="n", mgp=c(3.5,1.2,0))
axis (1, seq(-1,1,.5), cex.axis=2.5, mgp=c(3.5,1.2,0))
axis (2, seq(0,1.5,.5), cex.axis=2.5, mgp=c(3.5,1.2,0))
curve (fixef(M2)["(Intercept)"] + fixef(M2)["u.full"]*x, lwd=1, col="black", add=TRUE)
for (j in 1:J){
  lines (rep(u[j],2), a.hat.M2[j,] + c(-1,1)*a.se.M2[j,], lwd=.5, col="gray10")
}
# dev.off()


### 
# 5.  Some examples of predictions using lmer()

# new house in county 26 with x=1
x.squiggle <- 1
a.hat <- fixef(M2)[1] + fixef(M2)[3]*u + ranef(M2)$county
b.hat <- fixef(M2)[2]
sigma.y.hat <- sigma.hat(M2)$sigma$data

y.squiggle <- rnorm (1, a.hat[26, ] + b.hat*x.squiggle, sigma.y.hat)      # we didn't add in inferential uncertainty

n.sims <- 1000
y.squiggle <- rnorm (n.sims, a.hat[26, ] + b.hat*x.squiggle, sigma.y.hat)

# more complicated sims that we won't do: what about add in inferential uncertainty here??? 
#
#sim.M1 <- sim (M1, n.sims)
#a.sim <- sim.M1$county[,,1]
#b.sim <- sim.M1$county[,,2]

quantile (y.squiggle, c(.25,.5,.75))
exp (quantile (y.squiggle, c(.25,.5,.75)))

unlogged <- exp (y.squiggle)
mean (unlogged)


# new house in a new county
u.squiggle <- mean (u)              # mean value of county-level predictor. 
g.0.hat <- fixef(M2)["(Intercept)"]
g.1.hat <- fixef(M2)["u.full"]
sigma.a.hat <- sigma.hat(M2)$sigma$county

a.squiggle <- rnorm (n.sims, g.0.hat + g.1.hat*u.squiggle, sigma.a.hat)
y.squiggle <- rnorm (n.sims, a.squiggle + b.hat*x.squiggle, sigma.y.hat)


quantile (y.squiggle, c(.25,.5,.75))
exp (quantile (y.squiggle, c(.25,.5,.75)))

# new house in county 2
y.squiggle <- rnorm (n.sims, a.hat[2, ] + b.hat*x.squiggle, sigma.y.hat)
quantile (y.squiggle, c(.25,.5,.75))
