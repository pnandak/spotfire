## logit: 

library(arm)
#dd <- c("C:/XunCao/Teaching/Method_Stat/multilevel/Lec/lec2") 
#setwd(dd)


# the link function as the inverse logit function:
# postscript("inverselogit.ps", horizontal=T,height=3.5, width=7)
par(mfrow=c(1,2))
x<--8:8
p<-exp(x)/(1+exp(x))
plot(x, p, type="l", main="p=exp(x)/(1+exp(x))"); abline(h=.5, lty=2); abline(v=0, lty=2)

x<--10:14
p<-exp(-2+.5*x)/(1+exp(-2+.5*x))
plot(x, p, type="l", main="p=exp(-2+.5*x)/(1+exp(-2+.5*x))"); abline(h=.5, lty=2); abline(v=4, lty=2)
# dev.off()



# Logistic regression for arsenic data
wells <- read.table ("http://privatewww.essex.ac.uk/~caox/teaching/Day2/wells.dat", header=TRUE)
attach.all (wells)

# Logistic regression of switching on distance to nearest safe well
fit.1 <- glm (switch ~ dist, family=binomial(link="logit"))
display (fit.1)

# Redefine distance in 100-meter units and fit the model again
dist100 <- dist/100
fit.2 <- glm (switch ~ dist100, family=binomial(link="logit"))
display (fit.2)

# histogram of distances
#postscript ("arsenic.distances.bnew.ps", height=3, width=4, horizontal=TRUE)
hist (dist, breaks=seq(0,10+max(dist[!is.na(dist)]),10), freq=TRUE, xlab="Distance (in meters) to nearest safe well", ylab="", main="", mgp=c(2,.5,0))
#dev.off ()


# plots of model fit
jitter.binary <- function(a, jitt=.05){
  a + (1-2*a)*runif(length(a),0,jitt)
}


# postscript ("arsenic.logitfit.1new.a.ps", height=4.5, width=5.5, horizontal=TRUE)
par(mfrow=c(1,1))
plot(c(0,max(dist, na.rm=TRUE)*1.02), c(0,1), 
    xlab="Distance (in meters) to nearest safe well", 
    ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve (invlogit(coef(fit.1)[1]+coef(fit.1)[2]*x), lwd=1, add=TRUE)
points (dist, jitter.binary(switch), pch=20, cex=.1)
# dev.off ()


# histogram of As levels
#postscript ("arsenic.levels.a.ps", height=3, width=4, horizontal=TRUE)
hist (arsenic, breaks=seq(0,.25+max(arsenic[!is.na(arsenic)]),.25), freq=TRUE, xlab="Arsenic concentration in well water", ylab="", main="", mgp=c(2,.5,0))
#dev.off ()



# model with 2 predictors
fit.3 <- glm (switch ~ dist100 + arsenic, family=binomial(link="logit"))
display (fit.3)

# postscript ("arsenic.2variables.a.ps", height=3.5, width=4, horizontal=TRUE)
plot(c(0,max(dist,na.rm=TRUE)*1.02), c(0,1), xlab="Distance (in meters) to nearest safe well", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
points (dist, jitter.binary(switch), pch=20, cex=.1)
curve (invlogit(coef(fit.3)[1]+coef(fit.3)[2]*x/100+coef(fit.3)[3]*.50), lwd=.5, add=TRUE)
curve (invlogit(coef(fit.3)[1]+coef(fit.3)[2]*x/100+coef(fit.3)[3]*1.00), lwd=.5, add=TRUE)
text (50, .27, "if As = 0.5", adj=0, cex=1)
text (75, .50, "if As = 1.0", adj=0, cex=1)
# dev.off ()

# postscript ("arsenic.2variables.b.ps", height=3.5, width=4, horizontal=TRUE)
plot(c(0,max(arsenic,na.rm=TRUE)*1.02), c(0,1), xlab="Arsenic concentration in well water", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
points (arsenic, jitter.binary(switch), pch=20, cex=.1)
curve (invlogit(coef(fit.3)[1]+coef(fit.3)[2]*0+coef(fit.3)[3]*x), from=0.5, lwd=.5, add=TRUE)
curve (invlogit(coef(fit.3)[1]+coef(fit.3)[2]*0.5+coef(fit.3)[3]*x), from=0.5, lwd=.5, add=TRUE)
text (.5, .78, "if dist = 0", adj=0, cex=1)
text (2, .5, "if dist = 50", adj=0, cex=1)
# dev.off ()



# including an interaction
fit.4 <- glm (switch ~ dist100 + arsenic + dist100:arsenic,
  family=binomial(link="logit"))
display(fit.4)

# postscript ("arsenic.interact.a.ps", height=3.5, width=4, horizontal=TRUE)
plot(c(0,max(dist,na.rm=TRUE)*1.02), c(0,1), xlab="Distance (in meters) to nearest safe well", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
points (dist, jitter.binary(switch), pch=20, cex=.1)
curve (invlogit(coef(fit.4)[1]+coef(fit.4)[2]*x/100+coef(fit.4)[3]*.50+coef(fit.4)[4]*(x/100)*.50), lwd=.5, add=TRUE)
curve (invlogit(coef(fit.4)[1]+coef(fit.4)[2]*x/100+coef(fit.4)[3]*1.00+coef(fit.4)[4]*(x/100)*1.00), lwd=.5, add=TRUE)
text (50, .29, "if As = 0.5", adj=0, cex=.8)
text (75, .50, "if As = 1.0", adj=0, cex=.8)
# dev.off ()

# postscript ("arsenic.interact.b.ps", height=3.5, width=4, horizontal=TRUE)
plot(c(0,max(arsenic,na.rm=TRUE)*1.02), c(0,1), xlab="Arsenic concentration in well water", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
points (arsenic, jitter.binary(switch), pch=20, cex=.1)
curve (invlogit(coef(fit.4)[1]+coef(fit.4)[2]*0+coef(fit.4)[3]*x+coef(fit.4)[4]*0*x), from=0.5, lwd=.5, add=TRUE)
curve (invlogit(coef(fit.4)[1]+coef(fit.4)[2]*0.5+coef(fit.4)[3]*x+coef(fit.4)[4]*0.5*x), from=0.5, lwd=.5, add=TRUE)
text (.50, .78, "if dist = 0", adj=0, cex=.8)
text (2.00, .6, "if dist = 50", adj=0, cex=.8)
# dev.off ()





### stop here if possible ###

# centering the input variables: does this make sense ??? 
c.dist100 <- dist100 - mean (dist100)
c.arsenic <- arsenic - mean (arsenic)

fit.5 <- glm (switch ~ c.dist100 + c.arsenic + c.dist100:c.arsenic,
  family=binomial(link="logit"))


# adding social predictors
educ4 <- educ/4

fit.6 <- glm (switch ~ c.dist100 + c.arsenic + c.dist100:c.arsenic +
  assoc + educ4, family=binomial(link="logit"))
display (fit.6)

fit.7 <- glm (switch ~ c.dist100 + c.arsenic + c.dist100:c.arsenic +
  educ4, family=binomial(link="logit"))
display (fit.7)

c.educ4 <- educ4 - mean(educ4)

fit.8 <- glm (switch ~ c.dist100 + c.arsenic + c.educ4 + c.dist100:c.arsenic +
  c.dist100:c.educ4 + c.arsenic:c.educ4, family=binomial(link="logit"))
display (fit.8)
