library(R2WinBUGS)
library(arm)

# dd <- c("C:/XunCao/Teaching/Method_Stat/multilevel/Lec/lec6") 
dd <- c("C:/temp") 
setwd(dd)


#######################################################################
## second example on blue/red states: this is very interesting stuff ##
fips.cbs <- read.dta("QJPS_6026_supp/fips.icpsr.cbs.naes.dta") ## this carries info for the 51 states. 

## 1:  state-level regressions:
st <- read.table("QJPS_6026_supp/st.dat")
st.all <- merge(st, fips.cbs, by.x="stateabb", by.y="st")  # all states
st.s <- subset(st.all, st.all$regioncbs==3)                # southern states
st.ns <- subset(st.all, st.all$regioncbs!=3)               # the rest ... 

p.st.yr <- seq(min(st$year), max(st$year),4)               # years: jump by 4. 
n.st.yr <- length(p.st.yr)

col.st.names <- c("st.int", "z.st.inc.pop")
n.st.coef <- length(col.st.names)


## create arrays to save the cofficients and standard errors:
B.st.lm <- array(NA, c(n.st.yr, n.st.coef))   # all states
colnames(B.st.lm) <- col.st.names
SE.st.lm <- array(NA, c(n.st.yr, n.st.coef))
colnames(SE.st.lm) <- col.st.names

B.st.s.lm <- array(NA, c(n.st.yr, n.st.coef)) # southern states
colnames(B.st.s.lm) <- col.st.names
SE.st.s.lm <- array(NA, c(n.st.yr, n.st.coef))
colnames(SE.st.s.lm) <- col.st.names

B.st.ns.lm <- array(NA, c(n.st.yr, n.st.coef))# non-southern states
colnames(B.st.ns.lm) <- col.st.names
SE.st.ns.lm <- array(NA, c(n.st.yr, n.st.coef))
colnames(SE.st.ns.lm) <- col.st.names


for (i in 1:n.st.yr){
    st.temp <- subset(st, st$year==p.st.yr[i])
    st.reg <- summary(lm(st.temp$st.repshare ~ st.temp$z.st.inc.pop)) # z.st.inc.pop: this is logged state average income ($ 10,000, 1996)
    B.st.lm[i,] <- st.reg$coef[,1]
    SE.st.lm[i,] <- st.reg$coef[,2]
}   
# this saves the intercept and slopes and their standard errors in the arrays defined before. 

for (i in 1:n.st.yr){
    st.s.temp <- subset(st.s, st.s$year==p.st.yr[i])
    st.s.reg <- summary(lm(st.s.temp$st.repshare ~ st.s.temp$z.st.inc.pop)) 
    B.st.s.lm[i,] <- st.s.reg$coef[,1]
    SE.st.s.lm[i,] <- st.s.reg$coef[,2]
}

for (i in 1:n.st.yr){
    st.ns.temp <- subset(st.ns, st.ns$year==p.st.yr[i])
    st.ns.reg <- summary(lm(st.ns.temp$st.repshare ~ st.ns.temp$z.st.inc.pop))
    B.st.ns.lm[i,] <- st.ns.reg$coef[,1]
    SE.st.ns.lm[i,] <- st.ns.reg$coef[,2]
}

B.st.lm <- data.frame(B.st.lm)
SE.st.lm <- data.frame(SE.st.lm)

B.st.s.lm <- data.frame(B.st.s.lm)
SE.st.s.lm <- data.frame(SE.st.s.lm)

B.st.ns.lm <- data.frame(B.st.ns.lm)
SE.st.ns.lm <- data.frame(SE.st.ns.lm)


# Plot yearly coefficient of state average income on state share of Republican support: 
# postscript ("ols_state.ps",width=8, height=2.5, horizontal=T)
par(mfrow=c(1,3))
n.st <- NROW(B.st.lm)
plot(0, 0, type='n', ylim=c(-.4, .4), xlim=c(min(st$year), max(st$year)),
  cex.lab=1.15, xaxt="n", yaxt="n",  main="All States", cex.main=1, xlab="Year", ylab="Income Coefficient")
  axis(side=1, at=c(1960, 1980, 2000), cex.axis=1.1)
  axis(side=2, at=c(-.2, 0, .2), cex.axis=1.1)
  abline(h=0, col="gray")
    for (i in 1:n.st) {
    points(p.st.yr[i], B.st.lm[i,2], type='p', pch=19)
    segments(p.st.yr[i], (B.st.lm[i,2]-2*SE.st.lm[i,2]), p.st.yr[i], (B.st.lm[i,2]+2*SE.st.lm[i,2]))   # x 2: about 95% confidence interval.
  }

plot(0, 0, type='n', ylim=c(-.4, .4), xlim=c(min(st$year), max(st$year)),
  cex.lab=1.15, xaxt="n", yaxt="n",  main="Southern States", cex.main=1, xlab="Year", ylab="Income Coefficient")
  axis(side=1, at=c(1960, 1980, 2000), cex.axis=1.1)
  axis(side=2, at=c(-.2, 0, .2), cex.axis=1.1)
  abline(h=0, col="gray")
    for (i in 1:n.st) {
    points(p.st.yr[i], B.st.s.lm[i,2], type='p', pch=19)
    segments(p.st.yr[i], (B.st.s.lm[i,2]-2*SE.st.s.lm[i,2]), p.st.yr[i], (B.st.s.lm[i,2]+2*SE.st.s.lm[i,2]))
  }

plot(0, 0, type='n', ylim=c(-.4, .4), xlim=c(min(st$year), max(st$year)),
  cex.lab=1.15, xaxt="n", yaxt="n",  main="Non-Southern States", cex.main=1, xlab="Year", ylab="Income Coefficient")
  axis(side=1, at=c(1960, 1980, 2000), cex.axis=1.1)
  axis(side=2, at=c(-.2, 0, .2), cex.axis=1.1)
  abline(h=0, col="gray")
    for (i in 1:n.st) {
    points(p.st.yr[i], B.st.ns.lm[i,2], type='p', pch=19)
    segments(p.st.yr[i], (B.st.ns.lm[i,2]-2*SE.st.ns.lm[i,2]), p.st.yr[i], (B.st.ns.lm[i,2]+2*SE.st.ns.lm[i,2]))
  }
# dev.off()
## so far, typical ecological fallacy: we use state-level data to explain individual level behavior. 




##
## 2. now we move from state to the individual level: 
anes <- read.table("QJPS_6026_supp/recode.anes.qjps.txt")
anes.s <- subset(anes, anes$regioncbs==3)
anes.ns <- subset(anes, anes$regioncbs!=3)


## the following repeats what has been done: define arrays to save coefficients and se's
p.yr <- seq(min(anes$yr), max(anes$yr),4)
n.yr <- length(p.yr)
col.names <- c("int", "z.inc")
n.coef <- length(col.names)

B.glm <- array(NA, c(n.yr, n.coef))
colnames(B.glm) <- col.names
SE.glm <- array(NA, c(n.yr, n.coef))
colnames(SE.glm) <- col.names

B.s.glm <- array(NA, c(n.yr, n.coef))
colnames(B.s.glm) <- col.names
SE.s.glm <- array(NA, c(n.yr, n.coef))
colnames(SE.s.glm) <- col.names

B.ns.glm <- array(NA, c(n.yr, n.coef))
colnames(B.ns.glm) <- col.names
SE.ns.glm <- array(NA, c(n.yr, n.coef))
colnames(SE.ns.glm) <- col.names

for (i in 1:n.yr){
    temp <- subset(anes, anes$yr==p.yr[i])
    reg <- summary(glm(temp$vote ~ temp$z.inc, family=binomial(link=logit)))
    B.glm[i,] <- reg$coef[,1]
    SE.glm[i,] <- reg$coef[,2]
}

for (i in 1:n.yr){
    temp.s <- subset(anes.s, anes.s$yr==p.yr[i])
    reg.s <- summary(glm(temp.s$vote ~ temp.s$z.inc, family=binomial(link=logit)))
    B.s.glm[i,] <- reg.s$coef[,1]
    SE.s.glm[i,] <- reg.s$coef[,2]
}

for (i in 1:n.yr){
    temp.ns <- subset(anes.ns, anes.ns$yr==p.yr[i])
    reg.ns <- summary(glm(temp.ns$vote ~ temp.ns$z.inc, family=binomial(link=logit))) # z.inc is the categories of personal income: seems to be on log scale.
    B.ns.glm[i,] <- reg.ns$coef[,1]
    SE.ns.glm[i,] <- reg.ns$coef[,2]
}

B.glm <- data.frame(B.glm)
SE.glm <- data.frame(SE.glm)

B.s.glm <- data.frame(B.s.glm)
SE.s.glm <- data.frame(SE.s.glm)

B.ns.glm <- data.frame(B.ns.glm)
SE.ns.glm <- data.frame(SE.ns.glm)

# plot:
par(mfrow=c(1,3))
n <- NROW(B.glm)
plot(0, 0, type='n', ylim=c(-.7, 2.3), xlim=c(min(anes$yr), max(anes$yr)),
  cex.lab=1.15, xaxt="n", yaxt="n", main="All Individuals", cex.main=1, xlab="Year", ylab="Income Coefficient")
  axis(side=1, at=c(1960, 1980, 2000), cex.axis=1.1)
  axis(side=2, at=c(0, 1), cex.axis=1.1 )
  abline(h=0, col="gray")
    for (i in 1:n) {
    points(p.yr[i], B.glm$z.inc[i], type='p', pch=19)
    segments(p.yr[i], (B.glm$z.inc[i]-2*SE.glm$z.inc[i]), p.yr[i],  (B.glm$z.inc[i]+2*SE.glm$z.inc[i]))
  }

plot(0, 0, type='n', ylim=c(-.7, 2.3), xlim=c(min(anes$yr), max(anes$yr)),
  cex.lab=1.15, xaxt="n", yaxt="n", main="Southerners", cex.main=1, xlab="Year", ylab="Income Coefficient")
  axis(side=1, at=c(1960, 1980, 2000), cex.axis=1.1)
  axis(side=2, at=c(0, 1), cex.axis=1.1 )
  abline(h=0, col="gray")
    for (i in 1:n) {
    points(p.yr[i], B.s.glm$z.inc[i], type='p', pch=19)
    segments(p.yr[i], (B.s.glm$z.inc[i]-2*SE.s.glm$z.inc[i]), p.yr[i],  (B.s.glm$z.inc[i]+2*SE.s.glm$z.inc[i]))
  }

plot(0, 0, type='n', ylim=c(-.7, 2.3), xlim=c(min(anes$yr), max(anes$yr)),
  cex.lab=1.15, xaxt="n", yaxt="n", main="Non-Southerners", cex.main=1, xlab="Year", ylab="Income Coefficient")
  axis(side=1, at=c(1960, 1980, 2000), cex.axis=1.1)
  axis(side=2, at=c(0, 1), cex.axis=1.1 )
  abline(h=0, col="gray")
    for (i in 1:n) {
    points(p.yr[i], B.ns.glm$z.inc[i], type='p', pch=19)
    segments(p.yr[i], (B.ns.glm$z.inc[i]-2*SE.ns.glm$z.inc[i]), p.yr[i],  (B.ns.glm$z.inc[i]+2*SE.ns.glm$z.inc[i]))
  }




## 3: multilevel models kick in:

##
#2000 annenberg with region:
#income.new:  individual-level income
#state.new:  the state index variable
# avg.income.expanded: state-level income 
dat2000<-dget("dat2000")
attach.all(dat2000)

# varying intercept 
fit.vi.2000 <- lmer (y ~ income.new + factor(region) + avg.income.expanded +
                     (1 | state.new), family=binomial(link="logit"))
display(fit.vi.2000)
coef(fit.vi.2000)

# varying intercept, varying slopes
fit.2000 <- lmer (y ~ income.new*factor(region) + income.new*avg.income.expanded +
                  (1 + income.new | state.new), family=binomial(link="logit"))     # this is going to take a while ...
display (fit.2000)
coef(fit.2000)


##
#2004 annenberg with region:
dat2004<-dget("dat2004")
attach.all(dat2004)

fit.vi.2004 <- lmer (y ~ income.new + factor(region) + avg.income.expanded + (1 | state.new), family=binomial(link="logit"))
display(fit.vi.2004)
coef(fit.vi.2004)

fit.2004 <- lmer (y ~ income.new*factor(region) + income.new*avg.income.expanded + 
(1 + income.new | state.new), family=binomial(link="logit"))
# theta_i=[gamma_0 + gamma_1*avg.income.expanded + gamma_2*region_2 + gamma_3*region_3 + gamma_4*region_4 + eta_j[i]]
#        +[gamma_0^b + gamma_1^b*avg.income.expanded + gamma_2^b*region_2 + gamma_3^b*region_3 + gamma_4^b*region_4 + eta_j[i]^b] * income.new
display (fit.2004)
coef(fit.2004)



######## try this as well: no region is involved here. #########
fit.2004try <- lmer (y ~  income.new*avg.income.expanded + 
(1 + income.new | state.new), family=binomial(link="logit"))
display (fit.2004try)

fit.2004try <- lmer (y ~  income.new + avg.income.expanded + income.new:avg.income.expanded + 
(1 + income.new | state.new), family=binomial(link="logit"))
display (fit.2004try)
####################################









### Plots: ignore the following if you are not so much into figures  ###
source("QJPS_6026_supp/superplot function.R") # read in a plotting function. 
load(file="QJPS_6026_supp/cps.income.Rdata")
results=list()
years=seq(1968,2004,4)

state.region[8]="Northeast" # put Delaware into the Northeast
state.region[20]="Northeast" # put Maryland into the Northeast
region.indic<-read.dta("QJPS_6026_supp/region_indic_annen2000.dta",convert.factors=F)


## plot varying intercepts ##
# 2000
avg.income<-dget("avg.income.2000")
sl2000 = as.integer(rownames(coef(fit.vi.2000)[[1]]))
income.vi.slope.2000 <- coef(fit.vi.2000)$state.new[,2]  
income.vi.intercept.2000 <- coef(fit.vi.2000)$state.new[,1] + 
 region.indic[,1]*coef(fit.vi.2000)$state.new[,3] +
 region.indic[,2]*coef(fit.vi.2000)$state.new[,4] +
 region.indic[,3]*coef(fit.vi.2000)$state.new[,5] +
 coef(fit.vi.2000)$state.new[,6]*avg.income[sl2000]
results[[9]]=cbind(income.vi.intercept.2000, income.vi.slope.2000); rownames(results[[9]])=sl2000

# 2004
sl2004 = as.integer(rownames(coef(fit.vi.2004)[[1]]))
income.vi.slope.2004 <- coef(fit.vi.2004)$state.new[,2]  
income.vi.intercept.2004 <- coef(fit.vi.2004)$state.new[,1] + 
 region.indic[,1]*coef(fit.vi.2004)$state.new[,3] +
 region.indic[,2]*coef(fit.vi.2004)$state.new[,4] +
 region.indic[,3]*coef(fit.vi.2004)$state.new[,5] +
 coef(fit.vi.2004)$state.new[,6]*avg.income[sl2004]
results[[10]]=cbind(income.vi.intercept.2004, income.vi.slope.2004); rownames(results[[10]])=sl2004

windows(width=9.5,height=4)
superp=superplot(results, start=9, end=10, rows=1, columns=2, indiv=1, lmer=1, var.slope=1, lowbound=-2, hibound=2, sf=3)



## plot varying intercept, varying slopes ##
# 2000
sl2000 = as.integer(rownames(coef(fit.2000)[[1]]))
##
income.slope.2000 <- coef(fit.2000)$state.new[,2] + (coef(fit.2000)$state.new[,10])*(avg.income[sl2000] )+
 region.indic[,1]*coef(fit.2000)$state.new[,7] +
 region.indic[,2]*coef(fit.2000)$state.new[,8] +
 region.indic[,3]*coef(fit.2000)$state.new[,9] 
income.intercept.2000 <- coef(fit.2000)$state.new[,1] + (coef(fit.2000)$state.new[,6])*( avg.income [sl2000])+
 region.indic[,1]*coef(fit.2000)$state.new[,3] +
 region.indic[,2]*coef(fit.2000)$state.new[,4] +
 region.indic[,3]*coef(fit.2000)$state.new[,5] 
results[[9]]=cbind(income.intercept.2000,income.slope.2000); rownames(results[[9]])=sl2000

# 2004
sl2004 = as.integer(rownames(coef(fit.2004)[[1]]))
income.slope.2004 <- coef(fit.2004)$state.new[,2] + (coef(fit.2004)$state.new[,10])*(avg.income[sl2004] )+
 region.indic[,1]*coef(fit.2004)$state.new[,7] +
 region.indic[,2]*coef(fit.2004)$state.new[,8] +
 region.indic[,3]*coef(fit.2004)$state.new[,9] 
income.intercept.2004 <- coef(fit.2004)$state.new[,1] + (coef(fit.2004)$state.new[,6])*( avg.income [sl2004])+
 region.indic[,1]*coef(fit.2004)$state.new[,3] +
 region.indic[,2]*coef(fit.2004)$state.new[,4] +
 region.indic[,3]*coef(fit.2004)$state.new[,5] 
results[[10]]=cbind(income.intercept.2004,income.slope.2004); rownames(results[[10]])=sl2004

windows(width=9.5,height=4)
superp=superplot(results, start=9, end=10, rows=1, columns=2, indiv=1, lmer=1, var.slope=1, lowbound=-2, hibound=2, sf=3)
