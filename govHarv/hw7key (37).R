##
## hw7 answer key
## m.blackwell
## 23 Nov 08
##

library(xtable)

###############
## Problem 1 ##
###############

load("ps7.RData")

#####
# A #
#####

head(ps7)

mod.1a <- lm(earnings ~ assignmt, data=ps7)
summary(mod.1a)
confint(mod.1a)

## here's a little quickly made xtable
## with everything we need.

xtable(cbind(summary(mod.1a)$coef[,1:2],confint(mod.1a)), file="")


#####
# C #
#####

mod.1c <- lm(earnings ~ assignmt + sex:assignmt, data=ps7)
summary(mod.1c)
confint(mod.1c)
coef.1c <- coef(mod.1c)

treated.males.1c <- coef.1c["(Intercept)"] + coef.1c["assignmt"] +
                    coef.1c["assignmt:sex"]
control.males.1c <- coef.1c["(Intercept)"]
treated.females.1c <- coef.1c["(Intercept)"] + coef.1c["assignmt"]
control.females.1c <- coef.1c["(Intercept)"]

tab.1c<-
  rbind(femals=c(control.females.1c,treated.females.1c),males=c(control.males.1c,treated.males.1c))

colnames(tab.1c) <- c("Not Assigned", "Assigned")
tab.1c

xtable(tab.1c)

#####
# C #
#####


mod.1d <- lm(earnings ~ assignmt*sex, data=ps7)
summary(mod.1d)
confint(mod.1d)
coef.1d <- coef(mod.1d)

treated.males.1d <- coef.1d["(Intercept)"] + coef.1d["assignmt"] +
                    + coef.1d["sex"] + coef.1d["assignmt:sex"]
control.males.1d <- coef.1d["(Intercept)"] + coef.1d["sex"]
treated.females.1d <- coef.1d["(Intercept)"] + coef.1d["assignmt"]
control.females.1d <- coef.1d["(Intercept)"]

tab.1d <-
  rbind(femals=c(control.females.1d,treated.females.1d),males=c(control.males.1d,treated.males.1d))

colnames(tab.1d) <- c("Not Assigned", "Assigned")
tab.1d

xtable(tab.1d)


#####
# E #
#####

marg.eff.male <- coef.1d["assignmt"] + coef.1d["assignmt:sex"]
marg.eff.female <- coef.1d["assignmt"]

var.male <- vcov(mod.1d)["assignmt","assignmt"] +
            vcov(mod.1d)["assignmt:sex","assignmt:sex"] +
            2*vcov(mod.1d)["assignmt","assignmt:sex"]
var.female <- vcov(mod.1d)["assignmt","assignmt"]

marg.eff.male
marg.eff.female

var.male
sqrt(var.male)

var.female
sqrt(var.female)

#####
# F #
#####

mod.1f <- lm(earnings ~ assignmt*age, data=ps7)
summary(mod.1f)

assign.cols <- ifelse(ps7$assignmt==1,"red","orange")
assign.pchs <- ifelse(ps7$assignmt==1,17,19)

assign.slope <- coef(mod.1f)["age"]+coef(mod.1f)["assignmt:age"]
assign.int   <- coef(mod.1f)["assignmt"]+coef(mod.1f)["(Intercept)"]

control.slope <- coef(mod.1f)["age"]
control.int   <- coef(mod.1f)["(Intercept)"]

plot(x=ps7$age,y=ps7$earnings, col=assign.cols, pch=assign.pchs,
     xlab="Age", ylab="Earnings")
abline(a=control.int, b=control.slope, lty=2,col="orange", lwd=2)
abline(a=assign.int, b=assign.slope, lty=1,col="red", lwd=2)
legend(x="topright",col=c("red","orange"),lty=c(1,2),pch=c(17,19),
       legend=c("Assigned","Unassigned"), bty="n")


###############
## Problem 1 ##
###############

#####
# B #
#####

mod.2b <- lm(earnings ~ assignmt + age + I(age^2), data=ps7)
summary(mod.2b)
xtable(summary(mod.2b))

#####
# C #
#####

plot(x=ps7$age,y=ps7$earnings, pch=19, col="chocolate", )
curve(coef(mod.2b)["(Intercept)"] + x*coef(mod.2b)["age"] +
      x^2*coef(mod.2b)["I(age^2)"], col="darkgreen", add=TRUE,lwd=2)

#####
# D #
#####

marg.eff.age <- function(age) {
  out <- coef(mod.2b)["age"] + 2*age*coef(mod.2b)["I(age^2)"]
  return(out)
}

xtable(cbind(c(20,30,40,50,60),c(marg.eff.age(20),marg.eff.age(30),marg.eff.age(40),marg.eff.age(50),marg.eff.age(60))))


#####
# E #
#####

-coef(mod.2b)["age"]/(2*coef(mod.2b)["I(age^2)"])

#####
# F #
#####

vcv.2b <- vcov(mod.2b)

## the variance of the age coefficient:
vcv.2b["age","age"]

## the variance of the age-sqaured
vcv.2b["I(age^2)","I(age^2)"]

## the covariance between them:
vcv.2b["I(age^2)","age"]


marg.eff.sd <- function(age) {
  out <- vcv.2b["age","age"] + 4*age*vcv.2b["I(age^2)","age"] + 4*age^2*vcv.2b["I(age^2)","I(age^2)"]
  return(sqrt(out))
}


xtable(cbind(c(20,30,40,50,60),c(marg.eff.age(20),marg.eff.age(30),marg.eff.age(40),marg.eff.age(50),marg.eff.age(60)),c(marg.eff.sd(20),marg.eff.sd(30),marg.eff.sd(40),marg.eff.sd(50),marg.eff.sd(60))))
