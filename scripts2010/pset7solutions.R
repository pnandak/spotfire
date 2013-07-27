## Gov 2000
## Problem Set 7 Solution Key
## November 17, 2009
## prepared by msen 
## with code taken from past gov2k TFs mb and jh

############################################## problem 1

load("boix.RData")
attach(boix)
library(xtable)

#######
## B ##
#######

## boix's original model is the following:

my.lm <- lm(threshold ~ threat  + area + trade  + population  + ethnic)
summary(my.lm)

## but the proper specification is this:

full.lm <- lm(threshold ~ threat + socialism + parties +
    area + trade + population + ethnic)
summary(full.lm)

#######
## C ##
#######

const <- full.lm$coefficients[1]+ full.lm$coefficients[5] *mean(area) +
            full.lm$coefficients[6] *mean(trade) +
            full.lm$coefficients[7] *mean(population) +
            full.lm$coefficients[8] *mean(ethnic)

inters <- const + full.lm$coefficients[4] * c(0,1,2,3)
slopes <- full.lm$coefficients[3] + full.lm$coefficients[2]*c(0,1,2,3)

plot(x=socialism, y=threshold, main = "Our Model")
for (i in 1:4) {
  abline(a=inters[i], b=slopes[i], col = i, lty =i)
}
legend(x="topright",col=c("black","red","green","blue"),lty = 2,
       legend=c("p = 0","p = 1", "p = 2", "p = 3"), bty="n")

## let's see what it would look like under boix's original model

const <- my.lm$coefficients[1]+ my.lm$coefficients[3]*mean(area) +
            my.lm$coefficients[4] *mean(trade) +
            my.lm$coefficients[5] *mean(population) +
            my.lm$coefficients[6] *mean(ethnic)

inters <- const 
slopes <- my.lm$coefficients[2]*c(0,1,2,3)


plot(x=socialism, y=threshold, main = "Boix's Model")
for (i in 1:4) {
  abline(a=inters, b=slopes[i], col = i, lty =i)
}
legend(x="topright",col=c("black","red","green","blue"), lty=1,
       legend=c("p = 0","p = 1", "p = 2", "p = 3"), bty="n")


#######
## D ##
#######

## boix's model is this:

second.lm <- lm(threshold ~ threat +  ethnic:area.dummy)
summary(second.lm)

full.second.lm <- lm(threshold ~ threat + socialism + parties+ ethnic:area.dummy + ethnic + area.dummy)
summary(full.second.lm)

detach(boix)

############################################## problem 2

w  <- seq(1,20,1)
y <- w

## no treatment

plot(w,y, pch = 2, main = "No Treatment")
abline(lm(y~w))
points(w+.5, y+.5)

## const treatment
plot(w,y, main = "Constant Treatment")
abline(lm(y~w))
points(y+3, pch = 2)
abline(lm((y+3)~w))

## increasing treatment
plot(w,y, main = "Increasing Treatment")
abline(lm(y~w))
points(y+.5*w, pch = 2)
abline(lm((y+.5*w)~w))

############################################## problem 3

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


