
###############################################
## Day 1: Example: Rainfall changed by spraying chemicals?
###############################################
rainfall<-read.table("rainfall.txt",header=TRUE)
summary(rainfall)
plot(rainfall)
rainfall$code<-as.factor(rainfall$code)
boxplot(rain~code,data=rainfall)

tmp<-split(rainfall$rain,rainfall$code)
par(mfrow=c(2,1))
qqnorm(tmp[[1]])
qqline(tmp[[1]])
qqnorm(tmp[[2]])
qqline(tmp[[2]])
var.test(rain~code,data=rainfall)

boxplot(rain~code,data=rainfall)
boxplot(log(rain)~code,data=rainfall)

qqnorm(log(tmp[[1]]))
qqline(log(tmp[[1]]))
qqnorm(log(tmp[[2]]))
qqline(log(tmp[[2]]))

var.test(log(rain)~code,data=rainfall)
t.test(log(rain)~code,data=rainfall)
wilcox.test(log(rain)~code,data=rainfall)
