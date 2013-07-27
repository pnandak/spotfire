
###############################################
## Day 1: Example: Humerus length of arm of sparrows
###############################################
hum<-read.table("humerus.txt",header=TRUE)
summary(hum)

hum2<-split(hum$humerus,hum$code)
names(hum2)<-c("doa","alive")
par(mfrow=c(2,1))
qqnorm(hum2$doa)
qqline(hum2$doa)
qqnorm(hum2$alive)
qqline(hum2$alive)
t.test(hum2$doa,hum2$alive)

hum$code<-as.factor(hum$code)
summary(hum)
?t.test
t.test(humerus~code,data=hum)

plot(humerus~code,hum)
wilcox.test(hum2$doa,hum2$alive)
