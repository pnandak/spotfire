#lab 12
#REVISIT MULTIPLE REGRESSION
    #DUMMY
    #HIGHER ORDER TERM
    #INTERACTION
    #PLOT PREDICTED VALUES

#read in data: NES 2001 FILE
#convert.factor=FALSE asks R not to bring over the data dictionary

library(foreign)
library(car)

nesda<-read.dta("C:/Users/Iris/Documents/PS231A/03740-0001-Data.dta", convert.factor=FALSE)
dim(nesda) #check dimension
names(nesda) #check out the var names

attach(nesda)

#variables we need for this model
#v025057: conservative feeling therometer  (dep variable, ranges 0 to 100)
#v023131: education 7 categories
#v023153: sex
#v023138: religion #00-none; 01-protestant; 02-roman catholic; 03--jewish; 07-other

#create new variables, attach to dataset
cvft<-V025057
educ7<-V023131
sex<-V023153
relig<-V023138
libcon<-V023022
age<-V023126x
attend<-recode(V023085, "1=1; 2=.75; 3=.5; 4=.25; 5=0")
table(attend, V023085)

#~~~~~~~~~~~~~~~~~~~~~~~
#linear model
#~~~~~~~~~~~~~~~~~~~~~~~
m1<-lm(cvft~educ7+sex+libcon+age+attend)
summary(m1)

#plot m1
plot(educ7, cvft, col="white")
pred.frame<-data.frame(educ7=seq(1,7,1), sex=1, libcon=4, age=45, attend=3) #male, middle-of-road
pc<-predict(m1, newdata=pred.frame, interval="conf", level=.975)
matlines(pred.frame$educ7, pc, lty=c(1,3,3), lwd=c(2,1.5, 1.5), col=c("blue", "black", "black"))

#~~~~~~~~~~~~~~~~~~~~~~~
#linear model--creating dummies for religion
#~~~~~~~~~~~~~~~~~~~~~~~
m2<-lm(cvft~educ7+sex+libcon+age+attend+factor(relig))
summary(m2)

#plot dummies

relig1<-predict(m2, data.frame(attend=seq(0,1,.25), educ7=4, sex=1, libcon=4,age=45 ,relig=1))
relig2<-predict(m2, data.frame(attend=seq(0,1,.25), educ7=4, sex=1, libcon=4,age=45 ,relig=2))
relig3<-predict(m2, data.frame(attend=seq(0,1,.25), educ7=4, sex=1, libcon=4,age=45 ,relig=3))
relig7<-predict(m2, data.frame(attend=seq(0,1,.25), educ7=4, sex=1, libcon=4,age=45 ,relig=7))

plot(1:5, relig1, col="red", type='l', ylim=c(45,65), xlab="church attendance")
lines(1:5, relig2, col="blue", lty=4)
lines(1:5, relig3, col="purple", lty=5)
lines(1:5, relig7, col="dark green", lty=6)
legend("bottomright", c("red=protestant", "blue=catholic", "purple=jewish", "green=other"))


#~~~~~~~~~~~~~~~~~~~~~~~
#quadratic term to capture non-linear relationship
#~~~~~~~~~~~~~~~~~~~~~~~
m3q<-lm(cvft~educ7+age+attend+I(attend^2)+sex+libcon+factor(relig))
summary(m3q)

#plot with and without quadratic term
noquad<-predict(m2, data.frame(attend=seq(0,1,.25), educ7=4, sex=1, libcon=4,age=45 ,relig=1))
quad<-predict(m3q, data.frame(attend=seq(0,1,.25), educ7=4, sex=1, libcon=4,age=45 ,relig=1))

plot(1:5, noquad, type='l', col="red", ylim=c(50,65), xlab="attendance", ylab="predicted CVFT")
lines(1:5, quad, col="blue")

#~~~~~~~~~~~~~~~~~~~~~~~
#interaction term
#~~~~~~~~~~~~~~~~~~~~~~~
inact<-lm(cvft~educ7+sex+libcon+age+attend+factor(relig)+factor(relig)*attend)
summary(inact)

noquad<-predict(m2, data.frame(attend=seq(0,1,.25), educ7=4, sex=1, libcon=4,age=45 ,relig=3))
interact<-predict(inact, data.frame(attend=seq(0,1,.25), educ7=4, sex=1, libcon=4,age=45 ,relig=3))
plot(1:5, noquad, type='l', col="red", ylim=c(45,75), xlab="attendance", ylab="predicted CVFT")
lines(1:5, interact, col="blue")
legend("bottomright", c("red=no interaction", "blue=interaction jewish*attend"))

