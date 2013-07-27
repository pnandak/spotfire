library(RWinEdt)
library(MASS)
library(epitools)
library(lattice)
library(lme4)
options(width=75)

# From S-plus

Orthodont <- read.table("orthodont.txt", header=T)

Orthodont

trellis.device("postscript", file="../orthodont.eps",
width=9, height=6.5, horiz=F, col=T)
xyplot(distance ~ age | Subject, data=Orthodont, xlab="Age", ylab="Distance",
  layout=c(9,3))
dev.off()

options(contrasts=c("contr.treatment","contr.poly"))
orthodont.mix <- lmer(distance ~ Sex + age + (1|Subject), data=Orthodont)
orthodont.mix

orthodont.mix2 <- lmer(distance ~ Sex + age + (1|Subject) + (age - 1| Subject), data=Orthodont)
orthodont.mix2

anova(orthodont.mix2,orthodont.mix)

# Miles per gallon (NKNW 24.15)

mpg <- read.table("MPG.txt", header=T)
mpg$Driver <- as.factor(mpg$Driver)
mpg$Car <- as.factor(mpg$Car)
driver <- as.numeric(mpg$Driver)
car <- as.numeric(mpg$Car)

mpgtab <- tapply(mpg$MPG,list(Driver=mpg$Driver, Car=mpg$Car),mean)

postscript("../mpg.eps", width=5.5, height=4, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")
par(mfrow=c(1,1))

plot(MPG ~ as.numeric(Car), data=mpg, xlab="Car", ylab="Miles per Gallon", 
  type="n", pch=16)
for(i in 1:4) {
  points(car[driver == i], mpg$MPG[driver == i], 
    col=i+1, pch=16)
  lines(1:5, mpgtab[i,], col=i+1)
}

dev.off()

options(contrasts=c("contr.sum","contr.poly"))

mpg.rr <- lmer(MPG ~ 1 + (1|Driver) + (1|Car) + (1|Driver:Car) , data=mpg)
mpg.a.rr <- lmer(MPG ~ 1 + (1|Driver) + (1|Car), data=mpg)
anova(mpg.rr, mpg.a.rr)
mpg.d <- lmer(MPG ~ 1 + (1|Driver), data=mpg)
mpg.c<- lmer(MPG ~ 1 + (1|Car), data=mpg)
anova(mpg.a.rr, mpg.d)
anova(mpg.a.rr, mpg.c)
 
mpg.fr <- lmer(MPG ~ Driver + (1|Car) + (1|Driver:Car) , data=mpg)

mpg.rf <- lmer(MPG ~ Car + (1|Driver) + (1|Car:Driver) , data=mpg)
mpg.rf


mpg.a.rf <- lmer(MPG ~ Car + (1|Driver) , data=mpg)
mpg.a.rf
anova(mpg.a.rf, mpg.rf)

options(contrasts=c("contr.treatment","contr.poly"))
mpg.rf2 <- lmer(MPG ~ Car + (1|Driver) + (1|Car:Driver) , data=mpg)
mpg.rf2

options(contrasts=c("contr.sum","contr.poly"))


mpg.ff <- lm(MPG ~ Driver + Car + Driver:Car, data=mpg)
anova(mpg.ff)
