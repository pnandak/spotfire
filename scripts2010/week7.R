#### Supervisor Performance Data
attach( supervisor <- read.csv("supervisor.csv") )
pairs(supervisor, col=4, pch=20)
summary( bosslm <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 ) )
summary( bosslm2 <- lm(Y ~ X1 + X2) )
anova(bosslm2, bosslm)
anova(bosslm1 <- lm(Y~X1), bosslm2)
        
par(mfrow=c(1,3))
plot(X2,X3,col=2,pch=20, main=paste("r =", round(cor(X2,X3),1)))
plot(X2,X4,col=2,pch=20, main=paste("r =", round(cor(X2,X4),1)))
plot(X3,X4,col=2,pch=20, main=paste("r =", round(cor(X3,X4),1)))

summary(lm(Y~ X2 + X3 + X4))
summary(lm(Y ~ X2))
summary(lm(Y ~ X3))
summary(lm(Y ~ X4))


#### GPA/Score data
attach( grades <- read.csv("grades.csv") )
summary( lm(MBAGPA ~ BachGPA) )
summary( lm(MBAGPA ~ BachGPA*Age - Age) )


#### Census income data.
census <- read.csv("census2000.csv")
# only include folks working more than 500 hours AND earning more than $5000 AND less than age 60
workers <- (census$hours > 500)&(census$income > 5000)&(census$age < 60) 
log.WR <- log(census$income/census$hours)[workers]
age <- census$age[workers]
sex <- census$sex[workers]

par(mfrow=c(1,2))
boxplot(log.WR[sex=="M"] ~ age[sex=="M"], col=5, main="Male Income Curve", xlab="age", ylab="log wage rate", ylim=c(0,6))
boxplot(log.WR[sex=="F"] ~ age[sex=="F"], col=6, main="Female Income Curve", xlab="age", ylab="log wage rate", ylim=c(0,6))

malemean <- tapply(log.WR[sex=="M"], age[sex=="M"], mean) # tapply gets the mean wage at each age level
femalemean <- tapply(log.WR[sex=="F"], age[sex=="F"], mean)
par(mfrow=c(1,1))
plot(18:59, malemean, type="l", lwd=2, col=4, xlab="age", ylab="mean log wage rate", main="", xlim=c(19,60), ylim=c(1.8,3))
lines(18:59, femalemean, lwd=2, col=6)
text(x = rep(60,2), y = c(malemean[42],femalemean[42]), labels=c("M","F"), col=c(4,6))

summary( wagereg1 <- lm(log.WR ~ age) )
grid <- 18:59
plot(grid, wagereg1$coef[1] + wagereg1$coef[2]*grid, type="l", lwd=2,
     main="", xlab="age", ylab="predicted log wagerate") 

summary( wagereg2 <- lm(log.WR ~ age + sex) )
plot(grid, wagereg2$coef[1] + wagereg2$coef[2]*grid + wagereg2$coef[3], type="l", lwd=2, col=4,
     main="", xlab="age", ylab="predicted log wagerate", ylim=c(2,3.1)) 
lines(grid, wagereg2$coef[1] + wagereg2$coef[2]*grid, lwd=2, col=6)
legend("topleft", col=c(4,6), lwd=4, legend=c("M","F"), bty="n")

summary( wagereg3 <- lm(log.WR ~ age*sex) )
plot(grid, wagereg3$coef[1] + (wagereg3$coef[2]+wagereg3$coef[4])*grid + wagereg3$coef[3],
     type="l", lwd=2, col=4, main="", xlab="age", ylab="predicted log wagerate", ylim=c(2.2,3.2)) 
lines(grid, wagereg3$coef[1] + wagereg3$coef[2]*grid, lwd=2, col=6)
legend("topleft", col=c(4,6), lwd=4, legend=c("M","F"), bty="n")

age2 <- age*age
summary( wagereg4 <- lm(log.WR ~ age*sex + age2 ) ) 
plot(grid, wagereg4$coef[1] + (wagereg4$coef[2]+wagereg4$coef[5])*grid +  wagereg4$coef[3] + wagereg4$coef[4]*grid^2 ,
     type="l", lwd=2, col=4, main="", xlab="age", ylab="predicted log wagerate", ylim=c(2,3)) 
lines(grid, wagereg4$coef[1] + wagereg4$coef[2]*grid + wagereg4$coef[4]*grid^2, lwd=2, col=6)
legend("topleft", col=c(4,6), lwd=4, legend=c("M","F"), bty="n")

summary( wagereg5 <- lm(log.WR ~ age*sex + age2*sex) )
plot(grid, wagereg5$coef[1] + (wagereg5$coef[2]+wagereg5$coef[5])*grid +  wagereg5$coef[3] + (wagereg5$coef[4]+wagereg5$coef[6])*grid^2 ,
     type="l", lwd=2, col=4, main="", xlab="age", ylab="log wagerate", ylim=c(2,3)) 
lines(grid, wagereg5$coef[1] + wagereg5$coef[2]*grid + wagereg5$coef[4]*grid^2, lwd=2, col=6)
legend("topleft", col=c(4,6), lwd=2, legend=c("M fitted","F fitted"), bty="n")
lines(grid, malemean, col=4, lty=2)
lines(grid, femalemean, col=6, lty=2)
legend("bottomright", col=c(4,6), lwd=2, lty=2, legend=c("M data mean","F data mean"), bty="n")


edu <- census$education[workers]
summary( edureg <- lm(log.WR ~ edu*age) )
summary( edureg2 <- lm(log.WR ~ edu*age - edu) )
summary( edureg3 <- lm(log.WR ~ edu*age - age) )
