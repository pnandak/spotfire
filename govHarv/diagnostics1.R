#
# http://jsekhon.fas.harvard.edu/gov1000/diagnostics1.R
#

data.approval <-
read.table(file="http://www.courses.fas.harvard.edu/~gov1000/Data/approval.asc",header=T)

#what are the variables in the approval data set?
names(data.approval)
attach(data.approval)

lm4  <- lm(approval~unrate+inflation+lagApproval)
summary(lm4)
lm5  <- lm(approval~unrate+inflation+lagApproval+kennedy+johnson+nixon+ford+carter+regan+bush+clinton)
summary(lm5)

plot(lm4$residual,ylab="Residuals",xlab="Observation Number")
hist(lm4$residual,xlab="Residuals")
plot(approval,lm4$residual,xlab="Approval",ylab="Residual")

#scale the residuals to have variance 1
lm4.residual.scaled  <- lm4$residual/sd(lm4$residual)
plot(lm4.residual.scaled,ylab="Scaled Residuals",xlab="Observation Number")
hist(lm4.residual.scaled,xlab="Scaled Residuals")
plot(approval,lm4.residual.scaled,xlab="Approval",ylab="Scaled Residual")

qqnorm(lm4$residual)
qqline(lm4$residual)

plot(lm5$residual,ylab="Residuals",xlab="Observation Number")
hist(lm5$residual,xlab="Residuals")
plot(approval,lm5$residual,xlab="Approval",ylab="Residual")

#scale the residuals to have variance 1
lm5.residual.scaled  <- lm5$residual/sd(lm5$residual)
plot(lm5.residual.scaled,ylab="Scaled Residuals",xlab="Observation Number")
hist(lm5.residual.scaled,xlab="Scaled Residuals")
plot(approval,lm5.residual.scaled,xlab="Approval",ylab="Scaled Residual")

qqnorm(lm5$residual)
qqline(lm5$residual)


