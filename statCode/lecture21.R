library(RWinEdt)
library(MASS)
library(epitools)
library(lattice)
options(width=75)

# McCullagh and Nelder, section 8.4.1

temp <- read.table("GLM298_dat.txt", header=F)
temp <- as.matrix(temp)

temp2 <- rbind(temp[,1:2], temp[,3:4], temp[,5:6], temp[,7:8])

policy <- rep(c(rep("17-20",4), rep("21-24",4), rep("25-29",4), rep("30-34",4),
              rep("35-39",4), rep("40-49",4), rep("50-59",4), rep("60+",4)), 4)
group <- rep(c("A","B","C","D"),32)
vehicle <- factor(c(rep("0-3",32), rep("4-7",32), rep("8-9",32), 
                   rep(">10",32)), levels=c("0-3","4-7","8-9",">10"))
claims <- data.frame(claim=temp2[,1], m = temp2[,2], policy=policy,
                       group=group, vehicle=vehicle)
claims2 <- claims[claims$m > 0,]

postscript("../claims.data.eps", width=10, height=6, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")
par(mfrow=c(1,3))

boxplot(claim ~ policy, data=claims2, xlab="Policyholder Age",
  ylab="Average Claim")
boxplot(claim ~ group, xlab="Car Group",
  ylab="Average Claim", data=claims2)
boxplot(claim ~ vehicle, xlab="Vehicle Age",
  ylab="Average Claim", data=claims2)
dev.off()


claims.norm <- glm(claim ~ policy + group + vehicle, family=gaussian(),
                    data=claims, weight = m, subset = m > 0)
summary(claims.norm)

postscript("../claims.norm.eps", width=6, height=4, horiz=F)
par(mar=c(4,4,2,1) + 0.1, pty="m")
par(mfrow=c(1,1))

plot(fitted(claims.norm),resid(claims.norm), xlab="Fitted Average Claim",
  ylab="Residual", main="Normal Model - Identity Link")

dev.off()

claims.inv <- glm(claim ~ policy + group + vehicle, family=Gamma(),
                    data=claims, weight = m, subset = m > 0)
summary(claims.inv)

influence.measures(claims.inv)


claims.inv2 <- glm(claim ~ (policy + group + vehicle)^2, family=Gamma(),
                    data=claims, weight = m, subset = m > 0)
summary(claims.inv2)

anova(claims.inv, claims.inv2, test='F')

claims.inv3 <- glm(claim ~ policy + group + vehicle + policy:group, 
        family=Gamma(), data=claims, weight = m, subset = m > 0)
summary(claims.inv3)

anova(claims.inv, claims.inv3, test='F')

cbind(resid(claims.inv)^2,2*claims2[,2]*(log(claims2[,1]/fitted(claims.inv))-(claims2[,1]-fitted(claims.inv))/fitted(claims.inv)))
cbind(resid(claims.inv,type="pearson")^2,claims2[,2]*((claims2[,1]-fitted(claims.inv))/fitted(claims.inv))^2)

postscript("../claims.inv.eps", width=8, height=4, horiz=F)
par(mar=c(4,4,2,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(fitted(claims.inv),resid(claims.inv), xlab="Fitted Average Claim",
  ylab="Deviance Residual", main="Gamma Model - Inverse Link")
plot(fitted(claims.inv),resid(claims.inv,type="pearson"), 
  xlab="Fitted Average Claim", ylab="Pearson Residual", 
  main="Gamma Model - Inverse Link")

dev.off()

claims.log <- glm(claim ~ policy + group + vehicle, family=Gamma(link='log'),
                    data=claims, weight = m, subset = m > 0)
summary(claims.log)

postscript("../claims.log.eps", width=8, height=4, horiz=F)
par(mar=c(4,4,2,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(fitted(claims.log),resid(claims.log), xlab="Fitted Average Claim",
  ylab="Residual", main="Gamma Model - Log Link")
plot(fitted(claims.log),resid(claims.log,type="pearson"), 
  xlab="Fitted Average Claim", ylab="Pearson Residual", 
  main="Gamma Model - Log Link")

dev.off()

claims.ident <- glm(claim ~ policy + group + vehicle, family=Gamma(link='identity'),
                    data=claims, weight = m, subset = m > 0)
summary(claims.ident)

postscript("../claims.ident.eps", width=8, height=4, horiz=F)
par(mar=c(4,4,2,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(fitted(claims.ident),resid(claims.ident), xlab="Fitted Average Claim",
  ylab="Residual", main="Gamma Model - Identity Link")
plot(fitted(claims.ident),resid(claims.ident,type="pearson"), 
  xlab="Fitted Average Claim", ylab="Pearson Residual", 
  main="Gamma Model - Identity Link")

dev.off()

postscript("../claims.comp.eps", width=10, height=5, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="s")
par(mfrow=c(1,3))

plot(fitted(claims.ident),fitted(claims.inv), xlab="Fits - Identity Link",
  ylab="Fits - Inverse Link", xlim=c(90,510), ylim=c(90,510))
abline(a=0,b=1)
plot(fitted(claims.ident),fitted(claims.log), xlab="Fits - Identity Link",
  ylab="Fits - Log Link", xlim=c(90,510), ylim=c(90,510))
abline(a=0,b=1)
plot(fitted(claims.log), fitted(claims.inv), ylab="Fits - Inverse Link",
  xlab="Fits - Log Link", xlim=c(90,510), ylim=c(90,510))
abline(a=0,b=1)
dev.off()
