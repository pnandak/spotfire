#######################
## Regression III    ##
## Lecture 2         ##
## Summer 2010       ##
##                   ##
## author:           ##
## Dave Armtrong     ## 
## UW-Milwaukee      ##
## armstrod@uwm.edu  ##
#######################

###################################################
### chunk number 1: mod
###################################################
library(car)
data(Duncan)
mod <- lm(prestige ~ income + education, data=Duncan)
summary(mod)


###################################################
### chunk number 2: betavar
###################################################
X <- with(Duncan,  cbind(1,income, education))
y <- matrix(Duncan[["prestige"]], ncol=1)
b <- solve(t(X)%*%X)%*%t(X)%*%y 
b
coef(mod)
e <- matrix(y - X%*%b, ncol=1)
Vb <- c((t(e)%*%e)/(nrow(X) - 2 - 1))* solve(t(X)%*%X)
Vb
vcov(mod)


###################################################
### chunk number 3: ftest
###################################################
restricted.mod <- lm(prestige ~ 1, data=Duncan)
anova(restricted.mod, mod, test="F")
xtx <- solve(t(X)%*%X)
s2e <- (t(e) %*% e)/(nrow(X)-3)
F0 <- (t(b[2:3]) %*% solve(xtx[2:3,2:3]) %*% b[2:3])/(2*s2e)
F0
pf(F0, 2, nrow(X)-3, lower.tail=F)


###################################################
### chunk number 4: glht
###################################################
L <- matrix(c(0,0,1,0,0,1), nrow=2)
cmat <- matrix(c(0,0), ncol=1)

F0b <- (t(L%*%b - cmat)%*%solve(L%*%solve(t(X)%*%X)%*%t(L))%*%
  (L%*%b - cmat))/(2*s2e)
F0b
pf(F0b, 2, nrow(X)-3, lower.tail=F)


###################################################
### chunk number 5: varpred
###################################################
x1 <- matrix(c(1, 21, 26), ncol=1)
x2 <- matrix(c(1,64,84), ncol=1)
x3 <- matrix(c(1,41.87, 52.56))

pred1 <- t(x1) %*% b
pred1

pred2 <- t(x2) %*% b
pred2

pred3 <- t(x3) %*% b
pred3

v1 <- s2e * (1+ c(t(x1) %*% solve(t(X)%*% X) %*% x1))
sqrt(v1)

v2 <- s2e * (1+ c(t(x2) %*% solve(t(X)%*% X) %*% x2))
sqrt(v2)

v3 <- s2e * (1+ c(t(x3) %*% solve(t(X)%*% X) %*% x3))
sqrt(v3)


###################################################
### chunk number 6: varpred2
###################################################
vhat1 <- s2e * (c(t(x1) %*% solve(t(X)%*% X) %*% x1)) 
sqrt(vhat1)

vhat2 <- s2e * (c(t(x2) %*% solve(t(X)%*% X) %*% x2))
sqrt(vhat2)

vhat3 <- s2e * (c(t(x3) %*% solve(t(X)%*% X) %*% x3))
sqrt(vhat3)

newdat <- t(cbind(x1, x2, x3))
colnames(newdat) <- c("int", "income", "education")
newdat <- as.data.frame(newdat)

predict(mod, newdat, se.fit=T)


