library(boot) # contains certain cross validation functions.
library(lars) # contains lars
library(MASS) # contains lm.ridge
# ----- Building Prices

building=read.table("BuildingPrices.txt",header=T)
attach(building)

library(leaps)

# ----- step-wise, all subsets

# start with simplest model.
building.lm1=lm(Y~1);
building.stp = step(building.lm1,data=building,scope=list(upper=~X1+X2+X3+X4+X5+X6+X7+X8+X9,lower=~1),direction="both")

# try another starting point.
building.lm1=lm(Y~X3+X4+X5+X6);
building.stp = step(building.lm1,scope=list(upper=~X1+X2+X3+X4+X5+X6+X7+X8+X9,lower=~1),direction="both")

# all subsets.

X <- model.matrix(lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9, data=building))[,-1]

building.lps=leaps(x=X,y=building$Y,nbest=5,method="C")
plot(building.lps$size, building.lps$Cp, pch=23, bg='orange', cex=2)
abline(0,1,col="blue")
building.best.Cp <- building.lps$which[which((building.lps$Cp == min(building.lps$Cp))),]
print(building.best.Cp)
#
#building.lps.R2=leaps(x=X,y=building$Y,nbest=5,method="r2")
#plot(building.lps.R2$size, building.lps.R2$r2, pch=23, bg='orange', cex=2)
#building.best.R2 <- building.lps.R2$which[which((building.lps.R2$r2 == max(building.lps.R2$r2))),]
#print(building.best.R2)
#
#building.lps.ADJR2=leaps(x=X,y=building$Y,nbest=5,method="adjr2")
#plot(building.lps.ADJR2$size, building.lps.ADJR2$adjr2, pch=23, bg='orange', cex=2)
#building.best.ADJR2 <- building.lps.ADJR2$which[which((building.lps.ADJR2$adjr2 == max(building.lps.ADJR2$adjr2))),]
#print(building.best.ADJR2)


# ----- Ridge regression -----
building.ridge = lm.ridge(Y ~ ., data=building,
                   lambda = seq(0,12,0.1))
select(building.ridge)

#jpeg("building_ridge.jpg",height=600,width=600)
plot(building.ridge,xlab="lambda",ylab="t(beta)",lwd=2)
#dev.off()

plot(building.ridge$lambda,building.ridge$GCV)

# ----- lars -----

X <- model.matrix(lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9, data=building))
building.lars = lars(X,building$Y,type="lar",trace=TRUE)
plot(building.lars)

#jpeg("building_larscp.jpg",height=600,width=600)
plot(building.lars$Cp)
Cpmin = which.min(building.lars$Cp)
keepvars = seq(0:9)[building.lars$entry<=Cpmin]
keepvars

#dev.off()


#jpeg("building_lars.jpg",height=600,width=600)
plot(building.lars)
#dev.off()

# ----- Cross validation on LARS
#jpeg("building_lars_cv.jpg",height=600,width=600)
building.lars.cv=cv.lars(x=X,y=Y)
#dev.off()

