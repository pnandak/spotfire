
library(lars) # contains lars

# ----- Building Prices

building=read.table("BuildingPrices.txt",header=T)
attach(building)

# ----- lars -----

X <- model.matrix(lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9-1, data=building))
building.lars = lars(X,building$Y,type="lar",trace=TRUE)
plot(building.lars)

#jpeg("building_larscp.jpg",height=600,width=600)
plot(c(0:9),building.lars$Cp, pch=15,col="blue",cex=2)
keepvars = seq(1:9)[building.lars$entry<=2]
keepvars

#dev.off()


# ----- Cross validation on LARS
#jpeg("building_lars_cv.jpg",height=600,width=600)
building.lars.cv=cv.lars(x=X,y=Y)
#dev.off()


# ---- Flu data set -----

flu=read.table("Flu.txt",header=T)
attach(flu)
pairs(flu)
flu.glm <- glm(Shot ~ Age + Health.Aware,family=binomial(link='logit'))
print(summary(flu.glm))

flu.noAge.glm = glm(Shot~Health.Aware,family=binomial(link="logit"))
print(anova(flu.noAge.glm, flu.glm,test="Chisq"))

% sequential anova tests
print(anova(flu.glm,test="Chisq"))
print(anova(flu.glm,test="Cp"))


% get residuals
flu.glm.devres=residuals(flu.glm,type="deviance")
flu.glm.pres=residuals(flu.glm,type="pearson")

par(mfrow=c(3,1))
plot(flu.glm.devres,type="h")
plot(flu.glm.pres,type="h")
plot(hatvalues(flu.glm),type="h")


