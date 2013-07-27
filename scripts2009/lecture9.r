# ------ Two-way Random Effects --------
library(nlme)   # this library contains all mixed-effect models, linear and nonlinear.  
                # Here we only use the linear model function "lme".

#library(trellis)  # A library for plotting fancy graphics.

# if your R installation doesn't have trellis, try the following:
library(lattice)


miles = read.table("MilesPGallon.txt",header=T)
miles$Driver=as.factor(miles$Driver)
miles$Car=as.factor(miles$Car)
miles$Trial=as.factor(miles$Trial)
attach(miles)

jpeg("miles_trellis_CAR.jpg",height=600,width=600)
xyplot(MPG~Driver|Car,main="Gasoline Consumption, sorted by car")
dev.off()
jpeg("miles_trellis_Driver.jpg",height=600,width=600)
xyplot(MPG~Car|Driver,main="Gasoline Consumption, sorted by driver")
dev.off()

miles.anova=anova(lm(MPG~Driver*Car,data=miles))
print(miles.anova)
F.driver=miles.anova$Mean[1]/miles.anova$Mean[3]
1-pf(F.driver,miles.anova$Df[1],miles.anova$Df[3])
F.car=miles.anova$Mean[2]/miles.anova$Mean[3]
1-pf(F.car,miles.anova$Df[2],miles.anova$Df[3])

# you can also try the following, but it's not going to 
# give the same answers, because it's assuming a different model.

#miles.lme = lme(MPG~1, data=miles,random=~1|Driver/Car)
#summary(miles.lme)

# ------ Two-way Mixed Effects ---------
pearls = read.table("Pearls.txt",header=T)
pearls$Coats=as.factor(pearls$Coats)
pearls$Batch=as.factor(pearls$Batch)
pearls$Bead=as.factor(pearls$Bead)
attach(pearls)

# You can define a groupedData which records the hierarchical
# nature of data collection (e.g. Batch).
pearls.grouped=groupedData(Value~Coats|Batch,data=pearls)
plot(pearls.grouped)
# "lme" is the mixed effect model analog of "lm".
pearls.lme=lme(Value~Coats,data=pearls.grouped)


#jpeg("pearls_trellis_Batch.jpg",height=600,width=600)
xyplot(Value~Coats|Batch,main="Pearl value, sorted by batch")
#dev.off()

# Another way to do the above.
pearls.lme = lme(Value~Coats, data=pearls,random=~1|Batch)
summary(pearls.lme)
fixed.effects(pearls.lme)

# ---------------------------- #
# Transformations: bacteria
# example.
# ---------------------------- #

bacteria.table <- read.table("Bacteria.txt", header=T)
attach(bacteria.table)

##PLOT: bacteria vs. time -- exponential decay?
#jpeg('bacteria.jpg', height=600, width=600)
plot(bacteria.table, pch=23, cex=2, bg='orange')
#dev.off()
##PLOTEND

# Fit model with untransformed data

bacteria.lm <- lm(N_t ~ t)
plot(bacteria.lm, cex=2, pch=23, bg='orange')
plot(bacteria.table, pch=23, cex=2, bg='orange')
lines(t, fitted(bacteria.lm), lwd=2, col='red')

# Fit model with log-transformed data

bacteria.log.lm <- lm(log(N_t) ~ t)
par(mfrow=c(2,2))
plot(bacteria.log.lm, cex=2, pch=23, bg='orange')


par(mfrow=c(1,1))
plot(bacteria.table, pch=23, cex=2, bg='orange')
lines(t, fitted(bacteria.lm), lwd=2, col='red')
lines(t, exp(fitted(bacteria.log.lm)), lwd=2, col='green')
