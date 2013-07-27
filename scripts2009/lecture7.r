# Kidney example

kidney <- read.table("Kidney.txt", header=T)
kidney$Duration <- factor(kidney$Duration)
kidney$Weight <- factor(kidney$Weight)
attach(kidney)

# Plot data: is there visual evidence of interactions?

##PLOT: a graphical check of interactions in two-way ANOVA model. If
##there are interactions, we expect the lines to cross, or have
##different "traces."

jpeg('kidney.jpeg', height=600, width=600)
interaction.plot(Weight, Duration, Days, type='b', col=c('red','blue'),lwd=2, pch=c(23,24))
dev.off()

# Fit model 

kidney.lm <- lm(Days  ~ Duration * Weight)

# The model matrix tells you exactly what is being fitted.
print(model.matrix(kidney.lm))

mat=model.matrix(kidney.lm)
plot(c(1:60),mat[,3],ylim=c(0,2),pch=15,col="blue")
lines(c(1:60),mat[,4],ylim=c(0,2),pch=15,col="red")

# Look at ANOVA table

print(anova(kidney.lm))

# Another way to look at interactions

anova(lm(Days ~ Duration + Weight), kidney.lm)


# --------------- Salary example ------------------


salary <- read.table("Salary.txt", header=T)
salary$E <- factor(salary$E)
salary$M <- factor(salary$M)
attach(salary)


#jpeg('salaries.jpg', height=600, width=600)
plot(X,S, type='n', xlab='Experience', ylab='Salary')
colors <- c('red', 'green', 'blue')
symbols <- c(23,24)
for (i in 1:3) {
for (j in 0:1) {
subset <- as.logical((E == i) * (M == j))
points(X[subset], S[subset], pch=symbols[j+1], bg=colors[i], cex=2)
}
}
#dev.off()


# fit just additive model,
salary.lm <- lm(S ~ E + M + X)

#Check out model matrix.
cbind(salary,model.matrix(salary.lm))

# See ANOVA
print(anova(salary.lm))

# Look at the standardized residuals
#jpeg("salary_stdres.jpg",height=600,width=600)
plot(X,rstandard(salary.lm),xlab="Experience",ylab="Standardized residuals",pch=15,col="red")
#dev.off()

boxplot(rstandard(salary.lm)~E)
boxplot(rstandard(salary.lm)~M)
boxplot(rstandard(salary.lm)~M:E)

 # is there an interaction between Education and Experience?
salary.lm2 <- lm(S ~ E * X + M)
print(anova(salary.lm, salary.lm2))

# between education and Management?
salary.lm3 <- lm(S ~ E * M + X)
print(anova(salary.lm, salary.lm3)) # is there an interaction


# --------------- Personnel example ------------------
personnel=read.table("Personnel.txt",header=T)

n <- 4
r <- 5


personnel$Intervewer=as.factor(personnel$Interviewer)
personnel$Candidate=as.factor(personnel$Candidate)
attach(personnel)

#jpeg("personnel_boxplot.jpg",height=600,width=600)
boxplot(Rating~Interviewer,xlab="Interviewer",ylab="Rating",col="orange")
#dev.off()

print(names(anova(lm(Rating ~ Interviewer))))

# CI for overall mean, get the MS from attributes "Mean Sq",
# using "Mean" is enough -- R completes the rest

MSTR <- anova(lm(Rating ~ Interviewer))$Mean[1]
MSE <- anova(lm(Rating~ Interviewer))$Mean[2]


SE.mean <- sqrt(MSTR / (n*r))
center <- mean(Rating)
U <- center + qt(0.975, r-1) * SE.mean
L <- center - qt(0.975, r-1) * SE.mean


print(data.frame(center,L,U))

# Estimate sigma^2_mu

sigma.alpha <- sqrt((MSTR - MSE)/n)
print(sigma.alpha)

# MSTR is smaller than MSE in this case, set it to 0.
