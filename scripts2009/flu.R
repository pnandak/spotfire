
        png("flu.png", height=600, width=600)
        


flu.table <- read.table('http://www-stat.stanford.edu/~jtaylo/courses/stats191/data/flu.table', header=T)

pairs(flu.table, cex.labels=3, pch=23,
      bg='orange', cex=2)


        dev.off()
        

flu.glm = glm(Shot ~ Age + Health.Aware, data=flu.table, family=binomial())
print(summary(flu.glm))


        png("fludiag.png", height=600, width=600)
        

# Some standard diagnostics -- not always as useful as least squares

par(mfrow=c(2,2))
plot(flu.glm)
par(mfrow=c(1,1))

library(car)
print(influence.measures(flu.glm))

# Predicted values: there are different types

a = predict(flu.glm, list(Age=c(35,45),Health.Aware=c(50,50)))
print(a)

b = predict(flu.glm, list(Age=c(35,45),Health.Aware=c(50,50)), type="response")
print(b)

# Odds ratios:

print(exp(a[2]-a[1]))

# Ratio of probabilites

print(b[2]/b[1])

# "response" are the inverse logist of the usual predicted values

print(exp(a)/(1+exp(a)))


        dev.off()
        
# The anova function can be used for Deviance tests,
# which give different p-values than the summary table

reduced.glm = glm(Shot ~ Health.Aware, family=binomial(), data=flu.table)
print(anova(reduced.glm, flu.glm))
print(1-pchisq(16.863,1))

# The confidence intervals use the Likelihood, not
# the weighted least squares approximation

print(confint(flu.glm))

center = coef(flu.glm)['Age']
SE = sqrt(vcov(flu.glm)['Age', 'Age'])
U = center + SE * qnorm(0.975)
L = center + SE * qnorm(0.025)
print(data.frame(center, L, U))

# The step function can be easily used for logistic regression

step.glm = step(flu.glm, scope=list(upper=~.^2), direction='both')
print(summary(step.glm))
