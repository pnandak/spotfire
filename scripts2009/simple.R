
heights.table <- read.table('http://www-stat.stanford.edu/~jtaylo/courses/stats191/data/heights.table', header=T, sep=',')
attach(heights.table)


wife.lm <- lm(WIFE ~ HUSBAND)

# Compute coefficients by hand to make sure they agree

beta.1.hat <- cov(HUSBAND, WIFE) / var(HUSBAND)
beta.0.hat <- mean(WIFE) - beta.1.hat * mean(HUSBAND)
print(c(beta.1.hat, beta.0.hat))

# Estimate sigma squared -- df.resid are degrees of freedom

sigma.hat <- sqrt(sum(resid(wife.lm)^2) / wife.lm$df.resid)
print(sigma.hat)


print(summary(wife.lm)) # find where beta.0.hat, beta.1.hat and
                        # sigma.hat appear
