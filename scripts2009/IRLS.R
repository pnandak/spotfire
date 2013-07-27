

flu.table <- read.table('http://www-stat.stanford.edu/~jtaylo/courses/stats191/data/flu.table', header=T)

# Iteratively reweighted least squares

niter <- 10

mu <- rep(mean(flu.table$Shot), length(flu.table$Shot))

g <- function(x) {
  return(log(x / (1 - x)))
}

g.prime <- function(x) {
  return(1/(x*(1-x)))
}

g.inv <- function(y) {
  return(exp(y) / (1 + exp(y)))
}

V <- function(mu) {
  return(mu*(1-mu))
}

for (i in 1:niter) {
  Z <- g(mu) + g.prime(mu) * (flu.table$Shot - mu)
  W <- 1 / (g.prime(mu)^2 * V(mu))
  Z.lm <- lm(Z ~ Age + Health.Aware, weights=W, data=flu.table)
  eta <- predict(Z.lm)
  mu <- g.inv(eta)
  beta <- Z.lm$coef
 }


X <- model.matrix(Z.lm)
SE.beta <- sqrt(diag(solve(t(X) %*% diag(W) %*% X)))
T.beta <- beta / SE.beta

print(beta)
print(SE.beta)
print(T.beta)


        png("logitcurve.png", height=600, width=600)
        
x = seq(0.01,0.99,length=200)
plot(x, g(x), lwd=2, type='l', col='red')

        dev.off()
        
        png("invlogitcurve.png", height=600, width=600)
        
p = seq(g(0.01), g(0.99), length=200)
plot(p, g.inv(p), lwd=2, type='l', col='red')

        dev.off()
        