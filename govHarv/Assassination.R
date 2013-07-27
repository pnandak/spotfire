library(foreign)

assassination <- read.dta("mergeddata.dta")
names(assassination)
unique(assassination$leadername)
attempt <- !is.na(assassination$deadinattempt)
survived <- as.numeric(assassination$result > 19)

assassination <- cbind(assassination, attempt, survived)
names(assassination)
summary(assassination)

vars <- c("country","year","leadername","age","sumten","attempt","survived",
          "result","polity2","zGledCivil","zGledInter",
          "tpop","energy","solo","weapon1")
assassination <- assassination[,vars]
as <- assassination

colnames(as) <- c("country","year","leadername","age","tenure","attempt","survived",
          "result","dem_score","civil_war","war", "pop","energy", 
          "solo","weapon")

unique(as$weapon)
as$weapon[as$weapon == 1] <- "gun"
as$weapon[as$weapon == 2] <- "knife"
as$weapon[as$weapon == 3] <- "explosive device"
as$weapon[as$weapon == 4] <- "poison"
as$weapon[as$weapon == 8] <- "other"
as$weapon[as$weapon == 9] <- "unknown"

# interesting years 
as[as$country == "United States" & as$year == "1975",]

as[as$country == "Spain" & as$year == "1936",]
summary(as)

 
# let's clean up the data for our model
y <- as$attempt
X <- as[,c("tenure","age","dem_score","civil_war","war","pop","energy")]

yX <- na.omit(cbind(y,X))
y <- yX[,1]
X <- yX[,-1]

library(faraway)
kappa(X)

mean.tenure <- mean(X$tenure)
sd.tenure <- sd(X$tenure)
mean.pop <- mean(X$pop)
sd.pop <- sd(X$pop)
mean.energy <- mean(X$energy)
sd.energy <- sd(X$energy)


X$tenure <- (X$tenure-mean(X$tenure))/sd(X$tenure)
X$pop <- (X$pop-mean(X$pop))/sd(X$pop)
X$energy <- (X$energy-mean(X$energy))/sd(X$energy)



X <- as.matrix(cbind(1,X))

# check the precision of log(pnorm()) versus pnorm(,log= TRUE)
log(pnorm(-50))
pnorm(-50, log = TRUE)

# probit log-likelihood
ll.probit <- function(beta, y=y, X=X){
  phi <- pnorm(X%*%beta, sd=10, log = TRUE) 
  opp.phi <- pnorm(X%*%beta, sd=10, log.p = TRUE, lower.tail = FALSE)
  logl <- sum(y*phi + (1-y)*opp.phi)
  return(logl)
}

opt2 <- optim(par = rep(0,8), fn = ll.probit, y = y, X=X, method = "BFGS",
     control = list(fnscale = -1, maxit = 1000), hessian = TRUE) 
ses <- sqrt(diag(solve(-opt$hessian)))
table.dat <- cbind(opt$par, opt2$par)
rownames(table.dat) <- colnames(X)

library(xtable)

xtable(table.dat, digits = 4)

# fitted values 
attempt.vec <- y == 1
highrisk <- apply(X[attempt.vec,], 2, median)
lowrisk <- apply(X[!attempt.vec,], 2, median)

-0.3007831*sd.tenure + mean.tenure
-0.1766399 *sd.pop + mean.pop
-.225*sd.energy + mean.energy


set.seed(12345)
library(MASS)
beta.draws <- mvrnorm(10000, mu = opt$par, Sigma = solve(-opt$hessian))

p.ests <- c()
for(i in 1:10000){ 
  p.ass.att <- pnorm(highrisk%*%beta.draws[i,], sd=10)
  outcomes <- rbinom(10000, 1, p.ass.att)
  p.ests[i] <- mean(outcomes)  
}

savethis <- opt2

par(mfrow=c(1,2))
plot(density(savethese), main="Original Probit")
plot(density(p.ests), main="New Probit")


mean(p.ests) 
quantile(p.ests, .025); quantile(p.ests, .975)

p.ests2 <- pnorm(highrisk%*%t(beta.draws))
mean(p.ests2)
quantile(p.ests2, .025); quantile(p.ests2, .975)

dem.rng <- -10:10
p.ests <- matrix(data = NA, ncol = length(dem.rng),nrow=10000)

for(j in 1:length(dem.rng)){
  highrisk.dem <- highrisk
  highrisk.dem["dem_score"] <- dem.rng[j]
  p.ests[,j] <- pnorm(highrisk.dem%*%t(beta.draws))
}


plot(dem.rng, apply(p.ests,2,mean), ylim = c(0,.028),
     xlab = "Dem Score", ylab = "Probability of Ass. Attempt")
segments(x0 = dem.rng, x1 = dem.rng, 
         y0 = apply(p.ests, 2, quantile, .025), 
         y1 = apply(p.ests, 2, quantile, .975))

		 
		 
# some first differences
highrisk.war <- highrisk
highrisk.war["war"] <- 1
highrisk.nowar <- highrisk
highrisk.nowar["war"] <- 0

fd.ests <- pnorm(highrisk.war%*%t(beta.draws)) - pnorm(highrisk.nowar%*%t(beta.draws))
mean(fd.ests)
quantile(fd.ests, .025); quantile(fd.ests, .975)





