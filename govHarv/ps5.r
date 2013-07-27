## ps5


## Generate data

x <- rnorm(5000, mean = 20, sd = 4)
error <- rnorm(5000, mean = 0, sd = 1)
y <- 3*x + error

## OLS function

my.ols <- function(x, y){
  estimates <- matrix(NA, nrow = 2, ncol = 2)
  rownames(estimates) <- c("intercept", "slope")
  colnames(estimates) <- c("coefficient", "std error")
  xMinXBar <- x - mean(x)
  yMinYBar <- y - mean(y)
  b1Hat <- sum(xMinXBar*yMinYBar)/sum(xMinXBar^2)
  b0Hat <- mean(y) - b1Hat*mean(x)
  resids <- y - (b0Hat + b1Hat*x)
  sigHat <- sqrt(sum(resids^2)/(length(y)-2))
  seB1 <- sigHat / sqrt(sum(xMinXBar^2))
  seB0 <- sqrt((sigHat^2 * sum(x^2))/(length(y)*sum(xMinXBar^2)))
  estimates[,'coefficient'] <- c(b0Hat, b1Hat)
  estimates[, 'std error'] <- c(seB0, seB1)
  return(estimates)
}

my.ols(x = x, y = y)
summary(lm(y ~ x))

sims <- 1000
mat <- matrix(data = NA, nrow = sims, ncol = 2)

for(i in 1:sims){
  


pakistan <- read.csv("pakistan.csv")

