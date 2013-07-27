

makerw <- function(n) {
# Choose the length of series and set phi1 to 1 to get a unit root
phi1 <- 1

# Initialize y
y <- rep(0,n)

# Simulate periods 2 to n
for (i in 2:n)
    y[i] <- phi1*y[i-1] + rnorm(1)

y
}

lengthRW <- 1000
nRW <- 10000
keeplast <- NULL
for (i in 1:nRW) {

  y <- makerw(lengthRW)

  keeplast <- c(keeplast,y[lengthRW])
  
}
print(mean(keeplast))
print(sd(keeplast))
