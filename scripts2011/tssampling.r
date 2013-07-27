# Drawing random ARMA data in R
# Stationary and non-stationary examples
# Chris Adolph

set.seed(123456)

#########################################################
## Sampling from an AR(1), phi_1 = 0.5, using arima.sim()

# Simulate the time series y
y <- arima.sim(list(order = c(1,0,0),
                    ar = 0.50,
                    ma = NULL),
               n=1000)

# Plot the series against time
pdf("ar1of50.pdf",width=6,height=3.25)
plot(y,type="l",col="red",ylab="y",xlab="Time",
     main = expression(paste("Simulated AR(1) process with ",phi[1]," = 0.50"))
     )
abline(a=0,b=0,lty="dashed")
dev.off()

# Plot the ACF
pdf("acfar1of00.pdf",width=6,height=3.25)
acf(y,
    main = expression(paste("ACF of AR(1) process with ",phi[1]," = 0.50"))
    )
dev.off()

# Plot the PACF
pdf("pacfar1of00.pdf",width=6,height=3.25)
pacf(y,
     main = expression(paste("PACF of AR(1) process with ",phi[1]," = 0.50"))
     )
dev.off()



#################################################################
## Sampling from AR(1), phi_1 = 1.00

# arima.sim() doesn't work for non-stationary series!
# so we will code our own simulator

# Choose the length of series and set phi1 to 1 to get a unit root
n <- 1000
phi1 <- 1

# Initialize y
y <- rep(0,n)

# Simulate periods 2 to n
for (i in 2:n)
    y[i] <- phi1*y[i-1] + rnorm(1) 

# Plot the time series against time
pdf("ar1of100.pdf",width=6,height=3.25)
plot(y,type="l",col="red",ylab="y",xlab="Time",
     main = expression(paste("Simulated AR(1) process with ",phi[1]," = 1.0"))
     )
abline(a=0,b=0,lty="dashed")
dev.off()

# Plot the ACF
pdf("acfar1of100.pdf",width=6,height=3.25)
acf(y,
    main = expression(paste("ACF of AR(1) process with ",phi[1]," = 1.0"))
    )
dev.off()

# Plot the PACF
pdf("pacfar1of100.pdf",width=6,height=3.25)
pacf(y,
     main = expression(paste("PACF of AR(1) process with ",phi[1]," = 1.0"))
     )
dev.off()







