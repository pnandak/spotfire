solar.radiation <- c(11.1, 10.6 ,  6.3 ,  8.8 ,  10.7 ,  11.2 ,  8.9 ,  12.2)

summary(solar.radiation)
var(solar.radiation)

sr10 <- solar.radiation + 10

summary(sr10)
var(sr10)

srm2 <- solar.radiation*(-2)
summary(srm2)
var(srm2)

par(mfrow=c(1,3))
hist(solar.radiation)
hist(sr10)
hist(srm2)