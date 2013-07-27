##### Simulation from the SLR model ######

## try running this a few times with
## different data to see how things change...
x <- rnorm(100,0,sqrt(2))
y <- 3 + x + rnorm(100,0,2)
reg <- lm(y~x)
plot(x,y, pch=20, col=grey(.5), ylim=c(-5,10))
xx <- data.frame(x=seq(min(x),max(x), length=10))
lines(xx$x, 3+xx$x)
## subdivide the data
sub1 <- data.frame(x=x[1:10], y=y[1:10])
sub2 <- data.frame(x=x[11:100], y=y[11:100])
reg1 <- lm(y~x, data=sub1)
reg2 <- lm(y~x, data=sub2)
lines(xx$x, predict(reg1, newdata=xx), col=2)
lines(xx$x, predict(reg2, newdata=xx), col=4)
legend("topleft", col=c(2,4), lwd=2, legend=c("n=10","n=90")) 
## (iii): the true marginal mean is 3 + 1*EX = 3
margmean <- 3
samplemean <- mean(y)
cbind(margmean, samplemean)
## the true prediction intervals
lines(xx$x, 3 + xx$x + qnorm(.95, sd=2), lty=2)
lines(xx$x, 3 + xx$x + qnorm(.05, sd=2), lty=2)
## count the number outside the 90% interval
over <- y > 3 + x + qnorm(.95, sd=2)
under <- y < 3 + x + qnorm(.05, sd=2)
x[over]
x[under]
## the proprotion outside
## on average 0.1, but with lots of variability
outside <- (sum(over) + sum(under))/100
## Just for fun, lets label the plot with this
mtext(side=3, line=1, paste("SLR simulation example: ",
                outside*100, "% outside the 90% interval", sep=""))
## mtext adds text to the margins ('line' is how far out, side 3 is top)
## and paste puts text and values together (seperated by 'sep').

##### Classic Height Regression ######

attach( height <- read.csv("MBA-hgt.csv") )
colors <- c(4,6)
par(mfrow=c(1,2))
plot(FHGT, SHGT, pch=20, col=colors[Female+1])
legend("topleft", col=colors, lwd=2, legend=c("male","female"))
plot(MHGT, SHGT, pch=20, col=colors[Female+1])
## It looks like male and female heights
## have different intercepts in each case
print( corrs <- data.frame("Male.Students" =
                    c(cor(SHGT[Female==0],MHGT[Female==0]),
                      cor(SHGT[Female==0],FHGT[Female==0])),
                    "Female.Students" =
                    c(cor(SHGT[Female==1],MHGT[Female==1]),
                      cor(SHGT[Female==1],FHGT[Female==1])),
                    row.names=c("vsMom", "vsDad")) )
## Seems that Mom is the better linear predictor
print(reg.male <- lm(SHGT[Female==0] ~ MHGT[Female==0]))
print(reg.female <- lm(SHGT[Female==1] ~ MHGT[Female==1]))
lines(MHGT[Female==0], reg.male$fitted, col=colors[1])
lines(MHGT[Female==1], reg.female$fitted, col=colors[2])
## The anova results:
anova(reg.male)
print(male.anova <- list(SSR=119.87, SSE=505.98, SST=119.87+505.98))
print(male.R2 <- male.anova$SSR/male.anova$SST)
anova(reg.female)
print(female.anova <- list(SSR=64.168, SSE=169.018, SST=169.018+64.168))
print(female.R2 <- female.anova$SSR/female.anova$SST)
## I am 6ft and my mom is 5'2.  Residual is thus:
72 - (42 + .4494*62)


##### CAPM regression #####

attach( mkt <- read.csv("mktmodel.csv") )
stocks <- mkt[,-1]
plot(SP500, col=0, ## Just get the plot up
     xlab = "Month", ylab = "Returns",
     main = "Monthly returns for 1992-1996",
     ylim=range(unlist(mkt)))
colors <- rainbow(30)  ## 30 different colors
for(i in 1:30){ lines(stocks[,i], col=colors[i], lty=2) }
lines(SP500, lwd=2)
### Get the correlations
mcor <- cor(stocks, SP500)
mcor[order(mcor),] # sort them in increasing order
## The popular saying is that "GE is the market"...
## this is because of the diversity of GE holdings.
#### Get the alphas and betas
mreg <- lm(as.matrix(stocks) ~ SP500)
plot(mreg$coef[2,], mreg$coef[1,], col=0, xlab="beta", ylab="alpha")
text(mreg$coef[2,], mreg$coef[1,], labels = names(stocks))

#### Pairs trading:
# E[return1] - E[return2] = a1 + b1*SP500 - (a2 + b2*SP500)
#                         = a1 - a2 if b1=b2.
# You make the same amount regardless of the market.

## Amex and Wmt have similar betas and a big alpha spread...
buy <- stocks$AXP
sell <- stocks$WMT

### Lets see what we would have made...
# There are a couple of different ways to think about this,
# depending on mechanics of how you went long and short.
###
# I'll first calculate as though you cashed out at the end of each month,
# and then repeated the strategy of buying all the AXP you can afford 
# and immediately short selling the same amount of WMT stock.
# In this case, total return is a product of 1+monthly.returns, minus original $1
print(totalreturn <- prod(1+(buy-sell)) - 1) 
###
# Alternatively, if you just held each long and short stock
# over the entire time period, your the total amount of money
# you made is just [(value of long stock) - $1] + [$1 - (value of short)]
print( totalreturn.hold <- prod(1+buy) - prod(1+sell) ) 

##### monthly returns
returns <- buy-sell
## Our average monthly return 
mean(returns)
## is about the same as the difference in alphas!
lm(stocks$AXP~SP500)$coef[1]-lm(stocks$WMT~SP500)$coef[1]
