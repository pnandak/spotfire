## 2001 Problem Set 3

## First we implement our log-likelihood as a function:

ll.binom <- function(par, T, Y){#par is the parameter, T is the number
	#of trials for each observation and Y is the vector of successes in each 
      #of N trials
  k <- par[1] #select the first parameter
  k <- pnorm(k) #and reparameterize it to be between 0 and 1
  out <- sum(log(k^Y * (1-k)^(T - Y))) #calculate the log likelihood
  return(out)
}

set.seed(12345)

##Now we draw 2 samples, one of size 100, one of size 1000, both with size 10
##and probability of success .75

draw.100 <- rbinom(n = 100, size = 10, prob = .75)
draw.1000 <- rbinom(n = 1000, size = 10, prob = .75)

##Using our log likelihood ll.binom, we maximize the log likelihood,
##remembering that the maximum occurs at a value which we reparameterized
##so we need to convert the maximum in the same way our log likelihood 
##converts the maximum (with pnorm() )

opt.100 <- optim(par = 2, fn = ll.binom, Y = draw.100, T = 10, method = "BFGS",
   control=list(fnscale=-1))
opt.100
pnorm(opt.100$par)  #counteracting the reparameterization
#which returns 0.751

##We repeat the process for the sample of size 1000

opt.1000 <- optim(par = 2, fn = ll.binom, Y = draw.1000, T = 10, method = "BFGS",
   control=list(fnscale=-1))
opt.1000
pnorm(opt.1000$par) #counteracting the parameterization
# which returns .745


##To plot the log likelihood, first we evaluate the ll at a sequence of points

out <- seq(-3, 3, by = 0.01) #sequence of parameter values
store.100 <-store.1000 <-c() #holders for the values

for(i in 1:length(out)){
store.100[i]<-ll.binom(par = out[i], T = 10, Y = draw.100)
store.1000[i]<- ll.binom(par = out[i], T = 10, Y = draw.1000)
}

##And now we plot the log likelihood of the sample with 100 draws and add the 
##ll of the sample with 1000 draws with lines()

pdf(file = "2001_hw3_plot1.pdf")
par(mfrow = c(1,1))
plot(store.100~pnorm(out), type="l", col="red", lwd=2, lty = 1, xlab = 
   expression(paste(pi)), ylab = expression(paste("Log-likelihood of  " , pi)), 
ylim = c(-5000, 500))

#We can shift the next curves by adding the distance between the max
#of the first plotted curve and the max of the next curve

lines(I(store.1000 + (max(store.100) - max(store.1000)))~pnorm(out), col="purple", 
   lwd=2, lty = 2)
abline(v = .75, lty = 3)
legend(x = "topright", legend = c("N = 100", "N = 1000"), 
   lty = c(1,2), col = c("red", "purple"), lwd = c(2,2), bg = "white")
dev.off()



