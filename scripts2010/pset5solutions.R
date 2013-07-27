########################################### hi.
## problem set 5 solution key
## gov2k
## fall 2009
## prepared by msen :)

########################################### Problem 1

###### D

## an LM function that does this could look like


ols <- function(y,x) {
  beta1 <- cov(x,y)/var(x)
  beta0 <- mean(y) - beta1*mean(x)
  return(list(intercept=beta0,slope=beta1))
}

## OR, more explcitly:


ols2 <- function(y,x) {
  beta1 <- sum((x-mean(x))*(y-mean(x)))/sum((x-mean(x))^2)
  beta0 <- mean(y) - beta1*mean(x)
  return(list(intercept=beta0,slope=beta1))
}

###### E

load("suicides.RData")

my.lm <- ols(suicides$rate, suicides$unemployment)
my.lm


pdf(file= "1e.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
plot(suicides$unemployment, suicides$rate)
abline(a = my.lm$intercept, b = my.lm$slope)
dev.off()

###### F

my.lm$intercept + my.lm$slope * 9.8

########################################### Problem 2

load("truth.RData")

###### A

lm2 <- lm(my.data$y ~ my.data$x)

###### B

set.seed(12345)

sims <- 1000
size <- 50
holder <- matrix(data = NA, ncol = 2, nrow = sims)
colnames(holder) <- c("intercept", "slope")
for(i in 1:sims){
	my.samp <- my.data[sample(nrow(my.data),size),]
	samp.lm <- lm(my.samp$y ~ my.samp$x)
	holder[i,1] <- samp.lm$coefficients[1]
	holder[i,2] <- samp.lm$coefficients[2]
}

###### C

## to the marginal density distributions:

par(mfrow = c(1,1))
pdf(file= "2Ca.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
plot(density(holder[,1]), main = "density distribution, intercept")
dev.off()
pdf(file= "2Cb.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
plot(density(holder[,2]), main = "density distribution, intercept")
dev.off()

###### D

## to get the joint density distribution

par(mfrow = c(1,1))

pdf(file= "2d.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
plot(my.data$x,my.data$y, main = "Sampling Distribution of Regression Lines")
segments(x0=0, x1=100, y0=holder[1:100,1], y1=(holder[1:100,1] + 100*holder[1:100,2]), col="red")
dev.off()

########################################### Problem 3

load("CEO.Rdata")
USA <- money[issp$country == "USA United States",,]
CEO <- data.frame(USA[,"CEO",]) # note two commas

###### A

ols <- function(y,x) {

  beta1 <- cov(x,y)/var(x)
  beta0 <- mean(y) - beta1*mean(x)

  estimates <- matrix(data = NA, nrow = 2, ncol = 2)
  rownames(estimates) <- c("intercept", "slope")
  colnames(estimates) <- c("coefficient", "std error")

  estimates[1,1] <- beta0
  estimates[2,1] <- beta1

  xMinXBar <- x - mean(x)
  resids <- y - (beta0 + beta1*x)
  sigHat <- sqrt(sum(resids^2)/(length(y)-2))
  se1 <- sigHat / sqrt(sum(xMinXBar^2))
  se0 <- sqrt((sigHat^2 * sum(x^2))/(length(y)*sum(xMinXBar^2)))

  estimates[1,2] <- se0
  estimates[2,2] <- se1

 return(data.frame(estimates))

}


###### B

out <- ols(CEO$ideal, CEO$perceived)
test <- lm(CEO$ideal ~ CEO$perceived)


pdf(file= "3B.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
plot(CEO$perceived, CEO$ideal)
abline(a = out[1,1], b = out[2,1])
dev.off()


###### C

z95 <- qt(.975,length(CEO$perceived)-2)
out$coefficient[1] - z95*out$std.err[1]
out$coefficient[1] + z95*out$std.err[1]

out$coefficient[2] - z95*out$std.err[2]
out$coefficient[2] + z95*out$std.err[2]

###### D

ruler <- data.frame(x = c(1:632))
predict(lm(CEO$ideal ~ CEO$perceived))
predict(lm(CEO$ideal ~ CEO$perceived), ruler, se.fit = TRUE)

pred.conf <- predict(lm(CEO$ideal ~ CEO$perceived), ruler, interval="confidence")
object <- order(CEO[,1])

pdf(file= "3D.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
plot(CEO$perceived, CEO$ideal, xlab = "perceived earnings", ylab = "ideal earnings", pch = 19)
abline(test)
points(x=CEO[,1][object], y=pred.conf[,2][object], col="darkred", type="l")
points(x=CEO[,1][object], y=pred.conf[,3][object], col="darkred", type="l") 
dev.off()


###### E

tstat <- out$coefficient[2]/out$std.err[2]
tstat
1-pt(tstat,length(CEO$perceived) - 2)

#checked against summary(lm(ideal ~ perceived))


########################################### Problem 4

plot(CEO$perceived, CEO$ideal)
	## linear?

my.lm <- lm(CEO$ideal ~ CEO$perceived)
	## fitting a linear regression model

plot(CEO$perceived, CEO$ideal)
abline(my.lm, col = "darkred")
	## plotting the two together


###### A

library(car)

hatvalues(my.lm)

## more useful is to plot these to get a sense 
## of what's going on

pdf(file= "4A.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
plot(CEO$perceived, hatvalues(my.lm))
abline(h = 4/length(CEO$perceived), col = "darkred")
	## looks like there might be some problems
dev.off()
identify(CEO$perceived, hatvalues(my.lm))

###### B

stud.res <- rstudent(my.lm) 
	#calculates studentized residuals


pdf(file= "4B.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
plot(hatvalues(my.lm), rstudent(my.lm))
	## plots hat values against studentized residuals
abline(h = 2, col = "darkred")
abline(v = 4/length(CEO$perceived), col = "darkred")
dev.off()

## let's take out obs 398

my.lm <- lm(CEO$ideal ~ CEO$perceived)
my.lm2 <- lm(CEO$ideal[-398] ~ CEO$perceived[-398])

pdf(file= "4Ba.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
plot(CEO$perceived, CEO$ideal)
abline(my.lm, col = "cornflowerblue")
abline(my.lm2, col = "purple")
legend(x = "topleft", legend = c("with 398", "without 398"), lty = 1, col = 
       c("cornflowerblue","purple"))
dev.off()



###### C

pdf(file= "4C.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
plot(my.lm,1) 	## here, you are looking for the red line to be relatively flat
dev.off()
  
###### D


pdf(file= "4D.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
plot(my.lm,3) ## Again you want the line to be more or less flat across the plot
		  ## if it's sloped, then we might have non-constant error variance
dev.off()

###### E

## we can look at a histogram of the residuals

par(mfrow = c(1,1))

pdf(file= "4Ea.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
hist(my.lm$residuals)
dev.off()

## or, better yet, we can look at a QQ plot of the residuals
## following the lecture notes, we 
##	1. extract the residuals from the lm output
##	2. sort them
##	3. normalize them
##	4. plot them in a qqplot against a standard normal

residuals <- sort(my.lm$residuals)
residuals <- (residuals - mean(residuals))/sd(residuals)
theoretical <- rnorm(length(residuals))

pdf(file= "4Eb.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
qqplot(theoretical, residuals)
dev.off()

