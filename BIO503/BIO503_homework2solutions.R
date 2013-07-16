# homework 2 solutions

# question 1 
par(mfrow=c(3,4)) 

qqplot(rnorm(10), rnorm(10), xlab="Quantiles", ylab="Quantiles", col=rainbow(12)[1], pch=1, lwd=2, main="10 Normal RVS, Set 1")
qqplot(rnorm(10), rnorm(10), xlab="Quantiles", ylab="Quantiles", col=rainbow(12)[2], pch=2, lwd=2, main="10 Normal RVS, Set 2")
qqplot(rnorm(10), rnorm(10), xlab="Quantiles", ylab="Quantiles", col=rainbow(12)[3], pch=3, lwd=2, main="10 Normal RVS, Set 3")
qqplot(rnorm(10), rnorm(10), xlab="Quantiles", ylab="Quantiles", col=rainbow(12)[4], pch=4, lwd=2, main="10 Normal RVS, Set 4")

qqplot(rnorm(100), rnorm(100), xlab="Quantiles", ylab="Quantiles", col=rainbow(12)[5], pch=5, lwd=2, main="100 Normal RVS, Set 1")
qqplot(rnorm(100), rnorm(100), xlab="Quantiles", ylab="Quantiles", col=rainbow(12)[6], pch=6, lwd=2, main="100 Normal RVS, Set 2")
qqplot(rnorm(100), rnorm(100), xlab="Quantiles", ylab="Quantiles", col=rainbow(12)[7], pch=7, lwd=2, main="100 Normal RVS, Set 3")
qqplot(rnorm(100), rnorm(100), xlab="Quantiles", ylab="Quantiles", col=rainbow(12)[8], pch=8, lwd=2, main="100 Normal RVS, Set 4")

qqplot(rnorm(1000), rnorm(1000), xlab="Quantiles", ylab="Quantiles", col=rainbow(12)[9], pch=9, lwd=2, main="1000 Normal RVS, Set 1")
qqplot(rnorm(1000), rnorm(1000), xlab="Quantiles", ylab="Quantiles", col=rainbow(12)[10], pch=10, lwd=2, main="1000 Normal RVS, Set 2")
qqplot(rnorm(1000), rnorm(1000), xlab="Quantiles", ylab="Quantiles", col=rainbow(12)[11], pch=11, lwd=2, main="1000 Normal RVS, Set 3")
qqplot(rnorm(1000), rnorm(1000), xlab="Quantiles", ylab="Quantiles", col=rainbow(12)[12], pch=12, lwd=2, main="1000 Normal RVS, Set 4")

# question 2a

cats <- read.table("cats.txt", sep="\t", header=T)

# question 2b
female.cats <- cats[cats[,1] == "F",]
male.cats <- cats[cats[,1] == "M",]

# question 2c
female.mod <- lm(Hwt ~ Bwt, data=female.cats)
male.mod <- lm(Hwt ~ Bwt, data=male.cats) 

# question 2d
summary(female.mod)
summary(male.mod)

# question 2e
new.female.cat <- data.frame(Bwt = 2.5)
new.male.cat <- data.frame(Bwt = 2.9)

predict(female.mod, new.female.cat)
predict(male.mod, new.male.cat)

# question 2f 
par(mfrow=c(1,2))
plot(residuals(female.mod), ylim=range(residuals(female.mod), residuals(male.mod)), pch=3, col="orange", lwd=3, 
	main="Residuals for the Female Cat Model", ylab="Residual Values")
abline(a=0, b=0, lty=3, lwd=3)
plot(residuals(male.mod), ylim=range(residuals(female.mod), residuals(male.mod)), pch=4, col="purple", lwd=3, 
	main="Residuals for the Male Cat Model", ylab="Residual Values")
abline(a=0, b=0, lty=3, lwd=3)

par(mfrow=c(1,2))
qqnorm(residuals(female.mod), lwd=3, pch=3, col="orange", main="QQ Plot for Female Cat Model Residuals")
qqline(residuals(female.mod), lwd=3, col="blue")
qqnorm(residuals(male.mod), lwd=2, col="purple", pch=4, main="QQ Plot for Male Cat Model Residuals")
qqline(residuals(male.mod), lwd=3, col="gold")

# question 2g 

par(mfrow=c(1,2))
plot(female.cats$Bwt, female.cats$Hwt, pch=3, ylim=range(cats$Hwt), lwd=3, col="orange", ylab="Heart Weight (g)", xlab="Body Weight (kg)", 
	main="Female Cats")
abline(female.mod)
plot(male.cats$Bwt, male.cats$Hwt, pch=4, ylim=range(cats$Hwt), lwd=3, col="purple", ylab="Heart Weight (g)", xlab="Body Weight (kg)", 
	main="Male Cats")
abline(male.mod)

# bonus question

col.palette <- rainbow(12) 
lwd.width <- 2
pch.sym <- 1:20
nsize <- rep(c(10,100,1000), each=4)

par(mfrow=c(3,4))
for( i in 1:12 ){
	what.set <- i%%4 
	if( what.set == 0 ){
		title.text <- paste(nsize[i], " Normal RVS, Set 4", sep="")
	}
	else{
		title.text <- paste(nsize[i], " Normal RVS, Set ", what.set, sep="")
	}
	qqplot(rnorm(nsize[i]), rnorm(nsize[i]), xlab="Quantiles", ylab="Quantiles", col=col.palette[i], pch=pch.sym[i], lwd=lwd.width, main=title.text)
}

	
