### datastuff.r 
### for intermediate R class
### April 2007


### Creating our own data using the general linear model
### dependent variable: dep, depression score
### independent variables: age, sex, number of life events (LE), race (black, white, other)

N <- 200

set.seed(1)
age <- sample(18:65,200,replace=TRUE)

sex <- c(rep(0,80),rep(1,120))

set.seed(2)
race <- sample(0:2,replace=TRUE,size=N,prob=c(.7, .2, .1))


### Life Events

### for each year of age, you get 1/5 life event
### for blacks, 5 more
### for other 2 more
### for whites 1 less
### for men 2 more
### overall mean of 20, sd of 5

LE <- vector(mode="numeric",length=N)

set.seed(3)
for (i in 1:N)
	{
		## base LE due to unobservables unrelated to age, sex, or race
		lev <- rnorm(n=1,mean=20,sd=5)
		
		### age effect
		lev <- lev + .2 * (age[i] - 18)
		
		### sex effect
		if (sex[i] == 0) lev <- lev + 2
		
		### race effect
		if ( race[i] == 0 )  ### white
			{
				lev <- lev - 1
			}
			
		else if (race[i] == 1 )  ### black
			{
				lev <- lev + 5
			}
		else  ### other
			{
				lev <- lev + 2
			}
			
		### round to nearest integer
		
		LE[i] <- round(lev)
	}

### make data frame		
dd <- cbind(age,sex,race,LE)
dd <- as.data.frame(dd)

### create dependent variable, depression -- related to LE and gender but not race or age	
set.seed(4)
dep <- 7 + 0.4*LE + 5*sex + rnorm(N,mean=0,sd=7)
dep <- round(dep)
dd$dep <- dep
summary(dep)

### glm
fit <- glm(dep ~ age + sex + race + LE, data=dd)
summary(fit)


### plotting predicted values as a function of LE for 35 year old white male 


newd <- matrix(0,ncol=5,nrow=30)

colnames(newd) <- c("age","sex","race","LE","dep")

newd <- as.data.frame(newd)

newd$age <- 35
newd$sex <- 0
newd$race <- 0
newd$LE <- 12:41

newd$race <- as.factor(newd$race)

wm35 <- predict.glm(fit,newdata=newd)

plot(newd$LE,wm35)

pdf(file="glmplot.pdf",height=8,width=10)
plot(newd$LE,wm35,type="l",xlab="Number of Life Events",ylab="Depression Score",
     main="35 year old white male",las=1,col="blue",lwd=2)
dev.off()


### plotting predicted probabilities

lrdat <- newd
lrdat$clindep <- 0


p.wm35 <- predict.glm(lr.fit,newdata=lrdat,type="response")
plot(lrdat$LE,p.wm35,type="l",xlab="Number of Life Events",
     ylab="Predicted Probability of Clinical Depression",
     las=1,col="blue",lwd=2)


lrdat$sex <- 1
p.wf35 <- predict.glm(lr.fit,newdata=lrdat,type="response")
lines(lrdat$LE,p.wf35,col="red",lwd=2)


y.lim <- max(c(p.wm35,p.wf35))
plot(lrdat$LE,p.wm35,type="l",xlab="Number of Life Events",
     ylab="Predicted Probability of Clinical Depression",
     las=1,col="blue",lwd=2,ylim=c(0,y.lim))
lines(lrdat$LE,p.wf35,col="red",lwd=2)

legend(15,0.5,lwd=2,col=c("blue","red"),legend=c("35 year old white male","35 year old white female"))

