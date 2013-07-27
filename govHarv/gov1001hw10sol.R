##  Problem 3 (Agresti 15.2)

logitMale <- -2.40 + .02*30 + .08*16 + .20
exp(logitMale)/(1 + exp(logitMale))

logitFemale <- -2.40 + .02*30 + .08*16 
exp(logitFemale)/(1 + exp(logitFemale))

exp(logitMale)
exp(logitFemale)

exp(logitMale)/exp(logitFemale)
exp(.2)

exp(.08)


##  Problem 4

##  Part A: summary statistics

load("H:/vote04.Rdata")
nonAAM <- subset(vote04, female==0 & afram==0)
nonAAF <- subset(vote04, female==1 & afram==0)
AAM <- subset(vote04, female==0 & afram==1)
AAF <- subset(vote04, female==1 & afram==1)

c(nrow(nonAAM), nrow(AAM), nrow(nonAAF), nrow(AAF))
mean.table <- cbind(mean(nonAAM), mean(AAM), mean(nonAAF), mean(AAF))
sd.table  <- cbind(sd(nonAAM), sd(AAM), sd(nonAAF), sd(AAF))

write.table(round(mean.table, 2), sep = "&", eol="\\\\\n")
write.table(round(sd.table, 2), sep = "&", eol="\\\\\n")

##  Part B:  non-model estimates

probs <- mean.table[1,]
probs

odds <- mean.table[1,]/(1 - mean.table[1,])
odds

odds.ratio <- odds/odds[1]
odds.ratio

## Part C:  model estimates

glmout <- glm(bush ~ afram + female + afram*female, family = binomial, data = vote04)
summary(glmout)
exp(glmout$coef)
write.table(round(cbind(summary(glmout)$coef[,1:2], exp(glmout$coef)), 2), sep="&")

##  Part D:  adding ideology

glmout1 <- glm(bush ~ afram + female + afram*female + ideology, family = binomial, data = vote04)
summary(glmout1)
confint(glmout1)
exp(glmout1$coef)

##  Use this to calculate the predicted probabilities
predict(glmout1, newdata=data.frame(female=0, afram = 0, ideology = 1), type="response") - predict(glmout1, newdata=data.frame(female=0, afram = 0, ideology = 0), type="response") 
predict(glmout1, newdata=data.frame(female=0, afram = 1, ideology = 1), type="response") - predict(glmout1, newdata=data.frame(female=0, afram = 1, ideology = 0), type="response") 
predict(glmout1, newdata=data.frame(female=1, afram = 0, ideology = 1), type="response") - predict(glmout1, newdata=data.frame(female=1, afram = 0, ideology = 0), type="response") 
predict(glmout1, newdata=data.frame(female=1, afram = 1, ideology = 1), type="response") - predict(glmout1, newdata=data.frame(female=1, afram = 1, ideology = 0), type="response") 

##  Part E:  bootstrapping

attach(vote04)
n <- length(bush)
B <- 100

##  Set the values where you want to make predictions
newideo <- data.frame(afram=0, female = 0, ideology=seq(0,1, by=.05))

par(mfrow=c(1,2))
plot(jitter(ideology),jitter(bush), main = "Predicted Probabilities: non-AA Men")

for(bsam in 1:B){
	boot.ind <- sort(sample(1:n,replace=TRUE))
	glmBoot.out <- glm(bush~ideology + afram + female + afram*female,family=binomial(),data=vote04[boot.ind,])
	lines(seq(0,1, by=.05),predict(glmBoot.out,newdata=newideo,type="response"),type="l",col="yellow")
	}
lines(seq(0,1, by=.05),predict(glmout1,newdata=newideo,type="response"),type="l",col="red")


newideo <- data.frame(afram=1, female = 0, ideology=seq(0,1, by=.05))
plot(jitter(ideology),jitter(bush),main = "Predicted Probabilities: AA Men")

for(bsam in 1:B){
	boot.ind <- sort(sample(1:n,replace=TRUE))
	glmBoot.out <- glm(bush~ideology + afram + female + afram*female,family=binomial(),data=vote04[boot.ind,])
	lines(seq(0,1, by=.05),predict(glmBoot.out,newdata=newideo,type="response"),type="l",col="yellow")
	}
lines(seq(0,1, by=.05),predict(glmout1,newdata=newideo,type="response"),type="l",col="red")

##  Part F: 

glmout2 <- glm(bush ~ iraq + terrorism + economy + abortion + gaymarriage + female + afram + married + income + ideology + partyid, family = binomial)
glmout3 <- glm(bush ~ iraq + terrorism + economy + abortion + gaymarriage + female + afram + married + income + ideology + partyid + age, family = binomial)
summary(glmout2)
confint(glmout2)
summary(glmout3)
confint(glmout3)

round(cbind(rbind(cbind(glmout2$coef, confint(glmout2)), 0), cbind(glmout3$coef, confint(glmout3))), 2)