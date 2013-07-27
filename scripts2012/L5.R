### R code from vignette source 'L5.rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: lmcats
###################################################
library(MASS)
## dataset cats
str(cats)
cats.lmB <- lm(Hwt~Bwt, data=cats)
cats.lmBS <- lm(Hwt~Bwt + Sex, data=cats)


###################################################
### code chunk number 2: lmcats2
###################################################
 # Include all variables in the model
cats.all<-lm(Hwt~., data=cats) 
coefficients(cats.all)
# Remove Bwt from the model
cats.all1<-lm(Hwt~.-Bwt,  data=cats)
coefficients(cats.all1)


###################################################
### code chunk number 3: lmcatsPlot
###################################################
par(mfrow=c(2,2))
plot(cats.all, which=1:4)


###################################################
### code chunk number 4: family
###################################################
?family


###################################################
### code chunk number 5: glm
###################################################
library(vcd)
require(survival)
str(Arthritis)


###################################################
### code chunk number 6: glm1
###################################################
## transform the health status in a binary variable
Arthritis2 <- Arthritis
Arthritis2[ ,"Improved"] <- ifelse(Arthritis[ ,"Improved"] == "None", 0, 1)

## Randomize row order
Arthritis2 <- Arthritis2[sample(1:nrow(Arthritis2)), ]

## fit glm
logitm <- glm(Improved ~ Age + Sex + Treatment,
                  data = Arthritis2, family = binomial(link=logit))

#Here is the resulting logistic regression model:

summary(logitm)


###################################################
### code chunk number 7: glmPlotRes
###################################################
opar <- par(mfrow=c(2,2))
for (i in c('deviance', 'working', 'pearson', 'response'))
 plot(resid(logitm, type=i), ylab=i, pch=19, col="red")
mtext('Different types of residuals',line=-2, outer=T, cex=1.2)
par(opar)


###################################################
### code chunk number 8: logitmpredictnew
###################################################
predict(logitm, newdata=data.frame(Treatment="Placebo", Sex="Female", Age=32), type="response")
predict(logitm, newdata=data.frame(Treatment="Treated", Sex="Male", Age=70), type="response")


###################################################
### code chunk number 9: logitm2 (eval = FALSE)
###################################################
## logitm2 <- glm(Improved ~ Treatment,
##                   data = Arthritis2, family = binomial(link=logit))
## summary(logitm2)
## summary(logitm2)$aic


###################################################
### code chunk number 10: logitmfitted
###################################################
boxplot(logitm$fitted ~ Arthritis2[ ,"Improved"], main="Response vs fitted value", 
xlab="Response ('Improved')", 
ylab="Prediction (probability of improved health)")


###################################################
### code chunk number 11: logitmfitted (eval = FALSE)
###################################################
## boxplot(predict(logitm, newdata=Arthritis2, 
## type="response") ~ Arthritis2[ ,"Improved"], main=
##   "Response vs fitted value", xlab=
##   "Response ('Improved')", ylab=
##   "Prediction (probability of improved health)")


###################################################
### code chunk number 12: surv
###################################################
data(leukemia)
head(leukemia)
mysurv <- Surv(leukemia$time, leukemia$status)
head(mysurv)


###################################################
### code chunk number 13: survival
###################################################
leuk.km <- survfit(Surv(time, status) ~ x, data=leukemia)
plot(leuk.km, lty=1, col=c("darkblue","darkred"))
legend(100, 1, legend=c('Maintain', 'Non-main'), lty=1:2, col=c("darkblue","darkred"))


###################################################
### code chunk number 14: Survival_conf
###################################################
leuk.km2 <- survfit(Surv(time, status) ~ x,
                   data=leukemia,
                   conf.type='log-log')
summary(leuk.km2)
plot(leuk.km2, mark.time=FALSE, conf.int=TRUE, lty=1, col=c("darkblue","darkred"))
legend(100, 1, legend=c('Maintain', 'Non-main'), lty=1:2, col=c("darkblue","darkred"))


###################################################
### code chunk number 15: survivalTest
###################################################
survdiff(Surv(time, status) ~ x,
                   data=leukemia)


###################################################
### code chunk number 16: CoxProp
###################################################
leuk.ph <- coxph(Surv(time, status) ~ x, data=leukemia)
summary(leuk.ph)


###################################################
### code chunk number 17: loadMelanoma
###################################################
require(MASS)
data(Melanoma)
head(Melanoma)


###################################################
### code chunk number 18: survivalkm
###################################################
Melanoma<- Melanoma[!Melanoma$status==3,]
Melanoma$status[ Melanoma$status==2]<-0
MelanomaKM<-survfit(Surv(time, status)~sex, data=Melanoma)
plot(MelanomaKM, lty=1, col=c("darkblue",  "darkred"))
legend("bottomleft", legend=c("female", "male"), lty=1, fill=c("darkblue",  "darkred"))
## logrank test
survdiff(formula = Surv(time, status) ~ sex, data = Melanoma)


###################################################
### code chunk number 19: survivalcoxph (eval = FALSE)
###################################################
## summary(coxph(Surv(time, status) ~ ., data=Melanoma))


###################################################
### code chunk number 20: class
###################################################
a<- c(1:10)
class(a)
is.data.frame(a)
is.matrix(a)
is.character(a)
a<-as.factor(a)
class(a)
is.character(a)
is.factor(a)
class(as.character(a))


###################################################
### code chunk number 21: myMean Function
###################################################
myMean<-function(y1) {
        mean<-sum(y1)/length(y1)
        return(mean)
  	}


###################################################
### code chunk number 22: Functions: conditional statements- if
###################################################
x <- 9
if (x > 0) sqrt(x) else sqrt(-x)


###################################################
### code chunk number 23: Functions: conditional statements- ifelse
###################################################
ifelse(x >= 0, sqrt(x), sqrt(-x))


###################################################
### code chunk number 24: Functions: For loop
###################################################
for(i in 1:5) print(i^2)


###################################################
### code chunk number 25: colSd
###################################################
colSd<-function(df) apply(df, 2, sd)
colSd(women)


###################################################
### code chunk number 26: tapply
###################################################
women$age<-sample(30:39, 15, replace=TRUE)
ageSplit<- ifelse(women$age<35, "under35", "over35")
print(ageSplit)
tapply(women$weigh, ageSplit, length)
tapply(women$weigh, ageSplit, summary)


###################################################
### code chunk number 27: Functions: apply lapply sapply tapply
###################################################
myList <- list(WomenMat=women, WomenAge= women$age, beta = exp(-3:3), logicalVec = c(TRUE,FALSE,FALSE,TRUE))
# compute the list mean for each list element
res1=lapply(myList, length)
print(res1)
print(paste("Class of res1:", class(res1)))

res2=sapply(myList, length)
print(res2)
print(paste("Class of res2:", class(res2)))


###################################################
### code chunk number 28: testingApply
###################################################
myMA <- matrix(rnorm(1000000), 100000, 10, dimnames=list(1:100000, paste("C", 1:10, sep="")))

results <- NULL 
system.time(for(i in seq(along=myMA[,1])) results <- c(results, mean(myMA[i,]))) 

results <- numeric(length(myMA[,1])) 
system.time(for(i in seq(along=myMA[,1])) results[i] <- mean(myMA[i,])) 


system.time(myMAmean <- apply(myMA, 1, mean))
system.time(myMAmean <- rowMeans(myMA))

system.time(myMAsd <- apply(myMA, 1, sd))
system.time(myMAsd <- sqrt((rowSums((myMA-rowMeans(myMA))^2)) / (length(myMA[1,])-1)))


###################################################
### code chunk number 29: grep
###################################################
grep("A", LETTERS)
grep("A", LETTERS, value=TRUE)


###################################################
### code chunk number 30: sub
###################################################
sub("B", "A", LETTERS)


###################################################
### code chunk number 31: strsplit
###################################################
a<-date()
strsplit(a, " ")
strsplit(a, "J")
b<-strsplit(a, "11")
class(b)
b<-unlist(b)
class(b)


###################################################
### code chunk number 32: strsplit
###################################################
a<-"aedin@jimmy.harvard.edu"
strsplit(a, "\\.")


###################################################
### code chunk number 33: NA
###################################################
a<-c(1:10, NA, NA)
a<-c(1:10, NA, NA)
summary(a)
mean(a)
mean(a, na.rm=TRUE)


###################################################
### code chunk number 34: plot
###################################################
par(mfrow=c(2,2))
plot(1:10)
title("plot of 1 vector")
plot(1:20, rnorm(20), xlab="vec1", ylab="vec2")
title("plot of 2 vectors")
plot(factor(rep(c("A", "B", "C"), 7)),rnorm(21))
title("plot of a factor, vector")
hca<-hclust(eurodist)
plot(hca, main="Dendrogram", labels=LETTERS[1:21])


###################################################
### code chunk number 35: plot2
###################################################
plot(data.frame(matrix(rnorm(100), ncol=5)))


###################################################
### code chunk number 36: A (eval = FALSE)
###################################################
## solar.radiation <- c(11.1, 10.6 ,  6.3 ,  8.8 ,  10.7 ,  11.2 ,  8.9 ,  12.2)


###################################################
### code chunk number 37: B (eval = FALSE)
###################################################
## myFn<-function(x) {
##   results<-c(mean=mean(x), median=median(x), var=var(x))
##   print(results)
##   }
## myFn(solar.radiation)
## summary(solar.radiation)


###################################################
### code chunk number 38: C (eval = FALSE)
###################################################
## sr10 <- solar.radiation + 10
## myFn(sr10)


###################################################
### code chunk number 39: D (eval = FALSE)
###################################################
## par(mfrow=c(2,2))
## 
## myFn<-function(x, myTitle=NULL, myColor="red") {
## 	results<-c(mean=mean(x), median=median(x), var=var(x))
## 	print(results)
## 	hist(x,  main=myTitle, col=myColor)
## 	}
## 
## myFn(solar.radiation, myTitle="solar.radiation", myColor="magenta")
## myFn(sr10, myTitle="sr10", myColor="gold")


###################################################
### code chunk number 40: E (eval = FALSE)
###################################################
## myFn<-function(x, myTitle=NULL,yLab=NULL, myColor="red") {
## 	results<-c(mean=mean(x), median=median(x), var=var(x))
## 	print(results)
## 	hist(x,  main=myTitle, col=myColor)
## 	plot(x,  main=myTitle, col=myColor, ylab=yLab)
## 	}
## 
## myFn(solar.radiation, myTitle="solar radiation data", myColor="magenta", yLab="solar radiation observed")


###################################################
### code chunk number 41: F (eval = FALSE)
###################################################
## date <- 1:8 
## 
## myFn<-function(x, y, myTitle=NULL,yLab=NULL,xLab=NULL, myColor="red", abline=TRUE) {
## 	results<-c(mean=mean(x), median=median(x), var=var(x))
## 	print(results)
## 	hist(x,  main=myTitle, col=myColor)
## 	plot(y, x,  main=myTitle, col=myColor, ylab=yLab, xlab=xLab)
## 	lmObj <- lm(x ~ y)
## 	abline(lmObj, lwd=3, col="green", lty=2)
## 	}
## 
## myFn(solar.radiation, date,  myTitle="solar radiation data", yLab="solar radiation observed", xLab="date",myColor="magenta")


###################################################
### code chunk number 42: image (eval = FALSE)
###################################################
## save.image("L5.RData")
## savehistory("L5.Rhistory")
## dir()
## dir(pattern="RData")
## load("L5.RData")
## loadhistory("L5.Rhistory")


###################################################
### code chunk number 43: system.time
###################################################
df<-matrix(rnorm(5000000), ncol=20000)
system.time(apply(df,1,mean))
system.time(rowMeans(df))


###################################################
### code chunk number 44: pkgseleton (eval = FALSE)
###################################################
## package.skeleton(name="myFirstRPackage")


###################################################
### code chunk number 45: Rprofile (eval = FALSE)
###################################################
## sessionInfo()
## search()
## q()


###################################################
### code chunk number 46: Rprofile
###################################################
sessionInfo()
search()


###################################################
### code chunk number 47: installed
###################################################
mypkg<-installed.packages()
dim(mypkg)
summary(mypkg)


###################################################
### code chunk number 48: pkg (eval = FALSE)
###################################################
## packageStatus()


