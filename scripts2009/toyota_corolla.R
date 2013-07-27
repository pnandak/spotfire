# DATA MINING AND INFORMATION SYSTEMS
# INTRODUCING R, FEBRUARY, 19TH 2009 
# TOYOTA COROLLA DATA (COURTESY SHMUELI, PATEL, BRUCE)
############################################################################
# UNITS TOYOTA COROLLA used cars (n = 1436)
# VARIABLES 38 car features
############################################################################
# DATA INPUT
toy <-read.csv2("http://venus.unive.it/romanaz/datamin/dati/ToyotaCorolla.csv",
 header=TRUE)
str(toy)
n <- dim(toy)[1]
############################################################################
# Color
############################################################################
table(toy$Color) #abs. frequency distr.
round(100*table(toy$Color)/n,2) #% frequency distr.
t1 <- table(toy$Met_Color,toy$Color)
t1
round(100*prop.table(t1,1),2)
summary(t1) #chi-square test
############################################################################
# Number of accessories
############################################################################
n_acc <- rowSums(toy[,c(10,12,19,20,22:38)])
table(n_acc)
############################################################################
# Price (offer price, euros)
############################################################################
mean(toy$Price) #mean 10730.82
sd(toy$Price)   #standard deviation 3626.965
median(toy$Price) #median 9900
quantile(toy$Price,probs=seq(0,1,0.1)) #deciles
#  0%   10%   20%   30%   40%   50%   60%   70%   80%   90%  100% 
# 4350  7450  7950  8750  8950  9900 10500 11250 12500 15950 32500
summary(toy$Price) # main statistics
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4350    8450    9900   10730   11950   32500
hist(toy$Price,freq=FALSE,xlab="Offer Price",ylab="Density",
 main="Toyota Corolla Used Cars")
boxplot(toy$Price,xlab="Offer Price",main="Toyota Corolla Used Cars",
 horizontal=TRUE,col="lightgrey")
boxplot(toy$Price~toy$Airco,names=c("NO","YES"),
 xlab="Price, by Air Conditioning",main="Toyota Corolla Used Cars",
 horizontal=TRUE,col="lightgrey")
boxplot(toy$Price~toy$Mfr_Guarantee,names=c("NO","YES"),
 xlab="Price, by Manifacturer Guarantee Condition",main="Toyota Corolla Used Cars",
 horizontal=TRUE,col="lightgrey")
boxplot(toy$Price~n_acc,
 xlab="Price, by Number of Accessories",main="Toyota Corolla Used Cars",
 horizontal=TRUE,col="lightgrey")
############################################################################
# Relation Price, KM (accumulated kilometers),Age_08_04 (age in months)
############################################################################
summary(toy$KM)
hist(toy$KM,freq=FALSE,xlab="Accumulated Km's",ylab="Density",
 main="Toyota Corolla Used Cars")
summary(toy$Age_08_04)
hist(toy$Age_08_04,freq=FALSE,xlab="Age(Months)",ylab="Density",
 main="Toyota Corolla Used Cars")
plot(toy$Age_08_04,toy$KM,pch=20,cex=0.4,
 xlab="Age (Months)",ylab="Accumulated Km's",main="Toyota Corolla Used Cars")
cor(toy$Age_08_04,toy$KM) #corr. coefficient  0.5056722
plot(toy$KM,toy$Price,pch=20,cex=0.4,
 xlab="Accumulated Km",ylab="Offer Price",main="Toyota Corolla Used Cars")
cor(toy$KM,toy$Price) #corr. coefficient  -0.5699602 
plot(toy$KM,toy$Price,pch=as.character(toy$Mfr_Guarantee),cex=0.4,
 xlab="Accumulated Km's",ylab="Offer Price",main="Toyota Corolla Used Cars")
plot(log(toy$KM),log(toy$Price),pch=20,cex=0.4,
 xlab="Accumulated Km",ylab="Offer Price",main="Toyota Corolla Used Cars")
cor(log(toy$KM),log(toy$Price)) #corr. coefficient  -0.5699602 
plot(toy$Age_08_04,toy$Price,pch=as.character(toy$Mfr_Guarantee),cex=0.4,
 xlab="Age (Months)",ylab="Offer Price",main="Toyota Corolla Used Cars")
cor(toy$Age_08_04,toy$Price) #corr. coefficient  -0.8765905
############################################################################
regr <- lm(log(toy$Price)~ log(toy$KM))
summary(regr)
hist(scale(regr$residuals),freq=FALSE,xlab="STD Residuals")
plot(log(toy$KM),log(toy$Price),pch=20,cex=0.4,
 xlab="Accumulated Km",ylab="Offer Price",main="Toyota Corolla Used Cars")
abline(regr,col="red",lty="dashed")
points(mean(log(toy$KM)),mean(log(toy$Price)),pch="*",cex=1.5,col="red")
############################################################################
regr <- lm(log(toy$Price)~ log(toy$Age_08_04)+n_acc)
summary(regr)
hist(scale(regr$residuals),freq=FALSE,xlab="STD Residuals")
