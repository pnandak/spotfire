# DATA MINING AND INFORMATION SYSTEMS
# MULTIPLE REGRESSION, MARCH, 12TH 2009 
# TOYOTA COROLLA DATA (COURTESY SHMUELI, PATEL, BRUCE)
############################################################################
# UNITS TOYOTA COROLLA used cars (n = 1436)
# VARIABLES car features (p = 38)
############################################################################
# DATA INPUT
toy <-read.csv2("http://venus.unive.it/romanaz/datamin/dati/ToyotaCorolla.csv",
 header=TRUE)
toy <-read.csv2("d:/corso_datam&sinfo/esercitazioni/toyotacorolla1/ToyotaCorolla.csv",
 header=TRUE)
str(toy)
n <- dim(toy)[1]
############################################################################
# Number of accessories (how many accessories each car can have, excluding
# Met_Color)
############################################################################
n_acc <- rowSums(toy[,c(12,19,20,22:38)])
table(n_acc)
hist(n_acc,freq=FALSE,
 xlab="No. of Accessories",ylab="Density",main="Toyota Corolla Used Cars")
############################################################################
# Price (offer price, euros)
############################################################################
summary(toy$Price)
quantile(toy$Price,probs=seq(0,1,0.1))
hist(toy$Price,freq=FALSE,
 xlab="Offer Price",ylab="Density",main="Toyota Corolla Used Cars")
boxplot(toy$Price,xlab="Offer Price",main="Toyota Corolla Used Cars",
 horizontal=TRUE,col="lightgrey")
boxplot(toy$Price~n_acc,
 ylab="Offer Price",xlab="Number of Accessories",
 main="Toyota Corolla Used Cars",col="lightgrey")
############################################################################
# BUILDING A MODEL
# DEPENDENT VARIABLE: Price
# EXPLANATORY VARIABLES
# Age_08_04 (age in months), KM (accumulated kilometers),
# HP (engine power,horses), cc (cylinder volume, cc)
# Doors (no. of doors), Quarterly _Tax (quarterly road tax, euros)
# Weight (car weight, kg), n_acc (no. of accessories)
# Met_Color (metallic colour, dummy variable: 1 yes, 0 not)
############################################################################
df <- data.frame(toy[,c(3,4,7,9,10,13,14,17,18)],n_acc)
str(df)
summary(df[,-5])
round(cor(df[,-5]),2)
############################################################################
# GRAPHICAL EXPLORATION OF DATA
hist(toy$Age_08_04,freq=FALSE,xlab="Age(Months)",ylab="Density",
 main="Toyota Corolla Used Cars")
boxplot(toy$Age_08_04,xlab="Age(Months)",main="Toyota Corolla Used Cars",horizontal=TRUE)
############
hist(toy$KM,freq=FALSE,xlab="Accumulated Km's",ylab="Density",
 main="Toyota Corolla Used Cars")
boxplot(toy$KM,xlab="Accumulated Km's",main="Toyota Corolla Used Cars",horizontal=TRUE)
############
hist(toy$HP,freq=FALSE,xlab="HP",ylab="Density",main="Toyota Corolla Used Cars")
boxplot(toy$HP,xlab="HP",main="Toyota Corolla Used Cars",horizontal=TRUE)
df[toy$HP > 120,]
############
hist(toy$cc,freq=FALSE,xlab="Cylinder Volume",ylab="Density",main="Toyota Corolla Used Cars")
boxplot(toy$cc,xlab="Cylinder Volume",main="Toyota Corolla Used Cars",horizontal=TRUE)
df[toy$cc > 2000,] #unit no. 81, possible outlier!
############
hist(toy$Weight,freq=FALSE,xlab="Weight",ylab="Density",main="Toyota Corolla Used Cars")
boxplot(toy$Weight,xlab="Weight",main="Toyota Corolla Used Cars",horizontal=TRUE)
############
hist(toy$Quarterly_Tax,freq=FALSE,xlab="Quarterly Tax",ylab="Density",main="Toyota Corolla Used Cars")
boxplot(toy$Quarterly_Tax,xlab="Quarterly Tax",main="Toyota Corolla Used Cars",horizontal=TRUE)
############
plot(toy$Age_08_04,toy$KM,pch=20,cex=0.4,
 xlab="Age (Months)",ylab="Accumulated Km's",main="Toyota Corolla Used Cars")
plot(toy$KM,toy$Price,pch=20,cex=0.4,
 xlab="Accumulated Km",ylab="Offer Price",main="Toyota Corolla Used Cars")
plot(log(toy$KM),log(toy$Price),pch=20,cex=0.4,
 xlab="Accumulated Km (Log)",ylab="Offer Price (Log)",main="Toyota Corolla Used Cars")
df[log(toy$KM) < 8,]
plot(toy$Age_08_04,toy$Price,pch=20,cex=0.4,
 xlab="Age (Months)",ylab="Offer Price",main="Toyota Corolla Used Cars")
############################################################################
# MULTIPLE LINEAR REGRESSION MODEL
############################################################################
# TRAINING SET/VALIDATION SET SPECIFICATION
set.seed(07032009)
ind <- sort(sample(1:n,size=500,replace=FALSE))
hist(ind,freq=FALSE)
############################################################################
# MODEL ESTIMATION
regr <- lm(Price~Age_08_04+KM+HP+cc+Doors+Quarterly_Tax+Weight+n_acc,
 df,subset=ind)
summary(regr)
############################################################################
# CHECKING MODEL FITTING ON TRAINING SET
boxplot(scale(regr$residuals),xlab="STD Residuals (Training Set)",
 main="Toyota Corolla Used Cars",horizontal=TRUE)
df1 <- df[ind,]
df1[abs(scale(regr$residuals)) > 3,]
hist(scale(regr$residuals),freq=FALSE,xlab="STD Residuals (Training Set)",
 main="Toyota Corolla Used Cars")
plot(dnorm,-5,10,col="red",lwd=2,add=TRUE)
plot(regr$fitted.values,scale(regr$residuals),pch=20,cex=0.4,
 xlab="Offer Price (Predicted)",ylab="STD Residuals (Training Set)",
 main="Toyota Corolla Used Cars")
abline(h=0,col="red",lty="dashed",lwd=2)
qqnorm(scale(regr$residuals),pch=20,cex=0.4,
 xlab="Estimated Quantiles",ylab="Observed Quantiles",
 main="Normal Q-Q Plot of Residuals")
qqline(scale(regr$residuals),col="red",lwd=2)
############################################################################
# TRYING LOG TRANSFORM
regr1 <- lm(log(Price)~Age_08_04+log(KM)+HP+cc+Doors+Quarterly_Tax+Weight+n_acc,
 df,subset=ind)
summary(regr1)
boxplot(scale(regr1$residuals),xlab="STD Residuals (Training Set)",
 main="Toyota Corolla Used Cars",horizontal=TRUE)
plot(regr1$fitted.values,scale(regr1$residuals),pch=20,cex=0.4,
 xlab="Log(Offer Price) (Predicted)",ylab="STD Residuals (Training Set)",
 main="Toyota Corolla Used Cars")
abline(h=0,col="red",lty="dashed",lwd=2)
qqnorm(scale(regr1$residuals),pch=20,cex=0.4,
 xlab="Estimated Quantiles",ylab="Observed Quantiles",
 main="Normal Q-Q Plot of Residuals")
qqline(scale(regr1$residuals),col="red",lwd=2)
############################################################################
#CHECKING PREDICTION PERFORMANCE ON VALIDATION SET
df2 <- df[-ind,]
v_prev <- predict.lm(regr1,newdata=df[-ind,-1])
v_residuals <- log(df[-ind,1])-v_prev
summary(scale(v_residuals))
summary(scale(regr1$residuals))
boxplot(scale(regr1$residuals),scale(v_residuals),
 xlab="Standardized Residuals",names=c("TRAINING","VALIDATION"),
 horizontal=TRUE)
df2[abs(scale(v_residuals)) > 3,]
hist(scale(v_residuals),freq=FALSE,xlab="STD Residuals (Validation Set)",
 main="Toyota Corolla Used Cars",ylim=c(0,0.4))
plot(dnorm,-5,10,col="red",lwd=2,add=TRUE)
plot(v_prev,scale(v_residuals),pch=20,cex=0.4,
 xlab="Log(Offer Price) (Predicted)",ylab="STD Residuals (Validation Set)",
 main="Toyota Corolla Used Cars")
abline(h=c(-3,0,3),col="red",lty="dashed",lwd=c(1,2,1))
############################################################################
# SPECIAL TOPIC: DUMMY VARIABLES
regr2 <- lm(log(Price)~Age_08_04+log(KM)+HP+cc+Doors+Quarterly_Tax+Weight
 +n_acc+Met_Color,df,subset=ind)
summary(regr2)
############################################################################
# SPECIAL TOPIC: STEPWISE REGRESSION
stepregr <- step(regr2)
summary(stepregr)
stepregr$anova
############################################################################

