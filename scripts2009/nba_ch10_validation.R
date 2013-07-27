############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  8-16-06                                                           #
# PURPOSE: NBA data example for model validation in Section 9.5            #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

#1992-3 data
nba<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter6\\nba_data.txt", header=TRUE, sep = "")
head(nba)

#Model chosen on 1992-3 data
mod.fit<-lm(formula = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age, data = nba)
sum.fit<-summary(mod.fit)
sum.fit

#1998-9 data
nba.98.99<-read.table(file = "C:\\chris\\UNL\\STAT870\\Chapter10\\nba_98_99.txt", header=TRUE, sep = "")
head(nba.98.99)

#Predictions for 1998-9 data using the 1992-3 data
pred.98.99<-predict(object = mod.fit, newdata = nba.98.99)
mspr<-sum((nba.98.99$PPM - pred.98.99)^2)/nrow(nba.98.99)
mspr

#Fit same model to the 1998-9 data
mod.fit.98.99<-lm(formula = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age, data = nba.98.99)
summary(mod.fit.98.99)

#Relative change in estimated beta's
round((mod.fit.98.99$coefficients-mod.fit$coefficients)/mod.fit$coefficients,4)


#Examine semi-studentized residuals
semistud.resid<-(nba.98.99$PPM - pred.98.99)/sum.fit$sigma
plot(x = 1:nrow(nba.98.99), y = semistud.resid, xlab = "1998-9 Observation number", 
       ylab = "Semi-studentized residuals", main = "1998-9 Semi-studentized residuals", 
       panel.first = grid(col = "gray", lty = "dotted"), ylim = c(min(-3,semistud.resid), max(3,semistud.resid)))
abline(h = 0, col = "darkgreen")
abline(h = c(-3,3), col = "red", lwd = 2)
identify(x = 1:nrow(nba.98.99), y = semistud.resid)
nba.98.99[30,]


#####################################################################################
# Remove players greater than 37 years of age and examine again

mod.fit.98.99.2<-lm(formula = PPM ~ MPG + height + FGP + age + I(MPG^2) + MPG:age, data = nba.98.99[nba.98.99$age<=37,])
summary(mod.fit.98.99.2)

#Relative change in estimated beta's
round((mod.fit.98.99.2$coefficients-mod.fit$coefficients)/mod.fit$coefficients,4)

pred.98.99.2<-predict(object = mod.fit, newdata = nba.98.99[nba.98.99$age<=37,])
mspr<-sum((nba.98.99[nba.98.99$age<=37,]$PPM - pred.98.99.2)^2)/nrow(nba.98.99[nba.98.99$age<=37,])
mspr


#####################################################################################
# Split 1992-3 data set

#Sample 105 observations from uniform(0,1)
set.seed(9192)
unif.split<-runif(n = nrow(nba), min = 0, max = 1)

cutoff<-floor(nrow(nba)*0.80)

#Re-orders the observations by the uniform random numbers
nba.reorder<-nba[rank(unif.split),]
nba.model.build<-nba.reorder[1:cutoff,]
nba.validation<-nba.reorder[(cutoff+1):nrow(nba),]

head(nba.model.build)
nrow(nba.model.build)

head(nba.validation)
nrow(nba.validation)



#
