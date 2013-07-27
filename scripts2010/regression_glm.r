library(MASS)
library(foreign)
# dd <- c("C:/XunCao/Teaching/Method_Stat/multilevel/Lec/lec2") 
# setwd(dd)

##### Poisson #####
Iso<-read.csv("http://privatewww.essex.ac.uk/~caox/teaching/Day2/iso_081308.csv", header=T, na.strings="", 
              colClasses=c(rep("character", 2), rep("numeric", 49)))

oecd<-matrix(0, ncol=1, nrow=dim(Iso)[1])
colnames(oecd)<-c("oecd")
OECD<-c("AUL","AUS","BEL","CAN","CZR","DEN",
        "FIN","FRN","GMY","GRC","HUN","ICE",
        "IRE","ITA","JPN","ROK","LUX","MEX",
        "NTH","NEW","NOR","POL","POR","SLO",
        "SPN","SWD","SWZ","TUR","UKG","USA")
for (i in 1:dim(Iso)[1]){if (length(intersect(Iso$Country[i], OECD))==1){oecd[i,1]<-1}}        
        
Iso<-cbind(Iso, oecd) 
Iso<-subset(Iso, Iso$iso.adopt.na<=5000) # get rid of some extreme values. 
plot(density(Iso$iso.adopt.na, na.rm=na.omit))
sort(unique(Iso$iso.adopt.na))

fit.p<-glm(formula = iso.adopt.na ~  gdp
                                      + gdppc 
                                      + tradeopen 
                                      + log(fdi.r+1) 
                                      + eu 
                                      + I(govsump/gdp)
                                      + regulation  
                                      ,na.action=na.omit, 
                                      data=Iso
                                      , family=poisson
                                      , offset=log(iso.accum.lag+1)
                                      )
summary(fit.p)

## calculate overdispersion:
yhat<-predict(fit.p, type="response")
length(yhat)==length(Iso$iso.adopt.na) 
# there are observations deleted b/c of missing values. 
z<-(fit.p$y-yhat)/sqrt(yhat)
cat("overdispersion ratio is ", 
    sum(z^2)/(length(fit.p$y)-dim(model.matrix(fit.p))[2]), "\n")
cat("p-value of overdispersion test is ", 
    pchisq(sum(z^2), length(fit.p$y)-dim(model.matrix(fit.p))[2]), "\n")


fit.overp<-glm(formula = iso.adopt.na ~  gdp
                                      + gdppc 
                                      + tradeopen 
                                      + log(fdi.r+1) 
                                      + eu 
                                      + I(govsump/gdp)
                                      + regulation  
                                      ,
                                      na.action=na.omit, 
                                      data=Iso
                                      , family=quasipoisson
                                      , offset=log(iso.accum.lag+1)
                                      )
summary(fit.overp)
# notice the mean of coefficients do not change: but the standard errors are so much inflated.
# the factor of inflation is the square root of the overdispersion ratio. 






###### ordered logit and probit models ####
# load the data
nes96 <- read.table("http://privatewww.essex.ac.uk/~caox/teaching/Day2/nes96r.dat", header=TRUE)
# check to see if the data were read in properly
nes96[1:3,]

# OK, let's fit an ordered logit model where the response variable 
# is ClinLR: the variation in respondents perceptions of Clinton's conservatism
# 7 point scale running from 1 # (extremely liberal) to 7 (extremely conservative).

# Let's fit a model where ClinLR is the response variable, and TVnews,
# selfLR, PID, age, educ, and income are the covariates. 


polr.out <- polr(as.ordered(ClinLR)~ TVnews+selfLR+PID+age+educ+income, data=nes96)
# Note we had to turn our response variable (ClinLR) into an ordered
# factor (using the as.ordered() function) in order to use the 
# polr() function to fit our model. The response variable going into 
# polr always has to be an ordered factor.
#
# Let's look at the results:
summary(polr.out)


# The section of results titled "Coefficients" gives us our point 
# estimates and measures of uncertainty for the coefficients on 
# our covariates. 

# We can see that there is a very significant and substantively large
# negative effect of PID on the perceived conservatism of Clinton. 
# Since PID is coded from 0 (Strong Democrat) to 6 (Strong Republican)
# this means that Republican identifiers are much more likely to 
# perceive Clinton to be very politically liberal than Democratic 
# identifiers. Indeed, looking at exp(-1*-0.386311548) = 1.471543 we can 
# say that a one unit increase in PID (PID becoming slightly more
# Republican) results in the odds of placing Clinton in category m or 
# lower to increase by the factor 1.47. We can also see that there are 
# significant effects of age, education, and income on perceptions of
# Clinton's conservatism. Older respondents are more likely to believe
# Clinton is very liberal than younger respondents, more educated 
# respondents are more likely to perceive Clinton is very liberal than 
# less well educated respondents, and wealthier respondents are also 
# more likely to perceive Clinton to be very liberal than less wealthy
# respondents.


# The section of results title "Intercepts" gives the point estimates
# and measures of uncertainty for the cutpoints. We did not include an constant term in our 
# beta vector so we have estimated all 6 cutpoints for our 7-category 
# dependent variable. 


# Let's plot the probabilities of each of the outcome categories as 
# a function of PID for a someone who watches 3 days of TVnews a week 
# (this is the median value in our sample), considers themselves 
# politically moderate (selfLR = 4), is 44 years old (the median value
# in the sample), has more than 12 years of schooling but no higher
# degree (educ = 4), and has a family income of $35,000 to $39,999 
# dollars a year. 

beta <- coef(polr.out)
beta
tau <- polr.out$zeta
tau
X <- cbind(3, 4, 0:6, 44, 4, 17)
X

logit.prob <- function(eta){exp(eta)/(1+exp(eta))}

p1 <- logit.prob(tau[1] - X %*% beta)
p2 <- logit.prob(tau[2] - X %*% beta) - logit.prob(tau[1] - X %*% beta)
p3 <- logit.prob(tau[3] - X %*% beta) - logit.prob(tau[2] - X %*% beta)
p4 <- logit.prob(tau[4] - X %*% beta) - logit.prob(tau[3] - X %*% beta)
p5 <- logit.prob(tau[5] - X %*% beta) - logit.prob(tau[4] - X %*% beta)
p6 <- logit.prob(tau[6] - X %*% beta) - logit.prob(tau[5] - X %*% beta)
p7 <- 1.0 - logit.prob(tau[6] - X %*% beta)

par(mfrow=c(1,1))
plot(0:6, p1, type="l", col=1, ylim=c(0,1))
lines(0:6, p2, col=2)
lines(0:6, p3, col=3)
lines(0:6, p4, col=4)
lines(0:6, p5, col=5)
lines(0:6, p6, col=6)
lines(0:6, p7, col=7)
legend(0, 1, legend=c("P(extremely liberal)", "P(liberal)", 
"P(slightly liberal)", "P(moderate)", "P(slightly conservative)",
"P(conservative)", "P(extremely conservative)"), col=1:7, lty=1)



library(MASS)   #this library is required for the polr() function
# library(nnet)   #this library is required for the multinom() function
# posted for class CSSS/SOC/STAT 536:  Logistic Regression and Log-linear Models
# xun cao: Monday, 22.May.2006.

#### ordered probit ###

# data:
surveylab8<-dget("http://privatewww.essex.ac.uk/~caox/teaching/Day2/surveylab8")
# DV: Trust: 
# Question: 
# To what extent do you agree with the following statements? 
# Do you strongly agree, mostly agree, neither agree nor disagree, 
# mostly disagree or strongly disagree? 
# 
# "Among national groups, it is possible to create cooperation but never to fully trust."
# 1.  Strongly agree                  1
# 2.  Mostly agree                    2
# 3.  Neither agree nor disagree      3
# 4.  Mostly disagree                 4
# 5.  Strongly disagree               5

# 8.     Don't know                   8
# 9.     Refusal                      9

# for simplicity, we delete those 8 and 9;
# so we have five categories, 1-5 with 5 for people displaying highest level of "Trust"
# in inter-ethnic relations in Bosnia and North Caucasus.

# let's test the effects of Age, Gender, Education, Materialstatu
# we also use variable "bosnia" to make distinction between surveys from two different regions.
# of course, you can add more ...

# Age: just age, 18-82.

# Gender: male=0; female=1.

# Education: 
# 1. Below primary school                                          1
# 2. Primary school                                                2
# 3. Uncompleted secondary school                                  3
# 4. Compelted secondary or high school technical diploma          4
# 5. Technical college                                             5
# 6. Finished or partially completed higher (university) education 6
# 9.  Do not wish to answer                                        9

# Materialstatu:
# In relation to purchasing power, how would you rate your family's income level? (Circle only one answer!) 
# 1.  We can purchase all we need                                  1
# 2.      We can purchase all we need except for durable objects   2
# 3.      We only have enough money to provide food                3
# 4.      We do not have enough money to provide food              4
# Do not read!
# 8.     Don't know/ Difficult to say                              8
# 9.     Refusal                                                   9

# bosnia: 1 for yes.

surveylab8<-subset(surveylab8, surveylab8$Education!=9&surveylab8$Materialstatu!=9) # get rid of 8 and 9's. 
attach(surveylab8) # we are going to only use this data set, so attach it.
is.factor(Trust)
as.ordered(Trust) # factors with ranks.
# or:
Trust.f <- factor(Trust, labels=c("Strongly agree", "Mostly agree", "Neither agree nor disagree",
                                  "Mostly disagree", "Strongly disagree"))

polr.out <- polr(#as.ordered(Trust)~bosnia
                 Trust.f~bosnia
                                   +Gender+Age+Education+Materialstatus
                                   +russian
                                   +serb
                                   #+Currentsit+Violence
                                   #+Ethnicrelations+Ethnicfriends+Closestfriends
                                   #+Pride+org
                                   #+bosniac+serb+croat+bosnian+russian+avar+kabardin+chechen
                                   , 
                                   data=surveylab8,na.action=na.omit, method = "probit" , Hess=TRUE) # weights=survcomb.no.na$weight, 
summary(polr.out)

attributes(polr.out)
polr.out$zeta
round(polr.out$fitted.values[c(1, 8, 9),], digits=2)
predict(polr.out)[1:10]


## display the results:
# age will be a sequence of possible ages from 18 to 82
age <- seq(18,82,1)
mu.w <- rep(0, length(age))
#loop function for calculating the xbetas across possible age values for women in Bosnia, other things held at their mean
polr.out$coef
for (i in 1:length(age)){ 
mu.w[i] <- polr.out$coef[1]*mean(surveylab8$bosnia)+ polr.out$coef[2]*0 + polr.out$coef[3]*age[i]+ polr.out$coef[4]*mean(surveylab8$Education)+ polr.out$coef[5]*mean(surveylab8$Materialstatus) + polr.out$coef[6]*0 + polr.out$coef[7]*mean(surveylab8$serb)
    }
    

pi1hat <- pnorm(polr.out$zeta[1],mu.w)         
pi2hat <- pnorm(polr.out$zeta[2],mu.w) - pnorm(polr.out$zeta[1],mu.w)
pi3hat <- pnorm(polr.out$zeta[3],mu.w) - pnorm(polr.out$zeta[2],mu.w)
pi4hat <- pnorm(polr.out$zeta[4],mu.w) - pnorm(polr.out$zeta[3],mu.w)
pi5hat <- 1 - pi1hat - pi2hat - pi3hat - pi4hat


##
par(mfrow=c(2,1))
n<-100000
trust.simul<-rnorm(n, mean=mu.w[1], sd=1)
plot(density(trust.simul)) 
abline(v=c(polr.out$zeta))
abline(v=mu.w[1], col="red")

n<-100000
trust.simul<-rnorm(n, mean=mu.w[65], sd=1);age[65]
plot(density(trust.simul)) 
abline(v=c(polr.out$zeta))
abline(v=mu.w[65], col="red")
##


par(mfrow=c(1,1))

plot(age, pi1hat, ylim=c(0,.5), pch=15, col="blue", xlab="age", 
     ylab="predicted probability", 
     main="predicted trust: women in Bosnia, other things held at their mean")
lines(age, pi2hat, pch=16, col="red")
lines(age, pi3hat, pch=17, col="green")
lines(age, pi4hat, pch=18, col="purple")
lines(age, pi5hat, pch=19)    

legend(x=80,y=.52,                                       
        bty="n",                                        
        xjust=1,                                        
       legend=c("Strongly agree","Mostly agree","Neither","Mostly disagree", "Strongly disagree"),
       col=c("blue", "red", "green", "purple", "black"),pch=19)  
       
