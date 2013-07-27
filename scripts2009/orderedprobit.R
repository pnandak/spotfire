#### lab section 02.04.05
## ordered probit example

library(MASS)   #this library is required for the polr() function
library(nnet)

#data can be read in from my website
workmom <- read.table("http://students.washington.edu/fishes/CSSS536/data/ordwarm2.csv", header=TRUE, sep=",")

#workmom <- read.csv("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/long data/ordwarm2.csv", header=TRUE, sep=",")

#the data is from the 1977 and 1989 General Social Survey
#the dependent variable is called "warm"
#warm: response to Q: "A working mother can establish just as warm and secure of a relationship with her child as a mother who does not work."
#respondents could Strongly Disagree (SD), Disagree (D), Agree (A), or Strongly Agree (SA)

#the independent variables are as follows:
#yr89: survey year: 1=1989, 0=1977
#male: gender: 1=male
#white: race: white=1
#age: age in years
#ed: years of education
#prst: occupational prestige
#warmlt2: 1=SD          these are simply alternative codings - 1 for responses of SD and 0 for D, A, and SA
#warmlt3: 1=SD, D       1 for responses of SD and D and 0 for responses of A and SA
#warmlt4: 1=SD, D, A    1 for responses of SD, D, and A and 0 for SA

#attach the dataset
attach(workmom)

#see help(polr) for R documentation on the function
#the function requires that our dependent variable be a categorical variable
#R identifies these as factors
#if you take a look at the original "warm" variable, it's coded in numbers from 1 to 4 where 1 corresponds to SD and 4 corresponds to SA. 
#see help(factor) for R documentation on this function. it basically transforms the variable entered in the first argument using the labels you input
#the labels will be applied in the order they're concatenated (remember, this is the c() command), so 1="Strongly Disagree" and so on

warmf <- factor(warm, labels=c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))

#one of the arguments of the polr() function is method="". the function allows us to run either ordered probit or ordered logit, but it must be specified. 
#we'll also want the Hessian matrix in order to get the vavriance-covariance matrix. the default is Hess=FALSE, so we need to specify Hess=TRUE

#ordered probit
pmom <- polr(warmf ~ yr89 + male + white + age + ed + prst, method="probit", Hess=TRUE)

#ordered logit
lmom <- polr(warmf ~ yr89 + male + white + age + ed + prst, method="logistic", Hess=TRUE)

pmom
lmom

#summary() gets you a little more information
#note: what R calls intercepts are what are referred to as cutpoints or taus in lecture 
summary(pmom)
summary(lmom)

#take a look at the objects contained in the model
attributes(pmom)

cbind(pmom$coeff, lmom$coeff)
#note that coefficients are off by about the same factor (1.7), reflecting differing scaling of logit & probit models
#however, also note that the t-values are similar since they're not affected by the scaling


# calculating xb for a woman in 1989 across ages with mean of other variables for the ordered probit model
#ageh will be a sequence of possible ages from 18 to 89
ageh <- seq(18,89,1)
#loob function for calculating the xbetas across possible age values for women, other things held at their mean
for (i in 1:length(ageh)){ 
xbeta[i] <- pmom$coef[1]*1 + pmom$coef[2]*0 + pmom$coef[3]*mean(white) + pmom$coef[4]*ageh[i] + pmom$coef[5]*mean(ed) + pmom$coef[6]*mean(prst)
    }


#calculating predicted probabilities for each of the 4 response categories for a woman in 1989 (using the xbeta calculated above)
pi1hat <- pnorm(pmom$zeta[1],xbeta)         #the onject $zeta in the model is the vector of cutpoints estimated by polr (the intercepts in the summary of the model)
pi2hat <- pnorm(pmom$zeta[2],xbeta) - pnorm(pmom$zeta[1],xbeta)
pi3hat <- pnorm(pmom$zeta[3],xbeta) - pnorm(pmom$zeta[2],xbeta)
pi4hat <- 1 - pi1hat - pi2hat - pi3hat


#plot the predicted probabilities
plot(ageh, pi1hat, ylim=c(0,.5), pch=15, col="blue", xlab="age", ylab="predicted probability", main="woman in 1989, all else at mean")
points(ageh, pi2hat, pch=16, col="red")
points(ageh, pi3hat, pch=17, col="green")
points(ageh, pi4hat, pch=18, col="purple")
#add a legend to the plot
legend(x=80,y=.1,bty="n",
       xjust=1,
       legend=c("SD","D","A","SA"),col=c("blue", "red", "green", "purple"),pch=19)
       
#if all goes well, the resulting plot should look familiar - see the Long book where he uses the dataset in his discussion of ordered probit interpretations.





#### below is a multinomial logit example which, since we haven't gotten to it in lecture yet, you can safely ignore for the time being. 
##but it's here for future reference

##multinomial logit example       

occup <- read.table("http://students.washington.edu/fishes/CSSS536/data/nomocc2.csv", header=TRUE, sep=",")
 
occup <- read.csv("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/long data/nomocc2.csv", header=TRUE, sep=",")

#from the 1982 General Social Survey, occupations recoded by Long & Freese
#occ: occupation
#white: race: white=1
#ed: years of education
#exper: years of work experience

attach(occup)

occf <- factor(occ, labels=c("Menial", "Blue Collar", "Craft", "White Collar", "Professional"))

table(occf)

occatt <- multinom(occf ~ white + ed + exper, Hess=T)

occatt
summary(occatt)
summary(occatt, cor=F, Wald=T)

predictors <- expand.grid(white=c(1,0), ed=3:20, exper=2:66)

fit.occatt <- predict(occatt, predictors, type="probs")
