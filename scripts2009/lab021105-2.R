#### lab section 02.11.05
## ordered probit example using function and multinomial logit example

library(MASS)   #this library is required for the polr() function
library(nnet)   #this library is required for the multinom() function


################
#ordered probit#
################


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

# Likelihood function for 4 category ordered probit

llk.oprobit4 <- function(param, x, y) {     # hopefully this format is starting to look familiar
  os <- rep(1, nrow(x))                     # as usual, we'll want to estimate a constant, so we'll need to add a vector of ones to our X matrix
  x <- cbind(os, x)                         # binding those together (the vector of ones and our covariates)
  b <- param[1:ncol(x)]                     # this sets up the betas for our covariates - there will be as many betas as there are columns in the X matrix
  t2 <- param[(ncol(x)+1)]                  # the next estimated beta will be tau2, or the second cutpoint (remember, we assume tau1=0)
  t3 <- param[(ncol(x)+2)]                  # the next estimated beta will be tau3, or the third cutpoint (remember, we estimate M-2 cutpoints, where M=number of categories)

  # probabilities and penalty function
  xb <- x%*%b                               # x*beta
  p1 <- log(pnorm(-xb))                     # cdf = probability of first category
  if (t2<=0)  p2 <- -(abs(t2)*10000)        # penalty function to keep t2>0 (see slides for a reminder as to what a penalty function is)
  else p2 <- log(pnorm(t2-xb)-pnorm(-xb))   # probability of second category = cdf up to cutpoint 2, minus cdf up to cutpoint 1
  if (t3<=t2) p3 <- -((t2-t3)*10000)        # penalty to keep t3>t2
  else p3 <- log(pnorm(t3-xb)-pnorm(t2-xb)) # probability of third category    
  p4 <- log(1-pnorm(t3-xb))                 # probability of fourth category is one minus the probability of being in the other three categories (so they sum to 1)
                                            # everything's logged because we're maximizing the log-likelihood . . . 

  # -1 * log likelihood (optim is a minimizer)
  -sum(cbind(y==1,y==2,y==3,y==4) * cbind(p1,p2,p3,p4))     
}

# let's use the function on the GSS data
Y <- warm
X <- cbind(yr89, male, white, age, ed, prst)

# Use optim directly
ls.result <- lm(Y~X)                    # use linear estimates as starting values for the coefficients
stval <- c(ls.result$coefficients,1,2)  # initial guesses (note 1 and 2 added as starting values for the taus)

oprobit.result <- optim(stval, llk.oprobit4, method="BFGS", x=X, y=Y, hess=T)

peop <- oprobit.result$par                # point estimates
vcop <- solve(oprobit.result$hessian)     # var-cov matrix
seop <- sqrt(diag(vcop))                    # standard errors
llop <- -oprobit.result$value             # likelihood at maximum

#see help(polr) for R documentation on the function
#the function requires that our dependent variable be a categorical variable
#R identifies these as factors
#if you take a look at the original "warm" variable, it's coded in numbers from 1 to 4 where 1 corresponds to SD and 4 corresponds to SA. 
#see help(factor) for R documentation on this function. it basically transforms the variable entered in the first argument using the labels you input
#the labels will be applied in the order they're concatenated (remember, this is the c() command), so 1="Strongly Disagree" and so on

warmf <- factor(warm, labels=c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))

#one of the arguments of the polr() function is method="". the function allows us to run either ordered probit or ordered logit, but it must be specified. 
#we'll also want the Hessian matrix in order to get the vavriance-covariance matrix. the default is Hess=FALSE, so we need to specify Hess=TRUE

#if you'll recall, last week we ran this ordered probit
pmom <- polr(warmf ~ yr89 + male + white + age + ed + prst, method="probit", Hess=TRUE)

pmom
# take a look at the output and compare it to the estimates from optim()
# the coefficients for the covariates should be similar 
cbind(oprobit.result$par[2:7], pmom$coeff[1:6])

# however, you should now notice that polr() does not automatically estimate a constant for the model AND estimates one more cutpoint than
# the function does (M-1 as opposed to M-2, where M=number of categories)
# it therefore does NOT assume that tau1=0 as we do in the function and as suggested in the lecture slides

#note: what R calls intercepts in the polr() output are what are referred to as cutpoints or taus in lecture 

#take a look at the objects contained in the model
attributes(pmom)

#you'll note an absence of a likelihood. you should also notice that if you try logLik(pmom), it won't work.
#however, recall that the log-likelihood = -1/2*deviance, and that the deviance is one of the objects in the polr model

llop2 <- -.5*pmom$deviance    # which should result in a value equivalent to ll above

###### despite the variation in estimation, the two models (polr versus the llk.oprobit4 function) result in the same expected probabilities. if you need proof:

### POLR model
#ageh will be a sequence of possible ages from 18 to 89
ageh <- seq(18,89,1)
xbeta <- rep(0, length(ageh))
#loop function for calculating the xbetas across possible age values for women, other things held at their mean
for (i in 1:length(ageh)){ 
xbeta[i] <- pmom$coef[1]*1 + pmom$coef[2]*0 + pmom$coef[3]*mean(white) + pmom$coef[4]*ageh[i] + pmom$coef[5]*mean(ed) + pmom$coef[6]*mean(prst)
    }

#calculating predicted probabilities for each of the 4 response categories for a woman in 1989 (using the xbeta calculated above)
pi1hat <- pnorm(pmom$zeta[1],xbeta)         #the object $zeta in the model is the vector of cutpoints estimated by polr (the intercepts in the summary of the model)
pi2hat <- pnorm(pmom$zeta[2],xbeta) - pnorm(pmom$zeta[1],xbeta)
pi3hat <- pnorm(pmom$zeta[3],xbeta) - pnorm(pmom$zeta[2],xbeta)
pi4hat <- 1 - pi1hat - pi2hat - pi3hat


### llk.oprobit4 model
ageh <- seq(18,89,1)
xbeta2 <- rep(0, length(ageh))
for (i in 1:length(ageh)){ 
xbeta2[i] <- peop[1]*1 + peop[2]*1 + peop[3]*0 + peop[4]*mean(white) + peop[5]*ageh[i] + peop[6]*mean(ed) + peop[7]*mean(prst)    # note extra term due to the constant!
    }

pi1hat2 <- pnorm(0,xbeta2)        # since function assumes tau1=0
pi2hat2 <- pnorm(peop[8],xbeta2) - pnorm(0,xbeta2)    # last two parameter estimates are the two taus
pi3hat2 <- pnorm(peop[9],xbeta2) - pnorm(peop[8],xbeta2)
pi4hat2 <- 1 - pi1hat2 - pi2hat2 - pi3hat2

# and we wind up with two identical plots
par(mfrow=c(1,2))

plot(ageh, pi1hat, ylim=c(0,.5), pch=15, col="blue", xlab="age", ylab="predicted probability", main="polr plot")
points(ageh, pi2hat, pch=16, col="red")
points(ageh, pi3hat, pch=17, col="green")
points(ageh, pi4hat, pch=18, col="purple")

legend(x=80,y=.1,bty="n",
       xjust=1,
       legend=c("SD","D","A","SA"),col=c("blue", "red", "green", "purple"),pch=19)
       
       
plot(ageh, pi1hat2, ylim=c(0,.5), pch=15, col="blue", xlab="age", ylab="predicted probability", main="llk.oprobit4 plot")
points(ageh, pi2hat2, pch=16, col="red")
points(ageh, pi3hat2, pch=17, col="green")
points(ageh, pi4hat2, pch=18, col="purple")

legend(x=80,y=.1,bty="n",
       xjust=1,
       legend=c("SD","D","A","SA"),col=c("blue", "red", "green", "purple"),pch=19)
       



###################
#multinomial logit#
###################


# data can once again be read in from my website
occup <- read.table("http://students.washington.edu/fishes/CSSS536/data/nomocc2.csv", header=TRUE, sep=",")
 
#occup <- read.csv("C:/Documents and Settings/Shauna Fisher/My Documents/Winter 2005/CSSS 536/long data/nomocc2.csv", header=TRUE, sep=",")

#from the 1982 General Social Survey
#the dependent variable is respondent's reported occupation, recoded (or recategorized) by Long & Freese into 5 categories
#occ: occupation - Menial, Blue Collar, Craft, White Collar, Professional 

#there are three independent variables
#white: race: white=1
#ed: years of education
#exper: years of work experience

attach(occup)

#see help(multinom) for help on this function
#again, R expects categorical data for the dependent variable when using this function
#again, the original data is coded in numbers from 1-5 corresponding to each of the occupations
#thus, we need to transform this variable into a factor, or categorical, variable
occf <- factor(occ, labels=c("Menial", "Blue Collar", "Craft", "White Collar", "Professional"))

#look at the distribution of the occupations
table(occf)

#fit the model to occf with white, ed, and exper as the covariates
occatt <- multinom(occf ~ white + ed + exper, Hess=T)

occatt                          #which is the reference category?
summary(occatt)
summary(occatt, cor=F, Wald=T)  # there are actually options associated with the summary function. this produces the coefficients, their se, and their t-statistics.

#how to get the maximum log-likelihood?
attributes(occatt)
#you'll note the model has an object called "value" - this is much like the "value" objects after we run optim(). if we negate it, we get the log-likelihood at its maximum.
llml <- -occatt$value
llml2 <- -.5*occatt$deviance

#get the variance covariance matrix
vcml <- solve(occatt$Hessian)

#how to get coefficients?
#you'll notice an absence of a $coefficients object in the model. however, there is another way:
co <- occatt$wts    #they're hidden in this object, but so are a bunch of zeros
co
peml <- c(co[7], co[8], co[9], co[10], co[12], co[13], co[14], co[15], co[17], 
         co[18], co[19], co[20], co[22], co[23], co[24], co[25])

#the command coef() also extracts them
#peml1 <- coef(occatt)   #note order of coefficients
#peml2 <- t(peml1)        
#peml <- as.vector(peml2)
peml <- as.vector(t(coef(occatt)))  # to get the same vector as the peml above


## let's calculate and plot some expected probabilities
b <- coef(occatt)                   # just the coefficients vector

edhat <- seq(5, 20, 1)              # years of education

X <- cbind(1, mean(white), 5:20, mean(exper))   # counterfactual X matrix

Xb1 <- X %*% b[1,] 
Xb2 <- X %*% b[2,]
Xb3 <- X %*% b[3,]
Xb4 <- X %*% b[4,]

denomsum <- exp(Xb1) + exp(Xb2) + exp(Xb3) + exp(Xb4)   # denominator

p0 <- 1/(1+denomsum)            # menial
p1 <- exp(Xb1)/(1+denomsum)     # blue collar
p2 <- exp(Xb2)/(1+denomsum)     # craft
p3 <- exp(Xb3)/(1+denomsum)     # white collar
p4 <- exp(Xb4)/(1+denomsum)     # professional

plot(edhat, p0, xlab="years of education", ylab="probability", ylim=c(0,1), type="l", col="blue", pch=16)
lines(edhat, p1, col="purple")
lines(edhat, p2, col="orange")
lines(edhat, p3, col="green")
lines(edhat, p4, col="red")

legend(10, .8, legend=c("Menial", "Blue Collar", "Craft", "White Collar", "Professional"), col=c("blue", "purple", "orange", "green", "red"), lty=1)




#####
#hw3#       (a quick look)
#####

nes1992 <- read.table("http://students.washington.edu/fishes/CSSS536/data/nes1992.csv", header=TRUE, sep=",")

summary(nes1992)

dim(nes1992)

nes1992.na <- na.omit(nes1992)


#####
#MNP#
#####

library(MNP)

help(mnp)

# load detergent data
data(detergent)

## run the standard multinomial probit model with intercepts and the price
res1 <- mnp(choice ~ 1, choiceX = list(Surf=Surf, Tide=Tide, Wisk=Wisk,
EraPlus=EraPlus, Solo=Solo, All=All),
cXnames = "price", data = detergent, n.draws = 500, burnin = 100,
thin = 3, verbose = TRUE)
summary(res1)

# load the Japanese election data
data(japan)
## run the multinomial ordered probit model
res2 <- mnp(cbind(LDP, NFP, SKG, JCP) ~ sex + education + age, data = japan,
verbose = TRUE)
summary(res2)
