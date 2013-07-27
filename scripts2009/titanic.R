###################################################
## titanic survivor analysis
##
## simon jackman, dept of polisci, stanford univ
## march 15, 2002
## april 4, 2007
###################################################

## read data, unit record form
titanic <- read.table(file="titanic.dat",
                      header=TRUE)

## see also
## help.search("Titanic")
## library(alr3)
## data(titanic)            ## grouped form, ok for glm
##
## data(Titanic)            ## 

## look at breakdown of vars (all discrete)
apply(titanic,2,table)

## convert this to a series of dummies with labels
titanic$class2 <- factor(titanic$class,
                        levels=0:3,
                        labels=c("Crew","1st","2nd","3rd"))

## null model
## binary data via glm with family = binomial
## default is logit
## see below for probit

logit1 <- glm(survive ~ 1, data=titanic, family=binomial)

## logit of survive on everything
logit2 <- glm(survive ~ class2 + adult + male,
              data=titanic,
              family=binomial)

## link option gives us probit
probit2 <- glm(survive ~ class2 + adult + male,
              data=titanic,
              family=binomial(link=probit))

## just gender and age
logit3 <- glm(survive ~ -1 + adult + male,
              data=titanic,
              family=binomial)

## interact gender and age
logit4 <- glm(survive ~ -1 + class2 + adult*male,
              data=titanic,family=binomial)

logit5 <- glm(survive ~ class2 + adult*male,
              data=titanic,family=binomial)

titanic$womenOrChild <- titanic$adult==0 | titanic$male==0

logit6 <- glm(survive ~ class2*womenOrChild,
              data=titanic,
              family=binomial)

## LR tests via anova
anova(logit2,logit1,test="Chi")

#####################################################################
## demonstrate quasi-complete separation
## via dummy variable screw-up
dummy <- 1:(dim(titanic)[1])
dummy <- dummy<11
titanic$survive[dummy]

logit4a <- update(logit1, ~ . + dummy,
                 trace=TRUE)
logit4b <- update(logit4a,
                  eps=1e-24,maxit=1000)  ## tighter convergence for MLE
