## Section April 24
## Jens Hainmueller and Jenn Larson
## Matching

## We use matching when we want to draw causal inferences about the effect 
## of a treatment on an outcome, controlling for potentially confounding
## covariates

## Let's look at an example

## First we load the data
load("d25")

## And take a look at the variable names
names(data)

## One typical quantity on which to match is the propensity score, which 
## tries to measure the probability of receiving treatment

#To calculate propensity scores, we use a logit model

prop <- glm(data$D ~ data$X1 + data$X2 + data$X3 + data$X4 + data$X5, family = binomial(link = "logit"), data = data)

#And retrieve the fitted values
prop.scores <- fitted.values(prop)


################################
## Implementing Matching
################################

## Now we create a matched data set with matchit()

library(MatchIt)

m.out <- matchit(data$D ~ data$X1 + data$X2 + data$X3 + data$X4 +
                   data$X5, data = data, method = "nearest")


## Note why nearest neighbor is called a greedy algorithm

m.out.full <- matchit(data$D ~ data$X1 + data$X2 + data$X3 + data$X4 +
                   data$X5, data = data, method = "full")

##################################
## Evaluating Balance
#################################

## Compare the balance in the distributions of the treated and control groups
## in the original and the matched datasets

## We can evaluate balance with 

summary(m.out)

## Note why nearest neighbor is called a 'greedy algorithm'

summary(m.out.full)

###################################
## Estimating causal effects
###################################

## We can extract our matched dataset with

m.data <- match.data(m.out)
m.data.full <- match.data(m.out.full)

## Now we want a difference of means conditional on the covariates.
## We can use OLS for this

m.ols <- lm(m.data$Y ~ m.data$D + m.data$X1 + m.data$X2 + m.data$X3 +
            m.data$X4 + m.data$X5, data = m.data)

summary(m.ols)

## Which piece is our causal effect?







## The coefficient on D, which here is 256.32


## If matching were exact or happened to balance the covariates perfectly,
## we wouldn't need to condition on the covariates and so could find
## our causal effect (ATT) with

mean(c(m.data$Y[m.data$D == 1]))- mean(c(m.data$Y[m.data$D ==0]))



##################################
## Genetic matching                         
#################################

## Now we use genetic matching to generate an optimally matched data set

#we want to match on all five covariates, so we define
gen.out <- matchit(data$D ~ data$X1 + data$X2 + data$X3 + data$X4 +
                   data$X5, data
        = data, method = "genetic", discard = "none")

## And get the dataset
gen.data <- match.data(gen.out)

## We could also implement genetic matching with GenMatch

## We need a matrix of covarites

X <- cbind(data$X1, data$X2, data$X3, data$X4, data$X5)

## And we'll estimate weights for the matching algorithm.  To make this run
## quickly for demonstration purposes, we'll set pop.size to 15.  Usually we'll
## want to have a larger population size (size 1000 is customary)

gen.weights <- GenMatch(Tr=data$D, X=X, BalanceMatrix=X, estimand="ATT", M=1,replace=T,
                   pop.size=1000, max.generations=100, wait.generations=10)


## And create a matched dataset using the weights
gen.out.2 <- Match(Y=data$Y, Tr=data$D, X=X, estimand="ATT",
                   Weight.matrix=gen.weights, Z=X, M=1,  BiasAdjust=T,
                   replace=T, sample=T)
summary(gen.out.2)

## Can also compare balance with

mb <- MatchBalance(data$D ~ data$X1 + data$X2 + data$X3 + data$X4 +
                   data$X5, data
        = data, match.out=gen.out.2, nboots=500, ks=TRUE, mv=F)


## To calculate the ATT with a parametric model, we can define the matched dataset

gen.data <- match.data(gen.out)

#And regress the dependent variables on the treatment and the covariates
m.ols <- lm(m.data$Y ~ m.data$D + m.data$X1 + m.data$X2 + m.data$X3 +
            m.data$X4 + m.data$X5, data = gen.data)

#And retrieve the coefficient on D
summary(m.ols)

