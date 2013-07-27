#
# Rcourse02.R
#
# Example code for lecture 2008-09-10 of "Modern Applied Statistics Using R"
#
# Alexander.Ploner@ki.se
#

##---------------------- The sleep example ------------------------------##

# The data file sleep.txt contains the increase in sleeping time (in hrs)
# for two groups of 10 subjects each who exposed to two different groups

# Check: does the file exist in the current working directory?
if (file.exists("sleep.txt")) {
	sleep = read.table(file="sleep.txt", header=TRUE, sep="\t")
} else { ## We read it directly from the course web page!
	sleep = read.table("http://www.meb.ki.se/~aleplo/R2007/sleep.txt",
                       header=TRUE, sep="\t")
}

# Look what we got
sleep
dim(sleep)
colnames(sleep)
summary(sleep) # Not good - Group is a numeric variable!
sleep$Group = factor(sleep$Group)
summary(sleep) # Now we're talking!

# A bit of plotting
boxplot(ExtraSleep ~ Group, data=sleep)
# dotchart has oldfashioned arguments: no data, no formula
dotchart(sleep$Extra, groups=sleep$Gr)

# Do an unpaired t.test
# By default, R assumes unequal variances between groups (Welch t-test)
t.test(ExtraSleep ~ Group, data=sleep)
# Judging from the boxplots, the standard deviations should be equal
tapply(sleep$Ex, sleep$Gr, sd)  # Close enough
# So let's do a standard Student t-test (equal variances between groups)
t.test(ExtraSleep ~ Group, data=sleep, var.equal=TRUE) # no difference

##-------------------- Linear models: Basics ------------------------------##

# Cholesterol example (Source: Biostat II, Rino Bellocco)
# Assumes the file chol.txt in the current working directory
chol = read.table("chol.txt",header=TRUE, sep="\t")

# A scatter plot
plot(chol)

# A linear model
lm.chol = lm(Trygliceride ~ Cholesterol, chol)
# The bare bones output
lm.chol

# Extract more useful information
summary(lm.chol)
confint(lm.chol)
anova(lm.chol)

# Plotting: useful to check assumptions
par(mfrow=c(2,2))    # create 2x2 grid of plots
plot(lm.chol)
# Only the first two plots 
plot(lm.chol, which=1:2)

# Extract more details of the model
coef(lm.chol)   # A vector of coefficients, with names
fitted(lm.chol) # The values on the regression line
resid(lm.chol)  # The residuals
# Plot data and straight line
plot(chol)
abline(coef(lm.chol)) 
abline(lm.chol)        # The same
# Decoration: add the residuals
fit_tr = fitted(lm.chol)
obs_ch = chol$Chol
obs_tr = chol$Try
segments(obs_ch, obs_tr, obs_ch, fit_tr)

# Predict for a range of values
newdata = data.frame(Cholesterol=seq(5, 11, length=30))
pred_tr = predict(lm.chol, newdata=newdata)
pred_tr
# A not very exciting plot
plot(newdata$Chol, pred_tr)
# Nicer with confidence interval
pred_tr2 = predict(lm.chol, newdata=newdata, interval="confidence")
pred_tr2
# Plot it: data plus straight line plus the interval
plot(chol)
abline(lm.chol)
lines(newdata$Chol, pred_tr2[,"lwr"], lty=2) # lower limit
lines(newdata$Chol, pred_tr2[,"upr"], lty=2) # upper limit


##-------------------- Linear models: Model building ----------------------##

# The data set cats is part of the package MASS, which needs to installed
library(MASS)
data(cats)
?cats

# Some exploration
summary(cats)
# A scatter plot matrix, showing numerical codes for the factor Sex
plot(cats)
boxplot(Hwt ~ Sex, data=cats)
boxplot(Bwt ~ Sex, data=cats)

# Basic model: heart weight depends on body weight
lm0.cats = lm(Hwt ~ Bwt, cats)
summary(lm0.cats)

# Include Sex as a main effect only (parallel regressions)
lm1.cats = update(lm0.cats, .~. + Sex)
summary(lm1.cats)

# Explorative models (demonstrate subset-argument)
lmF.cats = update(lm0.cats, subset=Sex=="F")
lmM.cats = update(lm0.cats, subset=Sex=="M")
# The confidence intervals are barely overlaping
confint(lmF.cats)
confint(lmM.cats)
# Let's try a model with interaction
lm2.cats = update(lm0.cats, ~Sex*Bwt)
summary(lm2.cats)

# Check assumptions
plot(lm2.cats, which=1) # Residuals vs fitted
plot(lm2.cats, which=2) # Normal quantile plot of residuals
qqnorm(resid(lm2.cats)) # The same

# Is the interaction really better than the starting model?
anova(lm0.cats, lm2.cats) # Not really - see below why

# A plot with three different regression lines
symbl = ifelse(cats$Sex == "M", "x", "o")
color = ifelse(cats$Sex == "M", "blue", "red")
# Just two character vectors
table(symbl)
table(color)
# Do it
plot(cats$Bwt, cats$Hwt, col=color, pch=symbl)
abline(lm0.cats, col="black")
# Note that these are the separate regressions
# Q: How can you show the regression lines for lm2.cats?
abline(lmM.cats, col="blue")
abline(lmF.cats, col="red")

##-------------------- Linear models: Matrix formulation -----------------##

# Entirely articifial example
# Note use of gl to generate regular factor variables
f1 = gl(4,3)
f2 = gl(3,4, ordered=TRUE)
x  = round(rnorm(12),1)
y  = c(1,1,1,0,0,0,1,1,1,0,0,0) + 2*x + round(rnorm(12, sd=0.3),1)
data.frame(f1, f2, x, y)

# The default contrasts
options()$contrasts
contrasts(f1)
contrasts(f2)

# The model matrix can be computed from a formula
model.matrix(~x + f1 + f2)
# ... or extracted from a model
lm1 = lm(y ~ x + f1 + f2)
model.matrix(lm1)

# Contrasts can be set as a property of a factor...
contrasts(f1) = contr.SAS
contrasts(f1) # last level is reference now
# ... or be specified as part of the model
lm2 = lm(y ~ x + f1 + f2, contrasts=list(f1=contr.helmert))
model.matrix(lm2)

# We can also extract the covariance matrix of a model
vcov(lm.chol)
cov2cor(vcov(lm.chol)) # ouch

# All kinds of influence measure are avaialble
influence.measures(lm.chol)
# ... as well as differen types of residuals
rstandard(lm.chol) # standardized
rstudent(lm.chol)  # studentized (standardized leave-1-out residuals)


##-------------------- Linear models: Extensions    ----------------------##

# Multiple comparisons
# Requires packages DAAG (for the data) and multcomp (for the comparisons)
require(DAAG)
data(cuckoos)
boxplot(length ~ species, cuckoos)
lm1 = lm(length ~ species, cuckoos)
require(multcomp)
# Tukey's pairwise test
lm1.mc = glht(lm1, mcp(species="Tukey"))
summary(lm1.mc)
plot(lm1.mc)

# Nonlinear regression
example(nls)

# Mixed linear effects
# Canonical reference: Pinheiro & Bates, Mixed-Effects Models in S and S-PLUS
require(nlme)
?Rail
# A constant model with random effect of rail
plot(Rail)
lme1 = lme(travel~1, random=~1|Rail, data=Rail)
summary(lme1)
predict(lme1)

##-------------------- Generalized linear models -------------------------##

# Example: hypertension
# Source: P. Dalgaard, Introduction to Statics Using R, 11.2

# We build the data frame by hand 
noyes   = c("No","Yes")
smoking = gl(2,1,8, noyes)
obesity = gl(2,2,8, noyes)
snoring = gl(2,4,8, noyes)
n.total = c(60, 17, 8,2,187, 85, 51, 23)
n.hypte = c(5,2,1,0,35,13,15,8)
data.frame(smoking, obesity, snoring, n.total, n.hypte)

# Frequency data needs special left hand side for formula
ht_cnt = cbind(n.hypte, n.total-n.hypte)
ht_cnt

# The model
lr_ht = glm(ht_cnt ~ smoking + obesity + snoring, family=binomial)
lr_ht # As usually, minimal output

# More information on the model
summary(lr_ht)
anova(lr_ht, test="Chi")
drop1(lr_ht, test="Chi")

# The analysis of deviance generally depends on the order of variables!
lr_ht_rev = update(lr_ht, .~snoring+obesity+smoking)
anova(lr_ht_rev, test="Chi")

# Ok, eliminate smoking (for once)
lr_ht2 = update(lr_ht, .~.-smoking)
summary(lr_ht2)
# Judging from the residual deviance, the model fits well
# we try fo an interaction anyway
lr_ht3 = update(lr_ht2, .~.+snoring:obesity)
summary(lr_ht3)
anova(lr_ht2, lr_ht3, test="Chi") # Neh

# The usual extraction functions apply
coef(lr_ht2)
resid(lr_ht2)               # Deviance residuals
resid(lr_ht2, "response")   # On the original (prob.) scale
fitted(lr_ht2)              # On the logit scale
fitted(lr_ht2, "response")  # On the original (prob.) scale
# Compare observed and model probabilities
cbind(n.hypte/n.total, fitted(lr_ht2, "response"))


# This data set requires that package MASS is installed
# This is a classic, see also Hosmer & Lemeshow, Applied Logisitc Regression
require(MASS) # does not reload if already in the search path
data(birthwt)
?birthwt

# Create a nicer data frame with factors
# Taken from ?birthwt
attach(birthwt)
race = factor(race, labels = c("white", "black", "other"))
ptd  = factor(ptl > 0)
ftv  = factor(ftv)
levels(ftv)[-(1:2)] = "2+"
bwt = data.frame(low = factor(low), age, lwt, race,
    smoke = (smoke > 0), ptd, ht = (ht > 0), ui = (ui > 0), ftv)
detach("birthwt")

# Look at the data frame
summary(bwt)

# Start with a model containing all variables
lr_bwt = glm(low ~ ., binomial, bwt)
summary(lr_bwt)

# Run a stepwise selection procedure to arrive at a smaller model
lr_bwt2 = stepAIC(lr_bwt)
summary(lr_bwt2)
anova(lr_bwt, lr_bwt2, test="Chi")

##----------------------- Survival data ------------------------------##

# Example from package survival (part of standard installation)
# Small data set on acute myelogenous leukemia
library(survival)
data(aml)
?aml
summary(aml)

# Define the censoring
Surv(aml$time, aml$status)
st = Surv(aml$time, aml$status)

# A Kaplan Meier curve
km_aml = survfit(st~x, data=aml)
km_aml
summary(km_aml)
plot(km_aml)
# The corresponding log-rank test
survdiff(st~x, data=aml)

# A Cox proportional hazard model
cox_aml = coxph(st~x, data=aml)
cox_aml
summary(cox_aml)
anova(cox_aml, test="Chi")


# 
