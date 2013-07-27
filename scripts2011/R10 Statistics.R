
# BASIC STATISTICAL TESTS.

# Read data, print, attach & set line width

setwd("myRworkshop")

load("mydata100.Rdata")

head(mydata100)

attach(mydata100)

options(linesize=64)

# ---UNIVARIATE STATISTICS---

# ---FRANK HARRELL'S Hmisc PACKAGE---
#
# A more SAS/SPSS/Stata-like version of summary
#
# Gets counts, % on factors
#
# Does counts, %, mean on vars with <=20 values
#
# Does mean, quantiles, 5 hi/lo values 
# on vars with >20 values 
#
# Uses variable labels (rare in R)


library("Hmisc")

describe(mydata100)


# ---R'S BUILT-IN FUNCTIONS---
#
# More sparse than Hmisc's
#
# Easier to use in other programming steps
#
# No variable labels
# 
# summary() rounds off large values!! 
# Use digits argument to adjust.

summary(mydata100)


# ---R'S BUILT-IN FREQUENCIES---.

table(workshop)

prop.table( table(workshop) )


# ---R'S BUIT-IN MEAN & STD DEVIATION---

options(width=64)

myVars <- c("q1","q2","q3","q4",
  "pretest","posttest")
mean( mydata100[ myVars ], na.rm=TRUE )
sd(   mydata100[ myVars ], na.rm=TRUE ) 

# or
summary( mydata100 )

# ---CROSSTABULATIONS---

# ---GREGORY WARNES' gmodels PACKAGE---
#
# Crosstabs with format=SAS or format=SPSS
#
# Row/Column/Total percents
#
# As in SAS/SPSS/Stata, it does not perform a 
#   Chi-squared test unless you request it


library("gmodels")

CrossTable(workshop, gender, 
  chisq=TRUE, format="SAS")


# ---R'S BUILT-IN CROSSTAB---

myWG <- table(workshop, gender)

myWG

# ---R'S BUILT-IN CHI-SQUARED---

chisq.test(myWG)

# ---TABLE AS A DATA FRAME---

myWGdata <- as.data.frame(myWG)

myWGdata # Summary/Aggregation/Collapse format.

# ---CORRELATION & LINEAR REGRESSION---

# ---CORRELATIONS---
#
# R Commander rcorr.adjust function provides 
# output similar to SAS/SPSS/Stata
#
# Adjust’s p-values for number tested
#
# We’ll use small mydata to see impact adjustment
#
# R's built-in function, cor(), does not calculate 
# p-values. cor.test() does them one at a time


library("Rcmdr")

# load smaller data frame.
load("mydata.RData")
library("Rcmdr")
myQs <- c("q1","q2","q3","q4") #repeated for clarity
rcorr.adjust( mydata[myQs], 
  type="pearson")


# ---R'S BUILT-IN cor() FUNCTION---

options(digits=4) #Default is 7.
cor( mydata[myQs],
  method="pearson", use="pairwise")


# ---R'S BUILT-IN cor.test() FUNCTION--- 

cor.test( mydata$q1, mydata$q2, 
  use="pairwise")


# ---LINEAR MODELS WITH THE lm() FUNCTION---
#
# In SAS & SPSS you specify all in advance, 
# then see the results
#
# In R, you create a model object and use 
# “extractor functions” to see the parts you want
#
# summary, plot and other functions have specialized 
# methods for model objects 


lm( q4 ~ q1 + q2 + q3, data=mydata100)

myModel <- lm( q4 ~ q1 + q2 + q3, data=mydata100 )

myModel

summary(myModel)

anova(myModel)


# Set graphics parameters for MultiFrame plot (optional).
par( mfrow=c(2,2) ) 

plot(myModel)


# Set graphics parameters back to default settings.

par( mfrow=c(1,1) )


# --------COMPARING GROUPS----------

# ---t-TEST FOR INDEPENDENT GROUPS---

t.test( q1 ~ gender, data=mydata100)

# ---t-TEST FOR MATCHED GROUPS---

t.test(posttest, pretest, paired=TRUE)

# ---MEANS FOR MATCHED GROUPS---

mean(pretest)
mean(posttest)

# ---WILCOXON/MANN-WHITNEY TEST---

wilcox.test( q1 ~ gender, data=mydata100)

# ---GETTING GROUP MEDIANS---

by( q1, gender, 
  median, na.rm=TRUE)

# or 

median( q1[gender=="Female"], na.rm=TRUE )
median( q1[gender=="Male"], na.rm=TRUE )


# ---WILCOXON SIGNED-RANK TEST---

wilcox.test( posttest, pretest, paired=TRUE)

# ---MEDIANS FOR MATCHED GROUPS---

median(pretest)

median(posttest)


# ---ANOVA: GETTING MEANS---

by( posttest, workshop, 
    mean, na.rm=TRUE)

# ---ANOVA: GETTING VARIANCES---

by( posttest, workshop, 
    var, na.rm=TRUE)

# ---ANOVA: TESTING VARIANCES---

library("car")

levene.test(posttest, workshop)

# ---ANOVA: RUNNING A MODEL---

myModel <- aov(posttest~workshop, 
  data=mydata100)

myModel

anova(myModel)

summary(myModel) #same as anova result.

# Set PARamaters for 2 by 2
# Multi-Frame Plot by Row

par( mfrow=c(2,2) )

plot(myModel)

# reset to one plot per page
par( mfrow=c(1,1) ) 

# ---POST-HOC COMPARISONS---

pairwise.t.test(posttest, workshop)

TukeyHSD(myModel, "workshop")

plot( TukeyHSD(myModel, "workshop") )

# ---TYPE III SUMS OF SQUARES---
#
# The default in SAS, SPSS & Stata
#
# R aficionados strongly disparage partial tests
#
# Automatically testing for main effects 
# in presence of interactions is often a bad idea
#
# John Fox's car package has an Anova function 
# that does Type III 
# (car=Companion to Applied Regression)


# ---KRUSKAL-WALLIS: GETTING MEDIANS---

by( posttest, 
    workshop, 
    median, na.rm=TRUE)

# ---KRUSKAL-WALLIS TEST---

kruskal.test(posttest~workshop)

# ---POST-HOC COMPARISONS---

pairwise.wilcox.test(posttest, workshop)


# ---HTML & LaTeX OUTPUT---

library("xtable")

myXtable <- xtable(mydata)
print(myXtable, type="html")

myXtable <- xtable(mymodel)
print(myXtable, type="latex")


# Copyright 2007, 2008, 2009 2010 Robert A. Muenchen. All rights reserved.

