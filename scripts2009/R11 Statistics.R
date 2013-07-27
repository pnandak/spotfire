
# BASIC STATISTICAL TESTS.

# Read data, print, attach & set line width

setwd("myRworkshop")

load("mydata100.Rdata")

head(mydata100)

attach(mydata100)

options(linesize=64)






# ---FREQUENCIES & UNIVARIATE STATISTICS---

# The easy way using the Hmisc package.

library("Hmisc")

describe(mydata100)


# R's build in function.

summary(mydata100)


# The flexible way using built-in functions.

table(workshop)

table(gender)


# Proportions of valid values.

prop.table( table(workshop) )

prop.table( table(gender) )


# Rounding off proportions.

round( prop.table( table(gender) ), 2 )


# Converting proportions to percents.

round( 100* ( prop.table( table(gender) ) ) )


# Frequencies & Univariate

summary(mydata100)


# Means & Std Deviations

options(width=64)

sapply( mydata100[3:8], mean,  na.rm=TRUE)

sapply( mydata100[3:8], sd,    na.rm=TRUE)




# ---CROSSTABULATIONS---

# The easy way, using the gmodels package.

library("gmodels")

CrossTable(workshop, gender, 
  chisq=TRUE, format="SAS")


# The flexible way using built in functions.

# Counts

myWG <- table(workshop, gender)

myWG     # Crosstabulation format.

myWGdata <- as.data.frame(myWG)

myWGdata # Summary or Aggregation format.

chisq.test(myWG)


# Row proportions.

prop.table(myWG, 1)


# Column proportions.

prop.table(myWG, 2)

# Total proportions.

prop.table(myWG)


# Rounding off proportions.

round( prop.table(myWG, 1), 2 )


# Row percents.

round( 100* ( prop.table(myWG, 1) ) )


# Adding Row and Column Totals.

addmargins(myWG, 1)

addmargins(myWG, 2)

addmargins(myWG)




# ---CORRELATION & LINEAR REGRESSION---

# The rcorr function from the Hmisc package 

library("Hmisc")

rcorr(q1,q4)

myQs <- cbind(q1, q2, q3, q4)

rcorr(myQs, type="pearson")


# See just the P values.

options(digits=7)

myCorrs <- rcorr(myQs, type="pearson")

myCorrs$P


# See P values without scientific notation.

options(scipen=999) #Block scientific notation.

myCorrs$P

options(scipen=0) # Restore scientific notation.


# Spearman correlations using the Hmisc rcorr function.

rcorr( cbind(q1,q2,q3,q4), type="spearman" )


# The built-in cor function.

cor( data.frame(q1, q2, q3, q4),
  method="pearson", use="pairwise")


# The built-in cor.test function 

cor.test(q1, q2, use="pairwise")


# Linear regression.

lm( q4 ~ q1 + q2 + q3, data=mydata100)

myModel <- lm( q4 ~ q1 + q2 + q3, data=mydata100 )

myModel

summary(myModel)

anova(myModel) #Same as summary result.


# Set graphics parameters for 4 plots (optional).
par( mfrow=c(2,2), mar=c(5,4,2,1)+0.1 ) 

plot(myModel)


# Set graphics parameters back to default settings.

par( mfrow=c(1,1), mar=c(5,4,4,2)+0.1 )

myNoMissing <- na.omit(mydata100[ , c("q1","q2","q3","q4") ] )

myFullModel    <- lm( q4 ~ q1 + q2 + q3, data=myNoMissing)

myReducedModel <- lm( q4 ~ q1,       data=myNoMissing)

anova( myReducedModel, myFullModel)








#---GROUP COMPARISONS---

# Independent samples t-test.

t.test( q1 ~ gender, data=mydata100)

t.test( q1[gender=='Male'], q1[gender=='Female'] )


# Paired samples t-test.

t.test(posttest, pretest, paired=TRUE)


# Equality of variance.

library("car")

levene.test(posttest, gender)

var.test(posttest~gender)


# Wilcoxon/Mann-Whitney test.

wilcox.test( q1 ~ gender, data=mydata100)


# Same test specified differently.

wilcox.test( q1[gender=='Male'],  
             q1[gender=='Female'] )

aggregate( q1, data.frame(gender), 
  median, na.rm=TRUE)


# Wilcoxon signed rank test.

wilcox.test( posttest, pretest, paired=TRUE)

median(pretest)

median(posttest)


# Analysis of Variance (ANOVA).

aggregate( posttest, 
  data.frame(workshop), 
  mean, na.rm=TRUE)

aggregate( posttest, 
  data.frame(workshop), 
  var, na.rm=TRUE)

library("car")

levene.test(posttest, workshop)

myModel <- aov(posttest~workshop, 
  data=mydata100)

myModel

anova(myModel)

summary(myModel) #same as anova result.


# type III sums of squares

library("car")

Anova(myModel, type="III")

pairwise.t.test(posttest, workshop)

TukeyHSD(myModel, "workshop")

plot( TukeyHSD(myModel, "workshop") )


# Set graphics parameters for 4 plots (optional).

par( mfrow=c(2,2), mar=c(5,4,2,1)+0.1 ) 

plot(myModel)


# Set graphics parameters back to default settings.
par( mfrow=c(1,1), mar=c(5,4,4,2)+0.1 )


#Nonparametric oneway ANOVA using
# the Kruskal-Wallis test.

kruskal.test(posttest~workshop)

pairwise.wilcox.test(posttest, workshop)

aggregate( posttest, 
  data.frame(workshop), 
  median, na.rm=TRUE)

# ---HTML OUTPUT---

library("R2HTML")

HTML( summary(myModel), file="myModel.html")

browseURL("file:///C:/myRfolder/myModel.html")


# Copyright 2007, 2008, 2009 Robert A. Muenchen. All rights reserved.

