# QERM 514 Lab #5 (4/28/07)

# This week we'll be discussing
# Linear mixed effects model
# ANOVA
# setting up, evaluating the contrasts, and interpreting the results.  

#IF you are using S+
options(contrasts=c("contr.treatment", "contr.poly")) 
#Set your working directory
setwd("C:\\514")
################################################################################

library(nlme)
data(Pixel) 
Pixel <- groupedData(pixel ~ day | Dog/Side, data = Pixel, labels = list(x = "Time post injection", y = "Pixel intensity"), units = list(x = "(days)"))
# a formula of the form resp ~ cov | group where resp is the response, cov is the primary covariate, 
# and group is the grouping factor.  The expression 1 can be used for the primary covariate when there is no other suitable candidate. 
# Multiple nested grouping factors can be listed separated by the / symbol as in fact1/fact2.  
# In an expression like this the Side factor is nested within the Dog factor.

# An inner factor is used to determine which points within a panel are joined by lines.
plot(Pixel, display = 1, inner = ~Side)

# groupedData objects can be summarized by group using the function gsummary
gsummary(Pixel)

# When multiple levels of grouping are present, random must be given as a list of formulas,
Pixel.fit1 <- lme(fixed = pixel ~ day + I(day^2), data = Pixel, random = list(Dog = ~ day, Side = ~1))

summary (Pixel.fit1)
anova (Pixel.fit1)

#residuals check

plot(Pixel.fit1, resid(., type = "p") ~ fitted(.)|Dog,abline=0)
plot(Pixel.fit1, pixel ~ fitted(.) | Dog, abline=c(0,1))
plot(Pixel.fit1, form = Dog ~ resid(., type = "p"))

fixef(Pixel.fit1)

#random effects at Dog level
ranef(Pixel.fit1, level = 1)
coef(Pixel.fit1, level = 1)
plot(ranef(Pixel.fit1, level = 1))

intervals(Pixel.fit1)


###########################################################################
# Now onto more ANOVA work, where both predictor variables are of type factor.

# Download and read in the caterpillar data from class.
caterpillar.df<-read.table("caterpillar.dat")
names(caterpillar.df) <- c("protein", "alkaloid", "growth")

# The protein and alkaloid data needs to stored as 'factor' data.  One way to do this is:

caterpillar.df$protein<-as.factor(caterpillar.df$protein)
caterpillar.df$alkaloid<-as.factor(caterpillar.df$alkaloid)

# and just to check
is.factor(caterpillar.df$alkaloid)

# But, the data labels aren't so meaningful as 1,2 for alkaloid and 1,2,3 for 
#  protein.  So, we can recode these using:
levels(caterpillar.df$protein)<-c("L", "M", "H")
levels(caterpillar.df$alkaloid)<-c("-", "+")

# attach the data frame to do a little less typing
attach(caterpillar.df)

# look at all of the means
tapply(growth, interaction(protein, alkaloid), mean)
tapply(growth, interaction(protein, alkaloid), sd)

# you can also get either the protein or alkaloid means using this command, 
#  just modified slightly...

#For each level of the treatment factor, you make an initial graphical
#exploration of the response data by using the functions
#plot.design and plot.factor.

plot.design(caterpillar.df)

#in S+
plot.factor(caterpillar.df)

# and do an interaction plot
interaction.plot(protein, alkaloid, growth)
# or the other way
interaction.plot(alkaloid, protein, growth)

# The notes use the aov command to compute the anova table.  
caterpillar.lm<-lm(growth~protein*alkaloid)
caterpillar.aov<-aov(caterpillar.lm)
summary(caterpillar.aov)
#extract cell means
model.tables(caterpillar.aov,type="means")


# work with contrasts

# First, a couple quick hints on doing contrasts.  To create the Li
#  (the linear contrast value), just multiply the contrast vector by
#  the mean vector.  

# Extract the means for each cell
cater.means<-tapply(growth, interaction(alkaloid, protein), mean)
# Create a contrast vector, but be careful you know which cell is which!
#  The order of protein, alkaloid in the interaction() function matters.
my.contrast<-c(-1, 1, 1, -1, 0, 0)
# And then Li can be found by multiplying (just *, not %*%) them together
sum(cater.means*my.contrast)
# This should be -1.5, the value from the notes.  

# A more decent way to test any specified contrast
pro.alk<-interaction(alkaloid,protein)
levels(pro.alk)
contrasts(pro.alk) <-cbind( c(-1, 1,1,-1,0,0),c(-1/2,1/2,-1/2,1/2,1,-1))
contrasts(pro.alk) 

caterpillar.aov2<-aov(growth~pro.alk) 
summary(caterpillar.aov)
summary(caterpillar.aov2,split=list(pro.alk=1:2))

