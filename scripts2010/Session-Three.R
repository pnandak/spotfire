# Session Three Script.
require(lattice)
rm(list=ls())

# Load the files. 

Robey.url <- "http://www.stat.wisc.edu/~neely/data/Robey.csv"
Robey <- read.csv(Robey.url,row.names=1)

# Robey is now a data frame, we can attach a reference to Robey as 
# follows.

Robey.reference <- "Robey, B., Shea, M. A., Rutstein, O. and Morris, L. (1992) The reproductive revolution: New survey findings. Population Reports. Technical Report M-11."
attr(Robey,"Reference") <- Robey.reference



# Now load the dyson data set and attach a reference to it.

dyson.url <- "http://www.stat.wisc.edu/~neely/data/dyson.csv"
dyson <- read.csv(file=dyson.url,row.names=1)

dyson.reference <- list("Tim Dyson. HIV/AIDS and urbanization.  Population and Development Review, 29(3):427–442, September 2003."," ”hiv/aids surveillance data base 2004”. 
Technical report, U.S. Census Bureau 2004, 2004 
(http://www.census.gov/ipc/www/hivaidsd.html).") 

attr(dyson,"Reference") <- dyson.reference



# Now look at the structure of each, notice that they both contain
# factors. 

# In Robey, the region variable is a factor with 4 levels
str(Robey)

# In the dyson data frame we have two factors: Country and RiskArea
str(dyson)

# If you take a closer look at the region factor in the 
# Robey data, you will see that it has 50 entries and 
# four levels:Africa Asia Latin.Amer Near.East
Robey$region

# So what is a level?   For any factor variable you can find 
# the levels by using the levels() function:

levels(Robey$region)

# A level is actually a numeric value, try typing
unclass(Robey$region)

# Now let's look at a single entry in Robey$region.
Robey$region[1]
class(Robey$region[1])
str(Robey$region[1])
levels(Robey$region[1])

# Now that we have seen some existing factors, we explore
# how they are created.

# In the first session we created a factor this way
RiskArea <- factor(rep(c(0,1),c(7,7)),labels=c("OL","UL"))
RiskArea
# Using gl.
# This wasn't really the best solution.  Try the gl function 
# instead.  The name "gl" is short for "generate levels". 
RiskArea <- gl(2,7,labels=c('OL','UL'))
RiskArea

# Try the following command to get help.
?gl

# An overly verbose version of the last command might read
numberOfLevels <- 2
numberOfRepetitionsOfEachLevel <- 7
RiskArea <- gl(numberOfLevels, numberOfRepetitionsOfEachLevel,labels=c('OL','UL'))

# Using factor to create an ordered factor.
RiskArea <- factor(rep(c(0,1),c(7,7)),labels=c("OL","UL"),ordered=TRUE)
RiskArea

# Using  ordered to create an ordered factor.
RiskArea <- ordered(rep(c(0,1),c(7,7)),labels=c("OL","UL"))
RiskArea

# But it may be better to use gl to generate an ordered factor.
RiskArea <- gl(2,7,labels=c('OL','UL'),ordered=TRUE)
RiskArea


# We care about factors and how they're handled because they 
# affect how models are constructed.  Let's try modelling
# Prevalence ~ RiskArea where Risk Area is both ordered and 
# unordered.

dysonOrdered <- dyson
dysonOrdered$RiskArea <- ordered(dyson$RiskArea)

# you can try the commands
str(dyson)
str(dysonOrdered)

# Build the same linear model with both data sets. (The new
# parameter "subset" takes a logical vector that indicates
# which rows of the data we'll use. 

lmDyson <- lm(Prevalence ~ RiskArea,data=dyson,subset=(Year == 2006))

lmDyson

lmDysonOrdered <- lm(Prevalence ~ RiskArea,data=dysonOrdered,
                                          subset=(Year == 2006))

lmDysonOrdered


#Take a look at the different coefficients we get for each model.
coefficients(lmDyson)
coefficients(lmDysonOrdered)

# The summaries are also different.
summary(lmDyson)

summary(lmDysonOrdered)

# For the lmDyson model (with unordered factors) the intercept is
# the mean for the first (OL) group
OL.mean <- mean(dyson[dyson$Year==2006 & dyson$RiskArea=='OL','Prevalence'])
OL.mean
# and the second coefficient (RiskAreaUL) is the difference between the 
# OL.mean and the mean in the second(UL) group
UL.mean <- mean(dyson[dyson$Year==2006 & dyson$RiskArea=='UL','Prevalence'])

UL.mean - OL.mean


#########################################################################
# Finally, some logistic regression.   Here we will not do anything beyond
# fitting the model.

# First, the data set from the car package.   There's no need to 
# load it from the car package, just use the following code
# to download it from the class website.

Chile.url <- "http://www.stat.wisc.edu/~neely/data/Chile.csv"
Chile <- read.csv(Chile.url,row.names=1)
Chile.reference <- "Fox, J. (1997) Applied Regression, Linear Models, and Related Methods. Sage."
attr(Chile, "Reference") <- Chile.reference 


# Try looking at the structure of the Chile data.
str(Chile)

# and now look at the first 8 rows.
head(Chile,n=8)



xyplot(vote ~ statusquo,data = Chile, subset = (vote %in% c("Y","N")),
       type=c('p','smooth'), main="Chile data From Fox")
       
# Now fit the model
Chile.logistic.model <- glm(vote ~ statusquo,data = Chile, 
                              subset = vote %in% c("Y","N"),family=binomial)

Chile.logistic.model       

summary(Chile.logistic.model)
