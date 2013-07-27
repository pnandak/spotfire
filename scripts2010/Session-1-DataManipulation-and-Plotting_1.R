# FirstSession - Data Manipulation and Plotting

rm(list=ls())

########################################################################
# Basic R espressions

# Arithmetic
5 + 4*3

# Index vectors
1:7

# Numeric vectors
seq(0,1, by=0.2)


# Variable assignment
x <- 5.7

# An alternative syntax for variable assignment:
x = 5.7

# First load the lattice library.
library(lattice)


###################################################################################
# Building the data frame for Dyson's data from his table 1. (See slides for the 
# references.)


# First enter the data as vectors using the c command -- remember c is for combine.
urban <- c(18.6,42.2,28.5,22.7,13.3,27.1,31.1)
rural <- c(3.9,12.3,10.7,12.6,7.0,10.3,37)

country <- c("BURUNDI","LESOTHO","MALAWI","NAMIBIA","RWANDA","ZAMBIA","ZIMBABWE")


# Next bind the vectors together as a data frame.
dyson2003 <- data.frame(country, urban,rural)


###################################################################################
# Plotting Dyson's data 

# The first plot just shows the differences between urban and rural HIV prevalence
# rates by country.

xyplot(urban-rural ~ country,data=dyson2003)

# The second plot shows the damee data with a grid added by using the 
# type paramter.   The expression type=c('p','g') means plot with points (p) and
# show a grid (g).   You can also add 'r' for a regression line, 'l' for lines
# between successive points and 's' for a lowess smoother.

xyplot(urban-rural ~ country,data=dyson2003,type=c('p','g'))

#################################################################################
# Combining Dyson's data with the updated data from the US Census HIV/AIDS
# surviellance report (again, references are in the slides).  

# First we need to create a RiskArea factor consistent with the updated data. 
# Do this using the factor command.   Factor creates a vector of factor levels
# In the following command we create a factor with seven repetitions of 0 and
# seven repetitions of 1 and give them the labels OL (rural low risk) and UL 
# (urban low risk)
RiskArea <- factor(rep(c(0,1),c(7,7)),labels=c("OL","UL"))

# Now we bind the original data into a new data frame that includes the RiskArea 
# factor.

dyson2003 <- data.frame(Country = country, RiskArea, Prevalence = c(rural,urban))

# This line reorders the rows of the dyson2003 data frame so that the country names
# are in alphabetical order. 
dyson2003 <- dyson2003[order(dyson2003$Country),]


# Here's a replot of the new data as represented in the new dyson2003 data frame.
xyplot(Prevalence ~ RiskArea|Country,data = dyson2003,type = c('p','g','r'))

################################################################################
# Loading the updated data. 

# This is the url of the updated data.  The format has been changed from the data
# offered by the US Census by changing an excel file to a comma separated value 
# file. 

data.file.url <- "http://www.stat.wisc.edu/~neely/data/hiv3afri.csv"

# The function read.csv turns the file into a data frame.
hiv3afri <- read.csv(data.file.url)

# The data fram hiv3afri contains information about many countries not included in the 
# paper by Dyson. We will use this vector to pull out just the subset of hiv3afri that
# contains the rows corresponding to countries that Dyson examined.

dysonCountries <-  c("BURUNDI","LESOTHO","MALAWI","NAMIBIA","RWANDA","ZAMBIA","ZIMBABWE")

# These two lines create vectors of TRUE/FALSE values.   For the new vector countrySubset
# we will have a TRUE if a line in hiv3afri referes to one of the entries in dysonCountries.
# For the populationSubset vector we get a TRUE value only if the SubPopulation column of 
# hiv3afri is PREGNANT WOMEN (these correspond to either UL or OL codings in the data frame).

countrySubset <- hiv3afri[,'Country'] %in% dysonCountries
populationSubset <- hiv3afri[,'SubPopulation'] %in% c('PREGNANT WOMEN')

# Dyson only uses three of the columns of hiv3afri, so we us only these three
# columns and the rows for which countrySubset and populationSubset are both
# TRUE.
dysonVariables <- c('Country','RiskArea','Prevalence')
dyson2006 <- hiv3afri[countrySubset & populationSubset,dysonVariables]


xyplot(Prevalence ~ RiskArea|Country,data = dyson2006,type = c('p','g','r'))


#######################################################################################
# Next we combine the two data frames.

# Add a column for the year to both.
dyson2006$Year <- 2006
dyson2003$Year <- 2003

# And now bind the two data.frames together using rbind (row bind).
dyson <- rbind(dyson2003,dyson2006)

xyplot(Prevalence ~ RiskArea|Country,data = dyson,type = c('p','g','r'),groups=Year)

#######################################################################################
# Another data set presented by Dyson, HIV prevalence rates plotted against a scale of
# of urbanization.
#


hiv.prev <-    c(35.8,11.3,2.9,10.6,14,23.6,16,13.2,19.5,11.2,19.9,25.3,8.1,8.3,20,25.1)
urbanization <-c(49,9,18.7,15.5,33.4,28,14.7,32.1,30.9,6.2,56.9,26.4,32.3,14.2,39.6,35.3)

xyplot(hiv.prev ~ urbanization,data.frame(hiv.prev,urbanization),type=c('r','g','p'))

