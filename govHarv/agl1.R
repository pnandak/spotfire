#
# Replication of Beck et al. (1993) Table 1, column 1
#

#make printing wider
options(width=150)

#load up the dataset called "dta"
dta  <- as.data.frame(dget(file="http://jsekhon.fas.harvard.edu/gov2000/R/agl1.dpt"))

#load up code for panel corrected standard errors
source("http://jsekhon.fas.harvard.edu/gov2000/R/pcse.R")

#data from
#Government Partisanship, Labor Organization and Macroeconomic Performance: A
#Corrigendum (Nathaniel Beck, Jonathan Katz, Michael Alvarez, Geoffrey
#Garrett, and Peter Lange), American Political Science Review, 87(4) December
#1993: 945-948.

#An earlier paper on this is:

#Government Partisanship, Labor Organization and Macroeconomic Performance,
#1967-1984 (R. Michael Alvarez, Geoffrey Garrett and Peter Lange), American
#Political Science Review, 85(3) June 1991: 539-556.
#
#(both available on JSTOR)


#print(names(data))
#print(as.matrix(names(dta)))
#      [,1]        
# [1,] "y"         : gdp growth 
# [2,] "country"   : country number (see "cname" for the name)
# [3,] "imports"   : OECD import prices (vulnerability to OECD supply conditions)
# [4,] "exports"   : OECD export prices (vulnerability to OECD supply conditions)
# [5,] "left"      : Lefitst participation in cabinet government
# [6,] "demand"    : Vulnerability to OECD demand conditions
# [7,] "growth.lag": the lag of gdp growth
# [8,] "labor.org" : Labor ortanization index (use "lo" for replication)
# [9,] "year"      : year
#[10,] "cname"     : name of country
#[11,] "lo"        : labor organization index (USE THIS ONE)
#[12,] "growth"    : dgp growth (same as "y")


#attach the datset for easy access to variables
attach(dta)

#Let's replicate Table 1, Column 1 of the 1993 article.  

a1  <- lm(y~growth.lag + demand + exports + imports + lo + left + I(lo*left) + as.factor(year),
         data=dta)
#standard ols results
summary(a1)

#calculate panel corrected standard errrors. pcse returns the variance-covariance matrix
VC.pcse  <- pcse(a1, group=dta$country)
#calculate the panel corected standard errors
se  <-sqrt(diag(VC.pcse))

#put on nice lables
parms  <- as.data.frame(cbind(a1$coef, se, a1$coef/se, summary(a1)$coef[,2:3]))
names(parms)  <- c("parms", "pcse", "pcse t-stats", "ols se", "ols t-stats")

#print the results with 3 significant digits
print(signif(parms, digits=3))
