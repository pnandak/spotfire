############################################

#   Goverment 50
#   Section 6 - Contingency Tables
#   March 13, 2008
#   
#   Prof: Adam Glynn
#   TF: Mike Kellermann

############################################


#   Goals for today:

#   1) Constructing tables in R

#   2) Chi-square test

#   3) Fisher Exact Test

############################################


##  1) Constructing tables in R

##  The basic data structure for testing independence 
##  between two categorical variables is a contingency
##  table.  This is just a table that gives the counts 
##  of observations for each possible combination of 
##  the categories.

##  To go from a regular dataset to a contingency table, 
##  we can use the table() function in R.

##  Let's start by loading the dataset from the Licklider 
##  paper:

civwar <- read.csv("C:/datasets/licklider.csv")
head(civwar)

##  To construct the table that we looked at in lecture, 
##  we want to classify according to the terminate variable
##  and the morewar variable:

table(civwar$terminate, civwar$morewar)

##  You can see that there are more than 2 categories for
##  each of these variables.  We want to focus on categories
##  2 and 3 for terminate and categories 1 and 4 for morewar:

civsubset <- subset(civwar, (civwar$terminate == 2 |civwar$terminate==3)
                        &(civwar$morewar==1 | civwar$morewar==4))

civtable <- table(civsubset$terminate, civsubset$morewar)
civtable

##  We can get the marginals by using table on one variable
##  at a time:

rowmarg <- table(civsubset$terminate)
rowmarg
colmarg <- table(civsubset$morewar)
colmarg

##  We can get proportions by dividing by the sum of the table:

sumtab <- sum(table(civsubset$terminate, civsubset$morewar))
sumtab

##  Here are the observed proportions

table(civsubset$terminate, civsubset$morewar)/sumtab

##  Sometimes you may not have individual observations, but
##  just have the contingency table.  You can enter that in
##  R using the matrix() function, which takes 3 arguments:
##  the vector of values, the number of rows, and the 
##  number of columns.  

civmat <- matrix(c(29, 6, 5, 6), 2, 2)
civmat

##  Note that R fills the first column first, then the second
##  column, so check to make sure you have the order correct.

##  2) Chi-square test

##  The chi-square test is a test of the null hypothesis of 
##  independence of the rows and columns in a contingency
##  table.  If the rows and columns are independent, then
##  we would expect that the proportion in each of the 
##  interior cells should be close to the overall proportion
##  in a row times the overall proportion in the column.

##  Remember that the test statistic is the sum over all the  
##  cells of (observed - expected)^2/expected

observed <- as.numeric(civtable)
observed

expected <- c(rowmarg[1]*colmarg[1]/sumtab, 
			rowmarg[2]*colmarg[1]/sumtab,
			rowmarg[1]*colmarg[2]/sumtab,			
			rowmarg[2]*colmarg[2]/sumtab)
expected

##  Now it is simple to calculate the chi-square test statistic:

chistat <- sum((observed - expected)^2/expected)
chistat

##  We want to compare this to a chi-square distribution with 1 
##  degree of freedom, and find the probability in the upper 
##  tail (or 1 - pchisq(chistat, 1))

1 - pchisq(chistat, 1)

##  We can do the same thing using the built-in chisq.test() function.

##  We can either give the function the names of two variables:

chisq.test(civsubset$terminate, civsubset$morewar)

##  This isn't exactly what we had before because it uses a slight 
##  correction to account for the discreteness of the data.  To 
##  get the number we had before, we set correct=FALSE:

chisq.test(civsubset$terminate, civsubset$morewar, correct=FALSE)

##  Now this is exactly what we had before.  

##  We could also just use the table in the chisq.test() function; 
##  we don't need the individual observations:

chisq.test(civtable, correct=F)

##  This makes it easy to enter just the summary table.  Let's look
##  at an example from one of the earlier homeworks.  Surveys were taken
##  of primary voters in Pennsylvania and North Carolina.  We can think
##  of the responses as a two-by-two table:

##               Pennsylvania    |    North  Carolina
##  ---------------------------------------------------
##  Clinton   |      248         |         218        
##  Obama     |      258         |         290    
##  ---------------------------------------------------

##  We can test the hypothesis that support for Clinton or Obama
##  is independent of state of residence:

votemat <- matrix(c(248, 258, 218, 290), 2, 2)
votemat

chisq.test(votemat, correct=F)


##  3) Fisher exact test

##  The Fisher test is more appropriate when you have 
##  relatively small expected counts in some cells.  In
##  general, we want to have an expected count of at least
##  five before using the Fisher test.

##  The function is fisher.test() and the syntax is very 
##  similar to the chisq.test() function.  You can give 
##  it either two variables:

fisher.test(civsubset$terminate, civsubset$morewar)

##  Or you can give it a table 

fisher.test(civtable)

