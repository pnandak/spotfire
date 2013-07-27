## demonstration of reading comma-delimited (CSV files)
## produced by Excel

demo <- read.table(file="/Users/jackman/docs/classes/151B/06/class0412Demo.csv",
                   sep=",",
                   header=T)


## murder data, Table 3.1 in Agresti & Finlay
murder <- read.table(file="/Users/jackman/docs/classes/151B/04/table3.1.asc",
                     header=T)

x <- murder$MR   ## extract component MR

## create a logical flag for the District of Columbia
DC <- x>50
notDC <- x<50

## numerical summary command
summary(x)

## histogram
hist(x)

## re-run the analysis but drop DC
summary(x[notDC])
hist(x[notDC])

## note how the mean changes through the omission of DC
mean(x)
mean(x[notDC])

## what is the median???
## the median is the "middle" observation
## for even number of observations, take the
## average of the two bracketing the middle

median(x)
median(x[notDC])  ## much less difference thru dropping DC


## quantiles/quartiles/percentiles
## the q-th quantile of a variable x is the value V
## of x such that proportion q have x less than V, while
## proportion 1-q have x greater than or equal to V
quantile(x,.25)

## how to quantify dispersion???
## inter-quartile range (IQR)
## difference between the 75th percentile and the 25th percentile
quantile(x,c(.75,.25))



## what is the mode???
## the mode is the most frequently occuring value of a variable
## table 3.8, p52 of text
x <- c(1344,133,25,11,1)
names(x) <- c("0","1","2","3","4")


