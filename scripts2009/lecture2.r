
help.search("read data table")    ## help.search often does not help
?read.table

setwd("C:/projects/classes/R_course/lecture2")

adultReturn <- read.csv("chinook_adult_return_data.csv")
adultReturnTxt <- read.table("chinook_adult_return_data.txt")

## Check new data frames
head(adultReturn)  #show the first six lines of data frame
head(adultReturnTxt)  #show the first six lines of data frame
head(adultReturn, 10)    #show the first 10 lines of data frame
tail(adultReturn, 10)

## read.table needs to be told there is a header (read.csv does not)
read.table(“chinook_adult_return_data.txt”, header = T)

## Use scan to read in data
adultReturnScan <- scan("chinook_adult_return_data.txt")
adultReturnScan            #look at what scan looks like   (does not look too good)

adultReturnScan <- scan("chinook_adult_return_data.txt", what = list("", "", "", "", "", ""))
adultReturnScan            #look at what scan looks like     (looks better, but the first element is the column header, which will cause problems)

adultReturnScan <- scan("chinook_adult_return_data.txt", what = list("", "", "", "", "", ""), skip=1)
adultReturnScan            #look at what scan looks like    (skip the column header--we can put it back when we make the data frame below)

## Indexing lists
adultReturnScan[[5]]      # Fifth element of the list

firstElement <- adultReturnScan[[1]][1:10]      # First 10 entries of the fifth element
firstElement[1:10]

adultReturnScan[[1]][1:10] # short for the last two lines


## Now make a data frame out of scan
adultReturnScan.df <- data.frame(dam= adultReturnScan[[1]], End.Date= adultReturnScan[[2]], year= adultReturnScan[[3]], Run=adultReturnScan[[4]], Adult= as.numeric(adultReturnScan[[5]]), Jack = as.numeric(adultReturnScan[[6]]))

head(adultReturnScan.df)   # check data frame

## Subsetting data frame
adultReturn[1:6,]  # first six rows of adultReturn
head(adultReturn)   # head give the first six rows
names(adultReturn)   # gives the column names
adultReturn$Adult     # all of the column Adult  
tail(adultReturn$Adult)  # tail give the last six rows

# doing tail another way:
lenData <- length(adultReturn$Adult)
lenData1 <- dim(adultReturn$Adult)[1]
lenData == lenData1

adultReturn$Adult[lenData-6:lenData]

adultReturnBon <- adultReturn[adultReturn$Dam == "BON",]    ## only observations at Bonnevill Dam

## add a row
adultReturn1 <- rbind(adultReturn, c("FSH","31-Oct",2007,"summer", 12345,9876))   # Note the error that is produced, it is about factors

## Factors

adultReturn$Run
levels(adultReturn$Run)
adultReturn$Run <- factor(adultReturn$Run, levels= c("summer", "spring"))
adultReturn$Run

adultReturn$Run <- as.character(adultReturn$Run)   # make the column not a factor
adultReturn$Run

# determining if something is a factor
is.factor(adultReturn$Run)      # FALSE
is.factor(adultReturn$Dam)      # TRUE

## using sapply to deterine look at all columns at one time
?sapply
sapply(adultReturn, FUN = is.factor)

## reading a table from file without factors

?read.table
adultReturn <- read.csv("chinook_adult_return_data.csv", header = T, stringsAsFactors = F)

## Making factors
runFact <- as.factor(adultReturn$Run)
yearFact <- as.factor(adultReturn$year)

## using factors to group graphs
library(lattice)

histogram(~adultReturn$Adult)    # basic histogram of adults
histogram(~adultReturn$Adult|runFact)      # histogram of adults by group of run
histogram(~adultReturn$Adult|runFact*yearFact)    # histogram of adults by group of run and year

## Calculate column means
mean(adultReturn$Adult)    # take the mean of the Adult column
?mean
mean(adultReturn$Adult, na.rm = T)   # make sure to specify if you want to remove NA values

## Check for NA values
is.na(adultReturn$Adult)
any(is.na(adultReturn$Adult))   #Ask if any of the values are NA
all(is.na(adultReturn$Adult))   #Ask if all of the values are NA:

## Calculating means of subsetted data
Adult2007 <- adultReturn$Adult[adultReturn$year==2007]  # subset Adult for the year 2007
mean(Adult2007)

AdultSpring2007 <- adultReturn$Adult[(adultReturn$year==2007)&(adultReturn$Run=="spring")]   # subset Adult for the year 2007 and spring run
mean(AdultSpring2007)

## Aggregate
?aggregate
aggregate(adultReturn$Adult, by = list(yearFact, runFact), FUN = mean, na.rm=T)

## Time-series
numjobs <- c(982,981,984,982,981,983,983,983,983,979,973,979,974,981,985,987,986,980,983,983,988,994,990,999)

numjobs <- ts(numjobs, start=1995, frequency = 12)    # make a time series object
plot(numjobs)
