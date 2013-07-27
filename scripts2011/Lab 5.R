## Lab 5
## CSSS 508

## Histograms

## Draw a sample of size 500 from a chi squared distribution with 
## degrees of freedom = 3 using the rchisq() command, and store this 
## sample in a variable.

## Make a histogram of your sample

## Create a vector which contains the elements 0,2,4,6,8,....,20.

## Now make a histogram of your sample with bins whose ends are at 0,2,4,6,...,20

## Now make a histogram of your sample with bins whose ends are at 0,1,2,3,...,25
## and shade it light blue.


## Scatter plots and identify()

## Read in the incomedata data set from the homework page of the class website
## using the read.table() command.  Specify row.names=1 so that R recognizes 
## the first column as the row names rather than as a variable

## summarize the data set and attach it.

## Make a scatter plot with income on the y-axis and age on the x-axis.

## Use identify() to find the index number of the outlier with the highest
## income.

## Use the points() command to color this point in red.

## Now repeat the exact same points() command you used, but specify
## pch=16 to shade in this point.


## Save your plot as a pdf file.

## Detach your data set.


## Bar plots

## Load the VADeaths data set
data(VADeaths)

## Look at the data -- these are death rates per 1000 in Virginia in 1940
VADeaths

## The barplot() command makes bar plots; try the following:

barplot(VADeaths)

## The different age categories are shown by shading.

## You can show the age categories side by side with the option
## beside=T 

barplot(VADeaths,beside=T)


## Add a legend with the legend parameter

barplot(VADeaths,beside=T,legend=T)


## The barplot command produces numeric output which
## can be useful when you want to add text to a plot.
## The output is the position on the x-axis of each bar.
## Verify this with the following commands:

barplot(VADeaths, plot = FALSE)
barplot(VADeaths, plot = FALSE, beside = TRUE)

## Use the following code to add the column means to your plot
## The height of each mean is related to its size:
mp <- barplot(VADeaths) # default
tot <- colMeans(VADeaths)
text(mp, tot + 3, tot, col = "blue")


## Here is an example with side-by-side colored bars:

barplot(VADeaths, beside = T,
        col = c("lightblue", "mistyrose", "lightcyan",
                "lavender", "cornsilk"),
        legend = T, ylim = c(0, 100),
	main = "Death Rates in Virginia", font.main = 4)





