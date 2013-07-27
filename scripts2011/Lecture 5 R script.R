## Lecture 5 R script
## CSSS 508

data(PlantGrowth)
PlantGrowth
head(PlantGrowth)
summary(PlantGrowth)
attach(PlantGrowth)

char.group = rep(c("ctr", "trt1", "trt2"), rep(10,3))
summary(char.group)
summary(PlantGrowth$group)
## how to transform from character var into factor

as.factor(char.group)

boxplot(weight)

boxplot(weight~group)
boxplot(weight~group, 
   main="Plant Weight by Treatment Group")

boxplot(weight~group, 
main="Plant Weight by Treatment Group",
names=c("Control","Treatment 1","Treatment 2"))

boxplot(weight~group, 
main="Plant Weight by Treatment Group",
 xlab="Group", ylab="Weight",
 names=c("Control","Treatment1","Treatment 2"))

detach(PlantGrowth)
data(swiss)
attach(swiss)
	
## these two give the same plot:
plot(Examination ~ Education)
plot(Education, Examination)

abline(lm(Examination~Education))
abline(15,0)
abline(h=15,col="blue")
abline(v=15,col="blue")

abline(0,1)

## Fertility against Agriculture
plot(Fertility~Agriculture)
abline(lm(Fertility~Agriculture))

plot(Examination~Education)
identify(Education, Examination, rownames(swiss))

curve(dnorm(x))

curve(dnorm(x), xlim=c(-3,3))
curve(dnorm(x, mean=4, sd=2), xlim=c(-3,12))

normal.data <- rnorm(200, mean=4, sd=2)
gamma.data <- rgamma(200, shape=0.2)
unif.data <- runif(200)

hist(normal.data)
hist(normal.data, breaks= seq(-2,10,by=.5))
hist(normal.data, breaks= seq(-2,10,by=.5), freq=F)
curve(dnorm(x, mean=mean(normal.data),
 sd=sd(normal.data)), add=T)

hist(unif.data)
hist(gamma.data,breaks=seq(0,1.4,.1))

par(mfrow=c(2,2))

hist(normal.data)
hist(gamma.data)
hist(unif.data)
plot(normal.data~gamma.data)

par(mfrow=c(3,1))

hist(normal.data)
hist(gamma.data)
hist(unif.data)

qqnorm(normal.data)
qqline(normal.data)

qqnorm(gamma.data)
qqline(gamma.data)

qqnorm(unif.data)
qqline(unif.data)

plot(Examination~Education)
points(29,16,col="red", pch=16)

text(30,15,labels="My favorite data point")


getwd() ## this is where the plot will be saved
pdf(file="myplot.pdf")
plot(Examination~Education)

plot(Fertility~Education)
dev.off() 



## Lab 5
## CSSS 508

## Histograms

## Draw a sample of size 500 from a chi squared distribution with 
## degrees of freedom = 3 using the rchisq() command, and store this 
## sample in a variable.

x=rchisq(500,3)

## Make a histogram of your sample
hist(x)

## Create a vector which contains the elements 0,2,4,6,8,....,20.
seq(0,20,2)

## Now make a histogram of your sample with bins whose ends are at 0,2,4,6,...,20
hist(x,breaks=seq(2,20,2))

## Now make a histogram of your sample with bins whose ends are at 0,1,2,3,...,25
## and shade it light blue.
hist(x,breaks=1:52,col="lightblue")


## Scatter plots and identify()

## Read in the incomedata data set from the homework page of the class website
## using the read.table() command.  Specify row.names=1 so that R recognizes 
## the first column as the row names rather than as a variable

dat = read.table("http://students.washington.edu/gailp2/CSSS508/homework/incomedata",
	row.names=1)

## summarize the data set and attach it.
summary(dat)
attach(dat)

## NOTE: you will run into problems if you name your data set "income",
## since that is the name of one of the variables in the data set.

## Make a scatter plot with income on the y-axis and age on the x-axis.
plot(income~age)


## Use identify() to find the index number of the outlier with the highest
## income.
identify(age,income,rownames(dat))

## Use the points() command to color this point in red.
points(age[192],income[192],col="red")


## Now repeat the exact same points() command you used, but specify
## pch=16 to shade in this point.
points(age[192],income[192],col="red",pch=16)

## Save your plot as a pdf file.
	## go to file menu and select "Save as" ... pdf

## Detach your data set.
detach(income)

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
