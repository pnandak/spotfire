########################################
##R Programming Session 1
#########################################

##data entry from keyboard

ages<- c(30,40,55,46,57)
ages
##input data from spreadsheet

mydata<- read.csv('chickwt.csv')
summary(mydata)

##statistical summaries

mean(mydata$weight)

var(mydata$weight)

##modelling. Note the outer parentheses which tell R to print the result
##as well as storing it

(mylm<-lm(weight~feed,mydata))

##graphical

plot(density(mydata$weight))

plot(mylm)

boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        subset = supp == "VC", col = "yellow",
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg",
        ylab = "tooth length",
        xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i")
boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
        boxwex = 0.25, at = 1:3 + 0.2,
        subset = supp == "OJ", col = "orange")
legend(2, 9, c("Ascorbic acid", "Orange juice"),
       fill = c("yellow", "orange"))

## new functions

mycolSums<- colSums
fix(mycolSums)
#############
mymat<- data.matrix(mydata)
mymat[70,1]<- NA
mycolSums(mymat)
colSums(mymat)

##help

?help

help.search("variance")

help(var)

?`&&`

help.start()

## A brief demo

2+3

x<- 2+3

exp(-4*4/2)/sqrt(2*pi)

dnorm(4,0,1) ## or dnorm(0,4,1)

##introductory session

library(MASS)
search()

x <- rnorm(1000)
y <- rnorm(1000)
truehist(c(x,y+3),nbins=25)
##save objects
y<- 3
save(x, y, file = "xy.RData")
save.image()
load('xy.RData')
##packages

library(car)
