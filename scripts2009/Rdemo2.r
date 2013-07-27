##############################################################
##R Programming Session 2
###############################################################

x<- seq(0,0.5,0.1) ##generate a sequence from 0 to 0.5 in steps of 0.1
x ##Look at x
##is x equal to (0,0.1,0.2,0.3,0.4,0.5)?
##To find out, type
x==c(0,0.1,0.2,0.3,0.4,0.5)

##

myvar <- function(x) (sum(x^2) - length(x) * mean(x)^2)/(length(x)-1)
x<- seq(1:100)
myvar(x)
var(x)
x<- seq(1:100)+10000000000
myvar(x)
var(x)
##
##
### Inf, NaN

as.numeric(c('a','1'))
x/0
log(-x)
#######

age<- runif(100)*50
table(cut(age,c(0,10,20,30,40,50)))

##why is this different from that on the slide?
##data.frame
## copy the file from the webpage to the folder where you are running R

(mydata<- read.csv('chickwt.csv'))

##list
################
Empl <- list(employee = "Anna", spouse = "Fred",
children = 3, child.ages = c(4, 7, 9))

Empl
Empl[[1]]
Empl$spouse

Empl[[4]]
Empl[4]

ls()
str(Empl)

########################
##operators
#############
3^2
10%%3
10 %/%3

a<-matrix(1:4,nrow=2)
b<- matrix(c(2,1,2,4),nrow=2)
a%*%b
###########################
x<-c(1,2,3,4)
y<-c(5,6)
 x+3
#(4,5,6,7) and
x+y
#(6,8,8,10)

#### logical operators

x<-c(TRUE,FALSE,TRUE)
y<-c(FALSE,TRUE,TRUE)

x|y
x||y

x&y
x&&y

################

myDate<- as.Date('10-Jan-1993',format="%d-%b-%Y")

class(myDate)
as.numeric(myDate)

##############

## making vectors
c(1,"a")
seq(1,10,by=2)# gives (1,3,5,7,9)
rep(c(1,2),times=3) #gives (1,2,1,2,1,2)
rep(c(1,2),each=3) #gives (1,1,1,2,2,2)
##

paste(c(1,2),c('x','y','z'))
paste(c(1,2),c('x','y','z'),collapse=' ')

sort(mydata$weight)
sort(mydata$weight,decreasing=TRUE)

table(rpois(20,5))
