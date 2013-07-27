#########################################################
##R Programming Session 3
##########################################################
mydata<- read.csv('chickwt.csv',as.is=TRUE)
summary(mydata)
mydata[,2]<- factor(mydata[,2])
summary(mydata)

mydata<- read.csv('chickwt2.csv')
summary(mydata)
mydata[,2]<- factor(mydata[,2])
summary(mydata)

(mymat<-matrix(1:12,3,4))
##Entries go down columns unless you specify byrow=TRUE.
mymat<-matrix(1:12,3,4,byrow=TRUE)
mymat
dim(mymat) #

myarr<- mymat
dim(myarr)<- c(3,2,2)
myarr[,,1]

### indexing vectore
x<- c(2,4,6,8,10,12)
x[c(1,3,6,5)]  #2,6,12,10
use<- c(rep(TRUE,3),FALSE,TRUE,FALSE)
x[use] ##will give 2,4,6,10
x[-c(1:3)] #will give 8,10,12
x[]

##recycling, extending, extracting outside
x[c(1,3)]<- 4.5
x[10]<- 8
x[11]
x
### indexing matrices and arrays

mymat[1:2,-2]
mymat[1:2,1] #is a vector

mymat[1:2,1,drop=FALSE] # is a matrix

mydata<- read.csv('chickwt.csv')

mydata[1:2,1] #is a vector
mydata[1,1:2] #is a data.frame
class(mydata[1:2,1])
class(mydata[1,1:2])
mydata[mydata$weight>400,]

##functions
x<- rnorm(100,mean=0.3,sd=1.2)
std.dev<- function(x) sqrt(var(x))
t.test.p<- function(x,mu=0)
{
    n <- length(x)
    t <- sqrt(n) * (mean(x)-mu)/std.dev(x)
    2 * (1 - pt(abs(t), n-1))
}

std.dev(x)
t.test.p(x)
t.test.p(mu=1,x=x)
t.test.p(x,1)

myfn1<- function(obs=10,n=100)
{
    x<- rep(NA,100)
    for (i in 1:n)
    {
        tmp<- runif(obs)
        x[i]<- mean(tmp)
    }
    list(mn=mean(x),std=sd(x))
}

myfn2<- function(obs=10)
{
    x<- runif(obs)
    while(mean(x)<0.45)
    {
        obs<- 2*obs
        x<- runif(obs)
    }
    list(mn=mean(x),std=sd(x),obs=obs)
}

myfn3<- function(obs=10)
{
    repeat
    {
        x<- runif(obs)
        if(mean(x)>=0.45)
            break
        obs<- 2*obs
    }
    list(mn=mean(x),std=sd(x),obs=obs)
}

myfn1(n=50)
myfn2()
myfn3(20)

####
x<-c(0,1,1,2)
y<-c(44,45,56,77)

z<- rep(NA,4)
for (i in 1:length(x))
{
    if (x[i]>0)
        z[i]<- y[i]/x[i]
    else
        z[i]<- y[i]/99
}
z
#or
(z<- ifelse(x,y/x,y/99))
##
#######
mylist<-list(x=x,y=y,mymat=mymat)
lapply(mylist,length) ## note length of a matrix
sapply(mylist,length)

apply(mymat,2,sum,na.rm=TRUE)
###tapply
tapply(mydata$weight,mydata$feed,mean)
by(mydata[,'weight'],mydata$feed,mean)
(mylist<-split(mydata,mydata$feed))

##

cbind(c(1,2,3),c(4,5,6))
rbind(c(1,2,3),c(4,5,6))
df1<- mylist[[1]][,c(2,1)]
df2<- mylist[[2]]
rbind(df1,df2)
cbind(df1[1:10,],df2)

###########
##merge#


authors <- data.frame(
    surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
    nationality = c("US", "Australia", "US", "UK", "Australia"),
    deceased = c("yes", rep("no", 4)))
books <- data.frame(
    name = I(c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core")),
    title = c("Exploratory Data Analysis",
              "Modern Applied Statistics ...",
              "LISP-STAT",
              "Spatial Statistics", "Stochastic Simulation",
              "Interactive Data Analysis",
              "An Introduction to R"),
    other.author = c(NA, "Ripley", NA, NA, NA, NA,
                     "Venables & Smith"))

(m1 <- merge(authors, books, by.x = "surname", by.y = "name"))
(m2 <- merge(books, authors, by.x = "name", by.y = "surname"))
## "R core" is missing from authors and appears only here :
merge(authors, books, by.x = "surname", by.y = "name", all = TRUE)
##matrices

mymat
mymat2<- matrix(1:12,nrow=3,byrow=TRUE)

mymat+mymat2
mymat%*%t(mymat2)

mysq<- matrix(rnorm(9),nrow=3)
solve(mysq)

mysym<- mysq
mysym[lower.tri(mysym)]<- mysym[upper.tri(mysym)]
eigen(mysym)
colSums(mymat)
