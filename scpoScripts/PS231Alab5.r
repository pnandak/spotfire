#run R as batch mode
#source("C:/Users/Iris/Documents/PS231A/Lab5.r", echo=T)

#sink the console to destinated location
sink("C:/Users/Iris/Documents/PS231A/Lab5outMarch7.txt", type="output")

#lab5
#Topics covered
#Part I
#one sample t.test
#two sample t.test


#Part II: Increasing efficiency with
#function()
#apply()
#tapply()
#lapply()

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#                    PART I
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
#~~~~~~~~~~~~~~
#one sample t.test
#~~~~~~~~~~~~~~
#let's try a simple example to see how t.test work
#create a object "a"

a<-1:20
mean(a)

#null hypothesis: pop mean=10
#alternative hypothesis H1: popmean =/= 10 (two tail test)
#alternative hypothesis H2: popmean > 10 (right tail test)

t.test(a, mu=10) #default=two tail test ; t-stat=0.378
t.test(a, mu=10, alternative=c("greater")) #right tail; note change in p-value

#let's do the calculation by hand to double-check answer
#recall the formula for t-test
# (yobs-yexpected)/ESE
#where ESE=sqrt(var(yobs)/n))

#calculate the t-stat by hand
diffmean<-mean(a)-10
ese<-sqrt(var(a)/20)
tout<-diffmean/ese           #nice! we get the same t-stats 0.378!

#~~~~~~~~~~~~~~
#two sample t.test--assuming equal variance (homoskedasticity)
#~~~~~~~~~~~~~~
#t-formula
#(obs_diff-expected_diff)/ESE
# assuming equal variance
# pooled sample variance
#(N1-1)*var(N1)+(N2-1)*var(N2)/((N1-1)+(N2-1)-2)
#ese=sqrt(pooledvar*(1/N1+1/N2))

#use rnorm() to generate two variables, both with mean=5, sd=1
b<-rnorm(20, mean=5, sd=1)
b2<-rnorm(25, mean=5, sd=1)

mean(b)     #becoz it's randomly generated, the mean is roughly (not exactly)=5
mean(b2)
var(b)
var(b2)

#now test diff in means, assume homoskedasticity
t.test(b, b2, var.equal=TRUE) 

#now let's check work by hand
diffmeanb<-mean(b)-mean(b2)
sb1<-(length(b)-1)*var(b)
sb2<-(length(b2)-1)*var(b2)
sp<-(sb1+sb2)/(length(b)+length(b2)-2)
seb<-sqrt(sp*(1/length(b)+1/length(b2)))
diffmeanb/seb      #eureka! same t-stat 

#without assuming equal variance
#ESE=sqrt(var(x1)/N1+var(x2)/N2)
#now test diff in means 
t.test(b, b2) 
#now let's check work by hand
diffmeanb<-mean(b)-mean(b2)
seb2<-sqrt(var(b)/length(b)+var(b2)/length(b2))
diffmeanb/seb2


#F test equal variance?
var.test(b, b2)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#                    PART II
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#=====================================
#Increasing effiency WITH FUNCTION(), APPLY() ETC.
#=====================================

#=============================
#Download NES file from ICPSR
#www.icpsr.umich.edu
#ICPSR 3740
#NES 2002 Main data file
#=============================


#read in data
#convert.factor=FALSE asks R not to bring over the data dictionary

library(foreign)

nesda<-read.dta("C:/Users/Iris/Documents/PS231A/03740-0001-Data.dta", convert.factor=FALSE)
dim(nesda) #check dimension
names(nesda) #check out the var names

attach(nesda)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#function()
#you can write your own function
#and nest it with apply, tapply etc.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#EXAMPLE1
funexp<-function(x){
  x*3}

funexp

x<-1:10
funexp(x)

#EXAMPLE2  
#i create a homemade command to calculate covariance
#corr(x,y)=cov(x,y)/(sd(x)*sd(y)
#hence cov(x,y)=corr(x,y)*(sd(x)*sd(y))
#hence cov(x,y)=corr(x,y)*sqrt(var(x)*var(y))


covfun<-function(x,y){
  sqrt(var(x)*var(y))*cor(x,y)
  }
x0<-c(1,2,4,5,6,2,2)  
y0<-c(11,22,14,15,26,12,22)  

cov(x0, y0)  #using R built in cov() command
covfun(x0,y0) #using my homemade command

var(x0,x0)      #cov with itself=variance
covfun(x0,x0)                            



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#APPLY()
#apply() returns a vector or array or list of values obtained by applying a 
#function to margins of an array 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#does attention to campaign vary by region?
#v021203:region of respondents; V023001:pay attention to campaign
regtab<-table(V021203, V023001)

#dress up table
rownames(regtab)<-c("NE", "North Central", "south" , "west")
colnames(regtab)<-c("very much interest", "somewhat interest", "not interest")


#we have used margin.table to add the row sum (see session 2)
#now we use apply()

apply(regtab, 1, sum)   #1=operate by ROW; same as margin.table(regtab,1)
apply(regtab, 2, sum)   #2=operate by COLUMN; same as margin.table(regtab,2)

apply(regtab, 1, mean)   #1=operate by ROW; 
apply(regtab, 2, mean)   #2=operate by COLUMN;

#you can use apply() on many variables/vectors 
ftscore<-nesda[, 130:140]     #a number of FTScores

meanftscore<-apply(ftscore, 1, mean)   #get a mean for EACH FTscore

#respondents with at least one NA in these FTScores will NOT get a mean score
#to count up how many NAs for EACH respondent

nmiss<-apply(ftscore, 1, function(ftscore){
sum(is.na(ftscore))
})

#let's combine the mean score and number of NAs together 
examout<-cbind(ftscore, meanftscore, nmiss)

#examine the first 50 observations
examout[1:50,]



#~~~~~~~~~~~~~~~~~~~~~~~~
#tapply 
#allows summary values to ve calculated based on the values contained in 
#each CELL of a table
#tapply (VECTOR, categories, function)
#~~~~~~~~~~~~~~~~~~~~~~~~

#LET'S BEGIN WITH A SIMPLE EXAMPLE
aa<-1:20
grp<-rep(1:4, each=5)
aagrp<-cbind(aa, grp)

#how many distinct groups?
factor(grp)

#find group mean
tapply(aa, grp, mean)

#find group median
tapply(aa, grp, median)

#find group variance
tapply(aa, grp, var)

#LET'S USE REAL DATA
#what's the mean campaign interest by region?
#oops..there're NA in V023001
tapply(V023001, V021203, mean)    #group1 and 3 have NAs

#to use function() & na.rm option
tapply(V023001, V021203, function(V023001) {
mean(V023001, na.rm=T)
})

#~~~~~~~~~~~~~~~~~~~
#LAPPLY()
#To aplly a function over a list or vector
#esp. useful if you want to repeat the same process to many variables(columns)
#~~~~~~~~~~~~~~~~~~~~~

#to produce summary statisitcs for a bunch of variables
#combine a bunch of FTscores
ftscore<-nesda[, 130:140]

lapply(ftscore, summary)
#lapply(ftscore, sd)         #oops...NAs

lapply(ftscore, function(ftscore){
sd(ftscore, na.rm=T) 
})

#first divide each FTScore by 100 then do summary of each FTScore
lapply(ftscore, function(ftscore){
summary(ftscore/100)
})

#which is the SAME as:
apply(ftscore, 2, function(ftscore){
summary(ftscore/100)
})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#T TEST WITH REAL DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#use post-election GWBFT score
summary(V025043)

#sample mean (y-bar)=65.97
#now suppose you want to do a one-sample t.test,
#given null hypothesis that pop mean=64

t.test(V025043)   #default=2-tail test   #if mu not given, R assumes mu=0!!!
t.test(V025043, mu=64)   #default=2-tail test   #if mu not given, R assumes mu=0
t.test(V025043, mu=64, alternative=c("greater"))   #one-tail; test if yobs>popmean
                                                   #notice the diff in p-value

#diff-in-means test
#test whether GWBFT score vary by gender
table(V023153) #sex
factor(V023153)

t.test(V025043~V023153, data=nesda)   #default=2-tail test    
                  #V023153 (sex) composes of 2 groups: males and females
                  # ~V023153 tells R to do diff-in-means t.test for these two    
                  #distinct groups 

#this save the workspace                           
#save.image(file="C:/Users/Iris/Documents/PS231A/Lab5image.RData")                         

#install package (session)
#library(session)
#this will save the whole workspace, dataset, library loaded etc.
#save.session("C:/Users/Iris/Documents/PS231A/Lab5output2.Rda")
  