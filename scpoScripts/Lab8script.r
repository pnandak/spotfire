#run R as batch mode
#source("C:/Users/Iris/Documents/PS231A/lab8/Lab8script.r", echo=T)

#sink the console to destinated location
sink("C:/Users/Iris/Documents/PS231A/Lab8outApril111.txt", type="output")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#LAB 8
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TOPICS COVERED:
# STANDARD NORMAL DISTRIBUTION
# ANY NORMAL DISTRIBUTION
# UNIFORM DISTRIBUTION
# SAMPLING
# CENTRAL LIMIT THEOREM/SAMPLING DISTRIBUTION OF MEANS

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#PART 1---NORMAL DISTRIBUTION
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FOUR KEY TERMS
# P: PROBABILTY ASSOCIATED WITH A SPECIFIC QUANTILE
# q: QUANTILE ASSOCIATED WITH A SPECIFIC PROBABILITY
# d: HEIGHT (ORDINATE) OF A STATISTICAL DISTRIBUTION
# r: VALUE SELECTED AS RANDOM FROM A GIVEN DISTRIBUTION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#FOR NORMAL DISTRIBUTION
# ROOT NAME = norm
# CUMULATIVE PROBABILITY= pnorm()
# pnorm(x) is the area of probability density function to the LEFT of the value x under the curve describing the standard normal probability distribution


# WITH **STANDARD NORMAL** N~(0,1)
pnorm(1.96)              #1.96 is a z-stat with 0.975 area to the left of this value
pnorm(1.644854)
pnorm(c(-0.5, 0.5))

# WITH ANY NORMAL DISTRIBUTION N~(m, s) where m=mean; s=standard deviation
pnorm(2, 0,2)
pnorm(1:5, 3, 1.5)

#QNORM
#GET QUANTILES FOR THE STANDARD NORMAL DISTRIBUTION FOR SPECIFIC INPUT PROBABILITIES
qnorm(0.95)
qnorm(0.975)

#DNORM
#HEIGHT OF THE STANDARD NORMAL PROBABILITY DISTRIBUTION
dnorm(-0.1)
dnorm(0)
dnorm(0.1)

#rnorm()
#VALUES SELECTED AT RANDOM FROM A STANDARD NORMAL DISTRIBUTION
rnorm(50)

a<-rnorm(10)
pnorm(a)  #i.e. the same as pnorm(rnorm(10))
          #to get back the area to the left of the value

#GENERATE RANDOM NUMBERS FROM ANY NORMAL DISTRIBUTION
set.seed(666)        #for class demonstration, I set the seed                         
                     #so we all get the same result 
a<-rnorm(100, 5, 1)  #generate 100 random numbers from a normal distribution                 
                  #with mean=5 and st dev=1

#now check the numbers
mean(a)
sd(a)     #approx. to the mean/sd we specified

#To INCREASE THE SAMPLE SIZE

aa<-rnorm(1000)
plot(density(aa))         #examine the density plot

bb<-rnorm(1000, 5, 1)
plot(density(bb), xlim=c(-1, 10))         #examine the density plot
mean(bb)
sd(bb)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#PART II---UNIFORM DISTRIBUTION
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
runif(50)   #choose 50 random numbers between 0 and 1

round(runif(50),0)    #use round() to get integers


runif(50, 10, 20)   #choose 50 random numbers between 10 and 20

round(runif(50, 10, 20))      #to get integers


#flip a coin
sum(ifelse(runif(10)<.5, 1,0))
sum(ifelse(runif(100)<.5, 1,0))
sum(ifelse(runif(1000)<.5, 1,0))
sum(ifelse(runif(10000)<.5, 1,0))      #closer to .5 as n increases






#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#PART III---SAMPLING
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                    
#SIMPLE RANDOM SAMPLING (SRS)
#USING SAMPLE() COMMAND

#SAMPLE(SAMPLE SPACE, NUMBER OF SAMPLE)
sample(1:40,5)

#WITH OR WITHOUT REPLACEMENT
sample(c("h", "t"),10, replace=T)  #default is replace=F

#SAMPLING WITH PROBABILITY
#E.G. TO SIMULATE DATA WITH UNEQUAL PROBABILITY OF OUTCOME
sample(c("h", "t"),10, replace=T, prob=c(0.8, 0.2)) 


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#PART IV---CENTRAL LIMIT THEOREM/
#SAMPLING DISTRIBUTION OF THE MEANS
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#LAURA USED THE URN EXAMPLE AT THE BEGINNING OF SEMESTER
#URN CONSISTS OF THREE BALLS--1,3,5
urn<-c(1,3,5)
mean(urn)         #mean=3
prop.table(table(urn))

#SRS n=2 with replacement
n2<-sample(urn, 2, replace=T)      #possible to draw e.g. 5,5

mean(n2)

#SRS n=2 without replacement
n2nr<-sample(urn, 2, replace=F)    #impossible to draw e.g. 5,5

mean(n2nr)

#now stay with sampling with replacement
#draw 2 balls each time
#repeat the process 50 times
d2<-matrix(sample(urn, 2*50, replace=T),ncol=2, nrow=50)
d2_mean<-apply(d2, 1, mean)
mean(d2_mean)    
plot(density(d2_mean), xlim=c(0,8))
abline(v=3, col='red', lwd=5)

#draw 5 balls each time
#repeat the process 50 times

d5<-matrix(sample(urn, 5*50, replace=T),ncol=5, nrow=50)
d5_mean<-apply(d5, 1, mean)
mean(d5_mean)    
plot(density(d5_mean), xlim=c(0,8))
abline(v=3, col='red', lwd=5)

#draw 10 balls each time
#repeat the process 50 times

d10<-matrix(sample(urn, 10*50, replace=T),ncol=10, nrow=50)
d10_mean<-apply(d10, 1, mean)
mean(d10_mean)    
plot(density(d10_mean), xlim=c(0,8))
abline(v=3, col='red', lwd=5)

#draw 25 balls each time
#repeat the process 50 times

d25<-matrix(sample(urn, 25*50, replace=T),ncol=25, nrow=50)
d25_mean<-apply(d25, 1, mean)
mean(d25_mean)    
plot(density(d25_mean), xlim=c(0,8))
abline(v=3, col='red', lwd=5)

#draw 100 balls each time
#repeat the process 50 times

d100<-matrix(sample(urn, 100*50, replace=T),ncol=100, nrow=50)
d100_mean<-apply(d100, 1, mean)
mean(d100_mean)    
var(d100_mean)    
plot(density(d100_mean) , xlim=c(0,8))
abline(v=3, col='red', lwd=5)

#draw 500 balls each time
#repeat the process 50 times

d500<-matrix(sample(urn, 500*50, replace=T),ncol=500, nrow=50)
d500_mean<-apply(d500, 1, mean)
mean(d500_mean)   
var(d500_mean)     
plot(density(d500_mean),  xlim=c(0,8), col='blue')
abline(v=3, col='red', lwd=2)


#summary
plot(density(d2_mean), xlim=c(0,8), ylim=c(0,3), lwd=3, main="sampling distribution of the means", xlab="different sample size")
lines(density(d5_mean), col='green', lwd=3)
lines(density(d10_mean), col='purple', lwd=3)
lines(density(d25_mean), col='blue', lwd=3)
lines(density(d100_mean), col='red', lwd=3)
abline(v=3, lty=2, lwd=3)

#more summary
plot(density(d2_mean), xlim=c(0,8), ylim=c(0,7), lwd=3, main="sampling distribution of the means", xlab="different sample size")
lines(density(d5_mean), col='green', lwd=3)
lines(density(d10_mean), col='purple', lwd=3)
lines(density(d25_mean), col='blue', lwd=3)
lines(density(d100_mean), col='red', lwd=3)
lines(density(d500_mean), col='orange', lwd=3)
abline(v=3, lty=2, lwd=3)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#APPENDIX: ROUNDING
#THREE WAYS TO ROUND
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

a<-seq(1,2,by=0.1)          #create a sequence of numbers from 1 to 2, each separated by 0.1

round(a)   #values less than 1.5 gets round DOWN to 1; 1.5 or greater gets round UP to 2
ceiling(a) #anything greater than 1 gets round UP to 2
floor(a)   #anything greater than 1 but smaller than 2 gets round DOWN to 1
