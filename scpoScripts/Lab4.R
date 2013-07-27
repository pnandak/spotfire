#Sijeong lim

################################Multinomial Logit############################
rm(list=ls())
options(scipen=10)

library(MASS)
library(tile)
library(simcf)
library(nlme)
library(verification)
library(ROCR)
library(Hmisc)
library(nnet) # for multinomial logit, download from CRAN
library(foreign) # we will use data on the web

source("avp.r")

data <- read.csv(url("http://www.ats.ucla.edu/stat/r/dae/mlogit.csv"))
#data<-read.csv("mlogit.csv", header=TRUE)

summary(data)
names(data) # y=choice of brand among 1, 2, 3 /  x= age and female
attach(data)


mlogit.result<-multinom(brand~female+age, Hess=TRUE, na.rm=T)
names(mlogit.result)
pe<-mlogit.result$wts[c(6,7,8,10,11,12)]
vc<-solve(mlogit.result$Hess) 
ll<-mlogit.result$deviance/-2
se<-sqrt(diag(vc)) 


################################################B


sims <- 10000
simbetas <- mvrnorm(sims,pe,vc)       # draw parameters, using MASS::mvrnorm
agerange <- seq(24,38, by=1)          # range of counterfactual ages
femalerange <- c(0,1)                 # range of counterfactual sexes
simycat1 <- simycat2 <-simycat3<- cfactage <- cfactfemale <- NULL
for (ife in 1:length(femalerange)) { # loop over sex scenarios
    for (jage in 1:length(agerange)) { # loop over age scenarios

        # Set up a hypothetical X vector for this scenario
        hypx <- rbind(1, femalerange[ife], agerange[jage])

        # Calculate simulated MNL denominators for this scenario
        simdenom <- (1 + exp(simbetas[,1:3]%*%hypx) + exp(simbetas[,4:6]%*%hypx))

        # Add simulated probabilities for each category to storage vectors
        simycat1 <- c( simycat1, 1/simdenom ) 
        simycat2 <- c( simycat2, exp(simbetas[,1:3]%*%hypx)/simdenom )        
        simycat3 <- c( simycat3, exp(simbetas[,4:6]%*%hypx)/simdenom )

        # Save hypothetical X's to storage vectors:
        # must match simulated probabilities element for element
        cfactage <- c(cfactage, rep(agerange[jage],sims) ) #rep each age for 10000 times
        cfactfemale <- c(cfactfemale, rep(femalerange[ife],sims) ) # rep 0 for 150000 times, rep 1 for 150000 times 
    }
} # total 300000 (10000 for each scenario) prob estimated for each category!

######################################################C

# Create one trace for each predicted category of the response (brand) and of gender, so total 6 traces

#male choosing brand 1
trace1 <- lineplot(x=cfactage[cfactfemale==0],
                   y=simycat1[cfactfemale==0],
                   simulates="y",
                   ci=list(mark="shaded",levels=0.67),
                   extrapolate=list(data=cbind(age,female),
                                    cfact=cbind(agerange,rep(0,length(agerange))),
                                    omit.extrapolated=FALSE),
                   col="black",
                   plot=1
                   )
#male choosing brand 2
trace2 <- lineplot(x=cfactage[cfactfemale==0],
                   y=simycat2[cfactfemale==0],
                   simulates="y",
                   ci=list(mark="shaded",levels=0.67),
                   extrapolate=list(data=cbind(age,female),
                                    cfact=cbind(agerange,rep(0,length(agerange))),
                                    omit.extrapolated=FALSE),
                   col="blue",
                   plot=1
                   )

#male choosing brand3
trace3 <-lineplot(x=cfactage[cfactfemale==0],
                   y=simycat3[cfactfemale==0],
                   simulates="y",
                   ci=list(mark="shaded",levels=0.67),
                   extrapolate=list(data=cbind(age,female),
                                    cfact=cbind(agerange,rep(0,length(agerange))),
                                    omit.extrapolated=FALSE),
                   col="red",
                   plot=1
                   )

#female choosing brand1
trace4 <- lineplot(x=cfactage[cfactfemale==1],
                   y=simycat1[cfactfemale==1],
                   simulates="y",
                   ci=list(mark="shaded",levels=0.67),
                   extrapolate=list(data=cbind(age,female),
                                    cfact=cbind(agerange,rep(0,length(agerange))),
                                    omit.extrapolated=FALSE),
                   col="black",
                   plot=2
                   )

#female choosing brand2
trace5 <- lineplot(x=cfactage[cfactfemale==1],
                   y=simycat2[cfactfemale==1],
                   simulates="y",
                   ci=list(mark="shaded",levels=0.67),
                   extrapolate=list(data=cbind(age,female),
                                    cfact=cbind(agerange,rep(0,length(agerange))),
                                    omit.extrapolated=FALSE),
                   col="blue",
                   plot=2
                   )

#female choosing brand3                  
trace6 <- lineplot(x=cfactage[cfactfemale==1],
                   y=simycat3[cfactfemale==1],
                   simulates="y",
                   ci=list(mark="shaded",levels=0.67),
                   extrapolate=list(data=cbind(age,female),
                                    cfact=cbind(agerange,rep(0,length(agerange))),
                                    omit.extrapolated=FALSE),
                   col="red",
                   plot=2
                   )


linelables<-textTile(labels=c("brand1", "brand2", "brand3"), 
                     x=c(27,   27,   27), 
                     y=c(0.6,  0.3, 0.1),
                     col=c("black", "blue", "red"),
                     cex=0.75, 
                     plot=c(1,2))


at.x <- seq(24,38,1)
at.y <- c(0,0.2,0.4,0.6,0.8,1)

# Plot traces using tile
tc <- tile(trace1, trace2, trace3, trace4, trace5, trace6, linelables,
           RxC = c(1,2),
           limits = c(25,40,0,1),
           output = list(wide=6.5,outfile="mlogit",type="pdf"),
           xaxis = list(at=at.x),
           yaxis = list(at=at.y),
          xaxistitle = list(labels=c("age","age")),
          yaxistitle = list(labels=c("Pr(brand choice)","Pr(brand choice)")),
           undertitle = list(labels=c("Male","Female")),
         maintitle = list(labels=c("Impact of Age on Brand Choice")),
          gridlines = list(type="xy"),
           frame=TRUE
           )