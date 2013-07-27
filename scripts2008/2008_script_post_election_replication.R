#note: data and graphs appear roughly in the order they appear in the paper.
#some code is redundant and may involve more steps than strictly necessary, 
    #but this is done for for the sake of clarity.
library(arm)
library(foreign)
rm(list=ls(all=TRUE))
#block source

#get average district and total vote time series

#first get dem seats and total vote for 1946--2006; unit of observation here is the election year (i.e. "aggregate")
agg.house.data <- read.dta("House_1946_2006_aggregate.dta", convert.underscore = T)
attach.all(agg.house.data, overwrite=T)
unique.year.all <- unique(year)#create variable for each uniqye year
dem.seats.actual <- dem.seats
dem.seats <- dem.seats.per#Dem percentage of seats in House stemming from each election
#get total vote for 1946-2006
dem.per.total <- dem.per.total#dems percentage of total vote in each election

#get average district vote for all years
###########################################
detach()
house.4606 <- read.dta("House_1946_2006_updated.dta", convert.underscore = T) #1946-2006 data -- statecd is unit
house.4606 <- house.4606[house.4606$winner < 9,]#drop handful of 3rd party candidates
                        #not-including Bernie Sanders, who we count as a Democrat
detach()
attach.all(house.4606, overwrite=T)
unique.year.4606 <- unique(year)#create year var for 1946-2004 (helps avoid confusion when looping)
adv <- rep(NA, length(unique.year.4606))#create empty vector for imputed avg. district vote, 1946-2006
for (i in 1:length(unique.year.4606)){
    adv[i] <- mean(dvoteimputed[year==unique.year.4606[i]])#get adv for each year, 1946-2004
}


#use regressions from 1946-2006 (in non-redistricting years) 
  #to get coefficients to set parameter estimates below

house.4606no02 <- house.4606[year !=1952 & year != 1962 & year != 1972 
    & year !=1982 & year != 1992 & year != 2002,]
detach()
attach.all(house.4606no02, overwrite =T)
unique.year.4606no02 <- unique(year)
coefs <- array(NA, c(length(unique.year.4606no02 ), 4)) #create empty matrix to store coefficients
resid.errors <- rep(NA, length(unique.year.4606no02 )) #empty vector to store residual errors
for (i in 1:length(unique.year.4606no02 )){#loop over each 
    fit <- lm(dvoteimputed ~ dlagimputed + incumb +  
        partycontrol, subset = year == unique.year.4606no02[i])
    coefs[i,] <- coef(fit)
    resid.errors[i] <- sigma.hat(fit)
        }
start.year.indicator <- 21#1996
end.year.indicator <- 25#2006
rho <-  mean(coefs[start.year.indicator:end.year.indicator,2])#get rho by taking mean coef from 5 years leading up to election years: 
sigma <- mean(resid.errors[start.year.indicator:end.year.indicator])#get mean residual standard error from past 5 years
phi <- .08

#these come from yearly regression;see 2006 script
rmse.predict <- .022
rmse <- .0097
phi <- .08#estimated incumbency advantage
#thse also come from yearly regressions; while they're are based on rolling averages,
  #basically the same for 2006 and 2008, so define here for convience and use in both elections
rho <- .71
sigma <- .069 
inc.impute <- .75 #for uncontested races

n.sims <- 1000
##################################################################
#PREDICTING THE 2008 SEATS VOTES CURVE
  #plot 2006 and 2008 together

#get 2006 Data
#use 2004 vars from 2006 data to account for GA switch of 3rd and 8th districts
    #for s-v analyis
house.2006 <- read.dta("2006_house_data.dta", convert.underscore = T)
detach()
attach.all(house.2006, overwrite =T)
i2006 <- house.2006[,"incumb06"]#incumbency in 2006
unc2006 <- house.2006[,"uncontested06"]#uncontested in 2006
dvote.2006 <- house.2006$dvoteimputed#imputed vote in 2006
adv.2006 <- mean(dvote.2006) #mean avg. district vote (imputed)
winner.2006 <- house.2006$winner#winner of each race (1 for Dems, 0 for GOP
dem.seats.2006 <- mean(winner.2006) #percent of Dem seats (233/435); counting FL-13 for GOP
v2004 <- dlag06imputed #use imputed lagged vote to get 2004 vote for 2006 s-v prediction
i2004 <- incumb04 #incumbency in 04 for s-v prediction

#use same range for 2006 and 2008
vbar.range <- seq(.35,.65, .002) #create range from 35 to 65

vbar.2004 <- mean(v2004)
sbar.50.2006 <- rep (NA, length(vbar.range))#vector for predicted seats (based on medians)
prob.2006 <- rep (NA, length(vbar.range)) #set up vector for prob. of winning house
for (j in 1:length(vbar.range)){#loop over intervals of vbar
 vbar <- vbar.range[j]
 sbar <- rep (NA, n.sims) 
 for (i in 1:n.sims){
   v.adj2004 <- v2004 - phi*i2004 #adjusted vote, taking out incumbency
   normvote2006 <- .5 + rho*(v.adj2004 - .5) #normal vote
   locfree2006 <- normvote2006 + phi*i2006 #location free: normal vote plus adjusted vote
   locfreenoisy2006 <- rnorm(length(locfree2006), locfree2006, sigma) #add noise to loc.free var
   withuncs2006 <- ifelse (unc2006==-1, 1-inc.impute,
                           ifelse (unc2006==1, inc.impute, locfreenoisy2006))
   swingfree2006 <- withuncs2006 + mean(v2004) - mean(withuncs2006) #take out swing
   v2006 <- swingfree2006 + vbar - mean(swingfree2006) 
   #V2006 <- withuncs2006 + vbar + mean(v2004) - mean(withuncs2006) - mean(swingfree2006) 
   sbar[i] <- mean(v2006>.5)
 }
 sbar.50.2006[j] <- mean(sbar)
 prob.2006[j] <- pnorm((sbar.50.2006[j] - 0.5)/rmse.predict)#use empirical predictive error for historical regressions (above)
}
#get v.bars when prob. == 10, 50, 90%
ten.percent.value.2006 <- mean(vbar.range[round(prob.2006,1) ==.10])
fifty.percent.value.2006 <- mean(vbar.range[round(prob.2006,1) ==.50])
ninety.percent.value.2006 <- mean(vbar.range[round(prob.2006,1) ==.90])
#get predicted seats and probability based on adv. for 2006
pred.seats.2006 <- mean(sbar.50.2006[vbar.range==round(adv.2006,3)])
seats.error.2006 <- dem.seats.2006 - pred.seats.2006
bias.2006 <- mean(2*(sbar.50.2006[round(vbar.range,2)==.50]-.5))
predicted.prob.2006 <- prob.2006[vbar.range == round(adv.2006,3)]
#get probability dems would have won house if they got the same adv as GOP in 1994
predicted.prob.1994 <- prob.2006[vbar.range == round(1-adv[unique.year.all==1994],2)]
#get number of seats GOP would have won with dems average district vote.
gop.seats.pred <- 1-mean(sbar.50.2006[round(vbar.range,3)==round(1-adv.2006,3)])
print(ten.percent.value.2006)
print(fifty.percent.value.2006)
print(ninety.percent.value.2006)
print(predicted.prob.2006)  
dem.50.seats.2006 <- mean(sbar.50.2006[round(vbar.range,1) ==.50])

 
######################################################
#read in 2008 data
house.2008 <- read.dta("2008_house_updated_post_election.dta", convert.underscore = T)
detach()
attach.all(house.2008, overwrite =T)

dem.seats.2008 <- mean(winner, na.rm= T)
adv.2008 <- mean(dvoteimputed, na.rm = T)
i2008 <- incumb.08#incumbency in 2008
unc2008 <- uncontested.08#uncontested in 2008

vbar.2006 <- mean(dvote.2006)
sbar.50.2008 <- rep (NA, length(vbar.range))#vector for predicted seats (based on medians)
prob.2008 <- rep (NA, length(vbar.range)) #set up vector for prob. of winning house
for (j in 1:length(vbar.range)){#loop over intervals of vbar
 vbar <- vbar.range[j]
 sbar <- rep (NA, n.sims) 
 for (i in 1:n.sims){
   v.adj2006 <- v2006 - phi*i2006 #adjusted vote, taking out incumbency
   normvote2008 <- .5 + rho*(v.adj2006 - .5) #normal vote
   locfree2008 <- normvote2008 + phi*i2008 #location free: normal vote plus adjusted vote
   locfreenoisy2008 <- rnorm(length(locfree2008), locfree2008, sigma) #add noise to loc.free var
   withuncs2008 <- ifelse (unc2008==-1, 1-inc.impute,
                           ifelse (unc2008==1, inc.impute, locfreenoisy2008))
   swingfree2008 <- withuncs2008 + mean(v2006) - mean(withuncs2008) #take out swing
   v2008 <- swingfree2008 + vbar - mean(swingfree2008) 
   #V2008 <- withuncs2008 + vbar + mean(v2006) - mean(withuncs2008) - mean(swingfree2008) 
   sbar[i] <- mean(v2008>.5)
 }
 sbar.50.2008[j] <- mean(sbar)
 prob.2008[j] <- pnorm((sbar.50.2008[j] - 0.5)/rmse.predict)#use empirical predictive error for historical regressions (above)
}
#get v.bars when prob. == 10, 50, 90%
ten.percent.value.2008 <- mean(vbar.range[round(prob.2008,1) ==.10])
fifty.percent.value.2008 <- mean(vbar.range[round(prob.2008,1) ==.50])
ninety.percent.value.2008 <- mean(vbar.range[round(prob.2008,1) ==.90])
print(ten.percent.value.2008)
print(fifty.percent.value.2008)
print(ninety.percent.value.2008)
#get predicted seats and probability based on adv. for 2006
pred.seats.2008 <- mean(sbar.50.2008[round(vbar.range,2)==round(adv.2008,2)])
seats.error.2008 <- dem.seats.2008 - pred.seats.2008
bias.2008 <- mean(2*(sbar.50.2008[round(vbar.range,2)==.50]-.5))
predicted.prob.2008 <- prob.2008[round(vbar.range,2) == round(adv.2008,2)]
#essentially, pred.prob == 1
predicted.prob.2008 <- 1
#get probability dems would have won house if they got the same adv as GOP in 1994
predicted.prob.1994 <- prob.2008[vbar.range == round(1-adv[unique.year.all==1994],2)]
#get number of seats GOP would have won with dems average district vote.
gop.seats.pred <- 1-mean(sbar.50.2008[round(vbar.range,3)==round(1-adv.2008,3)])

#get predicted seats and probability based on adv. for 2008
pred.seats.2008 <- mean(sbar.50.2008[round(vbar.range,2)==round(adv.2008,2)])
seats.error.2008 <- dem.seats.2008 - pred.seats.2008
bias.2008 <- mean(2*(sbar.50.2008[round(vbar.range,2)==.50]-.5))

########################
#plot both curves and probabilities together
axis.size <- 1.1 #control size of axis tick marks and numbers
axis.label <- 1.1 #control size of axis labels
lower.bound <- .4#for axis limits
upper.bound <- .6

pdf("Seats_Votes_2006_2008_post_election.pdf", height = 4, width = 8)

#Predicted SV Curves
  #2006
par (mfrow=c(1,2), mar = c(7,6,2,3), pty = "s")
plot(vbar.range, sbar.50.2006, type = "n", 
    ylab = "",  xlab = "", axes = F, xaxs="r", yaxs="r",
    ylim = c(lower.bound, upper.bound), xlim = c(lower.bound,upper.bound),, 
    main = "")
#lines(lowess(vbar.range.2006, sbar.50.2006))
axis(1, at = c(.40,.45, .50,.55,.60), label = c("40%", "", "50%", "", "60%"), mgp = c(2,.5,0), cex.axis = axis.size)
axis(2, at = c(.40,.45, .50,.55,.60), label = c("40%", "", "50%", "", "60%"), las = 2, mgp = c(2,.5,0), cex.axis = axis.size)
mtext("Average district vote\nfor Democrats", 1, cex = axis.label, line = 3.1)
mtext("Democratic share\nof House seats", 2, cex = axis.label, line=3.1)
abline(h=.5,  col="gray", lwd=.5)
abline(v=.5,  col="gray", lwd=.5)
points(adv.2006, dem.seats.2006, type = "p", pch = 19, cex = .8, col = "blue")
points(adv.2008, dem.seats.2008, type = "p", pch = 19, cex = .8, col = "black")

box()
#add curves
points(vbar.range, sbar.50.2006, type = "l", lty = 2, col = "blue")#2006  
points(vbar.range, sbar.50.2008, type = "l", lty = 1, col = "dark green")#2008
#label curves
text(.53, .48, "2006")
text(.515, .58, "2008")

#PROBABILITY CURVES
par (mar = c(7,6,2,3))
plot (vbar.range, prob.2006, type="n", 
    main = "", 
    xlab ="", ylab ="", axes = F,  xaxs="r",
     yaxs="r", xlim = c(lower.bound,upper.bound), ylim = c(0,1))
axis(1, at = c(.40,.45, .50,.55,.60), label = c("40%", "", "50%", "", "60%"), mgp = c(2,.5,0), , cex.axis = axis.size)
axis(2, at = c(seq(0,1,by =.25)), label = c(0,".25",".5",".75",1), las = 2, mgp = c(2,.5,0), cex.axis = axis.size)
mtext("Average district vote\nfor Democrats", 1, cex = axis.label, line = 3.1)
mtext("Probability Democrats\ncontrol House", 2, cex = axis.label, line =2.7)
abline (h=.5,col="gray", lwd=.5)#50% line
abline (h=.1, col="gray", lwd=.5)#10 % line
abline (h=.9, col="gray", lwd=.5)#90% line
#add curves
points(vbar.range, prob.2006, type="l", lty =2,  col = "blue")#2006
points(vbar.range, prob.2008, type="l", lty = 1, col = "dark green")#2008
#add point for actual avd vote and predicted probabloty
points(adv.2006, predicted.prob.2006, type = "p", pch = 19, cex = .8, col = "blue")
points(adv.2008, predicted.prob.2008, type = "p", pch = 19, cex = .8, col = "black")

text(.56, .76, "2006")
text(.465, .63, "2008")
box()

dev.off()
#########################
#
