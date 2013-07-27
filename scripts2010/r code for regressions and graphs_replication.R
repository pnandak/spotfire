rm(list=ls(all=TRUE))
library("arm")

setwd("/Users/jkastell/Documents/Server/SC Nominations/JOP Submission/Final submission files and replication files/Replication Data and Code")

#read in state estimates of public opinion: unit is state-nominee
nominees <- read.csv("all_nominees_estimates_long.csv") #note DC not in data
nominees$nominee <- nominees$nominee.long
nominees$state <- nominees$state.long
nominees$state.nominee <- paste(nominees$state, nominees$nominee)
attach.all(nominees, overwrite = TRUE)    

#merge opinion estimates onto roll call data -- unit is now senator-nominee
roll.call <- read.dta("roll_call_data.dta",  convert.underscore =TRUE)
dim(roll.call)
roll.call$state <- roll.call$ostate 
dim(roll.call)
roll.call.merged <- merge(roll.call, nominees)
dim(roll.call.merged)
roll.call.merged <- roll.call.merged[order(roll.call.merged$nominee),]

#merge Statelevel info onto roll call data
Statelevel <- read.dta("state_level_update.dta",convert.underscore = TRUE)
Statelevel <- Statelevel[order(Statelevel$sstate.initnum),]
Statelevel$ideology[Statelevel$sstate=="AK"] <- -21.6205
Statelevel$ideology[Statelevel$sstate=="HI"] <- -8.356
Statelevel$ideology[Statelevel$sstate=="NV"] <- -12.5819989013672
Statelevel$ostate <- Statelevel$sstate
Statelevel$ostate.initnum <- Statelevel$sstate.initnum

detach()
attach.all(Statelevel)
temp.1 <- data.frame(ostate.initnum, ideology)
dim(roll.call.merged)
roll.call.merged <- merge(roll.call.merged, temp.1)
dim(roll.call.merged)
detach()
attach.all(roll.call.merged, overwrite = TRUE)
#create state-level ideology var defined in terms of president: 
  #cs1 == senator common space score
  #ideology = state.ideology
roll.call.merged$senator.ideo.opposing.pres <-  ifelse(pres.dem == 1, rescale(cs1), -rescale(cs1))
roll.call.merged$ideology.temp <- 100 * (roll.call.merged$ideology - min(roll.call.merged$ideology)) / (max(roll.call.merged$ideology)- min(roll.call.merged$ideology))
roll.call.merged$state.ideo.opposing.pres <-  ifelse(pres.dem == 1, rescale(roll.call.merged$ideology.temp), -rescale(roll.call.merged$ideology.temp))
roll.call.merged <- roll.call.merged[order(roll.call.merged$nominee),] # resort so nominees are in alpha order
detach()
attach.all(roll.call.merged, overwrite = TRUE)

#create easier to use variable names, 
n <- length(vote)
roll.call.merged$lack.quality <- nom.lackquality
roll.call.merged$distance <- eucldist
roll.call.merged$same.party <- sameprty
roll.call.merged$sen.democrat <- democrat#to distinguish senators from pres
roll.call.merged$strong.pres <- strngprs
roll.call.merged$nominee.cs <- csnom
roll.call.merged$senator.cs <- cs1
roll.call.merged$state.ideology <- ideology
roll.call.merged$state <- ostate
roll.call.merged$nominee.state <-  paste(roll.call.merged$nominee,roll.call.merged$ostate.initnum)
#using opinion of opholders as "opinion", throughout code below
roll.call.merged$opinion <- statepred.confirm.withop
roll.call.merged$opinion.all <- statepred.confirm

detach()
attach.all(roll.call.merged)
dim(roll.call.merged)

#export data from use in other programs (e.g. Stata)
#write.csv(roll.call.merged, "roll_call_data_with_opinion_estimates.csv")

#remove sotomayor here -- we will predict her vote below
roll.call.with.sotomayor <- roll.call.merged #so we can return to her data below
roll.call.merged <- roll.call.merged[roll.call.merged$nominee !="sotomayor",] #drop sotomayor for now--we will predict her vote at end

detach()
attach.all(roll.call.merged) 
    
#create aggregate variables (means)
mean.opinion <- tapply(opinion, factor(nominee), mean)
mean.lack.quality <-  tapply(lack.quality, factor(nominee), mean)
#create unique variables
unique.quality <- tapply(lack.quality, factor(nominee), unique)
unique.approval <- tapply(pres.approval, factor(nominee), unique)
unique.strong.pres <- tapply(strong.pres, factor(nominee), unique)


#RUN SCRIPT TO CREATE FIGURE 1 (HEAT PLOT AND HISTOGRAMS)
source("create_figure_1.R")

################################################################
#for now, drop miers (since she didn't receive a vote)
roll.call.no.miers <-   roll.call.merged[roll.call.merged$nominee != "miers",]
detach()
attach(roll.call.no.miers)
#####################################################3
#####################################################

#RUN SCRIPT TO CREATE FIGURE 2 (LOGIT CURVES)
source("create_figure_2.R")

#RUN SCRIPT TO CREATE FIGURE 3 (opinion vs distance)
source("create_figure_3.R")


#############################
#MODELS FOR PAPER
#############################

#for calculating proportion reduction in error
   #100 x [(% correctly predicted - % in modal category )/(100%-% in modal category.]
modal.cat  <- mean(vote)

#1) Replicate Epstein et al for our nominees using plain logit
  #now add state ideology for 2,3, 5,6
  
logit.1 <- glm(vote ~ rescale(lack.quality) + rescale(distance) +  
  same.party + strong.pres, family=binomial(link="logit"))
display(logit.1)
pred.logit.1 <- logit.1$fitted.values >=.5
correct.logit.1 <- pred.logit.1 == vote
correct.percent.logit.1 <- sum(correct.logit.1)/length(vote)
prop.reduct.logit.1 <- 100*((100*correct.percent.logit.1 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.logit.1
prop.reduct.logit.1
aic.logit.1 <- AIC(logit.1)
aic.logit.1


#2) add in opinion

logit.2 <- glm(vote ~ rescale(opinion) + rescale(lack.quality) + rescale(distance) +  
  same.party + strong.pres, family=binomial(link="logit"))
display(logit.2)
pred.logit.2 <- logit.2$fitted.values >=.5
correct.logit.2 <- pred.logit.2 == vote
correct.percent.logit.2 <- sum(correct.logit.2)/length(vote)
prop.reduct.logit.2 <- 100*((100*correct.percent.logit.2 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.logit.2
prop.reduct.logit.2
aic.logit.2 <- AIC(logit.2)
aic.logit.2

#3) same as 2, but w/  approval instead of strong pres

logit.3 <- glm(vote ~ rescale(opinion) + rescale(lack.quality) 
  + rescale(distance) + same.party + rescale(pres.approval), family=binomial(link="logit"))
display(logit.3)
pred.logit.3 <- logit.3$fitted.values >=.5
correct.logit.3 <- pred.logit.3 == vote
correct.percent.logit.3 <- sum(correct.logit.3)/length(vote)
prop.reduct.logit.3 <- 100*((100*correct.percent.logit.3 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.logit.3
prop.reduct.logit.3
aic.logit.3 <- AIC(logit.3)
aic.logit.3

#4) include everything, including strong pres, approval and state ideology
logit.4 <- glm(vote ~ rescale(opinion) + state.ideo.opposing.pres + rescale(lack.quality) 
  + rescale(distance) + same.party + strong.pres + rescale(pres.approval), family=binomial(link="logit"))
display(logit.4)
pred.logit.4 <- logit.4$fitted.values >=.5
correct.logit.4 <- pred.logit.4 == vote
correct.percent.logit.4 <- sum(correct.logit.4)/length(vote)
prop.reduct.logit.4 <- 100*((100*correct.percent.logit.4 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.logit.4
prop.reduct.logit.4
aic.logit.4 <- AIC(logit.4)
aic.logit.4

#5) add  interaction between reelection and opion

logit.5 <- glm(vote ~ rescale(opinion) + state.ideo.opposing.pres + rescale(lack.quality) 
  + rescale(distance) + same.party + strong.pres + rescale(pres.approval) +
  reelect.binary + rescale(opinion):reelect.binary, family=binomial(link="logit"))
display(logit.5)
pred.logit.5 <- logit.5$fitted.values >=.5
correct.logit.5 <- pred.logit.5 == vote
correct.percent.logit.5 <- sum(correct.logit.5)/length(vote)
prop.reduct.logit.5 <- 100*((100*correct.percent.logit.5 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.logit.5
prop.reduct.logit.5
aic.logit.5 <- AIC(logit.5)
aic.logit.5

##############################
#Multilevel Models
##############################

#a) Model 6

mlm.1 <-  glmer(vote ~ rescale(lack.quality) + rescale(distance) +  
  same.party + strong.pres + (1|nominee), family=binomial(link="logit"))
display(mlm.1)
ranef(mlm.1)
se.ranef(mlm.1)
pred.mlm.1 <- fitted(mlm.1) >=.5
correct.mlm.1 <- pred.mlm.1 == vote
correct.percent.mlm.1 <- sum(correct.mlm.1)/length(vote)
prop.reduct.mlm.1 <- 100*((100*correct.percent.mlm.1 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.mlm.1
prop.reduct.mlm.1
aic.mlm.1 <- AIC(logLik(mlm.1)) 
aic.mlm.1 

#b) Model 7
mlm.2 <-  glmer(vote ~ rescale(opinion) + rescale(lack.quality) + rescale(distance) +  
  same.party + strong.pres + (1 | nominee), 
  family=binomial(link="logit"))
display(mlm.2)
ranef(mlm.2)
se.ranef(mlm.2)
pred.mlm.2 <- fitted(mlm.2) >=.5
correct.mlm.2 <- pred.mlm.2 == vote
correct.percent.mlm.2 <- sum(correct.mlm.2)/length(vote)
prop.reduct.mlm.2 <- 100*((100*correct.percent.mlm.2 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.mlm.2
prop.reduct.mlm.2
aic.mlm.2 <- AIC(logLik(mlm.2)) 
aic.mlm.2 


#c) Model 8 -- 
mlm.3 <-  glmer(vote ~ rescale(opinion) + rescale(lack.quality) 
  + rescale(distance) + same.party + rescale(pres.approval) + (1 | nominee), 
  family=binomial(link="logit"))
display(mlm.3)
ranef(mlm.3)
se.ranef(mlm.3)
pred.mlm.3 <- fitted(mlm.3) >=.5
correct.mlm.3 <- pred.mlm.3 == vote
correct.percent.mlm.3 <- sum(correct.mlm.3)/length(vote)
prop.reduct.mlm.3 <- 100*((100*correct.percent.mlm.3 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.mlm.3
prop.reduct.mlm.3
aic.mlm.3 <- AIC(logLik(mlm.3)) 
aic.mlm.3 

#d) Model 9 --- 
mlm.4 <-  lmer(vote ~ rescale(opinion) + state.ideo.opposing.pres + rescale(lack.quality) 
  + rescale(distance) + same.party + strong.pres + rescale(pres.approval) + (1 | nominee), 
  family=binomial(link="logit"))
display(mlm.4)
ranef(mlm.4)
se.ranef(mlm.4)
pred.mlm.4 <- fitted(mlm.4) >=.5
correct.mlm.4 <- pred.mlm.4 == vote
correct.percent.mlm.4 <- sum(correct.mlm.4)/length(vote)
prop.reduct.mlm.4 <- 100*((100*correct.percent.mlm.4 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.mlm.4
prop.reduct.mlm.4
aic.mlm.4 <- AIC(logLik(mlm.4)) 
aic.mlm.4 

#e) Model 10 -- 

mlm.5 <- glmer(vote ~ rescale(opinion) + state.ideo.opposing.pres + rescale(lack.quality) 
  + rescale(distance) + same.party + strong.pres + rescale(pres.approval) +
  reelect.binary + rescale(opinion):reelect.binary  + (1 | nominee), family=binomial(link="logit"))
display(mlm.5)
ranef(mlm.5)
se.ranef(mlm.5)
pred.mlm.5 <- fitted(mlm.5) >=.5
correct.mlm.5 <- pred.mlm.5 == vote
correct.percent.mlm.5 <- sum(correct.mlm.5)/length(vote)
prop.reduct.mlm.5 <- 100*((100*correct.percent.mlm.5 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.mlm.5
prop.reduct.mlm.5
aic.mlm.5 <- AIC(logLik(mlm.5)) 
aic.mlm.5 


#RUN SCRIPT TO CREATE FIGURE 4 (OPINION EFFECTS)
source("create_figure_4.R")

########          COUNTERFACTUALS
# 

#for counterfactuals, use Model 9 to maximize predictive power, but for simplicity don't rescale
mlm.for.counterfacts <-  glmer(vote ~  opinion + state.ideo.opposing.pres + lack.quality 
  + distance + same.party + strong.pres + pres.approval + (1 | nominee), 
  family=binomial(link="logit"))
display(mlm.for.counterfacts)
AIC(logLik(mlm.for.counterfacts))

#our prediction for justices as is
alito.pred <- sum(fitted(mlm.for.counterfacts)[nominee=="alito"] > .5)
alito.pred
bork.pred <- sum(fitted(mlm.for.counterfacts)[nominee=="bork"] > .5)
bork.pred
thomas.pred <- sum(fitted(mlm.for.counterfacts)[nominee=="thomas"] > .5)
thomas.pred

#What if Bork had Alito's opinion vals
bork.with.alito.op <- roll.call.no.miers[nominee=="bork",]
bork.with.alito.op$opinion <- roll.call.no.miers$opinion[nominee=="alito"]

bork.with.alito.op.pred <- invlogit(
        fixef(mlm.for.counterfacts)["(Intercept)"]
        + fixef(mlm.for.counterfacts)["opinion"]*(bork.with.alito.op$opinion)
        + fixef(mlm.for.counterfacts)["state.ideo.opposing.pres"]*(bork.with.alito.op$state.ideo.opposing.pres)+ fixef(mlm.for.counterfacts)["lack.quality"]*(bork.with.alito.op$lack.quality)
        + fixef(mlm.for.counterfacts)["distance"]*(bork.with.alito.op$distance)
        + fixef(mlm.for.counterfacts)["same.party"]*(bork.with.alito.op$same.party)
        + fixef(mlm.for.counterfacts)["pres.approval"]*(bork.with.alito.op$pres.approval)
        + fixef(mlm.for.counterfacts)["strong.pres"]*(bork.with.alito.op$strong.pres)
        + ranef(mlm.for.counterfacts)$nominee["bork",]
        )
        
bork.with.alito.op.pred.vote <- ifelse(bork.with.alito.op.pred<.5,0,1)
sum(bork.with.alito.op.pred.vote)

#What if Alito had Bork's opinion vals
alito.with.bork.op <- roll.call.no.miers[nominee=="alito",]
alito.with.bork.op$opinion <-roll.call.no.miers$opinion[nominee=="bork"]

alito.with.bork.op.pred <- invlogit(
          fixef(mlm.for.counterfacts)["(Intercept)"]
        + fixef(mlm.for.counterfacts)["opinion"]*(alito.with.bork.op$opinion)
        + fixef(mlm.for.counterfacts)["state.ideo.opposing.pres"]*(alito.with.bork.op$state.ideo.opposing.pres)+ fixef(mlm.for.counterfacts)["lack.quality"]*(alito.with.bork.op$lack.quality)
        + fixef(mlm.for.counterfacts)["distance"]*(alito.with.bork.op$distance)
        + fixef(mlm.for.counterfacts)["same.party"]*(alito.with.bork.op$same.party)
        + fixef(mlm.for.counterfacts)["pres.approval"]*(alito.with.bork.op$pres.approval)
        + fixef(mlm.for.counterfacts)["strong.pres"]*(alito.with.bork.op$strong.pres)
        + ranef(mlm.for.counterfacts)$nominee["alito",]
        )

alito.with.bork.op.pred.vote <- ifelse(alito.with.bork.op.pred<.5,0,1)
sum(alito.with.bork.op.pred.vote)

#What if Thomas had Bork's opinion vals
thomas.with.bork.op <- roll.call.no.miers[nominee=="thomas",]
thomas.with.bork.op$opinion <- roll.call.no.miers$opinion[nominee=="bork"]

thomas.with.bork.op.pred <-  invlogit(
          fixef(mlm.for.counterfacts)["(Intercept)"]
        + fixef(mlm.for.counterfacts)["opinion"]*(thomas.with.bork.op$opinion)
        + fixef(mlm.for.counterfacts)["state.ideo.opposing.pres"]*(thomas.with.bork.op$state.ideo.opposing.pres)
        + fixef(mlm.for.counterfacts)["lack.quality"]*(thomas.with.bork.op$lack.quality)
        + fixef(mlm.for.counterfacts)["distance"]*(thomas.with.bork.op$distance)
        + fixef(mlm.for.counterfacts)["same.party"]*(thomas.with.bork.op$same.party)
        + fixef(mlm.for.counterfacts)["pres.approval"]*(thomas.with.bork.op$pres.approval)
        + fixef(mlm.for.counterfacts)["strong.pres"]*(thomas.with.bork.op$strong.pres)
        + ranef(mlm.for.counterfacts)$nominee["thomas",]
        )
        
thomas.with.bork.op.pred.vote <- ifelse(thomas.with.bork.op.pred<.5,0,1)
sum(thomas.with.bork.op.pred.vote)

###make preds for miers.  use alito, but give miers op and ranef of 0
options(digits = 3)
#order and attach Miers data
miers.data <- roll.call.merged[roll.call.merged$nominee=="miers",]
miers.data <- miers.data[order(miers.data$state, miers.data$name),]
miers.data$miers.opinion <- miers.data$opinion #rename to distinguish from alito opinion
detach()
attach.all(miers.data)

#set up alito opinion so in same order
alito.data <- roll.call.merged[roll.call.merged$nominee=="alito",]
alito.data <- alito.data[order(alito.data$state, alito.data$name),]
miers.data$alito.opinion <- alito.data$opinion
detach()
attach.all(miers.data)

#descriptive states about miers
#mean opinion
mean(miers.opinion) # 52
#most supportive and least supportive states
state[miers.opinion==min(miers.opinion)]
opinion[miers.opinion==min(miers.opinion)]
state[miers.opinion==max(miers.opinion)]
opinion[miers.opinion==max(miers.opinion)]
#mean ideological distance
mean(distance) #.14
mean(roll.call.no.miers$distance) # all nominees -- .18
tapply(roll.call.no.miers$distance, roll.call.no.miers$nominee, mean) #check for all nominees

#predicition if Miers otherwise average
miers.average.pred <- invlogit(
        fixef(mlm.for.counterfacts)["(Intercept)"]
        + fixef(mlm.for.counterfacts)["opinion"]*(miers.opinion)
        + fixef(mlm.for.counterfacts)["state.ideo.opposing.pres"]*(state.ideo.opposing.pres)
        + fixef(mlm.for.counterfacts)["lack.quality"]*(lack.quality)
        + fixef(mlm.for.counterfacts)["distance"]*(distance)
        + fixef(mlm.for.counterfacts)["same.party"]*(same.party)
        + fixef(mlm.for.counterfacts)["strong.pres"]*(strong.pres)
        + fixef(mlm.for.counterfacts)["pres.approval"]*(pres.approval)
        + 0 #assume average nominee--> nom. effect of zero
        )

miers.average.pred.vote <- ifelse(miers.average.pred<.5,0,1)
sum(miers.average.pred.vote)

#prediction if Miers like Alito, with her actual opinion
miers.low.re.pred <- 
   invlogit(
        fixef(mlm.for.counterfacts)["(Intercept)"]
        + fixef(mlm.for.counterfacts)["opinion"]*(miers.opinion)
        + fixef(mlm.for.counterfacts)["state.ideo.opposing.pres"]*(state.ideo.opposing.pres)
        + fixef(mlm.for.counterfacts)["lack.quality"]*(lack.quality)
        + fixef(mlm.for.counterfacts)["distance"]*(distance)
        + fixef(mlm.for.counterfacts)["same.party"]*(same.party)
        + fixef(mlm.for.counterfacts)["strong.pres"]*(strong.pres)
        + fixef(mlm.for.counterfacts)["pres.approval"]*(pres.approval)
        + ranef(mlm.for.counterfacts)$nominee["alito",1]
        )
        
miers.low.re.pred.vote <- ifelse(miers.low.re.pred<.5,0,1)
sum(miers.low.re.pred.vote)

#predcition if Miers like Alito and with alito op
miers.low.re.pred.with.alito.op <- invlogit(
        fixef(mlm.for.counterfacts)["(Intercept)"]
        + fixef(mlm.for.counterfacts)["opinion"]*(alito.opinion)
        + fixef(mlm.for.counterfacts)["state.ideo.opposing.pres"]*(state.ideo.opposing.pres)
        + fixef(mlm.for.counterfacts)["lack.quality"]*(lack.quality)
        + fixef(mlm.for.counterfacts)["distance"]*(distance)
        + fixef(mlm.for.counterfacts)["same.party"]*(same.party)
        + fixef(mlm.for.counterfacts)["strong.pres"]*(strong.pres)
        + fixef(mlm.for.counterfacts)["pres.approval"]*(pres.approval)
        + ranef(mlm.for.counterfacts)$nominee["alito",1]
        )
        
miers.low.re.pred.with.alito.op.vote <- ifelse(miers.low.re.pred.with.alito.op<.5,0,1)
sum(miers.low.re.pred.with.alito.op.vote)

#SOTOMAYOR#########
#PREDICT HER VOTE USING MODEL 9
detach()
attach(roll.call.no.miers)

#d) Model 9 --- #take out rescaling for simplicity
mlm.4 <-  lmer(vote ~ opinion + state.ideo.opposing.pres + lack.quality
  + distance + same.party + strong.pres + pres.approval + (1 | nominee), 
  family=binomial(link="logit"))
display(mlm.4)

#get here mean opinion compared to other nominees
sort(tapply(roll.call.with.sotomayor$opinion, roll.call.with.sotomayor$nominee, mean))

sotomayor.data <- roll.call.with.sotomayor[roll.call.with.sotomayor$nominee=="sotomayor",]
detach()
attach(sotomayor.data)

pred.prob <- invlogit(
        fixef(mlm.4)["(Intercept)"]
    + fixef(mlm.4)["opinion"]*(opinion)
    + fixef(mlm.4)["state.ideo.opposing.pres"]*(state.ideo.opposing.pres)
    + fixef(mlm.4)["lack.quality"]*(lack.quality)
    + fixef(mlm.4)["distance"]*(distance)
    + fixef(mlm.4)["same.party"]*(same.party)
    + fixef(mlm.4)["strong.pres"]*(strong.pres)
    + fixef(mlm.4)["pres.approval"]*(pres.approval))

pred.vote <- ifelse(pred.prob > .5, 1,0)
table(pred.vote)
pred.correct <- ifelse(pred.vote == vote, 1,0)
table(pred.correct)
keep <- pred.correct == 0
cbind(name[keep], vote[keep], pred.vote[keep], round(pred.prob[keep],2))

#run model with soto in it.

detach()
attach(roll.call.with.sotomayor)
mlm.w.soto <-  lmer(vote ~ rescale(opinion) + state.ideo.opposing.pres + rescale(lack.quality) 
  + rescale(distance) + same.party + strong.pres + rescale(pres.approval) + (1 | nominee), 
  family=binomial(link="logit"))
display(mlm.w.soto)
ranef(mlm.w.soto)
se.ranef(mlm.w.soto)
pred.mlm.w.soto <- fitted(mlm.w.soto) >=.5
correct.mlm.w.soto <- pred.mlm.w.soto == vote
correct.percent.mlm.w.soto <- sum(correct.mlm.w.soto)/length(vote)
prop.reduct.mlm.w.soto <- 100*((100*correct.percent.mlm.w.soto - 100*modal.cat)/(100-100*modal.cat))
correct.percent.mlm.w.soto
prop.reduct.mlm.w.soto




###########################################
###########################################
###########################################
###########################################
###########################################
###########################################

#ROBUSTNESS CHECKs:
#########################################
#1) run models 8 and 9 nine times, each time leaving out a justice, to test for robustness.
detach()
attach.all(roll.call.no.miers)

#model 8
  opinion.coefs <- rep(NA,9)
  opinion.ses <- rep(NA, 9)
for (i in 1:9){
  keep <- factor(nominee) != unique(nominee)[i]
  model <- glmer(vote ~ rescale(opinion) + rescale(lack.quality) 
      + rescale(distance) + same.party + rescale(pres.approval) + (1 | nominee), 
  family=binomial(link="logit"), subset = keep)
  opinion.coefs[i] <- fixef(model)["rescale(opinion)"]
  opinion.ses[i] <- se.fixef(model)["rescale(opinion)"]
  print(unique(nominee)[i])  
  display(model)
  }
opinion.coefs
opinion.ses
opinion.coefs/opinion.ses#always significant
##

  opinion.coefs <- rep(NA,9)
  opinion.ses <- rep(NA, 9)
for (i in 1:9){
  keep <- factor(nominee) != unique(nominee)[i]
  model <- glmer(vote ~ rescale(opinion) + state.ideo.opposing.pres + rescale(lack.quality) 
      + rescale(distance) + same.party + strong.pres + rescale(pres.approval) + (1 | nominee), 
  family=binomial(link="logit"), subset = keep)
  opinion.coefs[i] <- fixef(model)["rescale(opinion)"]
  opinion.ses[i] <- se.fixef(model)["rescale(opinion)"]
  print(unique(nominee)[i])  
  display(model)
  }
opinion.coefs
opinion.ses
opinion.coefs/opinion.ses#always significant


##CHECK TO MAKE SURE RESULTS ARE SAME IF WE OPINION OF ALL, RATHER THAN OPINION HOLDERS

roll.call.no.miers$opinion <- roll.call.no.miers$opinion.all
detach()
attach(roll.call.no.miers)

#2) add in opinion

logit.2 <- glm(vote ~ rescale(opinion) + rescale(lack.quality) + rescale(distance) +  
  same.party + strong.pres, family=binomial(link="logit"))
display(logit.2)
pred.logit.2 <- logit.2$fitted.values >=.5
correct.logit.2 <- pred.logit.2 == vote
correct.percent.logit.2 <- sum(correct.logit.2)/length(vote)
prop.reduct.logit.2 <- 100*((100*correct.percent.logit.2 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.logit.2
prop.reduct.logit.2
aic.logit.2 <- AIC(logit.2)
aic.logit.2

#3) same as 2, but w/  approval instead of strong pres

logit.3 <- glm(vote ~ rescale(opinion) + rescale(lack.quality) 
  + rescale(distance) + same.party + rescale(pres.approval), family=binomial(link="logit"))
display(logit.3)
pred.logit.3 <- logit.3$fitted.values >=.5
correct.logit.3 <- pred.logit.3 == vote
correct.percent.logit.3 <- sum(correct.logit.3)/length(vote)
prop.reduct.logit.3 <- 100*((100*correct.percent.logit.3 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.logit.3
prop.reduct.logit.3
aic.logit.3 <- AIC(logit.3)
aic.logit.3

#4) include everything, including strong pres, approval and state ideology
logit.4 <- glm(vote ~ rescale(opinion) + state.ideo.opposing.pres + rescale(lack.quality) 
  + rescale(distance) + same.party + strong.pres + rescale(pres.approval), family=binomial(link="logit"))
display(logit.4)
pred.logit.4 <- logit.4$fitted.values >=.5
correct.logit.4 <- pred.logit.4 == vote
correct.percent.logit.4 <- sum(correct.logit.4)/length(vote)
prop.reduct.logit.4 <- 100*((100*correct.percent.logit.4 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.logit.4
prop.reduct.logit.4
aic.logit.4 <- AIC(logit.4)
aic.logit.4

#5) add  interaction between reelection and opion

logit.5 <- glm(vote ~ rescale(opinion) + state.ideo.opposing.pres + rescale(lack.quality) 
  + rescale(distance) + same.party + strong.pres + rescale(pres.approval) +
  reelect.binary + rescale(opinion):reelect.binary, family=binomial(link="logit"))
display(logit.5)
pred.logit.5 <- logit.5$fitted.values >=.5
correct.logit.5 <- pred.logit.5 == vote
correct.percent.logit.5 <- sum(correct.logit.5)/length(vote)
prop.reduct.logit.5 <- 100*((100*correct.percent.logit.5 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.logit.5
prop.reduct.logit.5
aic.logit.5 <- AIC(logit.5)
aic.logit.5



##############################
#MLM's

#a) Model 6

mlm.1 <-  glmer(vote ~ rescale(lack.quality) + rescale(distance) +  
  same.party + strong.pres + (1|nominee), family=binomial(link="logit"))
display(mlm.1)
ranef(mlm.1)
se.ranef(mlm.1)
pred.mlm.1 <- fitted(mlm.1) >=.5
correct.mlm.1 <- pred.mlm.1 == vote
correct.percent.mlm.1 <- sum(correct.mlm.1)/length(vote)
prop.reduct.mlm.1 <- 100*((100*correct.percent.mlm.1 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.mlm.1
prop.reduct.mlm.1
aic.mlm.1 <- AIC(logLik(mlm.1)) 
aic.mlm.1 

#b) Model 7
mlm.2 <-  glmer(vote ~ rescale(opinion) + rescale(lack.quality) + rescale(distance) +  
  same.party + strong.pres + (1 | nominee), 
  family=binomial(link="logit"))
display(mlm.2)
ranef(mlm.2)
se.ranef(mlm.2)
pred.mlm.2 <- fitted(mlm.2) >=.5
correct.mlm.2 <- pred.mlm.2 == vote
correct.percent.mlm.2 <- sum(correct.mlm.2)/length(vote)
prop.reduct.mlm.2 <- 100*((100*correct.percent.mlm.2 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.mlm.2
prop.reduct.mlm.2
aic.mlm.2 <- AIC(logLik(mlm.2)) 
aic.mlm.2 


#c) Model 8 -- 
mlm.3 <-  glmer(vote ~ rescale(opinion) + rescale(lack.quality) 
  + rescale(distance) + same.party + rescale(pres.approval) + (1 | nominee), 
  family=binomial(link="logit"))
display(mlm.3)
ranef(mlm.3)
se.ranef(mlm.3)
pred.mlm.3 <- fitted(mlm.3) >=.5
correct.mlm.3 <- pred.mlm.3 == vote
correct.percent.mlm.3 <- sum(correct.mlm.3)/length(vote)
prop.reduct.mlm.3 <- 100*((100*correct.percent.mlm.3 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.mlm.3
prop.reduct.mlm.3
aic.mlm.3 <- AIC(logLik(mlm.3)) 
aic.mlm.3 

#d) Model 9 --- 
mlm.4 <-  lmer(vote ~ rescale(opinion) + state.ideo.opposing.pres + rescale(lack.quality) 
  + rescale(distance) + same.party + strong.pres + rescale(pres.approval) + (1 | nominee), 
  family=binomial(link="logit"))
display(mlm.4)
ranef(mlm.4)
se.ranef(mlm.4)
pred.mlm.4 <- fitted(mlm.4) >=.5
correct.mlm.4 <- pred.mlm.4 == vote
correct.percent.mlm.4 <- sum(correct.mlm.4)/length(vote)
prop.reduct.mlm.4 <- 100*((100*correct.percent.mlm.4 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.mlm.4
prop.reduct.mlm.4
aic.mlm.4 <- AIC(logLik(mlm.4)) 
aic.mlm.4 

#e) Model 10 -- 

mlm.5 <- glmer(vote ~ rescale(opinion) + state.ideo.opposing.pres + rescale(lack.quality) 
  + rescale(distance) + same.party + strong.pres + rescale(pres.approval) +
  reelect.binary + rescale(opinion):reelect.binary  + (1 | nominee), family=binomial(link="logit"))
display(mlm.5)
ranef(mlm.5)
se.ranef(mlm.5)
pred.mlm.5 <- fitted(mlm.5) >=.5
correct.mlm.5 <- pred.mlm.5 == vote
correct.percent.mlm.5 <- sum(correct.mlm.5)/length(vote)
prop.reduct.mlm.5 <- 100*((100*correct.percent.mlm.5 - 100*modal.cat)/(100-100*modal.cat))
correct.percent.mlm.5
prop.reduct.mlm.5
aic.mlm.5 <- AIC(logLik(mlm.5)) 
aic.mlm.5


