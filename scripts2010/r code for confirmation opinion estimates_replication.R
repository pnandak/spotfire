rm(list=ls(all=TRUE))
library("arm")

#############################
#prep statelevel variables, to be used for all nominees.
############################

Statelevel <- read.dta("state_level_update.dta",convert.underscore = TRUE)
Statelevel <- Statelevel[order(Statelevel$sstate.initnum),]
Statelevel$ideology[Statelevel$sstate=="AK"] <- -21.6205
Statelevel$ideology[Statelevel$sstate=="HI"] <- -8.356
Statelevel$ideology[Statelevel$sstate=="NV"] <- -12.5819989013672
Statelevel$ideology[Statelevel$sstate=="DC"] <- 17.1539978027344
attach(Statelevel)


stateinitlist<- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS",
   "MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY")


#########################                                       ############################
#########################                                       ############################
#########################                                       ############################
#########################       ALITO
#########################                                       ############################
#########################                                       ############################

Census <- read.dta("poststratification 2000.dta",convert.underscore = TRUE)
attach(Census)
Census <- Census[order(cstate),]
detach(Census)
attach(Census)


alito.megapoll <- read.dta("Alito_megapoll.dta",convert.underscore = TRUE)
attach(alito.megapoll)


#creates a score running: male white, male black, male hisp, female white, female black, female hisp
race.female <- (female *3) + race.wbh
crace.female <- (cfemale *3) + crace.WBH
age.edu.cat <- 4 * (age.cat -1) + edu.cat
cage.edu.cat <- 4 * (cage.cat - 1 ) + cedu.cat
cstate.initnum <-  match(cstate, stateinitlist)


p.evang.full<- p.evang[state.initnum]
cp.evang.full<- p.evang[cstate.initnum]
p.mormon.full <- p.mormon[state.initnum]
cp.mormon.full<- p.mormon[cstate.initnum]

p.relig <- p.evang + p.mormon
p.relig.full <- p.evang.full + p.mormon.full
cp.relig.full <- cp.evang.full + cp.mormon.full

p.gore.full <- gore.00[state.initnum]
cp.gore.full <- gore.00[cstate.initnum]

p.kerry.full <- kerry.04[state.initnum]
cp.kerry.full <- kerry.04[cstate.initnum]

p.clinton.full <- clinton.92[state.initnum]
cp.clinton.full <- clinton.92[cstate.initnum]

p.ideology.full <- ideology[state.initnum]
cp.ideology.full <- ideology[cstate.initnum]

alito.confirm <- glmer(formula = q.confirm ~  (1|race.female) + (1|age.cat) + (1|edu.cat) + (1|state) + (1|region)+ (1|poll) #+ (1|age.edu.cat) 
    +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- alito.confirm
response <- q.confirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.female[crace.female,1]
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

alito.statepred.confirm <- statepred
alito.stateraw.confirm <- stateraw 

cbind(alito.statepred.confirm,alito.stateraw.confirm)




alito.donotconfirm <- glmer(formula = q.donotconfirm ~  (1|race.female) + (1|age.cat) + (1|edu.cat) + (1|state) + (1|region)+ (1|poll)
   # +(1|age.edu.cat)
    +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- alito.donotconfirm
response <- q.donotconfirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.female[crace.female,1]
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

alito.statepred.donotconfirm <- statepred
alito.stateraw.donotconfirm <- stateraw 

cbind(alito.statepred.donotconfirm,alito.stateraw.donotconfirm)

alito.statepred.confirm.withop <- 100*alito.statepred.confirm/(alito.statepred.confirm+alito.statepred.donotconfirm)
alito.statepred.donotconfirm.withop <- 100*alito.statepred.donotconfirm/(alito.statepred.confirm+alito.statepred.donotconfirm)
alito.statepred.noop <- 100-(alito.statepred.confirm+alito.statepred.donotconfirm)

#doublechecking they each sum to 1
alito.statepred.confirm+ alito.statepred.donotconfirm + alito.statepred.noop
alito.statepred.confirm.withop+ alito.statepred.donotconfirm.withop 

alito.statepred.confirm
alito.statepred.donotconfirm
alito.statepred.confirm.withop 

export.alito <- data.frame(stateinitlist,alito.statepred.confirm, alito.statepred.donotconfirm,alito.statepred.confirm.withop)
write.csv(export.alito, "individual_estimates_alito.csv")


detach(alito.megapoll)
rm(alito.megapoll)
detach(Census)
rm(Census)

#########################                                       ############################
#########################                                       ############################
#########################                                       ############################
#########################       ROBERTS
#########################                                       ############################
#########################                                       ############################

Census <- read.dta("poststratification 2000.dta",convert.underscore = TRUE)
attach(Census)
Census <- Census[order(cstate),]
detach(Census)
attach(Census)





roberts.megapoll <- read.dta("Roberts_megapoll.dta",convert.underscore = TRUE)
attach(roberts.megapoll)


#creates a score running: male white, male black, male hisp, female white, female black, female hisp
race.female <- (female *3) + race.wbh
crace.female <- (cfemale *3) + crace.WBH
age.edu.cat <- 4 * (age.cat -1) + edu.cat
cage.edu.cat <- 4 * (cage.cat - 1 ) + cedu.cat
cstate.initnum <-  match(cstate, stateinitlist)




p.evang.full<- p.evang[state.initnum]
cp.evang.full<- p.evang[cstate.initnum]
p.mormon.full <- p.mormon[state.initnum]
cp.mormon.full<- p.mormon[cstate.initnum]

p.relig <- p.evang + p.mormon
p.relig.full <- p.evang.full + p.mormon.full
cp.relig.full <- cp.evang.full + cp.mormon.full

p.gore.full <- gore.00[state.initnum]
cp.gore.full <- gore.00[cstate.initnum]

p.kerry.full <- kerry.04[state.initnum]
cp.kerry.full <- kerry.04[cstate.initnum]

p.clinton.full <- clinton.92[state.initnum]
cp.clinton.full <- clinton.92[cstate.initnum]

p.ideology.full <- ideology[state.initnum]
cp.ideology.full <- ideology[cstate.initnum]

roberts.confirm <- glmer(formula = q.confirm ~  (1|race.female) + (1|age.cat) + (1|edu.cat) + (1|state) + (1|region)+ (1|poll)
    +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- roberts.confirm
response <- q.confirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.female[crace.female,1]
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

roberts.statepred.confirm <- statepred
roberts.stateraw.confirm <- stateraw 

cbind(roberts.statepred.confirm,roberts.stateraw.confirm)




roberts.donotconfirm <- glmer(formula = q.donotconfirm ~  (1|race.female) + (1|age.cat) + (1|edu.cat) + (1|state) + (1|region)+ (1|poll)
   # +(1|age.edu.cat)
    +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- roberts.donotconfirm
response <- q.donotconfirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.female[crace.female,1]
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

roberts.statepred.donotconfirm <- statepred
roberts.stateraw.donotconfirm <- stateraw 

cbind(roberts.statepred.donotconfirm,roberts.stateraw.donotconfirm)


roberts.statepred.confirm.withop <- 100*roberts.statepred.confirm/(roberts.statepred.confirm+roberts.statepred.donotconfirm)
roberts.statepred.donotconfirm.withop <- 100*roberts.statepred.donotconfirm/(roberts.statepred.confirm+roberts.statepred.donotconfirm)
roberts.statepred.noop <- 100-(roberts.statepred.confirm+roberts.statepred.donotconfirm)

#doublechecking they each sum to 1
roberts.statepred.confirm+ roberts.statepred.donotconfirm + roberts.statepred.noop
roberts.statepred.confirm.withop+ roberts.statepred.donotconfirm.withop 

cbind(sstate,
roberts.statepred.confirm,
roberts.statepred.donotconfirm,
roberts.statepred.confirm.withop
) 

export.roberts <- data.frame(stateinitlist,roberts.statepred.confirm, roberts.statepred.donotconfirm,roberts.statepred.confirm.withop)
write.csv(export.roberts, "individual_estimates_roberts.csv")


detach(roberts.megapoll)
rm(roberts.megapoll)
detach(Census)
rm(Census)


#########################                                       ############################
#########################                                       ############################
#########################                                       ############################
#########################       OCONNOR
#########################                                       ############################
#########################                                       ############################

#
#oconnor.confirm <- lmer(formula = q.confirm ~ +(1|race.female)  + (1|edu.cat)  + (1|region),
#    family=binomial(link="logit"),control=list(usePQL=FALSE))


Census1990 <- read.dta("poststratification 1990.dta",convert.underscore = TRUE)
Census1980 <- read.dta("poststratification 1980.dta",convert.underscore = TRUE)
Census <- read.dta("poststratification 1980.dta",convert.underscore = TRUE)
Census$cpercent.state <- (.9 * Census1980$cpercent.state)  + (.1 * Census1990$cpercent.state)
Census <- Census[order(Census$cstate),]
attach(Census)


oconnor.megapoll <- read.dta("OConnor_megapoll.dta",convert.underscore = TRUE)
attach(oconnor.megapoll)

#creates a score running: male white, male black, male hisp, female white, female black, female hisp
race.female <- (female *3) + race.wbh
crace.female <- (cfemale *3) + crace.WBH
#age.edu.cat <- 4 * (age.cat -1) + edu.cat
#cage.edu.cat <- 4 * (cage.cat - 1 ) + cedu.cat
cstate.initnum <-  match(cstate, stateinitlist)


oconnor.confirm <- glmer(formula = q.confirm ~  (1|race.female) + (1|edu.cat)  + (1|region)
    ,family=binomial(link="logit"))
mlm <- oconnor.confirm
response <- q.confirm
display(mlm,detail=TRUE)

        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.female[crace.female,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +region.ranefs[cregion,1]   
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        
oconnor.statepred.confirm <- statepred
oconnor.stateraw.confirm <- stateraw 

cbind(oconnor.statepred.confirm,oconnor.stateraw.confirm)

oconnor.donotconfirm <- glmer(formula = q.donotconfirm ~  (1|race.female) + (1|edu.cat)  + (1|region)
    ,family=binomial(link="logit"))
mlm <- oconnor.donotconfirm
response <- q.donotconfirm
display(mlm,detail=TRUE)

        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.female[crace.female,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +region.ranefs[cregion,1]   
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

oconnor.statepred.donotconfirm <- statepred
oconnor.stateraw.donotconfirm <- stateraw 

cbind(oconnor.statepred.donotconfirm,oconnor.stateraw.donotconfirm)


oconnor.statepred.confirm.withop <- 100*oconnor.statepred.confirm/(oconnor.statepred.confirm+oconnor.statepred.donotconfirm)
oconnor.statepred.donotconfirm.withop <- 100*oconnor.statepred.donotconfirm/(oconnor.statepred.confirm+oconnor.statepred.donotconfirm)
oconnor.statepred.noop <- 100-(oconnor.statepred.confirm+oconnor.statepred.donotconfirm)

#doublechecking they each sum to 1
oconnor.statepred.confirm+ oconnor.statepred.donotconfirm + oconnor.statepred.noop
oconnor.statepred.confirm.withop+ oconnor.statepred.donotconfirm.withop 

oconnor.statepred.confirm
oconnor.statepred.donotconfirm
oconnor.statepred.confirm.withop 

cor(oconnor.statepred.confirm,oconnor.statepred.confirm.withop)

export.oconnor <- data.frame(stateinitlist,oconnor.statepred.confirm, oconnor.statepred.donotconfirm,oconnor.statepred.confirm.withop)
write.csv(export.oconnor, "individual_estimates_oconnor.csv")


detach(oconnor.megapoll)
rm(oconnor.megapoll)
detach(Census)
rm(Census)


#########################                                       ############################
#########################                                       ############################
#########################                                       ############################
#########################       GINSBURG
#########################                                       ############################


Census1990 <- read.dta("poststratification 1990.dta",convert.underscore = TRUE)
Census2000 <- read.dta("poststratification 2000.dta",convert.underscore = TRUE)
Census <- read.dta("poststratification 2000.dta",convert.underscore = TRUE)
Census$cpercent.state <- (.3 * Census2000$cpercent.state)  + (.7 * Census1990$cpercent.state)
Census <- Census[order(Census$cstate),]
attach(Census)

ginsburg.megapoll <- read.dta("Ginsburg_megapoll.dta",convert.underscore = TRUE)
attach(ginsburg.megapoll)

#creates a score running: male white, male black, male hisp, female white, female black, female hisp
race.female <- (female *2) + race.wb
crace.female <- ifelse( crace.WBH == 3  , (cfemale *2) + 1 , (cfemale *2) + crace.WBH )
age.edu.cat <- 4 * (age.cat -1) + edu.cat
cage.edu.cat <- 4 * (cage.cat - 1 ) + cedu.cat
cstate.initnum <-  match(cstate, stateinitlist)
black <- ifelse (race.wb==2,1,0)
cblack <- ifelse (crace.WBH==2,1,0)



p.evang.full<- p.evang[state.initnum]
cp.evang.full<- p.evang[cstate.initnum]
p.mormon.full <- p.mormon[state.initnum]
cp.mormon.full<- p.mormon[cstate.initnum]

p.relig <- p.evang + p.mormon
p.relig.full <- p.evang.full + p.mormon.full
cp.relig.full <- cp.evang.full + cp.mormon.full

p.gore.full <- gore.00[state.initnum]
cp.gore.full <- gore.00[cstate.initnum]

p.kerry.full <- kerry.04[state.initnum]
cp.kerry.full <- kerry.04[cstate.initnum]

p.clinton.full <- clinton.92[state.initnum]
cp.clinton.full <- clinton.92[cstate.initnum]

p.ideology.full <- ideology[state.initnum]
cp.ideology.full <- ideology[cstate.initnum]



ginsburg.confirm <- glmer(formula = q.confirm ~  + (1|age.edu.cat) + (1|age.cat) + (1|edu.cat) + (1|state) + (1|region)+ (1|poll) +female + black # + (1|race.female) 
    +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- ginsburg.confirm
response <- q.confirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +(fixef(mlm)["black"] *cblack)
                    +(fixef(mlm)["female"] *cfemale)
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$age.edu.cat[cage.edu.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

ginsburg.statepred.confirm <- statepred
ginsburg.stateraw.confirm <- stateraw 

cbind(ginsburg.statepred.confirm,ginsburg.stateraw.confirm)




ginsburg.donotconfirm <- glmer(formula = q.donotconfirm ~    + (1|age.edu.cat) + (1|age.cat) + (1|edu.cat) + (1|state) + (1|region)+ (1|poll) +female + black # + (1|race.female) 
    +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- ginsburg.donotconfirm
response <- q.donotconfirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +(fixef(mlm)["black"] *cblack)
                    +(fixef(mlm)["female"] *cfemale)
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +ranef(mlm)$age.edu.cat[cage.edu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

ginsburg.statepred.donotconfirm <- statepred
ginsburg.stateraw.donotconfirm <- stateraw 

cbind(ginsburg.statepred.donotconfirm,ginsburg.stateraw.donotconfirm)



ginsburg.statepred.confirm.withop <- 100*ginsburg.statepred.confirm/(ginsburg.statepred.confirm+ginsburg.statepred.donotconfirm)
ginsburg.statepred.donotconfirm.withop <- 100*ginsburg.statepred.donotconfirm/(ginsburg.statepred.confirm+ginsburg.statepred.donotconfirm)
ginsburg.statepred.noop <- 100-(ginsburg.statepred.confirm+ginsburg.statepred.donotconfirm)



#doublechecking they each sum to 1
ginsburg.statepred.confirm+ ginsburg.statepred.donotconfirm + ginsburg.statepred.noop
ginsburg.statepred.confirm.withop+ ginsburg.statepred.donotconfirm.withop 

ginsburg.statepred.confirm
ginsburg.statepred.donotconfirm
ginsburg.statepred.confirm.withop 

export.ginsburg <- data.frame(stateinitlist,ginsburg.statepred.confirm, ginsburg.statepred.donotconfirm,ginsburg.statepred.confirm.withop)
write.csv(export.ginsburg, "individual_estimates_ginsburg.csv")


detach(ginsburg.megapoll)
rm(ginsburg.megapoll)
detach(Census)
rm(Census)

#########################                                       ############################
#########################                                       ############################
#########################                                       ############################
#########################       BREYER
#########################                                       ############################




Census1990 <- read.dta("poststratification 1990.dta",convert.underscore = TRUE)
Census2000 <- read.dta("poststratification 2000.dta",convert.underscore = TRUE)
Census <- read.dta("poststratification 2000.dta",convert.underscore = TRUE)
Census$cpercent.state <- (.4 * Census2000$cpercent.state)  + (.6 * Census1990$cpercent.state)
Census <- Census[order(Census$cstate),]
attach(Census)



breyer.megapoll <- read.dta("Breyer_megapoll.dta",convert.underscore = TRUE)
attach(breyer.megapoll)




race.female <- (female *2) + race.wb
crace.female <- ifelse( crace.WBH == 3  , (cfemale *2) + 1 , (cfemale *2) + crace.WBH )
age.edu.cat <- 4 * (age.cat -1) + edu.cat
cage.edu.cat <- 4 * (cage.cat - 1 ) + cedu.cat
cstate.initnum <-  match(cstate, stateinitlist)
black <- ifelse (race.wb==2,1,0)
cblack <- ifelse (crace.WBH==2,1,0)




p.evang.full<- p.evang[state.initnum]
cp.evang.full<- p.evang[cstate.initnum]
p.mormon.full <- p.mormon[state.initnum]
cp.mormon.full<- p.mormon[cstate.initnum]

p.relig <- p.evang + p.mormon
p.relig.full <- p.evang.full + p.mormon.full
cp.relig.full <- cp.evang.full + cp.mormon.full

p.gore.full <- gore.00[state.initnum]
cp.gore.full <- gore.00[cstate.initnum]

p.kerry.full <- kerry.04[state.initnum]
cp.kerry.full <- kerry.04[cstate.initnum]

p.clinton.full <- clinton.92[state.initnum]
cp.clinton.full <- clinton.92[cstate.initnum]

p.ideology.full <- ideology[state.initnum]
cp.ideology.full <- ideology[cstate.initnum]




breyer.confirm <- glmer(formula = q.confirm ~   + (1|age.cat) + (1|edu.cat) + (1|state) + (1|region)  + black + female +(1|age.edu.cat)#+ (1|poll)
    +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- breyer.confirm
response <- q.confirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +(fixef(mlm)["black"] *cblack)
                    +(fixef(mlm)["female"] *cfemale)
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +ranef(mlm)$age.edu.cat[cage.edu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

breyer.statepred.confirm <- statepred
breyer.stateraw.confirm <- stateraw 

cbind(breyer.statepred.confirm,breyer.stateraw.confirm)


breyer.donotconfirm <- glmer(formula = q.donotconfirm ~   + (1|age.cat) + (1|edu.cat) + (1|state) + (1|region)  + black + female +(1|age.edu.cat)#+ (1|poll)
    +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- breyer.donotconfirm
response <- q.donotconfirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +(fixef(mlm)["black"] *cblack)
                    +(fixef(mlm)["female"] *cfemale)
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +ranef(mlm)$age.edu.cat[cage.edu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

breyer.statepred.donotconfirm <- statepred
breyer.stateraw.donotconfirm <- stateraw 

cbind(breyer.statepred.donotconfirm,breyer.stateraw.donotconfirm)


breyer.statepred.confirm.withop <- 100*breyer.statepred.confirm/(breyer.statepred.confirm+breyer.statepred.donotconfirm)
breyer.statepred.donotconfirm.withop <- 100*breyer.statepred.donotconfirm/(breyer.statepred.confirm+breyer.statepred.donotconfirm)
breyer.statepred.noop <- 100-(breyer.statepred.confirm+breyer.statepred.donotconfirm)



#doublechecking they each sum to 1
breyer.statepred.confirm+ breyer.statepred.donotconfirm + breyer.statepred.noop
breyer.statepred.confirm.withop+ breyer.statepred.donotconfirm.withop 

breyer.statepred.confirm
breyer.statepred.donotconfirm
breyer.statepred.confirm.withop 

export.breyer <- data.frame(stateinitlist,breyer.statepred.confirm, breyer.statepred.donotconfirm,breyer.statepred.confirm.withop)
write.csv(export.breyer, "individual_estimates_breyer.csv")


detach(breyer.megapoll)
rm(breyer.megapoll)
detach(Census)
rm(Census)


#########################                                       ############################
#########################                                       ############################
#########################                                       ############################
#########################       BORK
#########################                                       ############################

bork.megapoll <- read.dta("Bork_megapoll.dta",convert.underscore = TRUE)
attach(bork.megapoll)


Census1990 <- read.dta("poststratification 1990.dta",convert.underscore = TRUE)
Census1980 <- read.dta("poststratification 1980.dta",convert.underscore = TRUE)
Census <- read.dta("poststratification 1980.dta",convert.underscore = TRUE)
Census$cpercent.state <- (.3 * Census1980$cpercent.state)  + (.7 * Census1990$cpercent.state)
Census <- Census[order(Census$cstate),]
attach(Census)



race.female <- (female *2) + race.wb
crace.female <- ifelse( crace.WBH == 3  , (cfemale *2) + 1 , (cfemale *2) + crace.WBH )
age.edu.cat <- 4 * (age.cat -1) + edu.cat
cage.edu.cat <- 4 * (cage.cat - 1 ) + cedu.cat
cstate.initnum <-  match(cstate, stateinitlist)
black <- ifelse (race.wb==2,1,0)
cblack <- ifelse (crace.WBH==2,1,0)


p.evang.full<- p.evang[state.initnum]
cp.evang.full<- p.evang[cstate.initnum]
p.mormon.full <- p.mormon[state.initnum]
cp.mormon.full<- p.mormon[cstate.initnum]

p.relig <- p.evang + p.mormon
p.relig.full <- p.evang.full + p.mormon.full
cp.relig.full <- cp.evang.full + cp.mormon.full

p.gore.full <- gore.00[state.initnum]
cp.gore.full <- gore.00[cstate.initnum]

p.kerry.full <- kerry.04[state.initnum]
cp.kerry.full <- kerry.04[cstate.initnum]

p.clinton.full <- clinton.92[state.initnum]
cp.clinton.full <- clinton.92[cstate.initnum]

p.ideology.full <- ideology[state.initnum]
cp.ideology.full <- ideology[cstate.initnum]


bork.confirm <- glmer(formula = q.confirm ~  (1|race.female) + (1|age.cat) + (1|edu.cat)+ (1|state) + (1|region) + (1|poll) + (1|age.edu.cat)#
        +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- bork.confirm
response <- q.confirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.female[crace.female,1]
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +ranef(mlm)$age.edu.cat[cage.edu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

bork.statepred.confirm <- statepred
bork.stateraw.confirm <- stateraw 

cbind(bork.statepred.confirm,bork.stateraw.confirm)



bork.donotconfirm <- glmer(formula = q.donotconfirm ~  (1|race.female) + (1|age.cat) + (1|edu.cat)+ (1|state) + (1|region) + (1|poll) + (1|age.edu.cat)#
        +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- bork.donotconfirm
response <- q.donotconfirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.female[crace.female,1]
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +ranef(mlm)$age.edu.cat[cage.edu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

bork.statepred.donotconfirm <- statepred
bork.stateraw.donotconfirm <- stateraw 

cbind(bork.statepred.donotconfirm,bork.stateraw.donotconfirm)






bork.statepred.confirm.withop <- 100*bork.statepred.confirm/(bork.statepred.confirm+bork.statepred.donotconfirm)
bork.statepred.donotconfirm.withop <- 100*bork.statepred.donotconfirm/(bork.statepred.confirm+bork.statepred.donotconfirm)
bork.statepred.noop <- 100-(bork.statepred.confirm+bork.statepred.donotconfirm)



#doublechecking they each sum to 1
bork.statepred.confirm+ bork.statepred.donotconfirm + bork.statepred.noop
bork.statepred.confirm.withop+ bork.statepred.donotconfirm.withop 

bork.statepred.confirm
bork.statepred.donotconfirm
bork.statepred.confirm.withop 

export.bork <- data.frame(stateinitlist,bork.statepred.confirm, bork.statepred.donotconfirm,bork.statepred.confirm.withop)
write.csv(export.bork, "individual_estimates_bork.csv")


detach(bork.megapoll)
rm(bork.megapoll)
detach(Census)
rm(Census)









#########################                                       ############################
#########################                                       ############################
#########################                                       ############################
#########################      REHNQUIST
#########################                                       ############################
#########################                                       ############################


rehnquist.megapoll <- read.dta("Rehnquist_megapoll_with_states.dta",convert.underscore = TRUE)
attach(rehnquist.megapoll)



Census1990 <- read.dta("poststratification 1990.dta",convert.underscore = TRUE)
Census1980 <- read.dta("poststratification 1980.dta",convert.underscore = TRUE)
Census <- read.dta("poststratification 1980.dta",convert.underscore = TRUE)
Census$cpercent.state <- (.4 * Census1980$cpercent.state)  + (.6 * Census1990$cpercent.state)
Census <- Census[order(Census$cstate),]
attach(Census)



race.female <- (female *2) + race.wb
crace.female <- ifelse( crace.WBH == 3  , (cfemale *2) + 1 , (cfemale *2) + crace.WBH )
age.edu.cat <- 4 * (age.cat -1) + edu.cat
cage.edu.cat <- 4 * (cage.cat - 1 ) + cedu.cat
cstate.initnum <-  match(cstate, stateinitlist)
black <- ifelse (race.wb==2,1,0)
cblack <- ifelse (crace.WBH==2,1,0)








p.evang.full<- p.evang[state.initnum]
cp.evang.full<- p.evang[cstate.initnum]
p.mormon.full <- p.mormon[state.initnum]
cp.mormon.full<- p.mormon[cstate.initnum]

p.relig <- p.evang + p.mormon
p.relig.full <- p.evang.full + p.mormon.full
cp.relig.full <- cp.evang.full + cp.mormon.full

p.gore.full <- gore.00[state.initnum]
cp.gore.full <- gore.00[cstate.initnum]

p.kerry.full <- kerry.04[state.initnum]
cp.kerry.full <- kerry.04[cstate.initnum]

p.clinton.full <- clinton.92[state.initnum]
cp.clinton.full <- clinton.92[cstate.initnum]

p.ideology.full <- ideology[state.initnum]
cp.ideology.full <- ideology[cstate.initnum]




rehnquist.confirm <- glmer(formula = q.confirm ~  (1|race.female) + (1|age.cat) + (1|edu.cat)+ (1|state) + (1|region) + (1|poll) + (1|age.edu.cat)#
        +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- rehnquist.confirm
response <- q.confirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.female[crace.female,1]
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +ranef(mlm)$age.edu.cat[cage.edu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

rehnquist.statepred.confirm <- statepred
rehnquist.stateraw.confirm <- stateraw 

cbind(rehnquist.statepred.confirm,rehnquist.stateraw.confirm)






rehnquist.donotconfirm <- glmer(formula = q.donotconfirm ~  (1|race.female) + (1|age.cat) + (1|edu.cat)+ (1|state) + (1|region) + (1|poll) + (1|age.edu.cat)#
        +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- rehnquist.donotconfirm
response <- q.donotconfirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.female[crace.female,1]
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +ranef(mlm)$age.edu.cat[cage.edu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

rehnquist.statepred.donotconfirm <- statepred
rehnquist.stateraw.donotconfirm <- stateraw 

cbind(rehnquist.statepred.donotconfirm,rehnquist.stateraw.donotconfirm)






rehnquist.statepred.confirm.withop <- 100*rehnquist.statepred.confirm/(rehnquist.statepred.confirm+rehnquist.statepred.donotconfirm)
rehnquist.statepred.donotconfirm.withop <- 100*rehnquist.statepred.donotconfirm/(rehnquist.statepred.confirm+rehnquist.statepred.donotconfirm)
rehnquist.statepred.noop <- 100-(rehnquist.statepred.confirm+rehnquist.statepred.donotconfirm)



#doublechecking they each sum to 1
rehnquist.statepred.confirm+ rehnquist.statepred.donotconfirm + rehnquist.statepred.noop
rehnquist.statepred.confirm.withop+ rehnquist.statepred.donotconfirm.withop 

rehnquist.statepred.confirm
rehnquist.statepred.donotconfirm
rehnquist.statepred.confirm.withop 

export.rehnquist <- data.frame(stateinitlist,rehnquist.statepred.confirm, rehnquist.statepred.donotconfirm,rehnquist.statepred.confirm.withop)
write.csv(export.rehnquist, "individual_estimates_rehnquist.csv")


detach(rehnquist.megapoll)
rm(rehnquist.megapoll)
detach(Census)
rm(Census)






#########################                                       ############################
#########################                                       ############################
#########################                                       ############################
#########################    THOMAS
#########################                                       ############################
#########################                                       ############################


thomas.megapoll <- read.dta("Thomas_megapoll.dta",convert.underscore = TRUE)
attach(thomas.megapoll)


Census1990 <- read.dta("poststratification 1990.dta",convert.underscore = TRUE)
Census2000 <- read.dta("poststratification 2000.dta",convert.underscore = TRUE)
Census <- read.dta("poststratification 2000.dta",convert.underscore = TRUE)
Census$cpercent.state <- (.1 * Census2000$cpercent.state)  + (.9 * Census1990$cpercent.state)
Census <- Census[order(Census$cstate),]
attach(Census)



race.female <- (female *2) + race.wb
crace.female <- ifelse( crace.WBH == 3  , (cfemale *2) + 1 , (cfemale *2) + crace.WBH )
age.edu.cat <- 4 * (age.cat -1) + edu.cat
cage.edu.cat <- 4 * (cage.cat - 1 ) + cedu.cat
cstate.initnum <-  match(cstate, stateinitlist)
black <- ifelse (race.wb==2,1,0)
cblack <- ifelse (crace.WBH==2,1,0)




p.evang.full<- p.evang[state.initnum]
cp.evang.full<- p.evang[cstate.initnum]
p.mormon.full <- p.mormon[state.initnum]
cp.mormon.full<- p.mormon[cstate.initnum]

p.relig <- p.evang + p.mormon
p.relig.full <- p.evang.full + p.mormon.full
cp.relig.full <- cp.evang.full + cp.mormon.full

p.gore.full <- gore.00[state.initnum]
cp.gore.full <- gore.00[cstate.initnum]

p.kerry.full <- kerry.04[state.initnum]
cp.kerry.full <- kerry.04[cstate.initnum]

p.clinton.full <- clinton.92[state.initnum]
cp.clinton.full <- clinton.92[cstate.initnum]

p.ideology.full <- ideology[state.initnum]
cp.ideology.full <- ideology[cstate.initnum]




thomas.confirm <- glmer(formula = q.confirm ~  (1|race.female) + (1|age.cat) + (1|edu.cat)+ (1|state) + (1|region) + (1|poll) + (1|age.edu.cat)#
        +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- thomas.confirm
response <- q.confirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.female[crace.female,1]
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +ranef(mlm)$age.edu.cat[cage.edu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

thomas.statepred.confirm <- statepred
thomas.stateraw.confirm <- stateraw 

cbind(thomas.statepred.confirm,thomas.stateraw.confirm)






thomas.donotconfirm <- glmer(formula = q.donotconfirm ~  (1|race.female) + (1|age.cat) + (1|edu.cat)+ (1|state) + (1|region) + (1|poll) + (1|age.edu.cat)#
        +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- thomas.donotconfirm
response <- q.donotconfirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.female[crace.female,1]
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +ranef(mlm)$age.edu.cat[cage.edu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

thomas.statepred.donotconfirm <- statepred
thomas.stateraw.donotconfirm <- stateraw 

cbind(thomas.statepred.donotconfirm,thomas.stateraw.donotconfirm)






thomas.statepred.confirm.withop <- 100*thomas.statepred.confirm/(thomas.statepred.confirm+thomas.statepred.donotconfirm)
thomas.statepred.donotconfirm.withop <- 100*thomas.statepred.donotconfirm/(thomas.statepred.confirm+thomas.statepred.donotconfirm)
thomas.statepred.noop <- 100-(thomas.statepred.confirm+thomas.statepred.donotconfirm)



#doublechecking they each sum to 1
thomas.statepred.confirm+ thomas.statepred.donotconfirm + thomas.statepred.noop
thomas.statepred.confirm.withop+ thomas.statepred.donotconfirm.withop 

thomas.statepred.confirm
thomas.statepred.donotconfirm
thomas.statepred.confirm.withop 

export.thomas <- data.frame(stateinitlist,thomas.statepred.confirm, thomas.statepred.donotconfirm,thomas.statepred.confirm.withop)
write.csv(export.thomas, "individual_estimates_thomas.csv")


detach(thomas.megapoll)
rm(thomas.megapoll)
detach(Census)
rm(Census)


#########################                                       ############################
#########################                                       ############################
#########################                                       ############################
#########################    SOUTER
#########################                                       ############################
#########################                                       ############################


souter.megapoll <- read.dta("Souter_megapoll.dta",convert.underscore = TRUE)
attach(souter.megapoll)


Census <- read.dta("poststratification 1990.dta",convert.underscore = TRUE)
Census <- Census[order(Census$cstate),]
attach(Census)



race.female <- (female *2) + race.wb
crace.female <- ifelse( crace.WBH == 3  , (cfemale *2) + 1 , (cfemale *2) + crace.WBH )
age.edu.cat <- 4 * (age.cat -1) + edu.cat
cage.edu.cat <- 4 * (cage.cat - 1 ) + cedu.cat
cstate.initnum <-  match(cstate, stateinitlist)
black <- ifelse (race.wb==2,1,0)
cblack <- ifelse (crace.WBH==2,1,0)


p.evang.full<- p.evang[state.initnum]
cp.evang.full<- p.evang[cstate.initnum]
p.mormon.full <- p.mormon[state.initnum]
cp.mormon.full<- p.mormon[cstate.initnum]

p.relig <- p.evang + p.mormon
p.relig.full <- p.evang.full + p.mormon.full
cp.relig.full <- cp.evang.full + cp.mormon.full

p.gore.full <- gore.00[state.initnum]
cp.gore.full <- gore.00[cstate.initnum]

p.kerry.full <- kerry.04[state.initnum]
cp.kerry.full <- kerry.04[cstate.initnum]

p.clinton.full <- clinton.92[state.initnum]
cp.clinton.full <- clinton.92[cstate.initnum]

p.ideology.full <- ideology[state.initnum]
cp.ideology.full <- ideology[cstate.initnum]


souter.confirm <- glmer(formula = q.confirm ~  (1|race.female) + (1|age.cat) + (1|edu.cat)+ (1|state) + (1|region) + (1|poll) + (1|age.edu.cat)#
        +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- souter.confirm
response <- q.confirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.female[crace.female,1]
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +ranef(mlm)$age.edu.cat[cage.edu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

souter.statepred.confirm <- statepred
souter.stateraw.confirm <- stateraw 

cbind(souter.statepred.confirm,souter.stateraw.confirm)

souter.donotconfirm <- glmer(formula = q.donotconfirm ~  (1|race.female) + (1|age.cat) + (1|edu.cat)+ (1|state) + (1|region) + (1|poll) + (1|age.edu.cat)#
        +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- souter.donotconfirm
response <- q.donotconfirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.female[crace.female,1]
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +ranef(mlm)$age.edu.cat[cage.edu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

souter.statepred.donotconfirm <- statepred
souter.stateraw.donotconfirm <- stateraw 

cbind(souter.statepred.donotconfirm,souter.stateraw.donotconfirm)


souter.statepred.confirm.withop <- 100*souter.statepred.confirm/(souter.statepred.confirm+souter.statepred.donotconfirm)
souter.statepred.donotconfirm.withop <- 100*souter.statepred.donotconfirm/(souter.statepred.confirm+souter.statepred.donotconfirm)
souter.statepred.noop <- 100-(souter.statepred.confirm+souter.statepred.donotconfirm)

#doublechecking they each sum to 1
souter.statepred.confirm+ souter.statepred.donotconfirm + souter.statepred.noop
souter.statepred.confirm.withop+ souter.statepred.donotconfirm.withop 

souter.statepred.confirm
souter.statepred.donotconfirm
souter.statepred.confirm.withop 

export.souter <- data.frame(stateinitlist,souter.statepred.confirm, souter.statepred.donotconfirm,souter.statepred.confirm.withop)
write.csv(export.souter, "individual_estimates_souter.csv")


detach(souter.megapoll)
rm(souter.megapoll)
detach(Census)
rm(Census)


#########################                                       ############################
#########################                                       ############################
#########################                                       ############################
#########################       MIERS
#########################                                       ############################
#########################                                       ############################

Census <- read.dta("poststratification 2000.dta",convert.underscore = TRUE)
attach(Census)
Census <- Census[order(cstate),]
detach(Census)
attach(Census)

miers.megapoll <- read.dta("Miers_megapoll.dta",convert.underscore = TRUE)
attach(miers.megapoll)


#creates a score running: male white, male black, male hisp, female white, female black, female hisp
race.female <- (female *3) + race.wbh
crace.female <- (cfemale *3) + crace.WBH
age.edu.cat <- 4 * (age.cat -1) + edu.cat
cage.edu.cat <- 4 * (cage.cat - 1 ) + cedu.cat
cstate.initnum <-  match(cstate, stateinitlist)


p.evang.full<- p.evang[state.initnum]
cp.evang.full<- p.evang[cstate.initnum]
p.mormon.full <- p.mormon[state.initnum]
cp.mormon.full<- p.mormon[cstate.initnum]

p.relig <- p.evang + p.mormon
p.relig.full <- p.evang.full + p.mormon.full
cp.relig.full <- cp.evang.full + cp.mormon.full

p.gore.full <- gore.00[state.initnum]
cp.gore.full <- gore.00[cstate.initnum]

p.kerry.full <- kerry.04[state.initnum]
cp.kerry.full <- kerry.04[cstate.initnum]

p.clinton.full <- clinton.92[state.initnum]
cp.clinton.full <- clinton.92[cstate.initnum]

p.ideology.full <- ideology[state.initnum]
cp.ideology.full <- ideology[cstate.initnum]

miers.confirm <- glmer(formula = q.confirm ~  +(1|race.wbh) + female + (1|age.cat) + (1|edu.cat) + (1|state) + (1|region)   + (1|age.edu.cat)#+ (1|poll) # 
    +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- miers.confirm
response <- q.confirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.wbh[crace.WBH,1]
                    +(fixef(mlm)["female"] *cfemale)
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

miers.statepred.confirm <- statepred
miers.stateraw.confirm <- stateraw 

cbind(miers.statepred.confirm,miers.stateraw.confirm)


miers.donotconfirm <- glmer(formula = q.donotconfirm ~  +(1|race.wbh) + female + (1|age.cat) + (1|edu.cat) + (1|state) + (1|region)   + (1|age.edu.cat)#+ (1|poll) # 
    +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- miers.donotconfirm
response <- q.donotconfirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.wbh[crace.WBH,1]
                    +(fixef(mlm)["female"] *cfemale)
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

miers.statepred.donotconfirm <- statepred
miers.stateraw.donotconfirm <- stateraw 

cbind(miers.statepred.donotconfirm,miers.stateraw.donotconfirm)


miers.statepred.confirm.withop <- 100*miers.statepred.confirm/(miers.statepred.confirm+miers.statepred.donotconfirm)
miers.statepred.donotconfirm.withop <- 100*miers.statepred.donotconfirm/(miers.statepred.confirm+miers.statepred.donotconfirm)
miers.statepred.noop <- 100-(miers.statepred.confirm+miers.statepred.donotconfirm)



#doublechecking they each sum to 1
miers.statepred.confirm+ miers.statepred.donotconfirm + miers.statepred.noop
miers.statepred.confirm.withop+ miers.statepred.donotconfirm.withop 

miers.statepred.confirm
miers.statepred.donotconfirm
miers.statepred.confirm.withop 

export.miers <- data.frame(stateinitlist,miers.statepred.confirm, miers.statepred.donotconfirm,miers.statepred.confirm.withop)
write.csv(export.miers, "individual_estimates_miers.csv")


detach(miers.megapoll)
rm(miers.megapoll)
detach(Census)
rm(Census)



#########################                                       ############################
#########################                                       ############################
#########################                                       ############################
#########################       sotomayor
#########################                                       ############################
#########################                                       ############################

Census <- read.dta("poststratification 2000.dta",convert.underscore = TRUE)
attach(Census)
Census <- Census[order(cstate),]
detach(Census)
attach(Census)


sotomayor.megapoll <- read.dta("sotomayor_megapoll.dta",convert.underscore = TRUE)
attach(sotomayor.megapoll)


#creates a score running: male white, male black, male hisp, female white, female black, female hisp
race.female <- (female *3) + race.wbh
crace.female <- (cfemale *3) + crace.WBH
age.edu.cat <- 4 * (age.cat -1) + edu.cat
cage.edu.cat <- 4 * (cage.cat - 1 ) + cedu.cat
cstate.initnum <-  match(cstate, stateinitlist)



p.evang.full<- p.evang[state.initnum]
cp.evang.full<- p.evang[cstate.initnum]
p.mormon.full <- p.mormon[state.initnum]
cp.mormon.full<- p.mormon[cstate.initnum]

p.relig <- p.evang + p.mormon
p.relig.full <- p.evang.full + p.mormon.full
cp.relig.full <- cp.evang.full + cp.mormon.full

p.gore.full <- gore.00[state.initnum]
cp.gore.full <- gore.00[cstate.initnum]

p.kerry.full <- kerry.04[state.initnum]
cp.kerry.full <- kerry.04[cstate.initnum]

p.clinton.full <- clinton.92[state.initnum]
cp.clinton.full <- clinton.92[cstate.initnum]

p.ideology.full <- ideology[state.initnum]
cp.ideology.full <- ideology[cstate.initnum]


sotomayor.confirm <- glmer(formula = q.confirm ~  +(1|race.wbh) + female + (1|age.cat) + (1|edu.cat) + (1|state) + (1|region)   + (1|age.edu.cat)#+ (1|poll) # 
    +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- sotomayor.confirm
response <- q.confirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.wbh[crace.WBH,1]
                    +(fixef(mlm)["female"] *cfemale)
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

sotomayor.statepred.confirm <- statepred
sotomayor.stateraw.confirm <- stateraw 

cbind(sotomayor.statepred.confirm,sotomayor.stateraw.confirm)

sotomayor.donotconfirm <- glmer(formula = q.donotconfirm ~  +(1|race.wbh) + female + (1|age.cat) + (1|edu.cat) + (1|state) + (1|region)   + (1|age.edu.cat)#+ (1|poll) # 
    +  p.ideology.full,
    family=binomial(link="logit"))
mlm <- sotomayor.donotconfirm
response <- q.donotconfirm
display(mlm,detail=TRUE)


        #create vector of state ranefs and then fill in missing ones
        state.ranefs <- array(NA,c(51,1))
        dimnames(state.ranefs) <- list(c(stateinitlist),"effect")
        for(i in stateinitlist){
            state.ranefs[i,1] <- ranef(mlm)$state[i,1]
        }
        #if missing states... set to 0
        state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
        
        #create vector of region ranefs and then fill in missing ones
                region.ranefs <- array(NA,c(5,1))
                dimnames(region.ranefs) <- list(c("dc","midwest","northeast","south","west"),"effect")
                for(i in c("dc","midwest","northeast","south","west")){
                    region.ranefs[i,1] <- ranef(mlm)$region[i,1]
                }
                #if missing region set to 0
                region.ranefs[,1][is.na(region.ranefs[,1])] <- 0     
        
        #create a prediction in each cell (each geographic-demographic type)
        cellpred <- invlogit(fixef(mlm)["(Intercept)"]
                    +ranef(mlm)$race.wbh[crace.WBH,1]
                    +(fixef(mlm)["female"] *cfemale)
                    +ranef(mlm)$age.cat[cage.cat,1]
                    +ranef(mlm)$edu.cat[cedu.cat,1]
                    +state.ranefs[cstate,1]
                    +region.ranefs[cregion,1]   
                    +(fixef(mlm)["p.ideology.full"] *cp.ideology.full)
                       )
        
        #weights the prediction by the freq of cell                                       
        cellpredweighted <- cellpred * cpercent.state
        
        #calculates the percent within each state (weighted average of responses)
        statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
        
        #calculates the raw percent within each state
        stateraw <- rep(NA,51)
        for (j in 1:51){
        stateraw[j] <-  100*mean(response[state.initnum==j],na.rm = TRUE)
        }
        
        

sotomayor.statepred.donotconfirm <- statepred
sotomayor.stateraw.donotconfirm <- stateraw 

cbind(sotomayor.statepred.donotconfirm,sotomayor.stateraw.donotconfirm)


sotomayor.statepred.confirm.withop <- 100*sotomayor.statepred.confirm/(sotomayor.statepred.confirm+sotomayor.statepred.donotconfirm)
sotomayor.statepred.donotconfirm.withop <- 100*sotomayor.statepred.donotconfirm/(sotomayor.statepred.confirm+sotomayor.statepred.donotconfirm)
sotomayor.statepred.noop <- 100-(sotomayor.statepred.confirm+sotomayor.statepred.donotconfirm)



#doublechecking they each sum to 1
sotomayor.statepred.confirm+ sotomayor.statepred.donotconfirm + sotomayor.statepred.noop
sotomayor.statepred.confirm.withop+ sotomayor.statepred.donotconfirm.withop 

sotomayor.statepred.confirm
sotomayor.statepred.donotconfirm
sotomayor.statepred.confirm.withop 

export.sotomayor <- data.frame(stateinitlist,sotomayor.statepred.confirm, sotomayor.statepred.donotconfirm,sotomayor.statepred.confirm.withop)
write.csv(export.sotomayor, "individual_estimates_sotomayor.csv")


detach(sotomayor.megapoll)
rm(sotomayor.megapoll)
detach(Census)
rm(Census)


###########################    end of specific justice files #########################

###########COMBINE ESTIMATES

export.all <- data.frame(stateinitlist,
    oconnor.statepred.confirm, oconnor.statepred.donotconfirm,oconnor.statepred.confirm.withop,
    rehnquist.statepred.confirm, rehnquist.statepred.donotconfirm,rehnquist.statepred.confirm.withop,
    bork.statepred.confirm, bork.statepred.donotconfirm,bork.statepred.confirm.withop,
    souter.statepred.confirm, souter.statepred.donotconfirm,souter.statepred.confirm.withop,
    thomas.statepred.confirm, thomas.statepred.donotconfirm,thomas.statepred.confirm.withop,
    ginsburg.statepred.confirm, ginsburg.statepred.donotconfirm,ginsburg.statepred.confirm.withop,
    breyer.statepred.confirm, breyer.statepred.donotconfirm,breyer.statepred.confirm.withop,
    roberts.statepred.confirm, roberts.statepred.donotconfirm,roberts.statepred.confirm.withop,
    alito.statepred.confirm, alito.statepred.donotconfirm,alito.statepred.confirm.withop,
		miers.statepred.confirm, miers.statepred.donotconfirm,miers.statepred.confirm.withop,
		sotomayor.statepred.confirm, sotomayor.statepred.donotconfirm,sotomayor.statepred.confirm.withop)
write.csv(export.all, "all_nominees_estimates_short.csv")


state.long <- rep(stateinitlist,11)
nominee.long <- rep(c("roberts","miers", "alito","ginsburg","souter","thomas","bork","rehnquist","oconnor","breyer", "sotomayor"),each=51)
statepred.confirm <- c(roberts.statepred.confirm, miers.statepred.confirm, alito.statepred.confirm,ginsburg.statepred.confirm,souter.statepred.confirm,thomas.statepred.confirm,
    bork.statepred.confirm,rehnquist.statepred.confirm,oconnor.statepred.confirm, breyer.statepred.confirm, sotomayor.statepred.confirm)
statepred.donotconfirm <- c(roberts.statepred.donotconfirm, miers.statepred.donotconfirm, alito.statepred.donotconfirm,ginsburg.statepred.donotconfirm,souter.statepred.donotconfirm,thomas.statepred.donotconfirm,
    bork.statepred.donotconfirm,rehnquist.statepred.donotconfirm,oconnor.statepred.donotconfirm, breyer.statepred.donotconfirm,  sotomayor.statepred.donotconfirm)
statepred.confirm.withop <- 100*statepred.confirm/(statepred.confirm + statepred.donotconfirm )


#create long data -- unit is state nominee
export.all.party.long <- data.frame(
    state.long,
    nominee.long,
    statepred.confirm,
    statepred.donotconfirm,
    statepred.confirm.withop)
export.all.party.long <- export.all.party.long[export.all.party.long$state.long !="DC",]
write.csv(export.all.party.long, "all_nominees_estimates_long.csv")


#########
#create list estimates used in paper -- confirm among those w/ an opinion
table.estimates <- data.frame("State" = sstatename, "O'connor" = round(oconnor.statepred.confirm.withop),
	"Rehnquist" = round(rehnquist.statepred.confirm.withop), "Bork" = round(bork.statepred.confirm.withop), 
	"Souter" = round(souter.statepred.confirm.withop), "Thomas" = round(thomas.statepred.confirm.withop), 
	"Ginsburg" = round(ginsburg.statepred.confirm.withop), "Breyer" = round(breyer.statepred.confirm.withop),
	"Roberts" = round(roberts.statepred.confirm.withop), "Alito"= round(alito.statepred.confirm.withop),  
	"Miers" = round(miers.statepred.confirm.withop))
table.estimates <- table.estimates[order(table.estimates$State, decreasing =F),]


write.csv(table.estimates, "all_nominees_estimates_of_with_opinion.csv")
