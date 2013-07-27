

# ---- Alligator example ----

# make the table of all the categories.

food.labs<-factor(c("fish","invert","rep","bird","other"),levels=c("fish","invert", "rep", "bird","other"))
size.labs<-factor(c("<2.3",">2.3"),levels=c(">2.3","<2.3"))
gender.labs<-factor(c("m","f"),levels=c("m","f"))
lake.labs<-factor(c("hancock","oklawaha","trafford","george"),levels=c("george", "hancock", "oklawaha","trafford"))
alligator.table<-expand.grid(food=food.labs,size=size.labs,gender=gender.labs, lake=lake.labs)

# this is the count of number of observations in each cell.
counts<-c(7,1,0,0,5,4,0,0,1,2,16,3,2,2,3,3,0,1,2,3,2,2,0,0,1,13,7,6,0,0,3,9,1,0,2,0,1,0,1,0,3,7,1,0,1,8,6,6,3,5,2,4,1,1,4,0,1,0,0,0,13,10,0,2,2,9,0,0,1,2,3,9,1,0,1,8,1,0,0,1)

# make the "expanded" table: counts(i) copies of each cell (i).
alligator.table<-structure(.Data=alligator.table[rep(1:nrow(alligator.table),counts),], row.names=1:219)

sum(alligator.table$food=='fish')
sum(alligator.table$food=='invert')
sum(alligator.table$food=='rep')
sum(alligator.table$food=='bird')
sum(alligator.table$food=='other')

library(nnet)
#options(contrasts=c("contr.treatment","contr.poly"))
fitS<-multinom(food~lake*size*gender,data=alligator.table) # saturated model
fit0<-multinom(food~1,data=alligator.table) # null
fit1<-multinom(food~gender,data=alligator.table) # G
fit2<-multinom(food~size,data=alligator.table) # S
fit3<-multinom(food~lake,data=alligator.table) # L

anova(fit0,fit1)
anova(fit0,fit2)
anova(fit0,fit3)

# colapsing over gender.
fit4<-multinom(food~size+lake,data=alligator.table)  # L + S
fit5<-multinom(food~size*lake,data=alligator.table)  # L * S

anova(fit3,fit4)
anova(fit4,fit5)

# Alternatively, select model that minimizes the AIC:
fit1$AIC
fit2$AIC
fit3$AIC
fit4$AIC
fit5$AIC

# ---- Get the fitted values.
# get the marginals.
marg.counts <- tapply(alligator.table$food, 
                      list(factor(alligator.table$size, levels = c("<2.3", ">2.3")),factor(alligator.table$lake,levels =c("hancock", "oklawaha", "trafford", "george"))), 
                        length)
# get the fitted values
row.names.fitted <- rev(expand.grid(dimnames(marg.counts)))

# use model 4
fitted.counts<-round(as.vector(marg.counts)* fitted(fit4)[!duplicated(as.data.frame( fitted(fit4))),],1)
data.frame(fitted.counts,row.names= apply(row.names.fitted, 1, paste, collapse = " "))


# ---- predict the probability of each type of food for a certain value of "Lake" and "Size".
predict(fit4, type="probs", newdata=data.frame(size=">2.3", lake="hancock"))



#  Contingency Tables

# ---- Alcohol, Cigarette, and Marijuana Use Example. ----
druguse<-data.frame(expand.grid(marijuana=factor(c("Yes","No"),levels=c("No","Yes")), cigarette=factor(c("Yes","No"),levels=c("No","Yes")),
alcohol=factor(c("Yes","No"),levels=c("No","Yes"))), count=c(911,538,44,456,3,43,2,279))
druguse

# mutual independence.
fitA.C.M.glm<-glm(count~alcohol+marijuana+cigarette,data=druguse,family=poisson)

# Joint independence of marijuana and alcohol and cigarette.
fitAC.M.glm<-glm(count~alcohol*cigarette+marijuana,data=druguse,family=poisson)

# Conditional independence of marijuana and cigarette given alcohol.
fitAM.AC.glm<-glm(count~alcohol*cigarette+alcohol*marijuana,data=druguse,family=poisson)

# Homogeneous association
fitAC.AM.CM.glm<-glm(count~alcohol*cigarette+alcohol*marijuana+cigarette*marijuana,data=druguse,family=poisson)

# Saturated Model
fitACM.glm<-glm(count~alcohol*cigarette*marijuana,data=druguse,family=poisson)

druguse.fitted.glm=data.frame(druguse, ACM=round(fitted(fitACM.glm)),AC.AM.CM=round(fitted(fitAC.AM.CM.glm)),AM.AC=round(fitted(fitAM.AC.glm)),AC.M=round(fitted(fitAC.M.glm)),A.C.M=round(fitted(fitA.C.M.glm)))
anova(fitA.C.M.glm,fitAC.M.glm,fitAM.AC.glm,fitAC.AM.CM.glm,fitACM.glm, test="Chisq")

# --- Another way of doing the same thing, you can also try loglm function ----

library(MASS)

# Joint independence of marijuana and alcohol and cigarette.
fitAC.M.glm<-glm(count~alcohol*cigarette+marijuana,data=druguse,family=poisson)

# Conditional independence of marijuana and cigarette given alcohol.
fitAM.AC.glm<-glm(count~alcohol*cigarette+alcohol*marijuana,data=druguse,family=poisson)

# Homogeneous association
fitAC.AM.CM.glm<-glm(count~alcohol*cigarette+alcohol*marijuana+cigarette*marijuana,data=druguse,family=poisson)

# Saturated Model
fitACM.glm<-glm(count~alcohol*cigarette*marijuana,data=druguse,family=poisson)


# -----------------------------------------------
# lumber example for Poisson regression.
#    The Miller Lumber Company conducted an in-store customer survey. 
#    The researcher counted the number of customers who visited the 
#    store from each nearby census tract. The researcher also collected 
#    and subsequently retained five (quantitative) predictor variables 
#    for use in the Poisson Regression.  
#    Predictors: number of housing units in region, average household income, average
#    housing unit age in region, distance to nearest competitor, distance to store in miles.
#    Response: number of customers visiting store from census tract.  Each data 
#    point represents one census tract.
# --------------------------------------------
lumber <- read.table('Lumber.txt', header=T)
attach(lumber)

# Fit model using log link

lumber.glm <- glm(Customers ~ Housing + Income + Age + Competitor + Store, family=poisson())

# Check to see if Store and Competitor can be dropped

lumber.R.glm <- glm(Customers ~ Housing + Income + Age, family=poisson(link='log'))

# partial deviance test of full vs. reduced

print(anova(lumber.R.glm, lumber.glm,test="Chisq"))

# look at BIC
library(BMA)
lumber.bic = bic.glm(Customers ~ Housing + Income + Age + Competitor + Store, data=lumber, glm.family=poisson())
summary(lumber.bic)

