#####################################################
## dummy variables and interactions
## Simon Jackman, Dept of Political Science
## Stanford Univ
##
## PS 151B Spring 2006
#####################################################

############################################################
## spruce tree example, damage as a function of elevation
############################################################
hamilton102 <- read.table(file="hamilton102.asc",
                          header=T)
attach(hamilton102)   
plot(damage ~ elevation)

## fit a simple linear regression
reg1 <- lm(damage ~ elevation)

plot(damage ~ elevation,type="n")
text(elevation,damage,location)
summary(reg1)

## then, reading off the output from summary
## overlay location-specific regression lines
abline(-41.34,.09118)    ## north
abline(-41.33 + 78.62,.09118 + -.10839)  ## south


scatterFunc <- function(x,y,col,pch,...){
  points(x,y,...)
  abline(lm(y~x))
  invisible(NULL)
}

coplot(damage ~ elevation | location,
       panel=scatterFunc)]

## fit better model
reg2 <- lm(damage ~ elevation*location,
           data=hamilton102)

## test the richer model (reg2) against the
## simple 2-parameter model (reg1)
anova(reg1,reg2)   ## f-test of silly restricted model against full model

##########################################################
## occupational prestige
##########################################################
## attach Fox R library 
library(car)

## access Duncan data
data(Duncan)

## look at data
boxplot(prestige ~ type,
        data=Duncan)

plot(prestige ~ income,
     data=Duncan)
identify(Duncan$income,
         Duncan$prestige,
         row.names(Duncan))

plot(prestige ~ education,
     data=Duncan)

## model fitting
model1 <- lm(prestige ~ education + income,
             data=Duncan)

## type enters as a series of dummy variables
## to produce intercept shifts
model2 <- lm(prestige ~ education + income + type,
             data=Duncan)
## compare this richer model against simple
## education and income model
anova(model1,model2)



## type enters as a series of intercept offsets
## AND slope offsets
model3 <- lm(prestige ~ (education + income)*type,
             data=Duncan)


######################################################
## logistic regression model
## Example 15.1 in Agresti and Finlay
######################################################
table15.1 <- read.table(file="table15.1.asc",
                        header=T)

table15.1$no <- table15.1$n - table15.1$yes

logit1 <- glm(cbind(yes,no)~income,
              data=table15.1,
              family=binomial)

#####################################################
## voter turnout data
nagler <- scan(file="nagler.asc",
               what=list(turnout=0,
                 educ=0,
                 age=0,
                 south=0,
                 govelec=0,
                 closing=0)
               )
nagler <- data.frame(nagler)

## recode education
z <- rep(NA,length(nagler$educ))   ## temp variable
z[nagler$educ==1] <- 2
z[nagler$educ==2] <- 6
z[nagler$educ==3] <- 8
z[nagler$educ==4] <- 10
z[nagler$educ==5] <- 12
z[nagler$educ==6] <- 14
z[nagler$educ==7] <- 16
z[nagler$educ==8] <- 19
nagler$educYrs <- z                ## years of Educ 
rm(z)                              ## clobber temp var z

## run a logit model
logit1 <- glm(turnout ~ age + educYrs,
              data=nagler,
              family=binomial)

logit2 <- update(logit1,
                 ~ . + south + govelec + closing)

