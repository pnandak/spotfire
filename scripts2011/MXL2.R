################################
### R code to estimate a MXL ###
################################


ReadData <- read.table("C:\\Essex\\BES2010.txt", header=T, sep="\t")

## Note we are not removing missing data here, since this data is already cleaned
## You might need to insert a step to remove missing values with other data


#install.packages("mlogit")

library(mlogit)

MXLData <- mlogit.data(ReadData, choice="vote_intent", shape="wide", varying=1:3, sep="_", alt.levels=c("Brown","Cameron","Clegg"))


## A CL model ##

clogit <- mlogit(vote_intent ~ app | persfin+natecon+affect_fc+app_Afgh+MP_resign, data=MXLData, reflevel="Clegg")

summary(clogit)



## A MXL model ##
## This puts a triangular distribution on approval ##

mixlogit <- mlogit(vote_intent ~ app | persfin+natecon+affect_fc+app_Afgh+MP_resign, rpar=c(app='t'), halton=NA, R=100, method="bfgs", data=MXLData, reflevel="Clegg")

summary(mixlogit)

plot(mixlogit)

## It looks like this package only gives random coefficients for variables that vary across alternatives ##
##but we can trick it into giving us random coefficients for variables that only vary across individuals ##

affect_fc.Brown <- MXLData$affect_fc*(MXLData$alt=="Brown")
affect_fc.Cameron <- MXLData$affect_fc*(MXLData$alt=="Cameron")

# This model has normally distributed coefficients on opinions on the war in Afghanistan #

mixlogit2 <- mlogit(vote_intent ~ app+affect_fc.Brown+affect_fc.Cameron | persfin+natecon+app_Afgh+MP_resign, rpar=c(affect_fc.Brown='n', affect_fc.Cameron='n'), halton=NA, R=100, method="bfgs", data=MXLData, reflevel="Clegg")

summary(mixlogit2)

plot(mixlogit2)

