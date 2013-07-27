#######################################################################
## demonstrate multiple regression issues, spuriousness etc
##
## this complements Agresti and Finlay, Chapter 11
##
## simon jackman
## dept of political science, stanford univ
## may 31, 2006
#######################################################################


## A&F Example 11.1, pp383ff
## crime rates in Florida counties
table916 <- read.csv(file="table9.16.asc",
                     header=TRUE)

## look at data
pairs(~ crime + highSchool + urban,
      data=table916)

## correlation matrix
## note the multicollinearity
## see Figure 11.8
cor(cbind(table916$crime,
          table916$highSchool,
          table916$urban))

## run three different regressions
reg1 <- lm(crime ~ highSchool,
           data=table916)

reg2 <- lm(crime ~ urban,
           data=table916)

## multiple regression
reg3 <- lm(crime ~ urban + highSchool,
           data=table916)

coplot(crime ~ highSchool | urban,
       data=table916,
       number=3,
       rows=1,columns=3,
       overlap=0)

## A&F Example 11.2, pp388ff
table111 <- read.table(file="table11.1.asc",
                       header=T)

## look at data
## Figure 11.5
pairs(~ impairment + lifeEvents + ses,
      data=table111)

## Figure 11.6
coplot(impairment ~ lifeEvents | ses,
       data=table111,
       number=3,
       rows=1,columns=3,
       overlap=0)      

cor(cbind(table111$impairment,
          table111$lifeEvents,
          table111$ses))

## run three different regressions
## Table 11.3
reg1 <- lm(impairment ~ lifeEvents,
           data=table111)

## Table 11.4
reg2 <- lm(impairment ~ ses,
           data=table111)

## Table 11.5
reg3 <- lm(impairment ~ lifeEvents + ses,
           data=table111)

## F test reg3 against reg2
anova(reg2,reg3)

## Example 11.6 interaction term
reg4 <- lm(impairment ~ lifeEvents*ses,
           data=table111)

#######################################################
## one way anova
table1219 <- read.table(file="table12.19.asc",
                        header=T)
beforeAvg <- tapply(table1219$before,
                    table1219$therapy,
                    mean)
afterAvg <- tapply(table1219$before,
                   table1219$therapy,
                   mean)

boxplot(before ~ therapy,
        data=table1219)

boxplot(after ~ therapy,
        data=table1219)

## transform the data set
newdata <- rbind(cbind(table1219$subj,
                       table1219$therapy,
                       table1219$before,
                       0),
                 cbind(table1219$subj,
                       table1219$therapy,
                       table1219$after,
                       1)
                 )
newdata <- as.data.frame(newdata)
names(newdata) <- c("subj","therapy","weight","time")
newdata$subj <- factor(newdata$subj)
newdata$therapy <- factor(newdata$therapy)
newdata$time <- factor(newdata$time)

reg1 <- lm(weight ~ therapy*time + subj,
           data=newdata)

