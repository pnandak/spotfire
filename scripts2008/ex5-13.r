# You don't have to enter the variables in a data frame:
race <- factor(rep(c("white","black"),c(2,2)))
gender <- factor(rep(c("male","female"),2))
intercourse <- matrix(c(43,134,26,149,29,23,22,36),byrow=TRUE,ncol=2)
adolsex.lr <- glm(intercourse ~ race + gender, family=binomial())
summary(adolsex.lr)
# But you can use a data frame to hold the data if you want to.
# Starting from scratch for fun ...
race <- rep(c("white","black"),c(2,2))
gender <- rep(c("male","female"),2)
intercourse <- matrix(c(43,134,26,149,29,23,22,36),byrow=TRUE,ncol=2)
dimnames(intercourse) <- list(NULL,intercourse=c("yes","no"))
adolsex <- data.frame(race=race,gender=gender,intercourse=intercourse)
adolsex
# Note that data.frame() automatically converts character variables
# to factors, so we didn't have to use the factor() function here.
is.factor(adolsex$race)
is.character(race)
is.factor(race)
adolsex.lr <- glm(intercourse ~ race + gender, family=binomial(), data=adolsex)
summary(adolsex.lr)
