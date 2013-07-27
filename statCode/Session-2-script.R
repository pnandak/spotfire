##---------------------------------##
## Script for Session 2: Models    ##
##    John Fox                     ##
## Statistical Computing in R/S    ##
##    ICPSR Summer Program 2008    ##
##---------------------------------##

    # multiple regression (Prestige data)
    
library(car)
names(Prestige)
some(Prestige)

attach(Prestige)
search()

prestige.mod <- lm(prestige ~ income + education + women)
summary(prestige.mod)

    # dummy regression

type    # a factor

detach(Prestige)
Prestige.2 <- na.omit(Prestige) # filter out missing data
attach(Prestige.2)
search()

type
length(type)

class(type)
unclass(type)

        # generating contrasts from factors
        
options("contrasts")

contrasts(type)

contrasts(type) <- contr.treatment(levels(type), base=2)  # changing the
                                                      # baseline category
contrasts(type)

contrasts(type) <- "contr.helmert"  # Helmert contrasts
contrasts(type)

contrasts(type) <- "contr.sum"  # "deviation" contrasts
contrasts(type)

contrasts(type) <- NULL  # back to default

type.ord <- ordered(type, levels=c("bc", "wc", "prof")) # ordered factor
type.ord
round(contrasts(type.ord), 3)   # orthogonal polynomial contrasts

prestige.mod.1 <- lm(prestige ~ income + education + type)
summary(prestige.mod.1)

anova(prestige.mod.1)   # sequential ("type-I") sums of squares

prestige.mod.0 <- lm(prestige ~ income + education) # note:NA's filtered!
summary(prestige.mod.0)
anova(prestige.mod.0, prestige.mod.1)   # incremental F-test

Anova(prestige.mod.1)   # "type-II" sums of squares

prestige.mod.3 <- update(prestige.mod.1, 
    . ~ . + income:type + education:type)     # adding interactions
summary(prestige.mod.3)
Anova(prestige.mod.3)

lm(prestige ~ income*type + education*type) # equivalent specification
lm(prestige ~ (income + education)*type)

detach(Prestige.2)

    # Anova Models

attach(Moore)
some(Moore)

fcategory <- factor(fcategory, 
    levels=c("low","medium","high"))     # reorder levels
fcategory

tapply(conformity, list(fcategory, partner.status), mean)  # means
tapply(conformity, 
    list(fcategory, partner.status), sd) #  standard deviations
tapply(conformity, list(fcategory, partner.status), length)  # counts

            # graph of means:
            
interaction.plot(fcategory, partner.status, conformity, type="b",
    ylim=range(conformity), pch=c("H", "L"), cex=1.5)
points(jitter(as.numeric(fcategory), 0.5), conformity,
    pch=ifelse(partner.status=="low", "L", "H"))
identify(fcategory, conformity)

            # ANOVA tables

options(contrasts=c("contr.sum", "contr.poly"))  
                        # contr.sum = deviation contrasts
moore.mod <- lm(conformity ~ fcategory*partner.status)
summary(moore.mod)

Anova(moore.mod)    # type II sums of squares
Anova(moore.mod, type="III")    # type III sums of squares

detach(Moore)

    # more on lm

args(lm)

some(Davis)
lm(weight ~ repwt, data=Davis, subset=sex == "F")  
        # observation selection (women only)
lm(weight ~ repwt, data=Davis, subset=1:100)

lm(prestige ~ income + education, data=Duncan, subset=-c(6, 16))

lm(conformity ~ partner.status*fcategory,  # specifying contrasts
    contrasts=list(partner.status=contr.sum, fcategory=contr.poly),
    data=Moore)

lm(100*conformity/40 ~ partner.status*fcategory, data=Moore)  
                # data argument; note computation of y

lm(prestige~I(income + education), data=Duncan)  
                # "protecting" expresssion on RHS of the model


    # generalized linear models
    
        # binary logit model
    
some(Mroz)

attach(Mroz)
mod.mroz <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
    family=binomial)
summary(mod.mroz)
    
anova(update(mod.mroz, . ~ . - k5), mod.mroz, test="Chisq")  
                                        # likelihood-ratio test

Anova(mod.mroz)  # analysis-of-deviance table

detach(Mroz)

        # Poisson regression

some(Ornstein)

attach(Ornstein)
tab <- table(interlocks)
tab

x <- sort(unique(interlocks))
plot(x, tab, type="h", xlab="Number of Interlocks",
    ylab="Frequency")
points(x, tab, pch=16)

options(contrasts=c("contr.treatment", "contr.poly"))  # restore
mod.ornstein <- glm(interlocks ~ assets + nation + sector,
    family=poisson)
summary(mod.ornstein)
Anova(mod.ornstein)
