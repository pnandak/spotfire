##----------------------------------------##
## Script for Part 3: Statistical Models  ##
##    John Fox                            ##
## An Introduction to R                   ##
##    UCLA Feb. 2005                      ##
##----------------------------------------##


    # multiple regression (Prestige data)
    
library(car)
data(Prestige)
names(Prestige)

attach(Prestige)
prestige.mod <- lm(prestige ~ income + education + women)
summary(prestige.mod)

    # dummy regression

type    # a factor

detach(Prestige)
Prestige.2 <- na.omit(Prestige) # filter out missing data
attach(Prestige.2)
type
length(type)

class(type)
unclass(type)

        # generating contrasts from factors
        
options("contrasts")

contrasts(type)

contrasts(type) <- contr.treatment(levels(type), base=2)    # changing
                                                 # the baseline category
contrasts(type)

contrasts(type) <- 'contr.helmert'  # Helmert contrasts
contrasts(type)

contrasts(type) <- 'contr.sum'  # "deviation" contrasts
contrasts(type)

contrasts(type) <- NULL  # back to default

type.ord <- ordered(type, levels=c("bc", "wc", "prof")) # ordered factor
type.ord
round(contrasts(type.ord), 3)   # orthogonal polynomial contrasts

prestige.mod.1 <- lm(prestige ~ income + education + type)
summary(prestige.mod.1)

anova(prestige.mod.1)   # sequential ("type-I") sums of squares

prestige.mod.0 <- lm(prestige ~ income + education)
summary(prestige.mod.0)
anova(prestige.mod.0, prestige.mod.1)   # incremental F-test

Anova(prestige.mod.1)   # "type-II" sums of squares

prestige.mod.2 <- lm(prestige ~ income + education + type - 1)
                                          # suppressing the constant
summary(prestige.mod.2)
Anova(prestige.mod.2)   # note: test for type is that all intercepts
                        #   are 0 (not sensible)

prestige.mod.3 <- update(prestige.mod.1, 
    .~. + income:type + education:type)     # adding interactions to the model
summary(prestige.mod.3)
Anova(prestige.mod.3)

lm(prestige ~ income*type + education*type) # equivalent specifications
lm(prestige ~ (income + education)*type)

lm(prestige ~ type + (income + education) %in% type)  # nesting
lm(prestige ~ type + (income + education) %in% type - 1)  # separate
                                              # intercepts and slopes

detach(Prestige.2)

    # more on lm

args(lm)

data(Davis)
lm(weight ~ repwt, data=Davis, subset=sex == "F")  # observation
                                                   #  selection

lm(weight ~ repwt, data=Davis, subset=1:100)

data(Duncan)
lm(prestige ~ income + education, data=Duncan, subset=-c(6, 16))

lm(prestige~I(income + education), data=Duncan)  # protecting
                                       # expresssion on RHS of the model


    # generalized linear models
    
        # binary logit model
    

data(Mroz)
some(Mroz)

attach(Mroz)
mod.mroz <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
    family=binomial)
summary(mod.mroz)
    
anova(update(mod.mroz, . ~ . - k5), mod.mroz, test="Chisq")
                                               # likelihood-ratio test

Anova(mod.mroz)  # analysis-of-deviance table

detach(Mroz)


