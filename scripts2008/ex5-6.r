party <- factor(c("dem","ind","rep"))
party.score <- c(1,2,3)
females <- c(279, 73, 225)
males <- c(165, 47, 191)
# Party as nominal (saturated model)
ex5.6.lg1 <- glm(cbind(females,males) ~ party, family=binomial())
# Party as ordinal
ex5.6.lg2 <- glm(cbind(females,males) ~ party.score, family=binomial())
# Independence model
ex5.6.lg0 <- glm(cbind(females,males) ~ 1, family=binomial())
# Model for part (a)
# Note that R assigns (1,0) dummy variables to the last two levels of
# party (dem,ind,rep in alphabetcial order), and not to the first level.
# This is the opposite of the SAS behaviour, but you can change it by
# reordering the levels of the factor (use the relevel function), or
# by creating your own contrast matrix for the factor by hand.  Usually
# it doesn't matter as long as you know what was done, which is obvious
# from the summary here.
summary(ex5.6.lg1)
# LRT for part (a)
# Goodness-of-fit test for independence model vs saturated, is
# equivalent to testing for independence.
anova(ex5.6.lg0,ex5.6.lg1,test="Chisq")
# Test for independence using ordinal model (part (b))
anova(ex5.6.lg0,ex5.6.lg2,test="Chisq")
# Parameter estimates for ordinal model
coef(ex5.6.lg2)
