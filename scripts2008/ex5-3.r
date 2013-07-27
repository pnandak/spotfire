snore.score <- c(0,2,4,5)
ex5.3.lg1 <- glm(snoring ~ snore.score, family=binomial())
summary(ex5.3.lg1)
# Here are the fitted probabilities
predict(ex5.3.lg1, type="response")
# To do the goodness of fit test of the ordinal model
# compare to a saturated model
snore <- factor(c("Never","Occasionaly","Nearly Every Night","Every Night"))
ex5.3.lg2 <- glm(snoring ~ snore, family=binomial())
summary(ex5.3.lg2)
anova(ex5.3.lg1, ex5.3.lg2, test="Chisq")

snoring <- matrix(c(24,1355,35,603,21,192,30,224), ncol=2, byrow=TRUE)
dimnames(snoring) <-
  list(snore=c("never","sometimes","often","always"),
       heartdisease=c("yes","no"))
snoring
snore.score <- c(0,2,4,5)
ex5.3.lg1 <- glm(snoring ~ snore.score, family=binomial())
summary(ex5.3.lg1)
# Here are the fitted probabilities
predict(ex5.3.lg1, type="response")
# To do the goodness of fit test of the ordinal model
# compare to a saturated model
snore <- factor(c("Never","Occasionaly","Nearly Every Night","Every Night"))
ex5.3.lg2 <- glm(snoring ~ snore, family=binomial())
summary(ex5.3.lg2)
anova(ex5.3.lg1, ex5.3.lg2, test="Chisq")
