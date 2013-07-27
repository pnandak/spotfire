snoring <- matrix(c(24,1355,35,603,21,192,30,224), ncol=2, byrow=TRUE)
dimnames(snoring) <-
  list(snore=c("never","sometimes","often","always"),
       heartdisease=c("yes","no"))
snoring
scores.a <- c(0,2,4,5)
scores.b <- c(0,2,4,6)
scores.c <- 0:3
scores.d <- 1:4
# Fitting and comparing logistic regression models
snoring.lg.a <- glm( snoring ~ scores.a, family=binomial() )
snoring.lg.b <- glm( snoring ~ scores.b, family=binomial() )
snoring.lg.c <- glm( snoring ~ scores.c, family=binomial() )
snoring.lg.d <- glm( snoring ~ scores.d, family=binomial() )
coef(snoring.lg.a)
coef(snoring.lg.b)
coef(snoring.lg.c)
coef(snoring.lg.d)
predict(snoring.lg.a, type="response")  # compare to table 4.1
predict(snoring.lg.b, type="response")
predict(snoring.lg.c, type="response")
predict(snoring.lg.d, type="response")
