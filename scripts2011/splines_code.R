### Splines Update

data <- read.dta("jacob.dta")

knots <- quantile(data$perotvote, probs=c(.5))
mod1 <- lm(chal_vote ~ bs(perotvote, knots=knots, degree=3), data=data)
plot(effect("bs(perotvote, knots=knots, degree=3)", mod1, default.levels=100))
AIC <- AIC(mod1)

knots <- quantile(data$perotvote, probs=c(.33,.67))
mod2 <- lm(chal_vote ~ bs(perotvote, knots=knots, degree=3), data=data)
plot(effect("bs(perotvote, knots=knots, degree=3)", mod2, default.levels=100))
AIC <- c(AIC, AIC(mod2))

knots <- quantile(data$perotvote, probs=c(.25,.5,.75))
mod3 <- lm(chal_vote ~ bs(perotvote, knots=knots, degree=3), data=data)
plot(effect("bs(perotvote, knots=knots, degree=3)", mod3, default.levels=100))
AIC <- c(AIC, AIC(mod3))

knots <- quantile(data$perotvote, probs=c(.2,.4,.6,.8))
mod4 <- lm(chal_vote ~ bs(perotvote, knots=knots, degree=3), data=data)
AIC <- c(AIC, AIC(mod4))
plot(effect("bs(perotvote, knots=knots, degree=3)", mod4, default.levels=100))
I