

library(car)
data(Florida)
out1 <- lm(GORE ~ Total, data=Florida)
summary(out1)
confint(out1)

out2 <- lm(I(GORE/1000) ~ Total, data=Florida)
summary(out2)
confint(out2)

out3 <- lm(GORE ~ I(Total/1000), data=Florida)
summary(out3)
confint(out3)

out4 <- lm(I(GORE/1000) ~ I(Total/1000), data=Florida)
summary(out4)
confint(out4)



