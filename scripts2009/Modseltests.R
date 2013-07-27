library(car)
data(Prestige)
attach(Prestige)
prestige.mod <- glm(prestige~income,family=gaussian)
prestige.mod2 <- glm(prestige~education,family=gaussian)

prestige.test <- mod.sel(prestige.mod,prestige.mod2)
summary(prestige.test)

library(foreign)
huth.data <- read.dta("c:/KAC/6355.dta")
attach(huth.data)
huth.mod <- glm(outcome~nuncp1+runcp13+nuncp2+runcp23+
risk23pm,family=binomial(link=probit))
huth.mod2 <- glm(outcome~dispbof+rinukes+defint+chint+riwhimp+
chwhimp+riothdis+chothdis,family=binomial(link=probit))

huth.test <- mod.sel(huth.mod,huth.mod2)
summary(huth.test)

library(car)

data(Ornstein)
attach(Ornstein)
ornstein.mod <- glm(interlocks ~ assets,
family=poisson)
ornstein.mod2 <- glm(interlocks ~ sector,
family=poisson)

ornstein.test <- mod.sel(ornstein.mod,ornstein.mod2)
summary(ornstein.test)
