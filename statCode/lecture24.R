library(RWinEdt)
library(MASS)
library(epitools)
library(lattice)
library(lme4)
options(width=75)

# NKNW table 24.10 example

fa = 8.65/(2.40 + 3.96 - 1.49)
fb = 4.20/(2.40 + 3.13 - 1.49)
fc = 6.20/(3.13 + 3.96 - 1.49)
fab = 2.40/1.49
fac = 3.96/1.49
fbc = 3.13/1.49
fabc = 1.49/2.30

dfa = (2.40 + 3.96 - 1.49)^2/(2.40^2/2 + 3.96^2/8 + 1.49^2/8)
dfb = (2.40 + 3.13 - 1.49)^2/(2.40^2/2 + 3.13^2/4 + 1.49^2/8)
dfc = (3.13 + 3.96 - 1.49)^2/(3.13^2/4 + 3.96^2/8 + 1.49^2/8)

pf(fa, 2, dfa, lower.tail=F)
pf(fb, 1, dfb, lower.tail=F)
pf(fc, 4, dfc, lower.tail=F)
pf(fab, 2, 8, lower.tail=F)
pf(fac, 8, 8, lower.tail=F)
pf(fbc, 4, 8, lower.tail=F)
pf(fabc, 8, 60, lower.tail=F)

pf(1.78,2,4.63, lower.tail=F)
pf(1.78,2,4, lower.tail=F)
pf(1.78,2,5, lower.tail=F)


# Orthodontics example - see lecture 23

options(contrasts=c("contr.treatment","contr.poly"))

orthodont.reml <- lmer(distance ~ Sex + age + (1|Subject), data=Orthodont,
  method="REML")
orthodont.mle <- lmer(distance ~ Sex + age + (1|Subject), data=Orthodont,
  method="ML")
  
orthodont.reml
orthodont.mle

orthodont.fix <- lm(distance ~ Sex + age + Subject, data=Orthodont)
orthodont.fix
