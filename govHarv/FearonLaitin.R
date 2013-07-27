foo <- as.data.frame(dget(file="http://jsekhon.fas.harvard.edu/gov2000/R/FearonLaitin.dpt"))

glm1  <- glm(onset ~ warl + gdpenl +lpopl1 +lmtnest +ncontig +Oil +nwstate +instab +polity2l +ethfrac +relfrac,
             data=foo, family=binomial)
print(summary(glm1))
