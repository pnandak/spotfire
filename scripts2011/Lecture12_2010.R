tscslag <- function(dat, x, id, time){
	obs <- apply(dat[, c(id, time)], 1, paste, collapse=".")
	tm1 <- dat[[time]] - 1
	lagobs <- apply(cbind(dat[[id]], tm1), 1, paste, collapse=".")
	lagx <- dat[match(lagobs, obs), x]
	lagx
}

gdm <- function(x, unit, data){
	ags <- aggregate(data[[x]], list(data[[unit]]), mean, na.rm=T)
	mean.x <- ags[match(data[[unit]], ags[,1]), 2]
	data[[x]]-mean.x
}



###################################################
### chunk number 1: tscslag
###################################################
dat <- Blackmoor
id <- "subj"
time <- "obs"
Blackmoor <- Blackmoor[order(Blackmoor$subject, Blackmoor$obs), ]
Blackmoor$subj <- match(Blackmoor$subject, unique(Blackmoor$subject))
Blackmoor$subj <- as.numeric(Blackmoor$subject)
Blackmoor$obs <- c(unlist(by(Blackmoor$age, list(Blackmoor$subject), function(x)seq(along=x))))


###################################################
### chunk number 2: readcws
###################################################
dat <- read.dta("cwssmall.dta")
convars <- c("sstran","flabfo","vturn","pctaged","cgdp",
    "tunemp","ofdi","openc")
dat.std <- dat
dat.std[, convars] <- scale(dat.std[, convars])


###################################################
### chunk number 3: makeid
###################################################
unid <- unique(dat[["idn"]])
unid <- unid[order(unid)]
dat[["id"]] <- match(dat[["idn"]], unid)


###################################################
### chunk number 4: femod
###################################################
gdm.dat <- data.frame(
sstran=gdm("sstran", 'idn', dat),     
leftcab=gdm("leftcab", 'idn', dat),    
cncrcab=gdm("cncrcab", 'idn', dat),    
flabfo=gdm("flabfo", 'idn', dat),     
pctaged=gdm("pctaged", 'idn', dat),    
cgdp=gdm("cgdp", 'idn', dat),       
tunemp=gdm("tunemp" , 'idn', dat),    
ofdi=gdm("ofdi", 'idn', dat),       
openc=gdm("openc", 'idn', dat),
vturn=gdm("vturn", 'idn', dat))

mod.fe1 <- lm(sstran ~ leftcab + cncrcab + flabfo + 
    vturn + pctaged + cgdp + tunemp  + ofdi + openc + 
    as.factor(idn), data=dat)

mod.fe2 <- lm(sstran ~ leftcab + cncrcab + flabfo + 
    vturn + pctaged + cgdp + tunemp  + ofdi + openc, data=gdm.dat)

###################################################
### chunk number 5: sumfe
###################################################
summary(mod.fe1)


###################################################
### chunk number 6: remod
###################################################
library(nlme)
mod.re <- gls(sstran ~ leftcab + cncrcab +
    flabfo + vturn + pctaged + cgdp +
    tunemp  + ofdi + openc, data=dat.std,
    correlation=corCompSymm(form=~1|idn))

mod.re2 <- lme(sstran ~ leftcab + cncrcab +
    flabfo + vturn + pctaged + cgdp +
    tunemp  + ofdi + openc, random = ~ 1|idn, data=dat.std)

###################################################
### chunk number 7: sumre
###################################################
summary(mod.re)


###################################################
### chunk number 8: fevdmod
###################################################
X <- model.matrix(mod.fe2, data=dat)
byX <- by(X, list(dat$idn), apply, 2, mean)
X <- do.call(rbind, byX)
sstran.ag <- aggregate(dat$sstran, list(dat$idn), mean)
e.ag <- aggregate(residuals(mod.fe2), list(dat$idn), mean)
ci <- sstran.ag[,2] - X%*%coef(mod.fe2)
res <- residuals(lm(ci[dat$id] ~ authleg + fed, data=dat))

fix.eff <- c(0, coef(mod.fe1)[11:22])
stage2.mod <- lm(fix.eff[dat[["id"]]] ~ fed + authleg, data=dat)
s2.resid <- residuals(stage2.mod)
mod.fevd <- lm(sstran ~ leftcab + cncrcab + flabfo +
    vturn + pctaged + cgdp + tunemp  + ofdi +
    openc + authleg + fed + res, data=dat.std)


###################################################
### chunk number 9: sumfevd
###################################################
summary(mod.fevd)


###################################################
### chunk number 10: hausman
###################################################
b <- mod.fe1$coef[2:10]
B <- mod.re$coef[2:10]
vb <- vcov(mod.fe1)[2:10,2:10]
vB <- vcov(mod.re)[2:10,2:10]

chi2 <- t(b-B) %*% solve(vb-vB) %*% (b-B)
1-pchisq(chi2, length(b))


