# ANES 2000 (subset) with auxiliary FEC and election returns data

asc <- function(x) { as.character(x) }

# ANES 2000 subset merged with FEC and election returns data
dat <- read.csv("assign1d.csv", row.names=1);
dim(dat);
names(dat);
#> dim(dat);
#[1] 1549   41
#> names(dat);
# [1] "stcd"      "weight"    "stfips"    "age"       "deminc"    "repinc"   
# [7] "demchal"   "repchal"   "othinc"    "othchal"   "openseat"  "demopen"  
#[13] "repopen"   "othopen"   "unopposed" "pid"       "libcon"    "demlibcon"
#[19] "demdist"   "replibcon" "repdist"   "demdist0"  "repdist0"  "residence"
#[25] "vote"      "ALL"       "DEM"       "REP"       "THIRD"     "Total"    
#[31] "DEM.C"     "DEM.I"     "DEM.O"     "REP.C"     "REP.I"     "REP.O"    
#[37] "state"     "votebal"   "voteObal"  "fecbal"    "fecObal"  

sum(dat$Total>0);
dat <- dat[dat$Total>0,]; # delete obs with unreported votes for unopposed cands
dat$Total[dat$Total<1000] <- dat$ALL[dat$Total<1000];

dat$age <- (dat$age - median(dat$age, na.rm=TRUE))/10;
dat$drdist <- dat$demdist - dat$repdist;

dat$vhouse <- ifelse(dat$vote==2 | dat$vote==3, 1, 0);
dat$voteDR <- ifelse(dat$vote==2, 1, ifelse(dat$vote==3, 0, NA));

dat$unopp <- ifelse(dat$DEM==0 | dat$REP==0, 1, 0);
sum(dat$unopp);

kidx <- !is.na(dat$vhouse + dat$age + dat$residence + dat$pid + dat$unopposed);

udat <- dat[kidx,];

library(mvtnorm);

# bivariate standard normal density
stN2pdf <- function(XY, mu=rep(0,2), rho=0) {
  sigma <- matrix(c(1,rho,rho,1),2,2);
  dmvnorm(XY, c(0,0), sigma);
}

# trivariate standard normal density
stN3pdf <- function(XYZ, mu=rep(0,3), sigma=diag(rep(1,3))) {
  dmvnorm(XY, c(0,0), sigma);
}

# bivariate standard normal cdf
stN2cdf <- function(XYlo, XYhi, mu=rep(0,2), rho=0) {
  sigma <- matrix(c(1,rho,rho,1),2,2);
  sapply(1:(dim(XYhi)[1]),
    function(i){ pmvnorm(lower=XYlo[i,], upper=XYhi[i,], mean=c(0,0), sigma=sigma) });
}

# stN2cdf(rep(-Inf,2),c(0,0));

# trivariate standard normal cdf
stN3cdf <- function(XYZlo, XYZhi, mu=rep(0,3), sigma=diag(rep(1,3))) {
  sapply(1:(dim(XYZhi)[1]),
    function(i){ pmvnorm(lower=XYZlo[i,], upper=XYZhi[i,], mean=c(0,0,0), sigma=sigma) });
}

# stN3cdf(rep(-Inf,3),c(0,0,0));

dat$unopposed==1 & dat$vote>=2

Y <- ifelse(cbind(udat$vhouse==0, udat$unopposed==1 & udat$vote>=2,
  udat$unopposed==0 & udat$voteDR==1, udat$unopposed==0 & udat$voteDR==0),1,0);
Y[is.na(Y[,3]),3] <- 0;
Y[is.na(Y[,4]),4] <- 0;

udat$drdist[is.na(udat$drdist)] <- 0;

udat$votebal2 <- udat$votebal^2;
udat$pid0 <- ifelse(udat$pid==0,1,0);
udat$pid1 <- ifelse(udat$pid==1,1,0);
udat$pid2 <- ifelse(udat$pid==2,1,0);
udat$pid3 <- ifelse(udat$pid==3,1,0);
udat$pid4 <- ifelse(udat$pid==4,1,0);
udat$pid5 <- ifelse(udat$pid==5,1,0);
udat$pid6 <- ifelse(udat$pid==6,1,0);
udat$age2 <- udat[,"age"]^2;
udat$Total10K <- udat$Total/10000;

summary(fecuglm <- glm(unopp ~ votebal + I(votebal^2) + fecbal, udat,
  family=binomial(link="probit")));

udat$millsuglm <- dnorm(predict(fecuglm))/(1-pnorm(predict(fecuglm)));

summary(vhouseglm <- glm(vhouse ~
  age + I(age^2) + residence + factor(pid) + unopposed, udat,
  family=binomial(link="probit")));

udat$millsvhouse <- dnorm(predict(vhouseglm))/(1-pnorm(predict(vhouseglm)));

summary(vhouseglm2 <- glm(vhouse ~
  age + I(age^2) + residence + factor(pid) + unopposed + millsuglm, udat,
  family=binomial(link="probit")));

summary(vDRglm <- glm(voteDR ~ factor(pid) + deminc + repinc + drdist + millsuglm + millsvhouse,
  udat, family=binomial(link="probit")));

summary(vDRglm2 <- glm(voteDR ~ factor(pid) + deminc + repinc + drdist + millsvhouse,
  udat, family=binomial(link="probit")));

logit <- function(x) { log(x/(1-x)) }

print(b20 <- coef(vhouseglm));
print(b30 <- coef(vDRglm2)[-length(coef(vDRglm2))]);
b0.2 <- c(b20,b30, logit((coef(vDRglm2)[length(coef(vDRglm2))]+1)/2) );
print(b10 <- coef(fecuglm));
print(b20 <- coef(vhouseglm2)[-length(coef(vhouseglm2))]);
print(b30 <- coef(vDRglm)[-(length(coef(vDRglm))-c(1,0))]);
b0.3 <- c(b10,b20,b30, 
  logit((coef(vhouseglm2)[length(coef(vhouseglm2))]+1)/2),
  logit((coef(vDRglm)[(length(coef(vDRglm))-c(1,0))]+1)/2) );

X1 <- cbind(1,as.matrix(udat[,c("votebal","votebal2","fecbal")]));

X2 <- cbind(1,as.matrix(udat[,c("age","age2","residence",
  "pid1","pid2","pid3","pid4","pid5","pid6","unopposed")]));

X3 <- cbind(1,as.matrix(udat[,c("pid1","pid2","pid3","pid4","pid5","pid6",
  "deminc","repinc","drdist")]));

stepfunc2 <- function(b, YY=Y, XX2=X2, XX3=X3) {
  b2 <- b[1:(dim(XX2)[2])];
  b3 <- b[((dim(XX2)[2])+(1:(dim(XX3)[2])))];
  rho <- 2*(1/(1+exp(-b[length(b)])))-1;
  Z2 <- XX2 %*% b2;
  Z3 <- XX3 %*% b3;
  g0 <- pnorm(-Z2);
  g1 <- 1-pnorm(-Z2);
  h0 <- stN2cdf(cbind(-Z2,-Inf),cbind(Inf,-Z3), rho=rho);
  h1 <- stN2cdf(cbind(-Z2,-Z3),matrix(Inf,length(Z2),2), rho=rho);
  -sum(log(ifelse(YY[,1]==1,g0, ifelse(YY[,2]==1,g1,
    ifelse(YY[,3]==1,h1,ifelse(YY[,4]==1,h0,NA))))));
}

model2 <- genoud(stepfunc2, length(b0.2), starting.values=b0.2,
  Domains=cbind(ifelse(b0.2>0,0,b0.2-2),ifelse(b0.2<0,0,b0.2+2)),
  hessian=TRUE, YY=Y, XX2=X2, XX3=X3);

save(model2, file=".Rdata.assign1dG");

W <- ifelse(cbind(udat$unopposed==1 & udat$vhouse==0, udat$unopposed==0 & udat$vhouse==0,
  udat$unopposed==1 & udat$vote>=2,
  udat$unopposed==0 & udat$voteDR==1, udat$unopposed==0 & udat$voteDR==0),1,0);
W[is.na(W[,4]),4] <- 0;
W[is.na(W[,5]),5] <- 0;

stepfunc3 <- function(b, YY=Y, XX1=X1, XX2=X2, XX3=X3) {
  b1 <- b[1:(dim(XX1)[2])];
  b2 <- b[(dim(XX1)[2])+(1:(dim(XX2)[2]))];
  b3 <- b[(dim(XX1)[2])+(dim(XX2)[2])+(1:(dim(XX3)[2]))];
  r12 <- 2*(1/(1+exp(-b[length(b)-2])))-1;
  r13 <- 2*(1/(1+exp(-b[length(b)-1])))-1;
  r23 <- 2*(1/(1+exp(-b[length(b)])))-1;
  sigma <- matrix(c(1,r12,r13,r12,1,r23,r13,r23,1),3,3);
  Z1 <- XX1 %*% b1;
  Z2 <- XX2 %*% b2;
  Z3 <- XX3 %*% b3;
  Infmat <- array(-Inf,dim(cbind(Z1,Z2)));
  g00 <- stN2cdf(Infmat,cbind(-Z1,-Z2), rho=r12);
  g10 <- stN2cdf(cbind(-Z1,-Inf),cbind(Inf,-Z2), rho=r12);
  g01 <- stN2cdf(cbind(-Inf,-Z2),cbind(-Z1,Inf), rho=r12);
  h10 <- stN3cdf(cbind(-Z1,-Z2,-Inf),cbind(Inf,Inf,-Z3), sigma=sigma);
  Infmat <- array(Inf,dim(cbind(Z1,Z2,Z3)));
  h11 <- stN3cdf(cbind(-Z1,-Z2,-Z3),Infmat, sigma=sigma);
  -sum(log(ifelse(YY[,1]==1,g00,ifelse(YY[,2]==1,g10,ifelse(YY[,3]==1,g01,
    ifelse(YY[,4]==1,h11,h10))))));
}

model3 <- genoud(stepfunc3, length(b0.3), starting.values=b0.3,
  Domains=cbind(ifelse(b0.3>0,0,b0.3-2),ifelse(b0.3<0,0,b0.3+2)),
  hessian=TRUE, YY=W, XX1=X1, XX2=X2, XX3=X3, T1=T1);

save(model2, model3, file=".Rdata.assign1dG");
