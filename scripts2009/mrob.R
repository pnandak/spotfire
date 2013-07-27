# Iran 2009 district vote counts

asc <- function(x) { as.character(x) }

asnc <- function(x) { as.numeric(as.character(x)) }

h <- c("province","town","Ahmadinejad","Rezaei","Karoubi","Mousavi","VG","VH","total");
dat <- read.csv(pipe("awk 'NR>3 {print $0;}' Iran2009.csv"), head=FALSE, col.names=h);
dim(dat);
names(dat);
#> dim(dat);
#[1] 366   9
#> names(dat);
#[1] "province"    "town"        "Ahmadinejad" "Rezaei"      "Karoubi"    
#[6] "Mousavi"     "VG"          "VH"          "total"      

getdigits <- function(num,digit) {
  s <- as.character(num);
  idx <- sapply(s, function(x){ nchar(x) >= digit; });
  dout <- ifelse(idx, NA, "");
  if (any(idx)) {
    dout[idx] <- sapply(s[idx], function(x){ substr(x,digit,digit) });
  }
  return(dout);
}

# Benford's Law probablity formulas
# source
# http://www.mathpages.com/home/kmath302/kmath302.htm
#
# PndB:  d, digit value (vector);  n, digit place;  B, base
# probability that the n-th digit following the first nonzero digit is d,
#   for numbers in base B
# d can be a vector;  n and B must be positive scalars
#
PndB <- function(d,n,B=10) {
  if (n==0) {
    p <- log(1+1/d)/log(B);
  }
  else {
    seq <- B^(n-1):(B^n-1);
    p <- d*0;
    for (i in 1:length(d)) {
      p[i] <- sum(log(1+1/(d[i]+seq*B)))/log(B);
    }
  }
  return(p);
}

runtest <- function(wrk, digit=2) {
  least <- ifelse(digit==1, 1, 0);  # initial digit for Benford test
  vec <- least:9;
  set <- rep(0,length(vec));
  names(set) <- as.character(vec);
  ndtable <- names(dtable <- table(getdigits(wrk, digit=digit)));
  set[ndtable[ndtable %in% vec]] <- dtable[ndtable[ndtable %in% vec]];
  sumset <- sum(set);
  # Benford's law logarithmic distribution test
  bpred <- sumset*PndB(vec, digit-1);
  chi <- sum(dev <- (set - bpred)^2 / bpred);
  dev <- sign(set - bpred) * sqrt(dev);
  # uniform distribution test
  upred <- rep(ifelse(digit==1,1/9,.1), ifelse(digit==1,9,10))*sumset;
  chiU <- sum((set - upred)^2 / upred);
#  if(is.na(chi)) print( table(getdigits(wrk, digit=digit)) );
  return(list(dtable=dtable, sumset=sumset, chiU=chiU, chi=chi, dev=dev));
}

# 2d digit Benford frequencies
benf2 <- PndB(0:9,1);
names(benf2) <- as.character(0:9);

# 2d digit Benford mean
print(benf2mean <- c(benf2 %*% 0:9));

vnames <- names(dat)[3:6];

digit <- matrix(NA, dim(dat)[1], length(vnames));
dimnames(digit)[[2]] <- vnames;

for (j in vnames) digit[,j] <- as.numeric(getdigits(dat[,j], digit=2));

dimnames(digit)[[2]] <- paste("d",vnames,sep="");
dat <- cbind(dat,digit);

for (j in paste("d",vnames,sep="")) print(summary(lm(I(digit[,j]-benf2mean) ~ 1)));

summary(lm(digit[,1] ~ I(Ahmadinejad/(Ahmadinejad+Mousavi)), dat));
summary(lm(digit[,4] ~ I(Ahmadinejad/(Ahmadinejad+Mousavi)), dat));

summary(lm(Ahmadinejad ~ Mousavi, dat));
summary(glm(Ahmadinejad ~ Mousavi, dat, family="poisson"));
summary(glm(Ahmadinejad ~ log(Mousavi), dat, family="poisson"));
summary(glm(Ahmadinejad ~ log(Mousavi), dat, family="quasipoisson"));

summary(glm(cbind(Ahmadinejad,Mousavi) ~ 1, dat, family="binomial"));
summary(glm(cbind(Ahmadinejad,Mousavi) ~ 1, dat, family="quasibinomial"));

summary(glm(cbind(Ahmadinejad,Mousavi) ~ digit[,1]+digit[,4], dat, family="quasibinomial"));

logistic <- function(x) { 1/(1+exp(-x)) }

library(multinomRob);

m0 <- multinomRob(list(Ahmadinejad ~ 1, Mousavi ~ 0), dat);
summary(m0);
logistic(m0$coefficients[1]);  # robust estimate for average of district vote proportions

# outliers from constant-vote-proportion model
cbind(
m0$weights[m0$weights[,2]==0,1],
(dat$Ahmadinejad/(dat$Ahmadinejad+dat$Mousavi)-logistic(m0$coefficients[1]))[m0$weights[,2]==0]);

# downweighted obs from constant-vote-proportion model
cbind(
m0$weights[m0$weights[,2]<1,1],
(dat$Ahmadinejad/(dat$Ahmadinejad+dat$Mousavi)-logistic(m0$coefficients[1]))[m0$weights[,2]==0]);

m1 <- multinomRob(list(Ahmadinejad ~ dAhmadinejad, Mousavi ~ 0), dat, start=c(.9,0));
summary(m1);

# outliers from constant-vote-proportion model
m1$weights[m1$weights[,2]==0,1];

# downweighted obs from constant-vote-proportion model
m1$weights[m1$weights[,2]<1,1];

m2 <- multinomRob(list(Ahmadinejad ~ dMousavi, Mousavi ~ 0), dat);
summary(m2);

# outliers from constant-vote-proportion model
m2$weights[m2$weights[,2]==0,1];

# downweighted obs from constant-vote-proportion model
m2$weights[m2$weights[,2]<1,1];

m3 <- multinomRob(list(Ahmadinejad ~ dAhmadinejad + dMousavi, Mousavi ~ 0), dat, start=c(.9,0,0));
summary(m3);

# outliers from constant-vote-proportion model
m3$weights[m3$weights[,2]==0,1];

# downweighted obs from constant-vote-proportion model
m3$weights[m3$weights[,2]<1,1];

