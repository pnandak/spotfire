# Iran 2005  and 2009 district vote counts

asc <- function(x) { as.character(x) }

asnc <- function(x) { as.numeric(as.character(x)) }

h05 <- c(
"Ahmadinejad.A.pct","Ahmadinejad.A","Rafsanjani.A.pct","Rafsanjani.A",
"unk.A","total.A","invalid.A","ballots.A","unknown.A",
"grand.total",
"Rafsanjani.B.pct","Rafsanjani.B","Ahmadinejad.B.pct","Ahmadinejad.B",
"unk.B","total.B","invalid.B","ballots.B","unknown.B",
"town","province");
dat05 <- read.delim(pipe("awk 'NR>2 {print $0;}' branch.htm.tab"), head=FALSE, col.names=h05);
dat05$total05 <-
  apply(as.matrix(dat05[,c("Ahmadinejad.A","Rafsanjani.A")]),1,sum);
dim(dat05);
names(dat05);
#> dim(dat05);
#[1] 325  22
#> names(dat05);
# [1] "Ahmadinejad.A.pct" "Ahmadinejad.A"     "Rafsanjani.A.pct" 
# [4] "Rafsanjani.A"      "unk.A"             "total.A"          
# [7] "invalid.A"         "ballots.A"         "unknown.A"        
#[10] "grand.total"       "Rafsanjani.B.pct"  "Rafsanjani.B"     
#[13] "Ahmadinejad.B.pct" "Ahmadinejad.B"     "unk.B"            
#[16] "total.B"           "invalid.B"         "ballots.B"        
#[19] "unknown.B"         "town"              "province"         
#[22] "total05"          

sapply(c(1:19,22),function(j){ sum(dat05[,j]) });

h09 <- c("province","town","Ahmadinejad","Rezaei","Karoubi","Mousavi","ballots","invalid","total");
dat09 <- read.csv(pipe("awk 'NR>3 {print $0;}' Iran2009.csv"), head=FALSE, col.names=h09);
dat09$total09 <-
  apply(as.matrix(dat09[,c("Ahmadinejad","Rezaei","Karoubi","Mousavi")]),1,sum);
dat09$obsnum <- (1:dim(dat09)[1]);
dim(dat09);
names(dat09);
#> dim(dat09);
#[1] 366   11
#> names(dat09);
#[1] "province"    "town"        "Ahmadinejad" "Rezaei"      "Karoubi"    
#[6] "Mousavi"     "ballots"     "invalid"     "total"       "total09"    
#[11] "obsnum"     

sapply(3:6,function(j){ sum(dat09[,j]) });

mtab <- read.delim("mismatch.txt", header=FALSE, col.names=c("obs09","obs05"));
dim(mtab);
names(mtab);
#> dim(mtab);
#[1] 371   2
#> names(mtab);
#[1] "obs09" "obs05"

dat <- data.frame(dat09[mtab[,1],c(11,1:2,3:6)],dat05[mtab[,2],c(1:19)]);
dim(dat);
names(dat);
#> dim(dat);
#[1] 371  28
#> names(dat);
# [1] "obsnum"            "province"          "town"             
# [4] "Ahmadinejad"       "Rezaei"            "Karoubi"          
# [7] "Mousavi"           "Ahmadinejad.A.pct" "Ahmadinejad.A"    
#[10] "Rafsanjani.A.pct"  "Rafsanjani.A"      "unk.A"            
#[13] "total.A"           "invalid.A"         "ballots.A"        
#[16] "unknown.A"         "grand.total"       "Rafsanjani.B.pct" 
#[19] "Rafsanjani.B"      "Ahmadinejad.B.pct" "Ahmadinejad.B"    
#[22] "unk.B"             "total.B"           "invalid.B"        
#[25] "ballots.B"         "unknown.B"        

idx <- apply(as.matrix(dat[,-(1:2)]),1,function(x){ !any(is.na(x)) });

datuse <- dat[idx,];
datuse$total09 <-
  apply(as.matrix(datuse[,c("Ahmadinejad","Rezaei","Karoubi","Mousavi")]),1,sum);
datuse$total05 <-
  apply(as.matrix(datuse[,c("Ahmadinejad.A","Rafsanjani.A")]),1,sum);
dim(datuse);
names(datuse);
#> dim(datuse);
#[1] 320  28
#> names(datuse);
# [1] "obsnum"            "province"          "town"             
# [4] "Ahmadinejad"       "Rezaei"            "Karoubi"          
# [7] "Mousavi"           "Ahmadinejad.A.pct" "Ahmadinejad.A"    
#[10] "Rafsanjani.A.pct"  "Rafsanjani.A"      "unk.A"            
#[13] "total.A"           "invalid.A"         "ballots.A"        
#[16] "unknown.A"         "grand.total"       "Rafsanjani.B.pct" 
#[19] "Rafsanjani.B"      "Ahmadinejad.B.pct" "Ahmadinejad.B"    
#[22] "unk.B"             "total.B"           "invalid.B"        
#[25] "ballots.B"         "unknown.B"         "total09"          
#[28] "total05"          

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


#plot(datuse$Ahmadinejad.A,datuse$Ahmadinejad);
#plot(datuse$Ahmadinejad.A/datuse$total05,datuse$Ahmadinejad/datuse$total09);

vnames <- names(datuse)[c(3:6,8,10)];

digit <- matrix(NA, dim(datuse)[1], length(vnames));
dimnames(digit)[[2]] <- vnames;

for (j in vnames) digit[,j] <- as.numeric(getdigits(datuse[,j], digit=2));

dimnames(digit)[[2]] <- paste("d",vnames,sep="");
datuse <- cbind(datuse,digit);

logistic <- function(x) { 1/(1+exp(-x)) }
logit <- function(x) { log(x/(1-x)) }

library(multinomRob);

m05 <- multinomRob(list(Ahmadinejad.A ~ 1, Rafsanjani.A ~ 0), dat05);
summary(m05);
logistic(m05$coefficients[1]);  # robust estimate for average of district vote proportions

# outliers from constant-vote-proportion model
cbind(
m05$weights[m05$weights[,2]==0,1],
(dat05$Ahmadinejad.A/(dat05$total05)-logistic(m05$coefficients[1]))[m05$weights[,2]==0]);

# downweighted obs from constant-vote-proportion model
cbind(
m05$weights[m05$weights[,2]<1,1],
(dat05$Ahmadinejad.A/(dat05$total05)-logistic(m05$coefficients[1]))[m05$weights[,2]<1]);

m05a <- multinomRob(list(Ahmadinejad.A ~ 1, Rafsanjani.A ~ 0), datuse);
summary(m05a);
logistic(m05a$coefficients[1]);  # robust estimate for average of district vote proportions

# outliers from constant-vote-proportion model
cbind(
datuse$obsnum[m05a$weights[,2]==0],
m05a$weights[m05a$weights[,2]==0,1],
(datuse$Ahmadinejad.A/(datuse$total05)-logistic(m05a$coefficients[1]))[m05a$weights[,2]==0]);

# downweighted obs from constant-vote-proportion model
cbind(
datuse$obsnum[m05a$weights[,2]<1],
m05a$weights[m05a$weights[,2]<1,1],
(datuse$Ahmadinejad.A/(datuse$total05)-logistic(m05a$coefficients[1]))[m05a$weights[,2]<1]);

# cross-year models

# 2005 A vote

datuse$A05logit <- logit(datuse$Ahmadinejad.A/datuse$total05);
m0509 <- multinomRob(list(Ahmadinejad ~ A05logit, Mousavi ~ 0), datuse);
summary(m0509);

# outliers from constant-vote-proportion model
linpred <- exp(cbind(1,datuse$A05logit) %*% m0509$coefficients);
pmat <- linpred/apply(linpred,1,sum);
rmat <-
  as.matrix(datuse[,c("Ahmadinejad","Mousavi")])/datuse$total09-pmat;
widx <- apply(m0509$weights[,-1,drop=FALSE]==0,1,any);
usermat <- rmat;
usermat[m0509$weights[,-1,drop=FALSE]!=0] <- NA;
cbind(datuse$obsnum[widx], usermat[widx,]);

# downweighted obs from constant-vote-proportion model
widx <- apply(m0509$weights[,-1,drop=FALSE]<1,1,any);
usermat <- rmat;
usermat[m0509$weights[,-1,drop=FALSE]==1] <- NA;
cbind(datuse$obsnum[widx], usermat[widx,]);

# 2009/2005 total ratio

datuse$ratio0905 <- datuse$total09/datuse$total05;
m0509r <- multinomRob(list(Ahmadinejad ~ ratio0905, Mousavi ~ 0), datuse);
summary(m0509r);

# outliers from constant-vote-proportion model
linpred <- exp(cbind(1,datuse$ratio0905) %*% m0509r$coefficients);
pmat <- linpred/apply(linpred,1,sum);
rmat <-
  as.matrix(datuse[,c("Ahmadinejad","Mousavi")])/datuse$total09-pmat;
widx <- apply(m0509r$weights[,-1,drop=FALSE]==0,1,any);
usermat <- rmat;
usermat[m0509r$weights[,-1,drop=FALSE]!=0] <- NA;
cbind(datuse$obsnum[widx], usermat[widx,]);

# downweighted obs from constant-vote-proportion model
widx <- apply(m0509r$weights[,-1,drop=FALSE]<1,1,any);
usermat <- rmat;
usermat[m0509r$weights[,-1,drop=FALSE]==1] <- NA;
cbind(datuse$obsnum[widx], usermat[widx,]);

# 2005 A vote and 2009/2005 total ratio

m0509ar <- multinomRob(list(Ahmadinejad ~ A05logit + ratio0905, Mousavi ~ 0), datuse);
summary(m0509ar);

# outliers from constant-vote-proportion model
linpred <- exp(cbind(1,datuse$A05logit,datuse$ratio0905) %*% m0509ar$coefficients);
pmat <- linpred/apply(linpred,1,sum);
rmat <-
  as.matrix(datuse[,c("Ahmadinejad","Mousavi")])/datuse$total09-pmat;
widx <- apply(m0509ar$weights[,-1,drop=FALSE]==0,1,any);
usermat <- rmat;
usermat[m0509ar$weights[,-1,drop=FALSE]!=0] <- NA;
cbind(datuse$obsnum[widx], usermat[widx,]);

# downweighted obs from constant-vote-proportion model
widx <- apply(m0509ar$weights[,-1,drop=FALSE]<1,1,any);
usermat <- rmat;
usermat[m0509ar$weights[,-1,drop=FALSE]==1] <- NA;
cbind(datuse$obsnum[widx], usermat[widx,]);
