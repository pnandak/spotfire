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

vnames <- names(dat)[3:6];

mat <- matrix(NA, length(vnames), 2);
dimnames(mat) <- list(vnames, c("sumset","chiB"));

for (i in 1:length(vnames)) {
  tt <- runtest(dat[,vnames[i]]);
  mat[vnames[i],] <- c(tt$sumset, tt$chi);
}
mat;
