# Iran 2005 district vote counts

asc <- function(x) { as.character(x) }

asnc <- function(x) { as.numeric(as.character(x)) }

h <- c(
"Ahmadinejad.A.pct","Ahmadinejad.A","Rafsanjani.A.pct","Rafsanjani.A",
"unk.A","total.A","invalid.A","ballots.A","unknown.A",
"grand.total",
"Rafsanjani.B.pct","Rafsanjani.B","Ahmadinejad.B.pct","Ahmadinejad.B",
"unk.B","total.B","invalid.B","ballots.B","unknown.B",
"town","province");
dat <- read.delim(pipe("awk 'NR>2 {print $0;}' branch.htm.tab"), head=FALSE, col.names=h);
dim(dat);
names(dat);
#> dim(dat);
#[1] 325  21
#> names(dat);
# [1] "Ahmadinejad.A.pct" "Ahmadinejad.A"     "Rafsanjani.A.pct" 
# [4] "Rafsanjani.A"      "unk.A"             "total.A"          
# [7] "invalid.A"         "ballots.A"         "unknown.A"        
#[10] "grand.total"       "Rafsanjani.B.pct"  "Rafsanjani.B"     
#[13] "Ahmadinejad.B.pct" "Ahmadinejad.B"     "unk.B"            
#[16] "total.B"           "invalid.B"         "ballots.B"        
#[19] "unknown.B"         "town"              "province"         

dat[,"Ahmadinejad"] <- dat[,"Ahmadinejad.A"] + dat[,"Ahmadinejad.B"];
dat[,"Rafsanjani"] <- dat[,"Rafsanjani.A"] + dat[,"Rafsanjani.B"];

sapply(c(1:19,22:23),function(j){ sum(dat[,j]) });

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

vnames <- names(dat)[c(2,4,12,14,22:23)];

mat <- matrix(NA, length(vnames), 2);
dimnames(mat) <- list(vnames, c("sumset","chiB"));

for (i in 1:length(vnames)) {
  tt <- runtest(dat[,vnames[i]]);
  mat[vnames[i],] <- c(tt$sumset, tt$chi);
}
mat;
