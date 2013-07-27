# ANES 2000 (subset) with auxiliary FEC and election returns data

asc <- function(x) { as.character(x) }

# ANES 2000 subset
dat <- read.csv("dat2000c.csv");
dim(dat);
names(dat);
#> dim(dat);
#[1] 1555   25
#> names(dat);
# [1] "weight"    "stfips"    "stcd"      "age"       "deminc"    "repinc"   
# [7] "demchal"   "repchal"   "othinc"    "othchal"   "openseat"  "demopen"  
#[13] "repopen"   "othopen"   "unopposed" "pid"       "libcon"    "demlibcon"
#[19] "demdist"   "replibcon" "repdist"   "demdist0"  "repdist0"  "residence"
#[25] "vote"     
sort(unique(asc(dat$stcd)));

# FEC 1999-2000 contributions by party (D and R) and candidate type
fec <- read.csv("fec2000.csv");
dim(fec);
names(fec);
#> dim(fec);
#[1] 440   7
#> names(fec);
#[1] "DEM.C" "DEM.I" "DEM.O" "REP.C" "REP.I" "REP.O" "stcd" 
sort(unique(asc(fec$stcd)));

# 2000 election returns from
# http://clerk.house.gov/member_info/electionInfo/index.html
returns <- read.csv("2000election.csv");
dim(returns);
names(returns);
#> dim(returns);
#[1] 563  60
#> names(returns);
# [1] "stcd"                          "Alaskan.Independence"         
# [3] "American.Constitution"         "American.Independent"         
# [5] "Blank.Scattering"              "By.Petition"                  
# [7] "Citzens.First"                 "Concerned.Citizens"           
# [9] "Concerns.of.People"            "Conscience.for.Congress"      
#[11] "Conservative"                  "Constitution"                 
#[13] "Constitutional"                "Democrat"                     
#[15] "Democratic"                    "Democratic.Farmer.Labor"      
#[17] "Democratic.Independent"        "Democratic.Nonpartisan.League"
#[19] "Democratic..Liberty.Union"     "Earth.Federation"             
#[21] "Freedom"                       "Grassroots"                   
#[23] "Green"                         "Independence"                 
#[25] "Independent"                   "Independent.American"         
#[27] "Liberal"                       "Libertarian"                  
#[29] "Liberty.Union"                 "Natural.Law"                  
#[31] "No.Vote.Cast"                  "Non.Partisan"                 
#[33] "None.Of.These.Candidates"      "Not.Designated"               
#[35] "Not.Identified"                "Other"                        
#[37] "Other.Candidates"              "Over.Vote"                    
#[39] "Pacific.Green"                 "Progressive.Green"            
#[41] "Prohibition"                   "Reform"                       
#[43] "Reform.Party.Minnesota"        "Republican"                   
#[45] "Right.to.Life"                 "Scattering"                   
#[47] "School.Choice"                 "Socialist"                    
#[49] "Socialist.Worker"              "Socialist.Workers"            
#[51] "Timesizing.Not.Downsizing"     "Total"                        
#[53] "U.S..Taxpayers"                "Unaffilitated"                
#[55] "Unenrolled"                    "United.Citizens"              
#[57] "Vermont.Grassroots"            "Workers.World"                
#[59] "Working.Families"              "WriteIn"                      

rdropidx <-
  regexpr("Pres",asc(returns$stcd))>0 |
  regexpr("Senator",asc(returns$stcd))>0 |
  regexpr("Total",asc(returns$stcd))>0;
unique(returns$stcd[rdropidx]);
returns <- returns[!rdropidx,];
returns$stcd <- factor(asc(returns$stcd));
sort(unique(returns$stcd));

rsub <- returns[,-c(1,5,31,33,34,35,37,52)];
returns$ALL <- apply(rsub, 1, sum, na.rm=TRUE);
returns$DEM <- apply(
  cbind(returns$Democrat,returns$Democratic,returns$Democratic.Farmer.Labor),1,
  sum, na.rm=TRUE);
returns$REP <- returns$Republican;
returns$THIRD <- returns$ALL - (returns$DEM + returns$REP);

retsub <- returns[,c("stcd","ALL","DEM","REP","THIRD","Total")];

retsub$stcd <- asc(retsub$stcd);
fec$stcd <- asc(fec$stcd);
dat$stcd <- asc(dat$stcd);

fecret <- merge(retsub, fec, by="stcd");
dim(fecret);
names(fecret);
#> dim(fecret);
#[1] 435  12
#> names(fecret);
# [1] "stcd"  "ALL"   "DEM"   "REP"   "THIRD" "Total" "DEM.C" "DEM.I" "DEM.O"
#[10] "REP.C" "REP.I" "REP.O"

fecret$state <- substr(asc(fecret$stcd),1,2);
fecret$votebal <- .5;
fecret$voteObal <- .5;
fecret$fecbal <- .5;
fecret$fecObal <- .5;

# compute proportion Dem in other districts in the same state (.5 if none)
for (i in 1:(dim(fecret)[1])) {
  istate <- fecret$state[i];
  iidx <- istate == fecret$state;
  iidx[i] <- FALSE;
  if (sum(iidx)>0) {
    ifr <- fecret[iidx,];
    fecret$votebal[i] <- sum(ifr$DEM + .5)/sum(ifr$DEM + ifr$REP + 1);
    fecret$fecObal[i] <- sum(ifr$DEM.O + .5)/sum(ifr$DEM.O + ifr$REP.O + 1);
    fecret$fecbal[i] <- sum(ifr$DEM.I + ifr$DEM.C + .5)/
      sum(ifr$DEM.I + ifr$DEM.C + ifr$REP.I + ifr$REP.C + 1);
    if (any(iOidx <- ifr$DEM.O>0 | ifr$REP.O>0)) {
      fecret$voteObal[i] <- sum(ifr$DEM[iOidx] + .5)/
        sum(ifr$DEM[iOidx] + ifr$REP[iOidx] + 1);
    }
  }
}

datall <- merge(dat, fecret, by="stcd");
dim(datall);
names(datall);
#> dim(datall);
#[1] 1549   41
#> names(datall);
# [1] "stcd"      "weight"    "stfips"    "age"       "deminc"    "repinc"   
# [7] "demchal"   "repchal"   "othinc"    "othchal"   "openseat"  "demopen"  
#[13] "repopen"   "othopen"   "unopposed" "pid"       "libcon"    "demlibcon"
#[19] "demdist"   "replibcon" "repdist"   "demdist0"  "repdist0"  "residence"
#[25] "vote"      "ALL"       "DEM"       "REP"       "THIRD"     "Total"    
#[31] "DEM.C"     "DEM.I"     "DEM.O"     "REP.C"     "REP.I"     "REP.O"    
#[37] "state"     "votebal"   "voteObal"  "fecbal"    "fecObal"  

write.csv(datall, file="assign1d.csv");
