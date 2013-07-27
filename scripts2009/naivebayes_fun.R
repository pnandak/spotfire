# DATA MINING AND INFORMATION SYSTEMS
# CLASSIFICATION METHODS - NAIVE BAYES FUNCTION, MARCH, 25TH 2009 
############################################################################
############################################################################
# NAIVE BAYES CLASSIFIER
############################################################################
# df data frame dei dati campionari (prima colonna variabile dipendente)
# n_var numero variabili (si suppongono qualitative o discrete;
# se continue devono essere preventivamente discretizzate)
# n_cat numero modalità delle variabili
# n numerosità campionaria
############################################################################
# Function Naive Bayes classifier naive_bayes
############################################################################
# Start
naive_bayes <- function(df) {
 n <- dim(df)[1]                              #numerosità campionaria
 n_var <- dim(df)[2]-1                        #numero variabili esplicative
 marg <- lapply(df[,-1],function(x) table(x)/n) #distr. marginali var. esplicative (tutte le classi)
 n_cat <- lapply(marg,function(x) length(x))     #vettori del numero delle modalit
 prior <- table(df[,1])                       #frequenze assolute delle classi
 n_classi <- length(prior)                    #numero gruppi 
 data_class <- split(df[,-1], as.factor(df[,1]))         #data sets parziali dei gruppi
 nlev <- c()
 for (i in 1:n_var) {
  nlev[i] <- nlevels(as.factor(df[,i+1]))
 }
 likelihood <- lapply(data_class, function(x) lapply(x, function(x) table(x)))
 for (i in 1:n_classi) {
  for (j in 1:n_var) {
   likelihood[[i]][[j]] <- tabulate(data_class[[i]][,j],nbins=nlev[j])/prior[i]
  }
 }
 posterior <- matrix(0,nrow=prod(unlist(n_cat)),ncol=n_classi)
 for (i in 1:length(likelihood)) {
  posterior[,i] <- apply(expand.grid(likelihood[[i]]),1,prod)*prior[i]
 }
 posterior <- posterior/rowSums(posterior)
 colnames(posterior) <- names(prior)
 nomi <- n_cat
 for (i in 1:n_var) nomi[[i]] <- 1:n_cat[[i]]
 celle <- apply(expand.grid(nomi),1, function(x) paste(x, sep=' ', collapse=','))
 rownames(posterior) <- celle
 class <- max.col(posterior)
 class1 <- rep(" ",length(class))
 for (i in 1:length(class)) {
  ind <- class[i]
  class1[i] <- names(prior)[ind]
 }
 class1 <- data.frame(celle,class1)              #classe con massima probabilità        
 prior <- prior/n
 nbayes <- list(n_classi,n,n_var,prior,posterior,likelihood,class1)
 names(nbayes) <- c("Number of Classes (Groups)","Sample Size",
  "Number of Observed Variables","Estimated Prior Probability of Classes",
  "Estimated Posterior Probability of Classes","Likelihoods","Winning Group")
 return(nbayes)
}
#End
##############################################################################
# Calling function
nbayes <- naive_bayes(df)
##############################################################################
#EXAMPLE 1
# RIDING MOWERS DATA (COURTESY SHMUELI, PATEL, BRUCE)
############################################################################
# UNITS 25 GARDEN OWNERS
# VARIABLES 3
# V1 INCOME *NUMERICAL
# V2 LOT SIZE *NUMERICAL
# V3 RIDING MOWER OWNERSHIP *CATEGORICAL
############################################################################
# INPUT DATA TABLE
rmow <- read.csv2("http://venus.unive.it/romanaz/datamin/dati/RidingMowers.csv",
 header=TRUE)
rmow <- read.csv2("D:/corso_datam&sinfo/esercitazioni/ridingmower/RidingMowers.csv",
 header=TRUE)
str(rmow)
############################################################################
# DATA PREPROCESSING 
############################################################################
income_cl <- cut(Income,breaks=quantile(Income,seq(0,1,0.25)),
 include.lowest=TRUE,labels=FALSE)
lotsize_cl <- cut(Lot_Size,breaks=quantile(Lot_Size,seq(0,1,0.25)),
 include.lowest=TRUE,labels=FALSE)
df <- data.frame(Ownership,income_cl,lotsize_cl)
names(df) <- c("classe","redd_cl","sup_cl")
############################################################################
#EXAMPLE 2
#PHONE FIRM CUSTOMERS
V1 <- rep(c(1,2,3),c(10,5,8))
V2 <- c("M","M","I","F","I","F","M","F","I","M","I","M","F","I","F","M","I",
        "F","M","F","F","I","I")
V3 <- as.factor(c(18,25,18,45,25,35,25,18,25,18,25,35,18,45,35,45,35,45,45,18,25,35,45))
V4 <- c("DB","DB","BP","DB","DB","CC","DB","DB","BP","DB","BP","DB","DB","CC",
        "CC","BP","DB","BP","BP","CC","BP","DB","BP")
V5 <- c(2,3,3,2,4,4,4,3,2,1,1,3,2,4,3,1,2,1,1,3,2,2,4)   
df <- data.frame(V1,V2,V3,V4,V5)
names <- c("classe","tipo","età","pag","q_tel")

