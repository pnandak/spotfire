#=======================================================#
# created: 07.19.06                                     #
# R class #3                                            #
# OTHER ESSENTIAL BITS AND PIECES (AND REVIEW)          #
#=======================================================#


# SETUP
setwd("C:\\temp")
getwd()

#load("class3.Rdata")


save.image("class3.Rdata")
q()
#--------------------------------------------------------#




#1  READING EXTERNAL DATA

dat <- read.table("C:\\temp\\SNPgenotype.csv", sep=",",header=TRUE)
dim(dat)
names(dat)
edit(dat)

names(dat)                                    #try:  length(names(dat))
names(dat)[1]                                 #access an element in the vector
names(dat)[1] <- "y"                          #rename first variable in data.frame
dat$new <- 1:dim(dat)[1]                      #add a new variable
dat$new <- NULL                               #remove a variable



# write.table() example
#write.table(output.file, file= "C:\\temp\\temp.csv", sep=",", row.names=FALSE)




#2  LISTS VERSUS DATA.FRAMES

is.data.frame(dat)
is.list(dat)
mode(dat)

## lists are a collection arbitrary objects (can be of different types) under a single name
## data.frames have the structure of a type of list.  requirement that all list elements are
## of the same length.

alist <- list(one= 1:100, two= c("a","b",LETTERS[18]), three= pi)
mode(alist)


#subscripting (indexing) data.frames and lists

dat[3]                                         #try:  mode(dat[3])
dat["group"]                                   #try:  mode(dat["group"])
dat[[3]]                                       #try:  mode(dat[[3]])
dat[,"group"]                                  #try:  mode(dat[,"group"])
dat$group                                      #try:  mode(dat$group)

#CAUTION: the default modes for extracted single rows and columns are different
mode(dat[,1])
mode(dat[1,])





#3  DATA.FRAMES VERSUS MATRICES

#all elements of a matrix have the same mode (numeric or character).  they allow for
#  efficient mathematical operations and manipulation not available for data.frames.

dat.m <- as.matrix(dat)
mode(dat.m[1,])


REVIEW:  so, in practice, when do i use lists, data.frames and matrices?
LIST:         useful for object oriented programming. usually the default storage from functions.
MATRIX:       when all data is of the same mode. when matrix operations are necessary.
DATA.FRAME:   combines the best properties of lists and matrices. data storage and quick access to variables.
             data analysis.





#4.  DATA ORGANIZATION

dim(dat)
dat2 <- dat[dat$group==1,]                     #always a logical evaluation
dim(dat2)

is.na(dat2$snp1)
table(is.na(dat2$snp1))
dat3 <- dat2[!is.na(dat2$snp1),]               #remove records with missing genotypes
dim(dat3)

# NOTE:  any operation or relation that involves 'NA' generates an 'NA'.
1 + 3
1 + NA
mean(c(1,2,3,4,5))
mean(c(1,2,3,NA,5))
mean(c(1,2,3,NA,5), na.rm=TRUE)




#5.  USEFUL FUNCTIONS

#A.  attach():  object in current database is added to search path
attach(dat3)
search()
detach(dat3)
search()



#B.  paste():  one type of character manipulation
paste("R", "is", "fun")
paste("R", "is", "fun", sep="")
paste("var",1:10, sep="")
paste(getwd(),"/output.csv", sep="")



#C.  "advanced paste":  directly evaluating paste output

D <- "snp1"
eval(parse(text= paste("table(dat3$",D,")",sep="")))



#D.  merge():  combining two data.frames

dataA <- data.frame(subject= 1:10, A= round(rnorm(10),2))
dataB <- data.frame(subject= 6:10, B= letters[1:5])

# Intersection.
merge(dataA, dataB, by="subject")

# Union.
merge(dataA, dataB, by="subject", all.x=T, all.y=T)



#E.  for loops

for (i in 1:5) {
 print(i)
}

#incrementing over a character string
C <- paste("var",1:5, sep="")
for (i in C) {
 print(i)
}





#6.  BASIC DATA ANALYSIS

table(dat3$snp1)


lm(effect ~ snp1, data=dat3)                          #simple linear regression
result <- lm(effect ~ snp1, data=dat3)
names(result)
mode(result)
result$coef                                           #NOTE: can be an unambiguous abbreviation
round(result$coef[2],2)
summary(result)

#include the dominance effect of marker for fun
dat3$dsnp1 <- ifelse(dat3$snp1==1,1,0)
table(dat3$dsnp1)
summary(lm(effect ~ snp1 + dsnp1, data=dat3))         #or can use factor() instead of dummy variable


#-----------------------------------------------------------------------------#
#Let's investigate this relationship a little more for fun
A1A1 <- mean(dat3[dat3$snp1==0,"y"])
A1A2 <- mean(dat3[dat3$snp1==1,"y"])
A2A2 <- mean(dat3[dat3$snp1==2,"y"])
ng <- table(dat3$snp1)
q <- (2*ng[3] + ng[2]) / (2*(sum(ng)))
p <- 1 - q


#breeding values
a <- A1A1 - mean(c(A1A1,A2A2))
d <- A1A2 - mean(c(A1A1,A2A2))
alpha <- a + d*(q-p)                  #average effect of gene substitution
bA1A1 <- 2*q*alpha
bA1A2 <- (q-p)*alpha
bA2A2 <- -2*p*alpha


M <- A1A1*p^2 + A1A2*2*p*q + A2A2*q^2   #pop mean
gA1A1 <- A1A1 - M
gA1A2 <- A1A2 - M
gA2A2 <- A2A2 - M

x <- data.frame(genotype= c(0,1,2),
               breeding= c(bA1A1,bA1A2,bA2A2),
               genotypic= c(gA1A1,gA1A2,gA2A2))
x

yaxis <- c(min(x$breeding,x$genotypic)-1, max(x$breeding,x$genotypic)+1)

plot(x$genotype, x$genotypic, pch=16, ylim=yaxis, ylab="Deviation from pop mean")
abline(lm(x$genotypic ~ x$genotype, weights=c(p^2,2*p*q,q^2)))
par(new=T)
plot(x$genotype, x$breeding, ylim=yaxis, yaxt="n", ylab="")

#end pgm
#-----------------------------------------------------------------------------#





#7.  PERMUTATION TEST
permute <- 1000                            #define how many times to permute
outputF <- numeric(permute)

for (i in 1:length(output)) {
 a <- summary(lm(sample(dat3$y) ~ dat3$snp1))
 outputF[i] <- a$fstatistic[1]
}

hist(outputF)
summary(outputF)
sort(outputF)[.99 * permute]





#8.  HOW TO DOWNLOAD AND INSTALL EXTERNAL PACKAGES
#  Easiest way:  use the the GUI menu.
#  Once installed, you must first load the package at the beginning of the session

system(paste('"c:/Program Files/Mozilla Firefox/firefox.exe"','-url cran.r-project.org'), wait = FALSE)

library(genetics)

# Example of pairwise LD using genetics package

# create a genotype objects
g1 <- genotype(c('T/A',NA,'T/T',NA,'T/A',NA,'T/T','T/A',
                'T/T','T/T','T/A','A/A','T/T','T/A','T/A','T/T',
                NA,'T/A','T/A',NA))

g2 <- genotype(c('C/A','C/A','C/C','C/A','C/C','C/A','C/A','C/A',
                'C/A','C/C','C/A','A/A','C/A','A/A','C/A','C/C',
                'C/A','C/A','C/A','A/A'))

LD(g1,g2)




#9.  BIOCONDUCTOR
#    Suite of packages to analyze genomic data
#    Very useful for microarray analysis and automated annotation
#    Suggestion:  Download as biocLite.R; then add packages as necessary
#                 getBioC.R contains more packages but will take for ever to install.

# why is extensive annotation facilities important? 1) dimension-reduction 2) add contraints to model parameters
#                                                   3) interpret discovered patterns; ----> DONE IN REAL TIME!!!

system(paste('"c:/Program Files/Mozilla Firefox/firefox.exe"','-url www.bioconductor.org'), wait = FALSE)


#Example1:  Extensive list of chip-specific data packages listing annotations in hash tables
#           Simple web query

library("hgu95av2")
ls("package:hgu95av2")

library(KEGG)
library(annotate)                       #one of the foundational packages in Bioconductor

getSYMBOL("1861_at", "hgu95av2")        #Get symbol for interesting probeset identifier
BADpath <- hgu95av2PATH$"1861_at"       #Get KEGG pathways that BAD participates in
mget(BADpath, KEGGPATHID2NAME)          #List all KEGG pathway names
mget(BADpath[1], hgu95av2PATH2PROBE)    #Lists all probeset in "KEGG, Neurodegenerative Disorders"



#Example2:  PubMed query
library("annotate")
library("hgu95av2")

affys <- c("c31730_at","31731_at","31732_at","31733_at","31734_at",
          "c31735_at","31736_at","31737_at","31738_at","31739_at")


#we used getPMID to obtain PubMed ids that are related to probesets.
#duplicate and missings were removed.
ids <- getPMID(affys, "hgu95av2")
ids <- unlist(ids, use.names=FALSE)
ids <- unique(ids[!is.na(as.numeric(ids))])
length(ids)


#the search will obtain abstracts from PubMed that can be processed locally
x <- pubmed(ids)
a <- xmlRoot(x)
numAbst <- length(xmlChildren(a))
numAbst

arts <- xmlApply(a, buildPubMedAbst)   #extracts interesting sections
arts[[123]]

absts <- sapply(arts, abstText)        #extract the abstract text

#search the assembled abstracts for keywords
found <- grep("cDNA", absts)
goodAbsts <- arts[found]
length(goodAbsts)                      #num. articles related to the probesets mention cDNA in their abstracts


fname <- paste(getwd(),"/fileout",sep="")
pmAbst2HTML(goodAbsts, filename=fname)
fnameBase <- paste(getwd(),"/fileout",sep="")
pmAbst2HTML(goodAbsts, filename=fnameBase, frames=TRUE)

#open the output with frames
system(paste('"C:/Program Files/Internet Explorer/iexplore.exe"','C:/temp/fileoutindex.html'), wait = FALSE)
