###################################################
### chunk number 1: setup
###################################################
rm(list = ls())
if (!file.exists("tables")) dir.create("tables")
set.seed(290875)
options(prompt = "R> ", continue = "+  ",
    width = 63, # digits = 4, 
    SweaveHooks = list(leftpar = function() 
        par(mai = par("mai") * c(1, 1.05, 1, 1))))
HSAURpkg <- require("HSAUR")
if (!HSAURpkg) stop("cannot load package ", sQuote("HSAUR"))
rm(HSAURpkg)
a <- Sys.setlocale("LC_ALL", "C")
book <- TRUE
refs <- cbind(c("AItR", "SI", "CI", "ANOVA", "MLR", "GLM", 
                "DE", "RP", "SA", "ALDI", "ALDII", "MA", "PCA", 
                "MDS", "CA"), 1:15)
ch <- function(x, book = TRUE) {
    ch <- refs[which(refs[,1] == x),]
    if (book) {
        return(paste("Chapter~\\\\ref{", ch[1], "}", sep = ""))
    } else {
        return(paste("Chapter~\\\\ref{", ch[2], "}", sep = ""))
    }
}


###################################################
### chunk number 2: singlebook
###################################################
book <- FALSE


###################################################
### chunk number 3: AItR-welcome
###################################################
HSAUR:::Rwelcome()
cat("\n>\n")
options(prompt = "> ")


###################################################
### chunk number 4: AItR-welcome
###################################################
options(prompt = "R> ")


###################################################
### chunk number 5: AItR-firstex
###################################################
x <- sqrt(25) + 2


###################################################
### chunk number 6: AItR-firstex-print
###################################################
x


###################################################
### chunk number 7: AItR-firstex-print
###################################################
print(x)


###################################################
### chunk number 8: AItR-recommended
###################################################
colwidth <- 4
ip <- installed.packages(priority = "high")
pkgs <- unique(ip[,"Package"])
nrows <- ceiling(length(pkgs) / colwidth)
cat(paste(c("\\begin{tabular}{", paste(rep("l", colwidth), collapse=""), "}"), collapse = ""), 
    "\n", file = "tables/rec.tex", append = FALSE)
for (i in 1:(nrows-1)) {
    cat(paste(pkgs[(1:colwidth) + (i-1)*colwidth], collapse = " & "), 
        file = "tables/rec.tex", append = TRUE)
    cat("\\\\ \n", file = "tables/rec.tex", append = TRUE)
}
rest <- pkgs[((nrows-1) * colwidth + 1):length(pkgs)]
cat(paste(rest, " & "), file = "tables/rec.tex", append = TRUE)
cat(paste(rep(" ", colwidth - length(rest)), collapse = " & "), "\\\\ \n",
    file = "tables/rec.tex", append = TRUE)
cat("\\end{tabular}\n", file = "tables/rec.tex", append = TRUE)
rm(ip, nrows, rest)


###################################################
### chunk number 9: AItR-CRAN
###################################################
cp <- available.packages(contriburl = "http://CRAN.r-project.org/src/contrib")
ncp <- sum(!rownames(cp) %in% pkgs)
rm(cp, pkgs)


###################################################
### chunk number 10: AItR-rm
###################################################
rm(ncp, colwidth, i)


###################################################
### chunk number 11: AItR-install-packages eval=FALSE
###################################################
## install.packages("sandwich")


###################################################
### chunk number 12: AItR-library eval=FALSE
###################################################
## library("sandwich")


###################################################
### chunk number 13: AItR-help eval=FALSE
###################################################
## help("mean")


###################################################
### chunk number 14: AItR-help-lib eval=FALSE
###################################################
## help(package = "sandwich")


###################################################
### chunk number 15: AItR-help-lib eval=FALSE
###################################################
## vignette("sandwich", package = "sandwich")


###################################################
### chunk number 16: AItR-Forbes2000
###################################################
data("Forbes2000", package = "HSAUR")
ls()


###################################################
### chunk number 17: AItR-Forbes2000-print eval=FALSE
###################################################
## print(Forbes2000)


###################################################
### chunk number 18: AItR-Forbes2000-print
###################################################
print(Forbes2000[1:3,])
cat("...\n")


###################################################
### chunk number 19: AItR-Forbes2000-str eval=FALSE
###################################################
## str(Forbes2000)


###################################################
### chunk number 20: AItR-Forbes2000-str
###################################################
str(Forbes2000, vec.len = 2)


###################################################
### chunk number 21: AItR-Forbes2000-help eval=FALSE
###################################################
## help("Forbes2000")


###################################################
### chunk number 22: AItR-Forbes2000-df
###################################################
class(Forbes2000)


###################################################
### chunk number 23: AItR-Forbes2000-dim
###################################################
dim(Forbes2000)


###################################################
### chunk number 24: AItR-Forbes2000-nrow-ncol
###################################################
nrow(Forbes2000)
ncol(Forbes2000)


###################################################
### chunk number 25: AItR-Forbes2000-names
###################################################
names(Forbes2000)


###################################################
### chunk number 26: AItR-Forbes2000-rank
###################################################
class(Forbes2000[,"rank"])


###################################################
### chunk number 27: AItR-Forbes2000-length
###################################################
length(Forbes2000[,"rank"])


###################################################
### chunk number 28: AItR-Forbes2000-one-to-three
###################################################
1:3
c(1,2,3)
seq(from = 1, to = 3, by = 1)


###################################################
### chunk number 29: AItR-Forbes2000-name
###################################################
class(Forbes2000[,"name"])
length(Forbes2000[,"name"]) 


###################################################
### chunk number 30: AItR-Forbes2000-first
###################################################
Forbes2000[,"name"][1]


###################################################
### chunk number 31: AItR-Forbes2000-category
###################################################
class(Forbes2000[,"category"])


###################################################
### chunk number 32: AItR-Forbes2000-nlevels
###################################################
nlevels(Forbes2000[,"category"])


###################################################
### chunk number 33: AItR-Forbes2000-levels eval=FALSE
###################################################
## levels(Forbes2000[,"category"])


###################################################
### chunk number 34: AItR-Forbes2000-levels
###################################################
levels(Forbes2000[,"category"])[1:3]
cat("...\n")


###################################################
### chunk number 35: AItR-Forbes2000-table eval=FALSE
###################################################
## table(Forbes2000[,"category"])


###################################################
### chunk number 36: AItR-Forbes2000-table
###################################################
table(Forbes2000[,"category"])[1:3]
cat("...\n")


###################################################
### chunk number 37: AItR-Forbes2000-sales
###################################################
class(Forbes2000[,"sales"])


###################################################
### chunk number 38: AItR-Forbes2000-numsum
###################################################
median(Forbes2000[,"sales"])
mean(Forbes2000[,"sales"])
range(Forbes2000[,"sales"])


###################################################
### chunk number 39: AItR-Forbes2000-summary
###################################################
summary(Forbes2000[,"sales"])


###################################################
### chunk number 40: AItR-Forbes2000-files
###################################################
pkgpath <- system.file(package = "HSAUR")
mywd <- getwd()
filep <- file.path(pkgpath, "rawdata")
setwd(filep)


###################################################
### chunk number 41: AItR-Forbes2000-read.table
###################################################
csvForbes2000 <- read.table("Forbes2000.csv", 
    header = TRUE, sep = ",", row.names = 1)


###################################################
### chunk number 42: AItR-Forbes2000-csv-names
###################################################
class(csvForbes2000[,"name"])


###################################################
### chunk number 43: AItR-Forbes2000-read.table2
###################################################
csvForbes2000 <- read.table("Forbes2000.csv",
    header = TRUE, sep = ",", row.names = 1,
    colClasses = c("character", "integer", "character", 
        "factor", "factor", "numeric", "numeric", "numeric", 
        "numeric"))
class(csvForbes2000[,"name"])   


###################################################
### chunk number 44: AItR-Forbes2000-all.equal
###################################################
all.equal(csvForbes2000, Forbes2000)


###################################################
### chunk number 45: AItR-Forbes2000-classes
###################################################
classes <- c("character", "integer", "character", "factor", 
    "factor", "numeric", "numeric", "numeric", "numeric")
length(classes)
class(classes)


###################################################
### chunk number 46: AItR-Forbes2000-RODBC eval=FALSE
###################################################
## library("RODBC")
## cnct <- odbcConnectExcel("Forbes2000.xls")
## sqlQuery(cnct, "select * from \"Forbes2000\\$\"")


###################################################
### chunk number 47: AItR-Forbes2000-RODBC
###################################################
setwd(mywd)


###################################################
### chunk number 48: AItR-Forbes2000-write.table
###################################################
write.table(Forbes2000, file = "Forbes2000.csv", sep = ",", 
            col.names = NA)


###################################################
### chunk number 49: AItR-Forbes2000-save
###################################################
save(Forbes2000, file = "Forbes2000.rda")


###################################################
### chunk number 50: AItR-Forbes2000-list
###################################################
list.files(pattern = "\\.rda")


###################################################
### chunk number 51: AItR-Forbes2000-load
###################################################
load("Forbes2000.rda")


###################################################
### chunk number 52: AItR-Forbes2000-vector-companies
###################################################
companies <- Forbes2000[,"name"]


###################################################
### chunk number 53: AItR-Forbes2000-vector-indexing
###################################################
companies[1]


###################################################
### chunk number 54: AItR-Forbes2000-vector-indexing
###################################################
1:3
companies[1:3]


###################################################
### chunk number 55: AItR-Forbes2000-vector-negative-indexing
###################################################
companies[-(4:2000)]


###################################################
### chunk number 56: AItR-Forbes2000-top-three
###################################################
Forbes2000[1:3, c("name", "sales", "profits", "assets")]


###################################################
### chunk number 57: AItR-Forbes2000-list-extract
###################################################
companies <- Forbes2000$name


###################################################
### chunk number 58: AItR-Forbes2000-vector-companies
###################################################
companies <- Forbes2000[,"name"]


###################################################
### chunk number 59: AItR-Forbes2000-sales
###################################################
order_sales <- order(Forbes2000$sales)


###################################################
### chunk number 60: AItR-Forbes2000-sales-small
###################################################
companies[order_sales[1:3]]


###################################################
### chunk number 61: AItR-Forbes2000-order
###################################################
Forbes2000[order_sales[c(2000, 1999, 1998)],
           c("name", "sales", "profits", "assets")]


###################################################
### chunk number 62: AItR-Forbes2000-logical
###################################################
Forbes2000[Forbes2000$assets > 1000,
           c("name", "sales", "profits", "assets")]


###################################################
### chunk number 63: AItR-Forbes2000-logical2
###################################################
table(Forbes2000$assets > 1000)


###################################################
### chunk number 64: AItR-Forbes2000-NA
###################################################
na_profits <- is.na(Forbes2000$profits)
table(na_profits)
Forbes2000[na_profits, 
           c("name", "sales", "profits", "assets")]


###################################################
### chunk number 65: AItR-Forbes2000-complete-cases
###################################################
table(complete.cases(Forbes2000))


###################################################
### chunk number 66: AItR-Forbes2000-UK
###################################################
UKcomp <- subset(Forbes2000, country == "United Kingdom")
dim(UKcomp)


###################################################
### chunk number 67: AItR-Forbes2000-summary
###################################################
summary(Forbes2000)


###################################################
### chunk number 68: AItR-Forbes2000-summary-output
###################################################
summary(Forbes2000)


###################################################
### chunk number 69: AItR-Forbes2000-lapply eval=FALSE
###################################################
## lapply(Forbes2000, summary)


###################################################
### chunk number 70: AItR-Forbes2000-tapply-category
###################################################
mprofits <- tapply(Forbes2000$profits, 
                   Forbes2000$category, median, na.rm = TRUE)


###################################################
### chunk number 71: AItR-Forbes2000-medianNA
###################################################
median(Forbes2000$profits)


###################################################
### chunk number 72: AItR-Forbes2000-mprofits
###################################################
rev(sort(mprofits))[1:3]


###################################################
### chunk number 73: AItR-Forbes2000-marketvalue
###################################################
layout(matrix(1:2, nrow = 2))
hist(Forbes2000$marketvalue)
hist(log(Forbes2000$marketvalue))


###################################################
### chunk number 74: AItR-Forbes2000-formula
###################################################
fm <- marketvalue ~ sales
class(fm)


###################################################
### chunk number 75: AItR-Forbes2000-marketvalue-sales
###################################################
plot(log(marketvalue) ~ log(sales), data = Forbes2000, 
     pch = ".")


###################################################
### chunk number 76: AItR-Forbes2000-country-plot eval=FALSE
###################################################
## boxplot(log(marketvalue) ~ country, data = 
##         subset(Forbes2000, country %in% c("United Kingdom", 
##             "Germany", "India", "Turkey")),
##         ylab = "log(marketvalue)", varwidth = TRUE)


###################################################
### chunk number 77: AItR-Forbes2000-country-plot
###################################################
tmp <- subset(Forbes2000, 
    country %in% c("United Kingdom", "Germany", 
                   "India", "Turkey"))
tmp$country <- tmp$country[,drop = TRUE]
boxplot(log(marketvalue) ~ country, data = tmp,
        ylab = "log(marketvalue)", varwidth = TRUE)


###################################################
### chunk number 78: AItR-analysis1
###################################################
file.create("analysis.R")


###################################################
### chunk number 79: AItR-analysis2 eval=FALSE
###################################################
## source("analysis.R", echo = TRUE)


###################################################
### chunk number 80: AItR-analysis3
###################################################
file.remove("analysis.R")


