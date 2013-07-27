###################################################
### Read-in data set and make some changes: 
###################################################
library(Hmisc)
pbc <- read.table("pbc.csv", header = TRUE,  sep = ",", na.strings = "", stringsAsFactors = FALSE)
pbc <- upData(pbc,
   lowernames = TRUE,
   status = factor(status, levels = 0:2, labels = c("Censored", "Censored due to liver treatment", "Dead")),
   censored = status,
   drug = factor(drug, levels = 1:2, labels = c("D-penicillamine", "Placebo")),
   sex = factor(sex, levels = c("Female", "Male")),
   ascites = factor(ascites, levels = c("No", "Yes")),
   stage = factor(stage, levels = 1:4),
   ageyrs = age/365.25,
   fuyrs = fudays/365.25,
   labels = c(ageyrs="Age", 
      fuyrs = "Follow Up",
      censored = "Collapsed Survival Status", 
      fudays = "Follow Up",
      status="Original Survival Status",
      drug = "Treatment",
      sex = "Gender",
      age = "Age",
      ascites = "Presence of Ascites", 
      bili = "Serum Bilirubin",
      chol = "Serum Cholesterol",
      album = "Serum Albumin",
      stage = "Histological stage of disease"),
   units = c(fudays = "days",
      fuyrs = "years",
      age = "days",
      ageyrs = "years",
      bili = "mg/dL", chol = "mg/dL", album = "mg/dL"),
   levels = list(censored = list("Censored" = c("Censored", "Censored due to liver treatment"), "Dead" = "Dead")))


###################################################
### Changes should match the output of the following: 
###################################################
contents(pbc)


###################################################
### Creating vectors: 
###################################################
rep(1:4, times = c(2, 4, 3, 1))
sample(c("M", "F"), size = 10, replace = TRUE)
paste("Treatment", c("A", "B", "C"))


###################################################
### Creating factors: 
###################################################
gl(n = 2, k = 8, label = c("Control", "Treatment"))
head(cut(pbc$ageyrs, breaks = 4))
interaction(levels(pbc$drug), levels(pbc$censored))


###################################################
### Embedded quotes: 
###################################################
"Single 'quotes' can be embedded in double quotes"
'Double "quotes" can be embedded in single quotes'
"Embedded double quotes, \", must be escaped"


###################################################
### Attributes of vectors: 
###################################################
length(pbc$ageyrs)

length(pbc$ascites)
sum(!is.na(pbc$ascites))

table(!is.na(pbc$ascites))

(x <- c(Dog = "Spot", Horse = "Penny", Cat = "Stripes"))
names(x)


###################################################
### Attributes of factors: 
###################################################
levels(pbc$stage)
nlevels(pbc$stage)


###################################################
### More attributes of vectors: 
###################################################
mode(x)
mode(pbc$ageyrs)
mode(pbc$drug)


###################################################
### Subsetting vectors with logical indexes: 
###################################################
set.seed(1)
(x <- sample(10, size = 10, replace = TRUE))
x[x == 4]
x[x > 2 & x < 5]

(x <- c(9, 5, 12, NA, 2, NA, NA, 1))
x[x > 2]
x[x > 2 & !is.na(x)]

subset(x, x > 2)
subset(x, x > 2 & x < 10)


###################################################
### Subsetting named vectors: 
###################################################
(fruit <- c(oranges = 5, bananas = 10, apples = 1, peaches = 30))
names(fruit)
fruit[c("apples", "oranges")]


###################################################
### Vectorization of functions and the recycling rule: 
###################################################
(y <- sample(1:100, size = 10))
log(y)

(x <- sample(1:20, size = 10))
x+y

y + 2
y < 50

(x <- sample(1:20, size = 2))
x+y

x <- 1:9
y <- 1:10
x+y


###################################################
### Conditional Expression: 
###################################################
(x <- 1:10)
x < 7 & x > 2
x < 7 && x > 2

(x <- sample(c("A", "B", "C", "D"), size = 10, replace = TRUE))
x %in% c("A", "C", "D")
x %nin% c("A", "B")

x <- 1:3; y <- 1:3
x == y
identical(x, y)
all.equal(x, y)

0.9 == (1 - 0.1)
identical(0.9, 1 - 0.1)
all.equal(0.9, 1 - 0.1)
0.9 == (1.1 - 0.2)
identical(0.9, 1.1 - 0.2)
all.equal(0.9, 1.1 - 0.2)
all.equal(0.9, 1.1 - 0.2, tolerance = 1e-16)


###################################################
### Coercion of vectors: 
###################################################
x <- c(34, 31, 80, 78, 64, 87)
x > 35
sum(x > 35)
sum(x > 35)/length(x)

(fac <- factor(sample(c("M", "F"), size = 5, replace = TRUE)))
as.character(fac)

ifelse(fac == "M", NA, fac)
ifelse(fac == "M", NA, as.character(fac))

as.numeric(fac)

(fac <- factor(sample(5:10, size = 10, replace = TRUE), levels = 5:10))
as.numeric(fac)
as.numeric(as.character(fac))


###################################################
### Dates: 
###################################################
as.Date("2007-10-18", format = "%Y-%m-%d")
as.Date("2007OCT18", format = "%Y%b%d")
as.Date("October 18, 2007", format = "%B %d, %Y")
strptime("10/18/2007 08:30:45", format = "%m/%d/%Y %H:%M:%S")
strptime("10/18/2007 12:30:45 AM", format = "%m/%d/%Y %I:%M:%S %p")

(x <- seq.Date(from = as.Date("2007-10-18"), to = as.Date("2007-10-30"), by = "3 days"))
x + 10
x > as.Date("2007-10-21")
x - as.Date(c("2006-01-10", "2007-08-15", "2005-06-24", "2004-12-30", "2005-04-05"))
diff(x)
weekdays(x)
format(x, "%Y")
format(x, "%m/%d/%Y")


###################################################
### Regular expressions: 
###################################################
names(pbc)
grep(pattern = "age", x = names(pbc), value = TRUE)

(char <- c("id", "patient.age", "date", "baseline_bmi", "follow.up.visit"))
grep(pattern = "\\.", x = char, value = TRUE)

char <- c("this is an option", "or perhaps this", "and don't forget about this one")
grep(pattern = "this", x = char, value = TRUE)
grep(pattern = "^this", x = char, value = TRUE)
grep(pattern = "this$", x = char, value = TRUE)

char <- c(" ", "3 times a day")
grep(pattern = "[a-zA-Z0-9]", x = char, value = TRUE)
grep(pattern = "[^a-zA-Z0-9]", x = char, value = TRUE)

char <- c("The", "moon is made", "of cheese")
grep(pattern = " +", x = char, value = TRUE)
grep(pattern = "o?o", x = char, value = TRUE)

char <- c("red", "ball", "blue", "sky")
grep(pattern = "d|e", x = char, value = TRUE)
grep(pattern = "al|lu", x = char, value = TRUE)

char <- c("red ball", "blue ball", "red sky", "blue sky")
grep(pattern = "red", x = char, value = TRUE)
grep(pattern = "(red ball)", x = char, value = TRUE)

char <- c("vit E", "vitamin e")
grep(pattern = "vit.*E", x = char, value = TRUE)
grep(pattern = "vit.*E", x = char, value = TRUE, ignore.case = TRUE)

char <- c("one.period", "two..periods", "three...periods")
sub(pattern = "\\.+", replacement = ".", x = char)

char <- c("45: Received chemo", "1, Got too sick", "2; Moved to another hospital")
sub(pattern = "^([0-9]+)[:,;].*$", "\\1", char)

char <- c("vit E", "vitamin E", "vitamin ESTER-C", "vit E ")
sub(pattern = "^(vit).*([E]).*$", "\\1 \\2", char)

sub(" a", " A", "Capitalizing all words beginning with an a")
gsub(" a", " A", "Capitalizing all words beginning with an a")


###################################################
### Creating matrices & arrays: 
###################################################
matrix(1:6, ncol = 3)
matrix(1:6, ncol = 3, byrow = TRUE)

matrix(c(1, 2, 3, 11, 12, 13), nrow = 2, ncol = 3, byrow = TRUE,
   dimnames = list(c("row1", "row2"), c("C.1", "C.2", "C.3")))

cbind(1:3, 7:9)
rbind(1:11, 5:15)

cbind(col1 = 1:3, col2 = 7:9)
rbind(row1 = 1:11, row2 = 5:15)

array(1:24, dim = c(3, 4, 2))


###################################################
### Subsetting matrices & arrays: 
###################################################
(x <- matrix(1:12, ncol = 4, byrow = TRUE))
x[1, ]
x[ , 4]
x[2, 3]
x[-(1:2), 3:4]
x[ x[ , 2] < 8 & x[, 4] < 10, ] 

(x <- array(1:24, dim = c(3, 4, 2)))
x[2, 3, 1]
x[1:2, c(1,4), -1]

x <- matrix(1:12, ncol = 4, byrow = TRUE)
x[, 4]
is.matrix(x[, 4])
is.vector(x[, 4])

x <- matrix(1:12, ncol = 4, byrow = TRUE)
x[, 4, drop = FALSE]
is.matrix(x[, 4, drop = FALSE])


###################################################
### Creating data frames & lists: 
###################################################
ourdf <- data.frame(id = 101:110, 
   sex = sample(c("M", "F"), size = 10, replace=TRUE),
   age = sample(20:50, size=10, replace=TRUE),
   tx = sample(c("Drug", "Placebo"), size=10, replace=TRUE),
   diabetes = sample(c(TRUE, FALSE)))
ourdf

ourlist <- list(comp1 = c(TRUE, FALSE), 
   comp2 = 1:4, 
   comp3 = matrix(1:20, nrow=2, byrow=TRUE))
ourlist


###################################################
### Attributes of lists: 
###################################################
str(ourlist)


###################################################
### Subsetting lists: 
###################################################
str(ourlist["comp1"])
ourlist[["comp1"]]
str(ourlist[["comp1"]])
ourlist$comp3
str(ourlist$comp3)


###################################################
### Subsetting data frames: 
###################################################
ourdf[, 4]
ourdf[["tx"]]
ourdf$tx
ourdf[ourdf$sex == "M", c("id", "tx")]

x <- "ageyrs"
head(pbc[[x]])

subset(pbc, subset = sex == "Female" & censored == "Dead" &
   drug == "D-penicillamine" & ascites == "Yes", select = age)
subset(pbc, subset = sex == "Female" & censored == "Dead" &
   drug == "D-penicillamine" & ascites == "Yes", 
   select = c(id, bili, stage))
subset(pbc, subset = sex == "Female" & censored == "Dead" &
   drug == "D-penicillamine" & ascites == "Yes", 
   select = bili:stage)
head(subset(pbc, select = -c(drug, censored, ascites)))
subset(pbc, subset = sex == "Female" & censored == "Dead" &
   drug == "D-penicillamine" & ascites == "Yes")$age

unique(subset(pbc, select = c(drug, censored, ascites)))


###################################################
### Merging data frames: 
###################################################
(authors <- data.frame(
   surname = c("Tukey", "Venables", "Tierney", "Ripley", "McNeil"),
   nationality = c("US", "Australia", "US", "UK", "Australia"),
   deceased = c("yes", rep("no", 4))))
(books <- data.frame(
   name = c("Tukey", "Venables", "Tierney", "Ripley", "Ripley",
      "McNeil", "R Core"),
   title = c("Exploratory Data Analysis", 
      "Modern Applied Statistics", "LISP-STAT", "Spatial Statistics", 
      "Stochastic Simulation", "Interactive Data Analysis",
      "An Introduction to R"),
   other.author = c(NA, "Ripley", NA, NA, NA, NA, 
      "Venables & Smith")))
merge(authors, books, by.x = "surname", by.y = "name")
merge(books, authors, by.x = "name", by.y = "surname", all.x = TRUE)
merge(authors, books, by.x = "surname", by.y = "name", all = TRUE)


###################################################
### Reshaping data frames: 
###################################################
x <- data.frame(id = sample(1:100, size = 50, replace = TRUE), 
   chemo = sample(Cs(Hormonal, Antibody, Other), size = 50, 
      replace = TRUE),
   visitdt = sample(paste(1:12, 1:30, 2004:2006, sep="/"),
      size = 50, replace = TRUE))
dim(x)
head(x)
table(x$id) # <-- notice that there are multiple records per ID
length(unique(x$id))

widechemo <- reshape(subset(x, select = Cs(id, chemo)), 
   v.names = "chemo", idvar = "id",
   timevar = "chemo", direction = "wide")
head(widechemo)
dim(widechemo) # should be equal to length(unique(x$id))

xx <- data.frame(id = rep(1:100, times = 10), 
   week = rep(1:10, each = 100),
   hgb = sample(seq(4, 16, by = 0.1), size = 1000, replace = TRUE))
dim(xx)
length(unique(xx$id))
head(xx)

wide.xx <- reshape(xx, v.names = "hgb", idvar = "id", timevar = "week", direction = "wide")
head(wide.xx)
dim(wide.xx) # should be equal to length(unique(xx$id))

subxx <- xx[sample(1:1000, size = 900, replace = FALSE), ] 
subxx$hgb[sample(1:900, size = 100, replace = FALSE)] <- NA
wide.subxx <- reshape(subxx, v.names = "hgb", idvar = "id",
   timevar = "week", direction = "wide")
head(wide.subxx) # Notice, columns are no longer in order
dim(wide.subxx) # should be equal to length(unique(subxx$id))

df <- data.frame(id=rep(1:4,rep(2,4)),
   visit=factor(rep(c("Before","After"),4)), x=rnorm(4), y=runif(4))
df
reshape(df, timevar="visit", idvar="id", direction="wide")

df2 <- df[1:7,]
df2
reshape(df2, timevar="visit", idvar="id", direction="wide")

library(MASS)
data(immer)
head(immer)
dim(immer)
immer.long <- reshape(immer, varying = list(c("Y1", "Y2")),
   direction = "long")
head(immer.long)

immer.long <- reshape(immer, varying = list(c("Y1", "Y2")),
   timevar = "Year", times = c(1931, 1932), v.names = "Yield",
   direction = "long")
head(immer.long)


###################################################
### Diverting screen output to a file: 
###################################################
cat("The mean age of the subjects in our PBC data set is", round(mean(pbc$ageyrs), 1), "years.")
cat("Mean", "\t", "SD", "\t", "N", "\n", round(mean(pbc$ageyrs), 2), "\t",
   round(sd(pbc$ageyrs), 2), "\t", sum(!is.na(pbc$ageyrs)), "\n")

format(2.00, nsmall = 2)
format(0.0000345, scientific = FALSE)
format(34567901567, big.mark = ",")

x <- data.frame(id = 1:2, 
   comment = c("Double \"quote\" example 1", 
      "Another double \"quote\" example"))
x
write.table(x, file = "", qmethod = "escape")
write.table(x, file = "", qmethod = "double")
write.table(x, file = "", quote = FALSE)


###################################################
### Object Management: 
###################################################
ls()

exists("length")
exists("lngth")

rm(list = setdiff(ls(), "pbc"))
ls()


###################################################
### Conditional Evaluation of expressions: 
###################################################
x <- c(2, 5, 7, NA, 10, NA, -1)
if(x < 0) {
   print("< 0")
} else {
   print("> 0")
}

x < 0

with(pbc, table(ifelse(ageyrs < 40, "< 40", ">= 40")))
with(pbc, table(ifelse(ageyrs < 40, "< 40", ifelse(ageyrs >= 40 & ageyrs <= 60, "40-60", "> 60"))))

x <- 10
switch(length(x), x, median(x), mean(x))
x <- c(5, 2)
switch(length(x), x, median(x), mean(x))
x <- c(3, 4, 7)
switch(length(x), x, median(x), mean(x))
x <- sample(1:10, size = 5)
switch(length(x), x, median(x), mean(x))

(x <- sample(c("horse", "cat", "dog"), size = 1))
switch(x, horse = "Penny", cat = "Stripes", dog = "Spot")

centre <- function(x, type) {
   switch(type, mean = mean(x), median = median(x), trimmed = mean(x, trim = 0.1))
}
x <- rcauchy(10)
centre(x, "mean")
centre(x, "median")
centre(x, "trimmed")


###################################################
### Repetitive evaluation of expressions: 
###################################################
for(i in 5:1) {
   print(i)
}
i

for(i in c("ageyrs", "fuyrs", "bili", "chol", "album")) {
   cat("Mean of", i, "=", round(mean(pbc[[i]], na.rm = TRUE), 2), "\n")
}
for(i in c("ageyrs", "fuyrs", "bili", "chol", "album")) {
   for(j in levels(pbc$sex)) {
      cat("Mean of", i, "in", j, "patients =", 
         round(mean(pbc[pbc$sex == j & !is.na(pbc$sex), i], na.rm = TRUE), 2), "\n")
   }
}

i <- 1
while(i <= 5) {
   cat("Iteration", i, "\n")
   i <- i+1
}
i

i <- 0
repeat {
   if(i > 10) break
   if(i > 2 && i < 5) {
      i <- i + 1
      next
   }
   print(i)
   i <- i + 1
}
for(i in 0:10) {
   if(i > 2 && i < 5) next;
   print(i)
}


###################################################
### 'Growing' data structures in loops: 
###################################################
numeric(length = 10)
character(length = 10)
logical(length = 10)

(x <- sample(1:10))
x[15] <- 20
x

(mat <- matrix(NA, nrow = 2, ncol = 4))
for(i in 1:nrow(mat)) {
   for(j in 1:ncol(mat)) {
      mat[i, j] <- rnorm(1)
   }
}
mat


###################################################
### Family of apply functions: 
###################################################
lapply(X= subset(pbc, select = c("fuyrs", "ageyrs", "bili", "chol", "album")), FUN = range, na.rm = TRUE)
sapply(X = subset(pbc, select = c("fuyrs", "ageyrs", "bili", "chol", "album")), FUN = range, na.rm = TRUE)

with(pbc, split(ageyrs, drug))
with(pbc, lapply(X = split(ageyrs, drug), FUN = mean, na.rm = TRUE))
with(pbc, sapply(X = split(ageyrs, drug), FUN = mean, na.rm = TRUE))

apply(X = subset(pbc, select = c("drug", "sex", "ascites", "stage", "censored")), 
   MARGIN = 2, FUN = table)

with(pbc, sapply(X = split(ageyrs, drug), FUN = function(x) {
   c(Mean = mean(x), SD = sd(x), Median = median(x), Min = min(x), Max = max(x))
}))

with(pbc, tapply(X = ageyrs, INDEX = sex, FUN = median, na.rm = TRUE))
with(pbc, tapply(X = ageyrs, INDEX = list(sex, drug), FUN = median, na.rm = TRUE))

x <- data.frame(id = 1:10, age = sample(20:40, size = 10, replace = TRUE),
   race = factor(sample(c("W", "B", "O"), size = 10, replace = TRUE)),
   gender = factor(sample(c("M", "F"), size = 10, replace = TRUE)))
y <- data.frame(id = sample(1:10, size = 100, replace = TRUE), 
   lab = sample(100:500, size = 100, replace = TRUE))
(aggout <- with(y, aggregate(x = id, by = list(id), FUN = length)))
x <- merge(x, aggout, by.x = "id", by.y = "Group.1", all = TRUE)
x


###################################################
### Apply family versus looping: 
###################################################
mat <- matrix(rnorm(100000), ncol = 4)
usingLoops <- function(mat) {
   col.scale <- matrix(NA, nrow(mat), ncol(mat))
   m <- NULL     # initialize the vector, m
   for(j in 1:ncol(mat)) {
      m[j] <- mean(mat[, j])     # compute the mean of each column
   }
   for(i in 1:nrow(mat)) {
      for(j in 1:ncol(mat)) {
         col.scale[i, j] <- mat[i, j] - m[j]     # centre the columns
      }
   }
   col.scale
}
usingApply <- function(mat) {
   apply(mat, 2, scale, scale = FALSE) # apply the scale() function and print the scaled matrix
}
system.time(usingLoops(mat))
system.time(usingApply(mat))


###################################################
### Writing your own functions: 
###################################################
printfun <- function() {
   print(x)
}
x <- 1
printfun()

x <- 1
printfun <- function() {
   x <- 2
   print(x)
}
printfun()
x

x <- 1
printfun <- function() {
   y <- 2
   print(x + y)
}

oneFUN <- function(x, y = 5) {
   return(20)
   x + y
}
oneFUN(x = 5)

examplefun <- function(x) {
  if( missing(x) ) {
    return("x is missing")
  } else {
    return("x is not missing")
  }
}
examplefun()
examplefun(10)


###################################################
### Example Rprofile.site / .Rprofile file: 
###################################################
op <- par(no.readonly = TRUE)

par(mfrow = c(2, 2), mar = c(5, 4, 3, 2) + 0.1)
# mosaicplot
mosaicplot(~ drug + censored, data = pbc, color = TRUE, xlab = label(pbc$drug),  ylab = label(pbc$censored), 
   main = "mosaicplot()", cex.axis = 0.40)
box("figure")
# spineplot
with(pbc, spineplot(censored ~ drug, main = "spineplot()", xlab = label(drug),  ylab = label(censored)))
box("figure")
# fourfoldplot
with(pbc, fourfoldplot(table(sex, drug, stage), color = gray(c(0.5, 0.75)), main = "fourfoldplot()"))
box("figure")
# assocplot
with(pbc, assocplot(table(stage, censored), col = c("black", "gray"), xlab = label(stage), ylab = label(censored),
   main = "assocplot()"))
box("figure")

par(op)


###################################################
### arrows() functions: 
###################################################
xbar <- with(pbc, tapply(X=ageyrs, INDEX=stage, FUN=mean, na.rm=TRUE))
sdev <- with(pbc, tapply(X=ageyrs, INDEX=stage, FUN=sd, na.rm=TRUE))
with(pbc, stripchart(ageyrs ~ stage, method="jitter", jitter=0.2, pch=1, vertical=TRUE, col="black", xlab=label(stage),
   ylab=paste(label(ageyrs), " (", units(ageyrs), ")", sep=""),
   main=paste("Age Across Stage of Disease", "- Mean and SD denoted -", sep="\n")))
arrows(1:4, xbar+sdev, 1:4, xbar-sdev, angle=90, code=3, length=0.1, lwd=2)
lines(1:4, xbar, pch=4, type="b", cex=4, lwd=2)
box("figure")


###################################################
### par() function usr= argument: 
###################################################
plot(0, 0, type = "n", axes = FALSE, xlab = "x", ylab = "y")
par(usr = c(1, 10, 1, 5))
axis(side = 1, at = 1:10)
axis(side = 2, at = 1:5)
points(x = sample(seq(1.5, 9.5, by = 0.5), size = 10, replace = TRUE), 
   y = sample(seq(1.5, 4.5, by = 0.5), size = 10, replace = TRUE))
box("figure")


###################################################
### axis() function: 
###################################################
op <- par(read.only = TRUE)
x <- 1:2
y <- runif(n = 2, min = 0, max = 100)
par(cex=0.8, mar=c(4, 5, 2, 5))
plot(x, y, type="n", xlim=c(0.5, 2.5), ylim=c(-10, 110), axes=FALSE, ann=FALSE)
axis(side = 2, at=seq(from = 0, to = 100, by = 20))
mtext("Temperature (Centigrade)", side=2, line=3)
axis(side = 1, at=1:2, labels=c("Treatment 1", "Treatment 2"))
axis(side = 4, at=seq(from = 0, to = 100, by = 20), labels=seq(from = 0, to = 100, by = 20)*9/5 + 32)
mtext("Temperature (Fahrenheit)", side=4, line=3)
box()
segments(x, 0, x, 100, lwd=20, col="dark grey")
segments(x, 0, x, 100, lwd=16, col="white")
segments(x, 0, x, y, lwd=16, col="light grey")
box("figure")
par(op)


###################################################
### par() function xpd= argument -- Clipping: 
###################################################
y1 <- rnorm(100)
y2 <- rnorm(100)
par(mfrow=c(2, 1), mar=c(2, 1, 1, 1), xpd=NA)
plot(y1, type="l", axes=FALSE, xlab="", ylab="", main="")
box(col="grey")
mtext("Left end of margin", adj=0, side=3)
lines(x=c(20, 20, 40, 40), y=c(-7, max(y1), max(y1), -7), lwd=3, col="grey")
plot(y2, type="l", axes=FALSE, xlab="", ylab="", main="")
box(col="grey")
mtext("Right end of margin", adj=1, side=3)
mtext("Label below x=30", at=30, side=1)
lines(x=c(20, 20, 40, 40), y=c(7, min(y2), min(y2), 7), lwd=3, col="grey")
box("outer")


###################################################
### layout() function: 
###################################################
library(Hmisc)
op <- par(no.readonly=TRUE)
layout(matrix(c(1, 0, 3, 2), ncol=2, nrow=2, byrow=TRUE), widths=c(3, 1), heights=c(1, 3), respect=TRUE)
agehist <- with(pbc, hist(ageyrs, plot=FALSE))
albumhist <- with(pbc, hist(album, plot=FALSE))
top <- max(agehist$counts, albumhist$counts)
par(mar=c(0,3,1,1))
barplot(albumhist$counts, axes=FALSE, ylim=c(0, top), space=0)
par(mar=c(3,0,1,1))
barplot(agehist$counts, axes=FALSE,  xlim=c(0, top), space=0, horiz=TRUE)
par(mar=c(4,4,1,1))
with(pbc, plot(ageyrs ~album, xlab=paste(label(album), " (", units(album), ")", sep=""), ylab=paste(label(ageyrs), 
   " (", units(ageyrs), ")",   sep="")))
par(op)


###################################################
### rug() function: 
###################################################
y <- rnorm(50)
hist(y, main = "", xlab = "", ylab = "") 
box()
rug(y)
box("figure")


###################################################
### Mathematical annotation: 
###################################################
par(mar = c(1, 1, 1, 1))
plot(0:10, 0:10, type = "n", axes = FALSE)
text(1, 10, expression(x %+-% y), cex = 1.5)
text(1, 9, expression(x[i]), cex = 1.5)
text(1, 8, expression(x^2), cex = 1.5)
text(1, 7, expression(sqrt(x)), cex = 1.5)
text(1, 6, expression(sqrt(x, 3)), cex = 1.5)
text(1, 5, expression(x != y), cex = 1.5)
text(1, 4, expression(x <= y), cex = 1.5)
text(1, 3, expression(hat(x)), cex = 1.5)
text(1, 2, expression(tilde(x)), cex = 1.5)
text(1, 1, expression(bar(x)), cex = 1.5)
text(1, 0, expression(x %<=>% y), cex = 1.5)
text(4, 10, expression(Alpha + Omega), cex = 1.5)
text(4, 9, expression(alpha + omega), cex = 1.5)
text(4, 8, expression(45 * degree), cex = 1.5)
text(4, 7, expression(frac(x, y)), cex = 1.5)
text(4, 5.5, expression(sum(x[i], i = 1, n)), cex = 1.5)
text(4, 4, expression(prod(plain(P)(X == x), x)), cex = 1.5)
text(4, 2.5, expression(integral(f(x) * dx, a, b)), cex = 1.5)
text(4, 0.5, expression(lim(f(x), x %->% 0)), cex = 1.5)
text(8, 10, expression(x^y + z), cex = 1.5)
text(8, 9, expression(x^(y + z)), cex = 1.5)
text(8, 8, expression(x^{y + z}), cex = 1.5)
text(8, 6, expression(hat(beta) == (X^t * X)^{-1} * X^t * y), cex = 1.5)
text(8, 4, expression(bar(x) == sum(frac(x[i], n), i==1, n)), cex = 1.5)
text(8, 2, expression(paste(frac(1, sigma*sqrt(2*pi)), " ", plain(e)^{frac(-(x-mu)^2, 2*sigma^2)})), cex = 1.5)
box("figure")


x <- seq(-4, 4, length = 101)
plot(x, sin(x), type = "l", xaxt = "n", xlab = expression(paste("Phase Angle ", phi)), ylab = expression("sin "*phi))
axis(side = 1, at = c(-pi, -pi/2, 0, pi/2, pi), label = expression(-pi, -pi/2, 0, pi/2, pi))
box("figure")

x <- 1:10
alpha <- 4
plot(x, x^alpha, xlab = "x", ylab = expression(x^alpha),
   main = substitute(paste("Power plot of ", x^alpha, " for ", alpha == ch.a), list(ch.a = alpha)))
box("figure")


