################################
# lecture 1
################################

# Here are the code chunks I'll be using in today's lecture, 
# in case you'd like to follow along. 
# Cut and paste each line into R. 

################################
# slide 6
################################
x <- 10 
x + 2
x*2
2^x
x <- c(x, x, x)
weight <- c(115, 190, 160, 148)

################################
# slide 8
################################
height <- c(150, 175, 188, 170) 
weight.kg <- weight/2.2
height.m <- height/100
bmi <- weight.kg/height.m^2

################################
# slide 9
################################
weight.kg <- weight/2.9193
height.m <- height/100
bmi <-log(weight.kg*sqrt(height.m), 2)

################################
# slide 10
################################
sqrt(height.m)
weight[1]
for( j in 1:3 ){ print(j) }

################################
# slide 12
################################
2^(3 + 5*10)/4
flag <- T
2 > 1
2 == 2 
2 != 3
log(-1)

################################
# slide 13
################################
help(plot)
help.start()
apropos("bar")
help.search("bar")

################################
# slide 15
################################
help(t.test)
t.out <- t.test(bmi, alternative="two.sided", mu=22.5)

################################
# slide 16
################################
names(t.out)
example(t.test)

################################
# slide 19
################################
names <- c("jess", "john", "shigeru", "manuela")
weight <- c(115, 190, 160, 148)
weight > 150 
bmi > 25

################################
# slide 20
################################
calcKg <- function(lbs){ 
			kg <- lbs/2.2 
			return(kg) 
		}
		
weight.kg <- calcKg(weight)
args(calcKg)

################################
# slide 21
################################
bio.info <- cbind(height, weight)
rownames(bio.info) <- names
colnames(bio.info) <- c("height", "weight")

dim(bio.info)
nrow(bio.info)
ncol(bio.info)
help(apply)

################################
# slide 22
################################
bio.df <- data.frame(id = names, h = height, w = weight) 
bio.df[,1]
bio.df[1,]
class(bio.df[,1])
class(bio.df[1,])

################################
# slide 23
################################
info.jess <- list(firstName = "jessica", surName = "mar", phone = c(6174591439, 6175828280))
names(info.jess)
info.jess$surName
info.jess[[2]]
help(lapply)

################################
# slide 24
################################
bio503.db <- new.env()
assign("jess", info.jess, bio503.db)
ls(bio503.db)

################################
# slide 25
################################
class(bio.info)
is.matrix(bio.info)
help(is.environment)

################################
# slide 26
################################
myList <- list(a = 9, b = 2, c = 3) 
mean(myList)
myList.new <- as.numeric(myList)
mean(myList.new)
is.vector(myList.new)

################################
# slide 27
################################
pain <- c(0,3,2,2,1)
fpain <- factor(pain)
levels(fpain) <- c("none", "mild", "medium", "severe")
fpain 

################################
# slide 28
################################
bmi > 25
as.numeric(bmi > 25)
sum(as.numeric(bmi > 25))

################################
# slide 29
################################
paste("Today is", date())
expLab <- paste(c("nfkb", "tnfa", "pik3"), "kd", sep="_")
paste(expLab, "txt", sep=".")
paste(1:5, collapse="+")

################################
# slide 30
################################
getwd()
fileName <- "yeastData.txt" 
yeastDat <- read.table(fileName, header=T, row.names=1, sep="\t")

################################
# slide 31
################################
days <- c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday")
days[1]
weekend <- days[c(1,7)]
week.days <- days[2:6]
odd.days <- days[seq(from=1, to=7, by=2)]
even.days <- days[seq(from=2, to=7, by=2)]

days[-1] 
week.days <- days[-c(1,7)]

################################
# slide 32
################################
bio.df <- data.frame(id = names, h = height, w = weight)
bio.df[,1]
bio.df$names

bio.df[1,]
bio.df[c(1,4),]
bio.df[-2,]
bio.df[2,2]

################################
# slide 33
################################

bio.info <- cbind(height, weight)
bio.info[,1]
bio.info[2,]
bio.info[1,1]

################################
# slide 34
################################
grep("a", letters)
grep("a|b", x.pattern)
grep("[a-z]", letters)

gsub("[a|e|i|o|u]","*", letters)
strsplit(getwd(), “/")

################################
# slide 35
################################
ls()
rm("names")
getwd()
help(setwd)

################################
# slide 36
################################
y <- 1/0
is.na(y)
x <- c(rnorm(1000), y)
mean(x)	
mean(x, na.rm=T)

################################
# slide 37
################################
x <- c(rnorm(1000), y) 
is.na(x)
x.complete <- x[!is.na(x)]
sum(x.complete)

################################
# slide 38
################################
for( j in 1:10 ){ print(j) }
yeastDat <- read.table("yeastData.txt", header=T, row.names=1, sep="\t")
yeastDat <- as.matrix(yeastDat)

################################
# slide 39
################################
yeastDat <- as.matrix(yeastDat)
n <- nrow(yeastDat)
geneAvg <- double(n) 
for( j in 1:length(geneAvg) ){ 
		m <- mean(yeastDat[j,], na.rm=T)
		geneAvg[j] <- m
 }

################################
# slide 40
################################
yeastDat <- as.matrix(yeastDat)
n <- nrow(yeastDat)
geneAvg <- NULL
for( j in 1:n ){ 
		m <- mean(yeastDat[j,], na.rm=T)
		geneAvg <- c(geneAvg, m)
 }
