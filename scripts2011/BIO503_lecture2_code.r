# Here is the code  I'll be using in today's lecture, 
# in case you'd like to follow along. 
# Cut and paste each line or block into R. 

# slide 3
weight <- c(115, 190, 160, 148)
weight <- c(weight, 120)
weight[1]
weight[1:3]

# slide 4

bio.info <- cbind(weight, height)
bio.info[,1]
bio.info[1,]
names <- c("jess", "lingling", "john", "eric", "sara")
bio.df <- data.frame(id = names, h = height)

# slide 5 
myList <- list(bio = bio.info, randomNumber = 2, threeCities = c("baltimore", "philadelphia", "pittsburgh")) 
whereToGo <- myList[[3]]
names(myList)
whereToGo <- myList$threeCities

# slide 6 
ls()
class(height)
is.matrix(bio.info)

funcName <- function(x){ 
				# code
}

# slide 7
a <- 1:5
b <- 6:10
x <- cbind(a,b)

x <- matrix(1:10, ncol=2, nrow=5, byrow=F)

x.med <- NULL
for( i in 1:5 ){ 
	m <- median(x[i,], na.rm=T)
	x.med <- c(x.med, m)
}

# slide 8
history()
history(max.show=Inf)

# slide 11
fileName <- "yeastData.txt" 
yeastDat <- read.table(fileName, header=T, sep="\t")

# slide 12
myG <- scan("GAPDH.fasta", what="txt", sep="\n") 

# slide 13
geneAvg <- apply(yeastDat, 1, mean, na.rm=T)
expAvg <- apply(yeastDat, 2, mean, na.rm=T)

# slide 14
fruitList <- list(red=c("apple", "tomato"), yellow=c("banana"), orange=c("orange", "carrot", "tangerine"))
l.out <- lapply(fruitList, length)
s.out <- sapply(fruitList, length)
class(l.out)
class(s.out)

# slide 15 
seq(from=1, to=20, by=2)
seq(from=1, to=20, length.out=9)
seq(50)
1:50

# slide 16
x <- 1:4
rep(x, 2)
rep(x, each=2)

rep(x, c(2,1,2,1)) 
rep(x, each = 2, len = 4) 
rep(x, each = 2, times = 3) 

# slide 17 
times <- c(10, 9.5, 11.2, 30.4, 21.5)
racers <- c("wallaby", "kangaroo", "emu", "koala", "wombat")
names(times) <- racers

sort(times)
order(times)
racers[order(times)]

rev(order(times))
racers[rev(order(times))]

# slide 18 
names(times) <- racers
sort(times)

# slide 19 
sky <- "blue" 
if( sky == "blue" ){ now <- "day" }          else{ now <- "night" }

now <- ifelse(sky == "blue", "day", "night")

# slide 20
y <- 12345 
x <- y/2 
while( abs(x*x-y) > 1e-10 ) x <- (x + y/x)/2
x^2

# slide 21
x <- y/2
repeat{ 
	x <- (x + y/x)/2
	if( abs(x*x-y) < 1e-10 ) break 
 }

x^2 



