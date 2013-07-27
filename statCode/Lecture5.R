###################################################
### chunk number 1: lmContrats
###################################################
args(contr.treatment)


###################################################
### chunk number 2: lmContrats2
###################################################
contrasts(cats$Sex) <- contr.treatment(2, base=2)
lm(Bwt ~ Sex, data=cats)
contrasts(ChickWeight$Diet) <- contr.treatment(4, base=4)
lm(weight ~ Diet, data=ChickWeight)


###################################################
### chunk number 3: lmgrid
###################################################
new.data <- expand.grid(Bwt=seq(2,5, by=.4), Sex=c('F','M'))
new.data
cats.lmBxS <- lm(Hwt~Bwt*Sex, data=cats)
new.cats <- predict(cats.lmBxS, new.data)
new.cats


###################################################
### chunk number 4: history
###################################################
args(history)


###################################################
### chunk number 5: Objects:is.as
###################################################
df<-rbind(1:5, LETTERS[1:5])
df
class(df)
is(df, "matrix")
is.matrix(df)
is(df, "data.frame")
is.data.frame(df)

df<- as.data.frame(df)
df
is(df, "matrix")
is(df, "data.frame")
is.data.frame(df)
is.matrix(df)


###################################################
### chunk number 6: ConvertbetweenObjects
###################################################
A1<-df[1,1]
class(A1)
A1+2
class(as.character(A1))
class(as.numeric(A1))
A1<-as.numeric(A1)
A1+2


###################################################
### chunk number 7: vector
###################################################
vec1<-1:10
names(vec1) = letters[1:10]
vec1
vec1["b"]
vec1[2]
vec1[-2]
vec1[rep(c(TRUE, FALSE), 5)]


###################################################
### chunk number 8: vector2
###################################################
4:6
vec1[4:6]
vec1[c(1,4,6)]
vec1[c("a", "d", "f")]
vec1[-2]
vec1[-c(1,5,6)]


###################################################
### chunk number 9: subsetting
###################################################
mat1<-matrix(rnorm(50), ncol=10)
dim(mat1)
mat1[1:4, c(1,4)]
colnames(mat1) <- LETTERS[1:10]
rownames(mat1) <- paste("row", 1:5, sep="")
mat1["row2", c("B", "D")]
mat1[1:2, colnames(mat1)%in%c("B", "D")]


###################################################
### chunk number 10: df-subsetting
###################################################
mat1$B
mat1<-as.data.frame(mat1)
mat1$B


###################################################
### chunk number 11: subsetting1
###################################################
attach(ChickWeight)
chick.t2and8 <- (Time == 2 | Time == 8) & Diet==2
weight[chick.t2and8]


###################################################
### chunk number 12: subsetting2
###################################################
ChickWeight[chick.t2and8, ]


###################################################
### chunk number 13: subsetting3
###################################################
subset(ChickWeight,
       subset=((Time == 2 | Time == 8) & Diet==2),

       select= weight, drop=T)
subset(ChickWeight,
       subset=((Time == 2 | Time == 8) & Diet==2))
detach(ChickWeight)


###################################################
### chunk number 14: transform
###################################################
airquality[1:2,]
newAir<-transform(airquality, log.oz = log(Ozone), Temp.C = (Temp-32)/1.8)
newAir[1:3,]


###################################################
### chunk number 15: scale
###################################################
cats[1:3,c('Bwt','Hwt')]
newCats<-scale(cats[,c('Bwt','Hwt')])
newCats[1:3,]


###################################################
### chunk number 16: grep
###################################################
attach(airquality)
search()
if(sum(grep("airquality", search()))>0)print("loaded")
detach(airquality)


###################################################
### chunk number 17: matching
###################################################
match(c(seq(3,9,2), seq(-3,-11,-2)), -20:20)


###################################################
### chunk number 18: in
###################################################
x <- rep(1:5,rep(3,5))
two4 <- x %in% c(2,4)
two4
x[two4]


###################################################
### chunk number 19: merge
###################################################
chick.numT <- table(ChickWeight$Chick)
chick.Tmat <- matrix(chick.numT, ncol=1,
                     dimnames=list(names(chick.numT), 'obsns'))
args(merge)
chick.merge <-  merge(ChickWeight, chick.Tmat,
                      by.x="Chick", by.y="row.names")


###################################################
### chunk number 20: strsplit
###################################################
Cars93$Make[1:3]
Cars93$Make<-as.character(Cars93$Make)
Cars93$Make[1:3]

car.names <- strsplit(Cars93$Make, ' ')
car.names[1:5]
car.brandname <- character() 
for (i in 1:length(car.names))
  car.brandname <- c(car.brandname, car.names[[i]][1])
car.brandname[1:10]
unique(car.brandname)


###################################################
### chunk number 21: Missingvalues
###################################################
table(airquality$Month, !is.na(airquality$Ozone))


###################################################
### chunk number 22: factors.gl
###################################################
args(gl)
gl(2,2,12, c(TRUE,FALSE))
gl(4,1,8, paste(LETTERS[1:4],1:4, sep="-"))


###################################################
### chunk number 23: sample
###################################################
args(sample)
sample(1:20)
sample(1:20, 5)
sample(1:20, 15, replace=TRUE)


###################################################
### chunk number 24: loop1
###################################################
xjj <- matrix(rnorm(4*1e6), nrow=10000)
xjj.ave <- numeric()
system.time(
for (i in 1:10000)
  xjj.ave[i] <- mean(xjj[i,])
)


###################################################
### chunk number 25: apply
###################################################
system.time( xjj.ave <- apply(xjj,1,mean) )
system.time( xjj.ave <- apply(xjj,1,sum)/ncol(xjj) )


###################################################
### chunk number 26: rowMeans
###################################################
system.time( xjj.ave <- rowMeans(xjj) )


###################################################
### chunk number 27: plotting1
###################################################
  plot(1:50, (pi*(1:50)^2), type='l',
       xlab='Radius', ylab=expression(Area == pi*r^2),
       main='Area of a circle for a given radius',
       cex.main=1.2, col.main='green',
       cex.axis=1.1, col.axis='blue',
       cex.lab=1.3, col.lab='dark violet')


###################################################
### chunk number 28: sum
###################################################
par(mfrow=c(3,2))
for (i in colnames(airquality))
  { hist(airquality[,i], prob=T,  main=paste('Histogram of ', i))
   lines(density(airquality[,i], na.rm=T))
   rug(airquality[,i])
  }


###################################################
### chunk number 29: lattice
###################################################
library(lattice)
plot(histogram(~Ozone|factor(Month), data=airquality))


###################################################
### chunk number 30: R2HTML eval=FALSE
###################################################
## library(R2HTML)
## HTMLStart(outdir="c:/mydir", file="myreport",
##    extension="html", echo=FALSE, HTMLframe=TRUE)
## HTML.title("My Report", HR=1)
## 
## HTML.title("Description of my data", HR=3)
## summary(mydata)
## 
## HTMLhr()
## 
## HTML.title("X Y Scatter Plot", HR=2)
## plot(mydata$y~mydata$x)
## HTMLplot()
## 
## HTMLStop()


###################################################
### chunk number 31: Rprofile eval=FALSE
###################################################
## help(Rprofile)


###################################################
### chunk number 32: Rcmdr eval=FALSE
###################################################
## install.packages("Rcmdr")


###################################################
### chunk number 33: loadRcmdr eval=FALSE
###################################################
## library(Rcmdr)


###################################################
### chunk number 34: prcomp
###################################################
prcomp(USArrests, scale = TRUE)
prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)
plot(prcomp(USArrests))


###################################################
### chunk number 35: prcompBiplot
###################################################
biplot(princomp(USArrests))


###################################################
### chunk number 36: hclust
###################################################
hc <- hclust(dist(USArrests), "ave")
plot(hc)
plot(hc, hang = -1)


###################################################
### chunk number 37: adeGUI eval=FALSE
###################################################
## library(ade4TkGUI)
## ade4TkGUI()


###################################################
### chunk number 38: made4-HC
###################################################
library(made4)
USArrests<-t(USArrests)
overview(USArrests)


###################################################
### chunk number 39: made4-CA
###################################################
USAord<-ord(USArrests, type="coa")
plot(USAord)


