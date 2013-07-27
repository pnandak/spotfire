
## @knitr loadData, comment="", prompt=TRUE, echo=FALSE, cache=TRUE
load("~/Dropbox/WinterRClass/Datasets/saved_datasets_list.rda")


## @knitr assignData, comment="", prompt=TRUE, echo=FALSE, cache=TRUE
Sal <- fileList[["Salaries2011"]]
mon <- fileList$Monuments
circ <- fileList$CirculatorRidership
bike <- fileList$BikeLanes
rest <- fileList$Restaurants

# sapply(Sal$Agency, function(x) {
#   dat <- Sal[Sal == x, ]
#   write.csv(x=dat, file=paste("~/Dropbox/WinterRClass/Datasets/Salary_", x, ".csv", sep=""))
#   })


## @knitr opts, comment="", prompt=TRUE, echo=FALSE



## @knitr curve, comment="", prompt=TRUE
foo <- rnorm(100, mean=1, sd=2)
hist(foo, prob=TRUE)
curve(dnorm(x, mean=mean(foo), sd=sd(foo)), add=TRUE)


## @knitr agg, comment="", prompt=TRUE
Sal$AnnualSalary <- as.numeric(gsub(Sal$AnnualSalary, pattern="$", replacement="", fixed=TRUE))
Sal$GrossPay <- as.numeric(gsub(Sal$GrossPay, pattern="$", replacement="", fixed=TRUE))
head(aggregate(Sal[, c("AnnualSalary", "GrossPay")], by=list(Sal$Agency), mean, na.rm=TRUE))


## @knitr cat.titanic, comment="", prompt=TRUE
DF <- data.frame(Titanic)
print(tab <- xtabs(Freq ~ Sex + Survived, DF))


## @knitr cat.titanic2, comment="", prompt=TRUE
fisher.test(tab)


## @knitr cat.titanic3, comment="", prompt=TRUE
chisq.test(tab)


## @knitr tt, comment="", prompt=TRUE
twolanes <- bike[ bike$type %in% c("BIKE LANE", "SHARE THE ROAD"),]
twolanes$type <- factor(twolanes$type)
t.test(twolanes$length[twolanes$type == "BIKE LANE"], twolanes$length[twolanes$type == "SHARE THE ROAD"])


## @knitr tt.formula, comment="", prompt=TRUE
t.test(length ~ type, data=twolanes)


## @knitr ttest, comment="", prompt=TRUE
bike$type[bike$type == ""] <-NA
bike$type <- factor(bike$type)
print(bike.anova <- aov(length ~ type, data = bike)) ## WHAT!! NO P-values!


## @knitr summ, comment="", prompt=TRUE
summary(bike.anova)
summary(aov(log(length) ~ type, data = bike)) #log transformed


## @knitr anova2, comment="", prompt=TRUE
summary(lm(length ~ type, data = bike))$fstat


## @knitr tt.wilcox, comment="", prompt=TRUE
t.test(length~type,data=twolanes)
wilcox.test(length ~ type, data=twolanes)


## @knitr tt.wilcox2, comment="", prompt=TRUE,fig.width=11,fig.height=5.5
par(mfrow = c(1,2))
fit=lm(length~type,data=twolanes)
hist(fit$resid,breaks=50,freq=F)
lines(density(rnorm(10000, mean=mean(fit$resid), sd=sd(fit$resid))),col="red")
plot(fit,2)


## @knitr nonpar.comp, comment="", prompt=TRUE
kruskal.test(length ~ type, data=twolanes)
wilcox.test(length ~ type, data=twolanes)


## @knitr kruskal, comment="", prompt=TRUE
kruskal.test(length ~ type, data = bike)
kruskal.test(log(length) ~ type, data = bike) ## same because only based on ranks


## @knitr friedman, comment="", prompt=TRUE
## from friedman.test documentation
head(RoundingTimes <-
matrix(c(5.40, 5.50, 5.55,
         5.85, 5.70, 5.75,
         5.20, 5.60, 5.50,
         5.55, 5.50, 5.40,
         5.90, 5.85, 5.70,
         5.45, 5.55, 5.60,
         5.40, 5.40, 5.35,
         5.45, 5.50, 5.35,
         5.25, 5.15, 5.00,
         5.85, 5.80, 5.70,
         5.25, 5.20, 5.10,
         5.65, 5.55, 5.45,
         5.60, 5.35, 5.45,
         5.05, 5.00, 4.95,
         5.50, 5.50, 5.40,
         5.45, 5.55, 5.50,
         5.55, 5.55, 5.35,
         5.45, 5.50, 5.55,
         5.50, 5.45, 5.25,
         5.65, 5.60, 5.40,
         5.70, 5.65, 5.55,
         6.30, 6.30, 6.25),
       nrow = 22, byrow = TRUE, dimnames = list(1 : 22, c("Round Out", "Narrow Angle", "Wide Angle"))), 2)
friedman.test(RoundingTimes)


## @knitr cars, prompt=TRUE,comment="",cache=TRUE
cars=read.csv("http://biostat.jhsph.edu/~ajaffe/files/kaggleCarAuction.csv",as.is=T)
summary(glm(IsBadBuy ~ VehBCost,data=cars, family="binomial"))


## @knitr for1, comment="",prompt=TRUE
for(i in 1:10) {
  print(i)
}


## @knitr for2, comment="",prompt=TRUE
Index = c(3,6,7,20,32,100,234,1000,6543)
for(i in 1:length(Index)) {
  print(Index[i])
}


## @knitr for3, comment="",prompt=TRUE
myList = vector("list",length=4)
mat1=matrix(rnorm(8), nc = 4)
mat2=matrix(rnorm(8), nc = 4)
mat1 
mat2


## @knitr for4, comment="",prompt=TRUE
for(i in seq(along=myList)) {
  myList[[i]] = cbind(mat1[,i],mat2[,i])
}
myList


## @knitr forCbind, comment="",prompt=TRUE
i=1
cbind(mat1[,i],mat2[,i])
i=2
cbind(mat1[,i],mat2[,i])
i=3
cbind(mat1[,i],mat2[,i])


## @knitr forDen2, comment="",prompt=TRUE,fig.width=5.5,fig.height=5.5,cache=TRUE
mat = matrix(rnorm(1000*50), nc = 50)
plot(density(mat[,1]), ylim = c(0,0.45))
for(i in 2:ncol(mat)) {lines(density(mat[,i]))}


## @knitr forList, comment="",prompt=TRUE
outList = vector("list",10)
start=1:10
end = sample(1:100, 10)
for(i in seq(along=outList)) {
  outList[[i]] = start[i]:end[i]
}
outList


## @knitr stopLoop, comment="",prompt=TRUE
dummy <- FALSE
for ( ii in 1:5 ) {
   for ( jj in 2:5 ) {
     cat("ii=",ii,"; jj=",jj,"\n",sep="")
     if ( ii == jj ) {
       dummy <- TRUE
       break
     }
   }
   if ( dummy ) break
}


## @knitr listF, comment="",prompt=TRUE
files = list.files("Reports", full.names=T)
length(files)
head(files)


## @knitr listF2, comment="",prompt=TRUE
name= sapply(strsplit(files,"/"), function(x) x[2])
name = sapply(strsplit(name,"\\."), function(x) x[1])
head(name)
names(files) = name
head(files)


## @knitr lapply1, comment="",prompt=TRUE
fileList = lapply(files, read.delim, header=T, as.is=T)
head(names(fileList))
head(fileList[[1]])


## @knitr lapply2, comment="",prompt=TRUE
fileList = lapply(files, read.delim, header=T, as.is=T)
head(names(fileList))
lapply(fileList,head,2)


## @knitr listOrder, comment="",prompt=TRUE
month=sapply(strsplit(name, "_"), function(x) x[1])
month = factor(month, levels = c("January","February","March","April","May","June","July","August",
                "September","October","November","December"))
year = as.integer(sapply(strsplit(name, "_"), function(x) x[2]))
fileList = fileList[order(year,month)]
names(fileList)


## @knitr numRow, comment="",prompt=TRUE
sapply(fileList, nrow)[1:10] # number of entries
sum(sapply(fileList, nrow)) # all reports


## @knitr tabReport1, comment="", prompt=TRUE
sapply(fileList, function(x) table(x$sex))


## @knitr tabReport2, comment="", prompt=TRUE
sapply(fileList, function(x) table(x$treat))


## @knitr tabReport3, comment="", prompt=TRUE
sapply(fileList, function(x) table(x$bgDrugs))


## @knitr tabReport4, comment="", prompt=TRUE
sapply(fileList, function(x) table(x$block))


## @knitr tabReport5, comment="", prompt=TRUE
sapply(fileList, function(x) quantile(x$age))


## @knitr tabReport6, comment="", prompt=TRUE
sapply(fileList, function(x) quantile(x$height))


## @knitr tabReport7, comment="", prompt=TRUE
sapply(fileList, function(x) quantile(x$bmi))


## @knitr table1a, comment="",prompt=TRUE
y = fileList[[1]]
y[1:5,]
cIndexes = split(1:nrow(y), y$treat) # splits 1st vector by levels of the 2nd
lapply(cIndexes,head) # indices for each outcome


## @knitr table1b, comment="",prompt=TRUE
mCont = sapply(cIndexes, function(x) colMeans(y[x,c("age","weight","height","bmi")]))
mCont # mean of continuous variables by outcome
sdCont =sapply(cIndexes, function(x) apply(y[x,c("age","weight","height","bmi")], 2 ,sd))
sdCont # sd of continuous variables by outcome


## @knitr table1c, comment="",prompt=TRUE
mat1 = matrix(paste(signif(mCont,4), " (SD=", signif(sdCont,2),")",sep=""), nc = 2)
dimnames(mat1) = dimnames(mCont) # copies row and column names
mat1


## @knitr table1d, comment="",prompt=TRUE
sex = sapply(cIndexes, function(x) table(y$sex[x]))
sex
sexF= signif(prop.table(sex,2),3)
sexF


## @knitr table1e, comment="",prompt=TRUE
mat1 = rbind(mat1,sexF[1,])
rownames(mat1)[nrow(mat1)] = "Sex (Female)"
mat1


## @knitr table1f, comment="",prompt=TRUE
pv = apply(y[,c("age","weight","height","bmi")], 2, function(x) t.test(x~y$treat)$p.value)
pv
pv = paste("p=",signif(pv,3),sep="")
pv
sexp = chisq.test(table(y$sex, y$treat))$p.value
sexp = paste("p=",signif(sexp,3),sep="")
sexp


## @knitr table1g, comment="",prompt=TRUE
pv = c(pv,sexp)
mat1 = cbind(mat1,pv)
colnames(mat1)[ncol(mat1)] = "p-value"
mat1


## @knitr table1h, comment="",prompt=TRUE
mat1 = rbind(mat1,c(sapply(cIndexes,length), nrow(y)))
rownames(mat1)[nrow(mat1)] = "Number"
mat1


## @knitr tabFunc, comment="",prompt=TRUE
# or we can write this as a general function
makeTable1 = function(y) {
  cIndexes = split(1:nrow(y), y$treat) 
  mCont = sapply(cIndexes, function(x) colMeans(y[x,c("age","weight","height","bmi")]))
  sdCont =sapply(cIndexes, function(x) apply(y[x,c("age","weight","height","bmi")], 2 ,sd))
  mat1 = matrix(paste(signif(mCont,4), " (SD=", signif(sdCont,2),")",sep=""), nc = 2)
  dimnames(mat1) = dimnames(mCont)
  sex = sapply(cIndexes, function(x) table(y$sex[x]))
  sexF= signif(prop.table(sex,2),3)
  apply(sexF, 2, function(x) paste(x[1], "M/",x[2],"F",sep=""))
  mat1 = rbind(mat1,sexF[1,])
  rownames(mat1)[nrow(mat1)] = "Sex (Female)"
  pv = apply(y[,c("age","weight","height","bmi")], 2, function(x) t.test(x~y$treat)$p.value)
  pv = paste("p=",signif(pv,3),sep="")
  sexp = chisq.test(table(y$sex, y$treat))$p.value
  sexp = paste("p=",signif(sexp,3),sep="")
  pv = c(pv,sexp)
  mat1 = cbind(mat1,pv)
  colnames(mat1)[ncol(mat1)] = "p-value"
  mat1 = rbind(mat1,c(sapply(cIndexes,length), nrow(y)))
  rownames(mat1)[nrow(mat1)] = "Number"
  return(mat1)
}



## @knitr tabFunc2, comment="",prompt=TRUE
tabList = lapply(fileList, makeTable1)
lapply(tabList,head,2)


## @knitr writeTab, comment="",prompt=TRUE, eval=FALSE
for(i in seq(along=tabList)) {
  fn = paste("Tables/",names(tabList)[i],"_table1.txt",sep="")
  write.table(tabList[[i]], fn,quote=F, sep="\t")
}


## @knitr docall, prompt=TRUE,comment=""
bigTab = do.call("rbind",fileList)
dim(bigTab)
class(bigTab)


## @knitr docall2, prompt=TRUE,comment=""
bigTab[1:10,]


## @knitr bigtab, prompt=TRUE,comment=""
makeTable1(bigTab)


## @knitr clean1, prompt=TRUE,comment=""
ss = function(x, pattern, slot=1,...) sapply(strsplit(x,pattern,...),function(y) y[slot])
month = ss(rownames(bigTab), "_", 1)
year = as.integer(ss(rownames(bigTab), "_", 2))
rownames(bigTab)=NULL
head(bigTab)
head(month)


## @knitr clean2, prompt=TRUE,comment=""
date = paste(month, " ", bigTab$recruitDate, ", ", year,sep="")
bigTab$Date = as.Date(date, format = "%B %d, %Y")
bigTab = bigTab[,names(bigTab) != "recruitDate"]
head(bigTab)


## @knitr clean3, prompt=TRUE,comment=""
bigTabDate = bigTab[order(bigTab$Date),]
head(bigTabDate)


## @knitr boxplots1, comment="",prompt=TRUE, fig.height=6, fig.width=12
par(mfrow = c(1,2))
boxplot(age ~ treat, data=bigTab,ylab="Age")
boxplot(bmi ~ bgDrugs, data=bigTab, ylab="BMI")


## @knitr plots1, comment="",prompt=TRUE, fig.height=6, fig.width=10
par(mfrow=c(1,1))
library(lattice)
xyplot(height ~ weight | block*treat, data=bigTab)


## @knitr plots2, comment="",prompt=TRUE, fig.height=6, fig.width=10
par(mfrow=c(1,1))
library(lattice)
xyplot(height ~ weight | bgDrugs*sex, data=bigTab)


