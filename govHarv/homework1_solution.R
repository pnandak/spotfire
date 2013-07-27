###################################################
### chunk number 1: Exercise1-CreateMatrix
###################################################
myMat<-cbind(rep(2:1, each=3), c(3:1,6:4),c(9, rep(1,2), 2,3,2)) 
myMat<-cbind(myMat, c(12,10,20,17,10,13))
myMat<-as.data.frame(myMat)
help(colnames)
colnames(myMat)<-c("case", "weeks", "id", "height")
rownames(myMat)<-LETTERS[1:6]
myMat


###################################################
### chunk number 2: Exercise1-ManipulateMatrixEvenRow
###################################################
myMat[seq(2, nrow(myMat),2),]


###################################################
### chunk number 3: Exercise1-ManipulateChangeElement
###################################################
myMat[2,4]<-20


###################################################
### chunk number 4: Exercise1-ManipulateMatrixSort
###################################################
myMatNew<-myMat[order(myMat$weeks, myMat$height),]
myMatNew


###################################################
### chunk number 5: Exercise1-boxplot
###################################################
par(mfrow=c(2,2))
tapply(myMat$height,myMat$case, boxplot)
boxplot(myMat$height~myMat$case)
boxplot(myMat$height~myMat$case, col=c("red","magenta"),
border="blue", xlab="CASE", ylab="HEIGHT", 
    names=c("Case1", "Case2"), main="Another Boxplot", 
    sub="with a few extra args")


###################################################
### chunk number 6: Exercise2
###################################################
data(women)

class(women)
mode(women)
length(women)
dim(women)
attributes(women)


###################################################
### chunk number 7: Exercise2-MoreComplicated
###################################################
for (i in c("class", "mode", "length", "dim")) 
print(paste(i, toString(get(i)(women)), sep=" : "))


###################################################
### chunk number 8: Exercise2-SummaryStats
###################################################
summary(women)
res= rbind(average=apply(women,2, mean), 
            median= apply(women,2, median),
            sd=apply(women,2, sd))
res


###################################################
### chunk number 9: Exercise2-Subset
###################################################
womenSub<-women[women$weight>124 & women$weight<150,]
mean(womenSub$height)
nrow(women[women$weight<120,])


###################################################
### chunk number 10: Exercise2-BMI
###################################################
ConversionFactor= 0.4536/0.0254^2
women$BMI = women$weight*ConversionFactor/women$weight


###################################################
### chunk number 11: Exercise2-BMI2
###################################################
ConversionFactor= 0.4536/0.0254^2
women$BMI = women$weight*ConversionFactor/women$weight


###################################################
### chunk number 12: Exercise2-AssignBMIfactor
###################################################
table(ifelse (women$BMI >= 18.5 & women$BMI <= 24.9, "Normal", "Not Normal"))

BMI<-cbind(minWeight=c(0,18.5, 25,30), 
      maxWeight=c(18.5, 24.9, 29.9, 50), 
      status=c("underweight", "normal", "overweight", "obese"))
women$status= sapply(women[,3], function(x) BMI[x>BMI[,1]  & x<BMI[,2],"status"] )
women


###################################################
### chunk number 13: Exercise3
###################################################
ids=1:20
dep= c(rep("Epi", 5), rep("Biostats", 4), rep("Env Health", 6), rep("other", 5))
status=c("reg", "reg" ,"reg" ,"aud" ,"aud","aud" ,"reg" ,"reg" ,"reg", "reg","reg" ,"reg" ,"aud", "aud","reg","reg", "aud" ,"reg" ,"reg" ,"reg")
students= cbind(ID=ids, Dept=dep, Status=status)
students= as.data.frame(students)
class(students)
students[1:2,]


###################################################
### chunk number 14: Exercise3-Sort
###################################################
students[order(students$Dept),]


###################################################
### chunk number 15: Exercise3-SubsettingIDs
###################################################
students[students$Status=="aud", "ID"]


###################################################
### chunk number 16: Exercise3-SortTabulate
###################################################
table(students$Dept, students$Status)


###################################################
### chunk number 17: Exercise4-ForLoop
###################################################
for (i in 1:10) print( 2^i )


###################################################
### chunk number 18: Exercise4-whileLoop
###################################################
x<-1
while(2^x<1000) {
   print(2^x)
   x<-x+1
   }



###################################################
### chunk number 19: Exercise5-twosam1
###################################################
twosam <- function(y1, y2) {
  n1 <- length(y1); n2 <- length(y2)
  yb1 <- mean(y1); yb2 <- mean(y2)
  s1 <- var(y1); s2 <- var(y2)
  s <- ((n1-1)*s1 + (n2-1)*s2)/(n1+n2-2)
  tst <- (yb1 - yb2)/sqrt(s*(1/n1 + 1/n2))
  return(tst)
}


###################################################
### chunk number 20: Exercise5-twosam1
###################################################
twosam2 <- function(y1, y2, paired=F)
{
  n1 <- length(y1); n2 <- length(y2)
  yb1 <- mean(y1); yb2 <- mean(y2)
  s1 <- var(y1); s2 <- var(y2)
  if (paired==F)
    {  s <- ((n1-1)*s1 + (n2-1)*s2)/(n1+n2-2)
       tst <- (yb1 - yb2)/sqrt(s*(1/n1 + 1/n2))
       p.val <- 2*(1 - pt(abs(tst), (n1+n2-2)))
    }
  else
    { if (n1==n2)
       { s <- var(y1-y2)
         tst <- (yb1-yb2)/sqrt(s/n1)
         p.val <- 2*(1 - pt(abs(tst), (n1-1)))
       }
      else stop('unequal vector lengths')
    }
list(tst=tst,p.val=p.val,pair=paired)
}


###################################################
### chunk number 21: Exercise5-twosam3
###################################################
twosam3 <- function(y1, y2, paired=F, sided="two")
{
  n1 <- length(y1); n2 <- length(y2)
  yb1 <- mean(y1); yb2 <- mean(y2)
  s1 <- var(y1); s2 <- var(y2)
  stat <-cbind(c(yb1, yb2), c(s1, s2))
  colnames(stat) = c("y1", "y2")
  rownames(stat) = c("mean", "variance")
  print("Summary Statistics")
  print(stat)
  if (paired==F)
    {  s <- ((n1-1)*s1 + (n2-1)*s2)/(n1+n2-2)
       tst <- (yb1 - yb2)/sqrt(s*(1/n1 + 1/n2))
       p.val <- 2*(1 - pt(abs(tst), (n1+n2-2)))
    }
  else
    { if (n1==n2)
       { s <- var(y1-y2)
         tst <- (yb1-yb2)/sqrt(s/n1)
         p.val <- 2*(1 - pt(abs(tst), (n1-1)))
       }
      else stop('unequal vector lengths')
    }
  if (!sided == "two")  {
       if  (tolower(sided) =="two")  p.val =p.val
       if  (tolower(sided) == "one")  p.val = p.val/2
       else {
            print("Input sided unknown. Returning a two-sided p.val")
            p.val =p.val
            }
     }
        
list(tst=tst,p.val=p.val,pair=paired, statistics=stat)
}


###################################################
### chunk number 22: Exercise5-TestTwoSam3
###################################################
x <- c(3.2, 4.5, 7.8, 0.4, 0.8, 5.8)
y <- c(0.2, 3.3, 3.4, 0.8, 1.2, 1.3, 0.6)
testTwoSided<-twosam3(x,y)
class(testTwoSided)
names(testTwoSided)
testTwoSided$p.val
t.test(x,y, alternative = "two.sided", var.equal = TRUE)


###################################################
### chunk number 23: Exercise5-Test2TwoSam3
###################################################
testOneSided<-twosam3(x,y, sided="one")
testOneSided
t.test(x,y, alternative =  "greater", var.equal=TRUE)


