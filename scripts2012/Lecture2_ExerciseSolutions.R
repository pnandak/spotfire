###################################################
### chunk number 1: Exercise1
###################################################
myVec<-c(letters[1:20], seq(0,200,10))
cat(myVec, file="myVec.txt")
scan(file="myVec.txt", what="text")


###################################################
### chunk number 2: Exercise1.2
###################################################
scan(file="myVec.txt", what="text", n=10)


###################################################
### chunk number 3: Exercise2
###################################################
women<-read.table("http://compbio.dfci.harvard.edu/courses/R/WomenStats.txt", sep=" ", header=TRUE)
nrow(women)
ncol(women) 
colnames(women)
summary(women)
rownames(women)<- LETTERS[1:nrow(women)]
write.table(women, "modifedWomen.txt", sep="\t")
women2<-read.table("modifedWomen.txt", sep="\t", as.is=TRUE, header=TRUE)


###################################################
### chunk number 4: Exercise3
###################################################
for (i in 1:10) print(2^i)

x <- 1
while (2^x < 1000) {
     print(2^x)
     x <- x + 1
}


