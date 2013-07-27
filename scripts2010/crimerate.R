
###############################################
## Day 2: Example: Crime
###############################################
cri<-read.table("crimerate.txt",header=TRUE)
summary(cri)
dim(cri)
lm1<-lm(R~.,cri)
summary(lm1)

lm2<-update(lm1,~.-NW)
summary(lm2)

lm3<-update(lm2,~.-LF)
summary(lm3)

lm4<-update(lm3,~.-N)
summary(lm4)

lm5<-update(lm4,~.-Ex1)
summary(lm5)

lm6<-update(lm5,~.-M)
summary(lm6)

lm7<-update(lm6,~.-S)
summary(lm7)

lm8<-update(lm7,~.-U1)
summary(lm8)

lm9<-update(lm8,~.-W)
summary(lm9)

## Too much work ...
pairs(cri)
lm1<-lm(R~.,cri)
lm2<-step(lm1)
summary(lm2)
lm3<-update(lm2,~.-W)
summary(lm3)
