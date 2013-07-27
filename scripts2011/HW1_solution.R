###################################################
### chunk number 1: Problem1
###################################################
#line 70 "HW1_solution.rnw"
women<-read.table("WomenStats.txt", header=TRUE)
class(women)
dim(women)
summary(women)


###################################################
### chunk number 2: Problem2
###################################################
#line 92 "HW1_solution.rnw"
newWoman<-c(66, 165,34)
women<-rbind(women, newWoman)
summary(women)


###################################################
### chunk number 3: Problem2.1
###################################################
#line 104 "HW1_solution.rnw"
women$weight<140
sum(women$weight<140)


###################################################
### chunk number 4: Problem2.1.1
###################################################
#line 112 "HW1_solution.rnw"
table(women$weight<140)

weight140 <- ifelse(women$weight<140, "under140", "over140")
table(weight140)


###################################################
### chunk number 5: Problem2.2.1
###################################################
#line 125 "HW1_solution.rnw"
index<- women$weight>135& women$weight<145
index
dim(women[index, ])


###################################################
### chunk number 6: Problem2.2.2
###################################################
#line 133 "HW1_solution.rnw"
index
mean(women[index, 1])
mean(women[index, "height"])
mean(women$height[index])


###################################################
### chunk number 7: Problem2.2.3
###################################################
#line 142 "HW1_solution.rnw"
help(subset)
subWomen<-subset(women, subset=women$weight>135& women$weight<145, select=height)
print(subWomen)
mean(subWomen)


###################################################
### chunk number 8: Problem2.3
###################################################
#line 153 "HW1_solution.rnw"
?colnames
help(colnames)


###################################################
### chunk number 9: Problem2.4
###################################################
#line 160 "HW1_solution.rnw"
rownames(women) <- LETTERS[1:nrow(women)]


###################################################
### chunk number 10: Problem2.5
###################################################
#line 167 "HW1_solution.rnw"
women["D",]
women["D", "age"] <-39
women["D",]


###################################################
### chunk number 11: Problem2.6
###################################################
#line 175 "HW1_solution.rnw"
newWomen<-women[order(women$weight),]
newWomen


###################################################
### chunk number 12: Problem3
###################################################
#line 193 "HW1_solution.rnw"
summary(women)


###################################################
### chunk number 13: note3.1
###################################################
#line 198 "HW1_solution.rnw"
colsd<-apply(women,2, sd)
signif(colsd, digits=3)


###################################################
### chunk number 14: note3.2
###################################################
#line 205 "HW1_solution.rnw"
apply(women,2, mean)
apply(women,2, mean, na.rm=TRUE)
apply(women,2, function(x) mean(x,na.rm=TRUE))


###################################################
### chunk number 15: note3.3
###################################################
#line 213 "HW1_solution.rnw"
apply(women,2, mean, na.rm=TRUE, trim=0.1)
apply(women,2, function(x) mean(x,na.rm=TRUE, trim=.1))


###################################################
### chunk number 16: Function3.4
###################################################
#line 221 "HW1_solution.rnw"

mySummary<-function(df) {
    colMeans<-apply(df,2, mean, na.rm=TRUE)
    colMedians<-apply(df,2, median, na.rm=TRUE)
    colSDs<-apply(df,2, sd, na.rm=TRUE)
    res<-rbind(mean=colMeans, median=colMedians, sd=colSDs)
    res<-apply(res,2, signif, digits = 3)
    return(res)
    }

mySummary(women)


###################################################
### chunk number 17: Problem4.1
###################################################
#line 247 "HW1_solution.rnw"

bmi<-function(weight, height) {
    bmi<- (weight/height^2)*703
    bmi<-  round(bmi, 2)
    return(bmi)
    }
       
bmi(weight=150, height=65)
bmi(150, 65)


###################################################
### chunk number 18: Problem4.2.1
###################################################
#line 265 "HW1_solution.rnw"
bmi(women$weight, women$height)
cat(paste("The BMI of woman", rownames(women),"is", bmi(women$weight, women$height), "\n"))



###################################################
### chunk number 19: Problem4.3.3
###################################################
#line 276 "HW1_solution.rnw"
bmiRes<-bmi(women$weight, women$height)
women<-cbind(women, bmi=bmiRes)
res<-sum(women$bmi<18.5 | women$bmi>24.9)
print(paste("The number of women with BMI values outside the normal range is:", res))


###################################################
### chunk number 20: Problem4.3.1
###################################################
#line 286 "HW1_solution.rnw"

for (i in 1:nrow(women))  {
     bmiRes<-bmi(women$weight[i], women$height[i])
     out<-ifelse(bmiRes<18.5 | bmiRes>24.9, "Not normal", "normal")
     print(out)
     }


###################################################
### chunk number 21: Problem4.3.2
###################################################
#line 298 "HW1_solution.rnw"

BMIres<-vector()

for (i in 1:nrow(women))  {
     bmiRes[i]<-bmi(women$weight[i], women$height[i])
     }

print(bmiRes)
print(paste("Number of abnormal bmi values: ", sum(bmiRes<18.5 | bmiRes>24.9)))


###################################################
### chunk number 22: Problem4.3.3
###################################################
#line 312 "HW1_solution.rnw"
res<- ifelse(bmiRes<18.5 | bmiRes>24.9, "abnormal BMI", "normal BMI")
table(res)


