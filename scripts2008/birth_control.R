#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-28-01                                                   #
# UPDATE: 12-8-03, 12-18-05                                         #
# PURPOSE: birth control example                                    #
#                                                                   #
# NOTES:                                                            #
#####################################################################

n.table<-array(data = c(49, 31, 46, 34, 21, 26, 8, 32, 4, 
                        49, 27, 55, 37, 22, 36, 16, 65, 17, 
                        19, 11, 25, 19, 14, 16, 15, 57, 16,
                         9, 11, 8, 7, 16, 16, 11, 61, 20),
                    dim=c(9,4), 
                    dimnames=list( Religous.attendance = c("Never", "<1 per year", "1-2 per year", "several times per year",
                    "1 per month", "2-3 per month", "nearly every week", "every week", 
                    "several times per week"), 
                    Teenage.birth.control = c("strongly agree", "agree", "disagree", "strongly disagree")))

######################################################
# Test for independence - Pearson

ind.test<-chisq.test(n.table, correct=F)
ind.test
mu.hat<-ind.test$expected


######################################################
# Test for independence - LRT



#easiest way
library(vcd)
assocstats(n.table)



######################################################
# Find residuals

n<-sum(n.table)
n.table
mu.hat
cell.dev<-n.table-mu.hat
cell.dev

pearson.res<-cell.dev/sqrt(mu.hat)
pearson.res

stand.res<-matrix(data = NA, nrow = 9, ncol = 4)
#find standardized residuals
for (i in 1:9) {
    for (j in 1:4) {
        stand.res[i,j]<-pearson.res[i,j]/sqrt((1-sum(n.table[i,]/n))*(1-sum(n.table[,j]/n)))
    }
}

stand.res

#########################################################
# ordinal measures

#scores 
u<-0:8
#u<-c(0, 1, 1.5, 7.5, 12, 25, 38.5, 52, 104) 
v<-1:4

all.data<-matrix(NA, 0, 2)

#Put data in "raw" form
for (i in 1:9) {
    for (j in 1:4) {
        all.data<-rbind(all.data, matrix(c(u[i], v[j]), n.table[i,j], 2, byrow=T))
    }
}

#find correlation
r<-cor(all.data) 
r

M.sq<-(sum(n.table)-1)*r[1,2]^2
M.sq
1-pchisq(M.sq,1)



##############################################################
# Additional plot to help see the positive dependence


all.data<-matrix(NA, 0, 2)

#Put data in "raw" form
for (i in 1:nrow(n.table)) {
    for (j in 1:ncol(n.table)) {
        all.data<-rbind(all.data, matrix(c(i, j), n.table[i,j], 2, byrow=T))
    }
}


plot(x = all.data[,1]+runif(n=nrow(all.data), min = -0.15, max = 0.15), 
     y = all.data[,2]+runif(n=nrow(all.data), min = -0.15, max = 0.15), 
     xlab = "Row number", ylab = "Column number")
abline(lm(all.data[,2]~all.data[,1]))
