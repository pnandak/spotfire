#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-26-01                                                   #
# UPDATE : 12-8-03, 12-15-05                                        #
# PURPOSE: Polio example                                            #
#                                                                   #
# NOTES:                                                            #
#####################################################################
 
n.table<-array(data = c(57, 142, 200688, 201087), dim = c(2,2), dimnames = list(Trt = c("vaccine", "placebo"), 
              Result = c("polio", "polio free")))
n.table


theta.hat<-n.table[1,1]*n.table[2,2]/(n.table[1,2]*n.table[2,1])
theta.hat
1/theta.hat

alpha<-0.05
lower<-exp(log(theta.hat)-qnorm(1-alpha/2)*
      sqrt(1/n.table[1,1] + 1/n.table[2,2] + 1/n.table[1,2] + 1/n.table[2,1]))
upper<-exp(log(theta.hat)+qnorm(1-alpha/2)*
      sqrt(1/n.table[1,1] + 1/n.table[2,2] + 1/n.table[1,2] + 1/n.table[2,1]))
cat("The Wald interval for OR is:", round(lower,4) , "<= theta <=", round(upper,4))

#Invert
cat("The Wald interval for 1/OR is:", round(1/upper,4) , "<= 1/theta  <=", round(1/lower,4))


######################################################
# Test for independence - Pearson chi-square

#The chisq.test function is in the ctest package.  
ind.test<-chisq.test(x = n.table, correct=F)
ind.test
ind.test$expected

#critical value
qchisq(p = 0.95, df = 1)
1-pchisq(q = ind.test$statistic, df = 1)


######################################################
# Test for independence - LRT


#easiest way
library(vcd)
assocstats(n.table)


#Different way
mu.hat<-matrix(data = NA, nrow = 2, ncol = 2)
n<-sum(n.table)
#Initialize G^2
G.sq<-0

#for loop to find the expected cell counts under independence
#  and G^2
for (i in 1:2) {
    for (j in 1:2) {
        mu.hat[i,j]<-sum(n.table[i,])*sum(n.table[,j])/n
        G.sq<-2*n.table[i,j]*log(n.table[i,j]/mu.hat[i,j])+G.sq
    }
}

mu.hat
G.sq
1-pchisq(q = G.sq, df = 1)

#See p. 146 of Agresti
loglin(n.table, list(1,2))


######################################################
# Find residuals

#General way
n.table
mu.hat
cell.dev<-n.table-mu.hat
cell.dev

pearson.res<-cell.dev/sqrt(mu.hat)
pearson.res

stand.res<-matrix(data = NA, nrow = 2, ncol = 2)
#find standardized residuals
for (i in 1:2) {
    for (j in 1:2) {
        stand.res[i,j]<-pearson.res[i,j]/sqrt((1-sum(n.table[i,]/n))*(1-sum(n.table[,j]/n)))
    }
}

stand.res
