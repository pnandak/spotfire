#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-31-01                                                   #
# UPDATE: 12-9-03, 12-18-05                                         #
# PURPOSE: Blood pressure example                                   #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#Create contingency table - notice the data is entered by columns
n.table<-array(data = c(716, 207, 79, 25, 819, 186, 67, 22), dim = c(2,2,2), 
        dimnames=list(Cholesterol = c("Normal", "High"), 
             BP = c("Normal", "High"), Personality = c("A", "B")))
n.table


#A different view of the data:
ftable(x = n.table)
ftable(x = n.table, row.vars = 1, col.vars = c(3,2))
ftable(x = n.table, row.vars = "Cholesterol", col.vars = c("Personality","BP"))

#Sum over the third variable
ftable(x = n.table, row.vars = "Cholesterol", col.vars = "BP")


all.data<-matrix(data = NA, nrow = 0, ncol = 3)

#Put data in "raw" form
for (i in 1:2) {
    for (j in 1:2) {
        for (k in 1:2) {
        all.data<-rbind(all.data, matrix(data = c(i, j, k), nrow = n.table[i,j,k], ncol = 3, byrow=T))
        }
    }
}

#Need in a data.frame format for the ftable() function call coming up
all.data2<-data.frame(cholestrol = all.data[,1], BP = all.data[,2], Personality = all.data[,3])

#Check data and another way to use ftable()
ftable(formula = Personality+BP~cholestrol, data = all.data2)


or.xy.1<-n.table[1,1,1]*n.table[2,2,1]/(n.table[1,2,1]*n.table[2,1,1])
or.xy.2<-n.table[1,1,2]*n.table[2,2,2]/(n.table[1,2,2]*n.table[2,1,2])

cat("Estimated OR for XY with Z=1 is:", round(or.xy.1,2), "\n")
cat("Estimated OR for XY with Z=2 is:", round(or.xy.2,2), "\n")

mantelhaen.test(x = all.data[,1], y = all.data[,2], z = all.data[,3], correct=F)

mantelhaen.test(x = n.table, correct=F)




###########################################################
#Common OR
#Two ways to calculate it:
OR1<-(n.table[1,1,1]*n.table[2,2,1]/sum(n.table[,,1])+n.table[1,1,2]*n.table[2,2,2]/sum(n.table[,,2])) /
   (n.table[1,2,1]*n.table[2,1,1]/sum(n.table[,,1])+n.table[1,2,2]*n.table[2,1,2]/sum(n.table[,,2]))

OR2<-sum(n.table[1,1,]*n.table[2,2,]/c(sum(n.table[,,1]), sum(n.table[,,2]))) / 
   sum(n.table[1,2,]*n.table[2,1,]/c(sum(n.table[,,1]), sum(n.table[,,2])))

OR2



#Below is the old way to calculate the C.I. for the MH OR.  
n.k<-c(sum(n.table[,,1]), sum(n.table[,,2]))

var.part1<-sum( (n.table[1,1,]+n.table[2,2,])*n.table[1,1,]*n.table[2,2,]  / n.k^2) /  
          (2*sum(n.table[1,1,]*n.table[2,2,]/n.k )^2)
             
var.part2<-sum( ((n.table[1,1,]+n.table[2,2,])*n.table[1,2,]*n.table[2,1,] + (n.table[1,2,]+n.table[2,1,])*n.table[1,1,]*n.table[2,2,])  / n.k^2) /
         (2*sum(n.table[1,1,]*n.table[2,2,]/n.k ) * sum(n.table[1,2,]*n.table[2,1,]/n.k ))

var.part3<-sum( (n.table[1,2,]+n.table[2,1,])*n.table[1,2,]*n.table[2,1,]  / n.k^2)  /
         (2*sum(n.table[1,2,]*n.table[2,1,]/n.k )^2)
var<-var.part1+var.part2+var.part3

var

alpha<-0.05
lower<-exp(log(OR2)-qnorm(1-alpha/2)*sqrt(var))
upper<-exp(log(OR2)+qnorm(1-alpha/2)*sqrt(var))

lower
upper
cat("The Wald interval for common OR is:", round(lower,4), "<= theta<-MH <=", round(upper,4))


#Below is an example of the newer way to calculate the C.I. for the MH OR:
mantelhaen.test(x = n.table, correct = F, conf.level = 0.90)




#############################################################################
# Test complete independence

save<-xtabs(~all.data[,1]+all.data[,2]+all.data[,3])
summary(save)

save<-table(all.data[,1], all.data[,2], all.data[,3])
summary(save)

I<-2
J<-2
K<-2

#find marginal totals
n.i<-c(sum(n.table[1,,]), sum(n.table[2,,]))
n.j<-c(sum(n.table[,1,]), sum(n.table[,2,]))
n.k<-c(sum(n.table[,,1]), sum(n.table[,,2]))
n<-sum(n.table)

mu.hat<-array(NA, c(2, 2, 2))

for (i in 1:2) {
    for (j in 1:2) {
        for (k in 1:2) {
            mu.hat[i,j,k]<-n.i[i]*n.j[j]*n.k[k]/n^2
        }
    }
}

pearson<-sum((n.table-mu.hat)^2/mu.hat)
lrt<-2*sum(n.table*log(n.table/mu.hat))

1-pchisq(q = pearson, I*J*K - I - J - K + 2)
1-pchisq(q = lrt, I*J*K - I - J - K + 2)


#Try finding mu.hat using Kronecker products - reason: faster than looping
#mu.hat vector
kronecker(kronecker(n.k, n.j), n.i)/n^2

mu.hat.array<-array(kronecker(kronecker(n.k, n.j), n.i)/n^2, dim=c(2,2,2), 
        dimnames=list(c("Cholesterol=Normal", "Cholesterol=High"), 
             c("BP=Normal", "BP=High"), c("Personality=A", "Personality=B")))

mu.hat.array
