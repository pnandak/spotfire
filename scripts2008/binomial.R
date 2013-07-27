#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-19-01                                                   #
# UPDATE: 12-7-03, 12-2-06, 12-13-07                                #
# PURPOSE: Find binomial distribution                               #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#n=5, pi=0.6
pdf<-dbinom(x = 0:5, size = 5, prob = 0.6) 
pdf

pdf<-dbinom(0:5, 5, 0.6) 
pdf


#Make the printing look a little nicer
save<-data.frame(Y=0:5, prob=round(pdf,4))
save 

plot(x = save$Y, y = save$prob, type = "h", xlab = "Y", ylab = "P(Y=y)", 
     main = "Plot of a binomial distribution for n=5, pi=0.6", 
     panel.first=grid(col="gray", lty="dotted"), lwd = 2)
abline(h = 0)

#Simulate observations from a Binomial distribution
set.seed(4848)
bin5<-rbinom(n = 1000, size = 5, prob = 0.6)
bin5[1:20]

mean(bin5)
var(bin5)

hist(bin5, main = "Binomial with n=5, pi=0.6, 1000 bin. observations", col = "blue")


#Better plot
save.count<-table(bin5)
save.count
barplot(height = save.count, names = c("0", "1", "2", "3", "4", "5"), main = "Binomial with n=5, pi=0.6, 1000 bin. observations", xlab = "x")
