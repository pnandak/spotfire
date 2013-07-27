#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  2-13-06                                                    #
# UPDATE:                                                           #
# PURPOSE: Show a QQ-plot for a sample from a chi-square            #
#                                                                   #
# NOTES:                                                            #
#####################################################################



df<-1
B<-1000

set.seed(8912)
x.axis<-sort(rchisq(n = B, df = df))  #Simulate observations from a chi-square and sort it
#x.axis<-sort(rnorm(n = B, mean = 0, sd = 1))  #Try normal
y.axis<-qchisq(p = seq(from = 1/(B+1), to = 1-1/(B+1), by = 1/(B+1)), df = df) #Find the quantiles of B evenly spaced points

#Examples
1/1001
y.axis[1] #0.000999 quantile from a chi-square(df)
x.axis[1] #0.000999 quantile from sample

951/1001  
y.axis[951] #0.95005 quantile from a chi-square(df)
x.axis[951] #0.95005 quantile from sample


#QQ-plot
par(pty = "s") #Make axes square; default is "m" for maximize size
plot(x = x.axis, y = y.axis, main = "QQ-Plot for chi-square distribution")
abline(a = 0, b = 1) #y = a + bx line with a = 0 and b = 1

#Put all the results together and examine parts of it
save<-data.frame(p = round(seq(from = 1/(B+1), to = 1-1/(B+1), by = 1/(B+1)),4), 
                 rand.chi = round(x.axis, 2), true.chi = round(y.axis,2))
save[300:325,]
save[700:725,]
save[900:925,]
save[975:1000,]
