# SIMPLE BINOMIAL CALCULATIONS
# 
# Calculate p(y) for y=0,1,...,5 when N=5 and pi=0.5
# (dbinom() is the binomial probability function or "density"):
dbinom(0:10, 10, 0.5)
# 
# Make a table of the distribution:
round( cbind(0:10, dbinom(0:10, 10, 0.5)), 4)
#
# See how things change when pi=1/6:
round(cbind(0:10, dbinom(0:10, 10, 1/6)), 4)
#
# Plot the two distributions:
plot(0:10,dbinom(0:10,10,0.5),type="h",xlab="y",ylab="p(y)",main="Binomial Distribution (N=10, pi=1/2)")
plot(0:10,dbinom(0:10,10,1/6),type="h",xlab="y",ylab="p(y)",main="Binomial Distribution (N=10, pi=1/6)")

