# Read data
d.salsolinol<-read.csv("data/salsolinol.csv")

# Make scatterplot matrix and transformation
pairs(d.salsolinol[,-1],pch=16)
d.salsolinol.log<-d.salsolinol
d.salsolinol.log[,-1]<-log(d.salsolinol[,-1])
pairs(d.salsolinol.log[,-1],pch=16)

# Compute summary statistics
options(digits=2)
mean(d.salsolinol.log[,-1])
sd(d.salsolinol.log[,-1])
cor(d.salsolinol.log[,-1])

mean(d.salsolinol.log[d.salsolinol.log[,1]==1,-1])
sd(d.salsolinol.log[d.salsolinol.log[,1]==1,-1])
cor(d.salsolinol.log[d.salsolinol.log[,1]==1,-1])

mean(d.salsolinol.log[d.salsolinol.log[,1]==2,-1])
sd(d.salsolinol.log[d.salsolinol.log[,1]==2,-1])
cor(d.salsolinol.log[d.salsolinol.log[,1]==2,-1])

# Plot time series
ggpcp(d.salsolinol.log[d.salsolinol.log[,1]==1,],scale="I",
      vars=c("Day1","Day2","Day3","Day4")) + geom_line()
ggpcp(d.salsolinol.log[d.salsolinol.log[,1]==2,],scale="I",
      vars=c("Day1","Day2","Day3","Day4")) + geom_line()
ggpcp(d.salsolinol.log,
      vars=c("Day1","Day2","Day3","Day4")) +
      geom_line(aes(col=factor(Group))) # On the same plot, colored by Group

par(mfrow=c(2,1))
plot(c(1,4),range(d.salsolinol.log[,-1]),xlab="Variable",ylab="Salsolinol",
     type="n",main="Moderate")
for (i in 1:6)
  lines(1:4,d.salsolinol.log[i,-1])
plot(c(1,4),range(d.salsolinol.log[,-1]),xlab="Variable",ylab="Salsolinol",
     type="n",main="Severe")
for (i in 7:14)
  lines(1:4,d.salsolinol.log[i,-1])


# Compute test statistic
summary(manova(cbind(Day1,Day2,Day3,Day4)~Group,data=d.salsolinol.log),test="Wilks")

# Compute ANOVA for each variable
summary(aov(Day1~Group,data=d.salsolinol.log))
summary(aov(Day2~Group,data=d.salsolinol.log))
summary(aov(Day3~Group,data=d.salsolinol.log))
summary(aov(Day4~Group,data=d.salsolinol.log))

