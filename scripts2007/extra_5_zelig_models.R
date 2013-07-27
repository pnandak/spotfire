source("http://svn.tables2graphs.com/tables2graphs/Rcode/Final%20Code%20for%20Website/plotReg.R")
library(Zelig)

##longley is a standard dataset included in R
m1 <- zelig(GNP~Unemployed,data=longley,model="ls") 
m2 <- zelig(GNP~Armed.Forces,data=longley,model="ls")
m3 <- zelig(GNP~Population,data=longley,model="ls")
m4 <- zelig(GNP~Unemployed+Armed.Forces,data=longley,model="ls")
m5 <- zelig(GNP~Armed.Forces+Population,data=longley,model="ls")
m6 <- zelig(GNP~Unemployed+Armed.Forces+Population,data=longley,model="ls")
plot.zelig.list(list(m1,m2,m3,m4,m5,m6),rot=0)


tmp <- plot.zelig.list(list(m1,m2,m3,m4,m5,m6),print=FALSE)
png(file="extra_longley.png",height=800,width=400,pointsize=1)
grid.draw(tmp)
graphics.off()
