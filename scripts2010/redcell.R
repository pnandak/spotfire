
###############################################
## Day 3: Redcell
###############################################
red<-read.table("redcell.txt",header=TRUE)
summary(red)
plot(folate~ventilation,red)
a1<-aov(folate~ventilation,red)
summary(a1)
summary.lm(a1)
levels(red$ventilation)
red$rVent<-relevel(red$ventilation,3)
levels(red$rVent)
a2<-lm(folate~rVent,red)
summary(a2)
