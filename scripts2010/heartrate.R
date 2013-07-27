###############################################
## Day 3: Heartrate
###############################################
hea<-read.table("heartrate.txt",header=TRUE)
summary(hea)
hea$subj<-factor(hea$subj)

hea<-read.table("heartrate.txt",header=TRUE,row.names=1)
summary(hea)

hea<-read.table("heartrate.txt",header=TRUE,row.names=1,colClasses=c(NA,"numeric","factor","factor"))
summary(hea)
a1<-aov(hr~subj*time,hea)
summary(a1)
summary.lm(a1)
a2<-aov(hr~subj+time,hea)
summary(a2)
summary.lm(a2)
anova(a1,a2)
a3<-aov(hr~time,hea)
anova(a3,a2)

summary.lm(a2)
hea$time2<-hea$time
levels(hea$time2)<-c("before",rep("after",3))
a2b<-aov(hr~subj+time2,hea)
summary(a2b)
summary.lm(a2b)
anova(a2,a2b)
anova(a2b,a2)

##Changing the order of the levels
hea$time<-factor(as.character(hea$time),levels=c("0","30","60","120"))
interaction.plot(hea$time,hea$subj,hea$hr)
interaction.plot(hea$time2,hea$subj,hea$hr)
