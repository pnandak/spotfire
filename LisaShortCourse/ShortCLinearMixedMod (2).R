library(lattice)
library(nlme)
library(xtable)
setwd('/Users/cirovelasco-cruz/Desktop/ascdata')
psid=read.table('psid.txt',header=T)
psid[1:20,]
par(mfrow=c(2,2))
setwd('/Users/cirovelasco-cruz/Desktop/NLmodelShortC')
pdf('income.pdf',height=5,width=7)
xyplot((income)~year|sex, group=person,psid,type='l', main='Income per person and gender')
dev.off()
pdf('logincome.pdf',height=5,width=7)
xyplot(log(income)~year|sex,group=person,subset=(person<=20),psid,type='l', main='log of the income for the first 20 people')
dev.off()

lmod1=lapply(levels(as.factor(psid$person)),function(x) lm(log(income)~I(year-78),data=psid,subset=(person==x)))

intercepts=unlist(lapply(1:length(lmod1),function(i) lmod1[[i]][[1]][[1]]))
slopes=unlist(lapply(1:length(lmod1),function(i) lmod1[[i]][[1]][[2]]))
pdf('Scatterplot.pdf')
plot(y=slopes,x=intercepts, main='Scatter plot', pch='*',cex=2,col='red')
identify(y=slopes,x=intercepts)
dev.off()
sex=psid$sex[match(1:85,psid$person)]

pdf('Boxplots.pdf',height=4,width=7)
par(mfrow=c(1,2))
boxplot(split(slopes,sex), main='Slope')
boxplot(split(intercepts,sex),main='Intercept')
dev.off()

t.test(slopes[sex=="M"],slopes[sex=='F'])
t.test(intercepts[sex=="M"],intercepts[sex=='F'])
#psid1sub=psid1[psid1$income<=50000,]
#psid1sub=psid1sub[psid1sub$income>5000,]
cyear=psid$year-78
psid1=cbind(psid,cyear)
mmodlin=lm(log(income)~cyear+sex+cyear*sex+age+educ,data=psid1)
xtable(mmodlin)
summary(mmodlin)
AIC(mmodlin)
mmod=lme(log(income)~cyear+sex+cyear*sex+age+educ,data=psid1,random=~1+cyear+sex|person, method='ML')
summary(mmod)
xtable(mmod)
mmodAR1=update(mmod,correlation=corAR1(form=~cyear|person))
summary(mmodAR1)
histogram(log(psid1sub$income))
plot.lme(mmodAR1,resid(.,type='p')~fitted(.)|sex,abline=0)
plot(mmodAR1,log(income)~fitted(.)|sex)
qqmath(~resid(mmodAR1),psid)
Box.test(resid(mmodAR1))
whitenoise=ar(resid(mmodAR1),order.max=2)
Box.test(whitenoise$resid)
xyplot( fitted(mmodAR1)~psid1sub$cyear|psid1sub$sex,group=psid1sub$person,type='l')+as.layer(
xyplot(log(income)~cyear|sex, data=psid1sub,type='o'))

