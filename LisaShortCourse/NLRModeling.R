library(nlme)
data(Soybean)
library(lattice)
xyplot((weight)~Time|Variety+Year,group=Plot,type='o',data=Soybean,pch=16,aspect=0.25,main='Profiles for the Soybeans Growth per Year and Variety')
getwd()
setwd('/Users/cirovelasco-cruz/Desktop/STATISTICS_VTECH/Advanced Regression/Final project')
pdf('growthdata.pdf')
xyplot((weight)~Time|Variety, group=Plot,type='o',data=Soybean,pch=16,aspect=0.25,main='Profiles for the Soybeans Growth per Variety, 1989',subset=Year==1989)
dev.off()

data1989=subset(Soybean,Year==1989)

write.csv(Soybean,'Soybean.csv')
indic=matrix(0,ncol=1,nrow=nrow(data1989))

indic[data1989$Variety=='P']=1
data1989=cbind(data1989,indic)


meanf1=function(t,z,b1s,b2s,b3s,b11,b22,b33){

	b1=b1s+b11*z
	b2=b2s+b22*z
	b3=b3s+b33*z
	f1=b1/(1+exp(-b3*(t-b2)))
	f1
	}
model3=nls(weight~meanf1(Time,indic,b1s,b2s,b3s,b11,b22,b33), 
#	fixed=list(b1s~1,b11~1,b2s~1,b22~1,b3s~1,b33~1),
#	random=list(a1~1,a2~1,a3~1),
	
	start=c(b1s=20.0,b11=1.0,b2s=52.31,b22=-1,b3s=.1,b33=-0.001),
	data=data1989)
summary(model3)	
setwd('/Users/cirovelasco-cruz/Desktop')
output=read.csv('output.csv')
xtable(output,digits=8)

	con1=nlmeControl(maxIter=500,msMaxIter=500)

meanf=function(t,z,b1s,b2s,b3s,b11,b22,b33,a1,a2,a3){
	b1=b1s+b11*z+a1
	b2=b2s+b22*z+a2
	b3=b3s+b33*z+a3
	f1=b1/(1+exp(-b3*(t-b2)))
	f1
	}
	con1=nlmeControl(maxIter=1000,msMaxIter=1000)
model3b=nlme(weight~meanf(Time,indic,b1s,b2s,b3s,b11,b22,b33,a1,a2,a3), 
	fixed=list(b1s+b11+b2s+b22+b3s+b33~1),
	random=list(a1~1,a2~1,a3~1),
	start=c(b1s=23.0,b11=5.0,b2s=50.31,b22=-1,b3s=.1,b33=-0.001),
	data=data1989,groups=~Plot,
	method='REML')
	,
	#control=con1)
	summary(model3b)
is.data.frame(data1989)	
model3bup=update(model3b,correlation=corAR1(form=~Time|Plot))

meanf=function(t,z,b1s,b2s,b3s,b11,b22,b33,a1,a2,a3){
	b1=b1s+b11*z+a1
	
	b2=b2s+b22*z+a2
	
	b3=b3s+b33*z+a3
	
	f1=b1/(1+exp(-b3*(t-b2)))
	f1
	}
model3b=nlme(weight~meanf(Time,indic,b1s,b2s,b3s,b11,b22,b33,a1,a2,a3), 
	fixed=list(b1s+b11+b2s+b22+b3s+b33~1),
	random=list(a1~1,a2~1,a3~1),
	start=list(fixed=c(b1s=10.0,b11=7.0,b2s=50,b22=-1,b3s=0.1,b33=-0.001)),
	data=data1989,groups=~Plot,
	method='ML',
	weights=varPower(.5),
		verbose=T,control=con1)

PopulationModel=model3b$fitted[,1]
residual=scale(model3b$residuals[,1])
PlotModel=model3b$fitted[,2]
	
fitt1=cbind(data1989, PopulationModel, PlotModel,residual)
setwd('/Users/cirovelasco-cruz/Desktop/NLmodelShortC')
pdf('dataandfit.pdf')
xyplot(weight~Time|Variety,data=fitt1,groups=Plot,type='o',main='Profiles raw data and fitted',ylab='Weight',
panel=function(x,y){
	panel.xyplot(x,y,pch=16,type='p',col='red')
	panel.xyplot(x,fitt1$PopulationModel,pch=3,type='o')})
dev.off()
#xyplot(scale(residual)~(1:8)|Variety,data=fitt1)
pdf('plotsperplot.pdf')
xyplot(PlotModel + PopulationModel ~Time|Plot,data=fitt1,type='o',pch=20, layout=c(4,4),auto.key=list(x=.6,y=.7,corner=c(-.5,13.5)),ylab='Weight',main='Profiles "Cluster specific" and "population curves", by Genotype')
dev.off()
	

	