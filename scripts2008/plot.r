### comment symbol

### plot first case
### life events and depression score on same plot

### first draft

### first plot life events 
plot(1:8,all[1,2:9],ylim=c(0,4),type="l",col="red")

### add line to plot with depression score * 10 to make it same in scale
lines(1:8,10*all[1,10:17],col="blue")


### second draft, add labels

### first plot life events 
plot(1:8,all[1,2:9],ylim=c(0,4),type="l",col="red",xlab="Year",
     ylab="Life Events (red) / Depression*10 (blue)", las=1,
     main=paste("Subject ",all$ID4[1]) )

### add line to plot with depression score * 10 to make it same in scale
lines(1:8,10*all[1,10:17],col="blue")


### third draft, make into pdf

### open pdf file
pdf(file="example.pdf",height=8,width=10)

### first plot life events 
plot(1:8,all[1,2:9],ylim=c(0,4),type="l",col="red",xlab="Year",
     ylab="Life Events (red) / Depression*10 (blue)", las=1,
     main=paste("Subject ",all$ID4[1]) )

### add line to plot with depression score * 10 to make it same in scale
lines(1:8,10*all[1,10:17],col="blue")

### close pdf file
dev.off()


### fourth draft, make 4 plots on one page

### open pdf file
pdf(file="example2.pdf",height=8,width=10)

par(mfrow=c(2,2))

### first case: plot life events 
plot(1:8,all[1,2:9],ylim=c(0,4),type="l",col="red",xlab="Year",
     ylab="Life Events (red) / Depression*10 (blue)", las=1,
     main=paste("Subject ",all$ID4[1]) )

### add line to plot with depression score * 10 to make it same in scale
lines(1:8,10*all[1,10:17],col="blue")

### second case: plot life events 
plot(1:8,all[2,2:9],ylim=c(0,4),type="l",col="red",xlab="Year",
     ylab="Life Events (red) / Depression*10 (blue)", las=1,
     main=paste("Subject ",all$ID4[2]) )

### add line to plot with depression score * 10 to make it same in scale
lines(1:8,10*all[2,10:17],col="blue")

### third case: plot life events 

case <- 3
plot(1:8,all[case,2:9],ylim=c(0,4),type="l",col="red",xlab="Year",
     ylab="Life Events (red) / Depression*10 (blue)", las=1,
     main=paste("Subject ",all$ID4[case]) )

### add line to plot with depression score * 10 to make it same in scale
lines(1:8,10*all[case,10:17],col="blue")

case <- 4
plot(1:8,all[case,2:9],ylim=c(0,4),type="l",col="red",xlab="Year",
     ylab="Life Events (red) / Depression*10 (blue)", las=1,
     main=paste("Subject ",all$ID4[case]) )

### add line to plot with depression score * 10 to make it same in scale
lines(1:8,10*all[case,10:17],col="blue")

### close pdf file
dev.off()


## fifth draft, make 4 plots on one page using loop, fix ylimit

y.limit <- max( c( max(all[1:4,2:9], na.rm=T), max(10*all[1:4,10:17],na.rm=T) ) )

### open pdf file
pdf(file="example3.pdf",height=8,width=10)

par(mfrow=c(2,2))

for (case in 1:4)
	{
		
		###plot life events 
		plot(1:8,all[case,2:9],ylim=c(0,y.limit),type="l",col="red",xlab="Year",
         ylab="Life Events (red) / Depression*10 (blue)", las=1,
         main=paste("Subject ",all$ID4[1]) )

		### add line to plot with depression score * 10
		lines(1:8,10*all[case,10:17],col="blue")
	}

### close pdf file
dev.off()



## sixth draft, make 4 plots on one page, ylimit optimal for each plot

### open pdf file
pdf(file="example5.pdf",height=8,width=10)

par(mfrow=c(2,2))

for (case in 1:4)
	{
		### set y limit
		y.limit <- max( c( max(all[case,2:9], na.rm=T), max(10*all[case,10:17],na.rm=T) ) )
		
		### plot life events
		plot(1:8,all[case,2:9],ylim=c(0,y.limit),type="l",col="red",xlab="Year",
		     ylab="Life Events (red) / Depression*10 (blue)", las=1,
		     main=paste("Subject ",all$ID4[case]) )
		
		### add line to plot with depression score * 10
		lines(1:8,10*all[case,10:17],col="blue")
	}
	
### close pdf file
dev.off()



### seventh draft -- all cases, 16 to a page

### open pdf file
pdf(file="example6.pdf",height=8,width=10)

par(mfrow=c(4,4))

for (case in 1:240)
	{
		### set y limit
		y.limit <- max( c( max(all[case,2:9], na.rm=T), max(10*all[case,10:17],na.rm=T) ) )
		
		### plot life events
		plot(1:8,all[case,2:9],ylim=c(0,y.limit),type="l",col="red",xlab="Year",
		     ylab="Life Events (red) / Depression*10 (blue)", las=1,
		     main=paste("Subject ",all$ID4[case]) )
		
		### add line to plot with depression score * 10 to make it same in scale
		lines(1:8,10*all[case,10:17],col="blue")
	}
	
### close pdf file
dev.off()


### eighth draft -- all cases, 16 to a page, fix all NA's, and y-axis title size

### open pdf file
pdf(file="example7.pdf",height=8,width=10)

par(mfrow=c(4,4))

for (case in 1:240)
	{
		### set y limit
		y.limit <- max( c( max(all[case,2:9], na.rm=T), max(10*all[case,10:17],na.rm=T) ) )
		if (y.limit <= 0) y.limit <- 10
		
		### plot life events
		plot(1:8,all[case,2:9],ylim=c(0,y.limit),type="l",col="red",xlab="Year",
		     ylab="LE (red) / Dep*10 (blue)", las=1,
		     main=paste("Subject ",all$ID4[case]) )
		
		### add line to plot with depression score * 10 to make it same in scale
		lines(1:8,10*all[case,10:17],col="blue")
	}
	
### close pdf file
dev.off()


### plot sample of cases

### get a sample of 20 cases
set.seed(314159)
the.cases <- sample(1:240,20)

### open pdf file
pdf(file="sample20.pdf",height=8,width=10)

par(mfrow=c(2,2))

for (case in the.cases)
	{
		### set y limit
		y.limit <- max( c( max(all[case,2:9], na.rm=T), max(10*all[case,10:17],na.rm=T) ) )
	  if (y.limit <= 0) y.limit <- 10
	
		### plot life events
		plot(1:8,all[case,2:9],ylim=c(0,y.limit),type="l",col="red",xlab="Year",
		     ylab="Life Events (red) / Depression*10 (blue)", las=1,
		     main=paste("Subject ",all$ID4[case]) )
		
		### add line to plot with depression score * 10 to make it same in scale
		lines(1:8,10*all[case,10:17],col="blue")
	}
	
### close pdf file
dev.off()







