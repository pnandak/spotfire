

check.CLT<-function(n.vector,no.of.sample,mean,sd){
	
	how.many<-length(n.vector)
	par(mfrow=c(ceiling(sqrt(how.many)),ceiling(sqrt(how.many))))
	
	for(i in 1:how.many){
		
		n<-n.vector[i]
		norm.data<-simulate.norm(no.of.sample,n,mean,sd)
		mean.vec<-rep(0,no.of.sample)
		for(j in 1:no.of.sample){
			mean.vec[j]<-mean(norm.data[j,])
		}
		hist(mean.vec,breaks=seq(-3,3,by=.25),main=paste("Distr. of Mean for n =",n))
		
	}
	
}

simulate.norm<-function(no.of.sample,n,mean,sd){
	
	data.matrix<-NULL
	for(j in 1:no.of.sample){
		data.matrix<-rbind(data.matrix,rnorm(n,mean,sd))
	}
	
	return(data.matrix)
	
}

