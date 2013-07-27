

means.and.sds<-function(matrix){

	col.num<-ncol(matrix)
	mean.vec<-sd.vec<-rep(0,col.num)

	for(i in 1:col.num){

		mean.vec[i]<-mean(matrix[,i],na.rm=T)
		sd.vec[i]<-sd(matrix[,i],na.rm=T)

	}

	return(mean.vec,sd.vec)

}

counts.and.percs<-function(vector){


	categ<-unique(vector)
	n.categ<-length(categ)
	
	count.vec<-perc.vec<-rep(0,n.categ)

	for(i in 1:n.categ){

		count.vec[i]<-sum(vector==categ[i])
		perc.vec[i]<-count.vec[i]/length(vector)

	}
	return(count.vec,perc.vec)

}


simulate.norm<-function(sizes,means,sds){

	no.sim<-length(sizes)
	
	par(mfrow=c(ceiling(sqrt(no.sim)),ceiling(sqrt(no.sim))))
	

	for(i in 1:no.sim){

		data<-rnorm(sizes[i], means[i], sds[i])
		density<-dnorm(data,means[i],sds[i])

		plot(data,density,col=i,pch=16,xlim=c(-4,4),ylim=c(0,1.5))
	}


}










