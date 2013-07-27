

###########################################################	
##Function 1
##Arguments:  vector:    list of numbers to be examined

##Output:     the third smallest number in the vector
###########################################################            


third.smallest<-function(vector){

	n<-length(vector)

	if(n>2){

		sorted.vec<-sort(vector)
		return(sorted.vec[3])
	}
	else cat("Vector must have 3 or more numbers","\n")


}


###########################################################	
##Function 2
##Arguments:  vector:    list of numbers to be sampled from
##	      n.sample:  how many samples we'll take

##Output:     orig.mean: the mean of the original vector
###########################################################            


bootstrap<-function(vector,n.sample){


	#The length of the original vector
	n<-length(vector)

	#Initializing the space for the sampled vectors' means.
	sampled.mean.vec<-rep(0,n.sample)

	#Sampling the data and finding the samples' means
	for(i in 1:n.sample){

		sampled.vec<-sample(vector,n,replace=T)
		sampled.mean.vec[i]<-mean(sampled.vec)

	}

	#plotting a histogram of the sampled means
	hist(sampled.mean.vec)

	#the original mean
	orig.mean<-mean(vector)

	return(orig.mean)
		
}



###########################################################	
##Function 3
##Arguments:  matrix to be sorted by columns
 
##Output:     matrix where each column is sorted     
###########################################################            

sort.by.columns<-function(matrix){

	#dimensions of the matrix

	n<-nrow(matrix)
	p<-ncol(matrix)

	#the space for the new matrix

	sort.matrix<-matrix(0,n,p)

	# sorting column by column
	for(i in 1:p){

	   col.to.be.sorted<-matrix[,i]
	   sort.matrix[,i]<-sort(col.to.be.sorted)

	}
	
	return(sort.matrix)


}

###########################################################	
##Function 4
##Arguments: vector:       list of numbers 

##Output:    cat.vector:   list of quartile membership     
###########################################################            

categ.by.quartile<-function(vector){

	#dimensions/making space
	n<-length(vector)
	cat.vector<-rep(0,n)

	##Finding the summary measures of the vector 
	##(Min, 1st Q, 2nd Q(Median), Mean, 3rd Q, Max)
	
	sum.vec<-summary(vector)
	quartile1<-sum.vec[2]
   	quartile2<-sum.vec[3]
	quartile3<-sum.vec[5]


	#assigning the quartiles
	cat.vector[vector <= quartile1]<-1
	cat.vector[vector > quartile1 & vector <= quartile2]<-2
	cat.vector[vector > quartile2 & vector <= quartile3]<-3
	cat.vector[cat.vector==0]<-4


	return(cat.vector)			

}





