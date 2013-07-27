comparisons <- function(number)
	{
		### the purpose of this function is give a demonstration of 
		### if ... else if ... else
		### ==, !=, <, <=, >, >=
		### &&, ||
		 
		### if ... else if ... else 
		if (number == 0)
			{
				cat(number,"equals 0\n")
			}
		else if (number > 0)
			{
				cat(number,"is positive\n")
			}
		else
			{
				cat(number,"is negative\n")
			}
		
		### if ... else 
		if (number != 1)
			{
				cat(number,"is not one\n")
			}
		else
			{
				cat(number,"is one\n")
			}
		
		### &&
		if ( (number > 0) && (number < 10) )
			{
				cat(number,"is between 0 and 10\n")
			}
		
		### ||
		### \n puts in a newline (carriage return/linefeed)
		### \t puts in a tab
		if ( (number >= 1) || (number <= 0) )
			{
				cat(number,"is greater than or equal to 1\n\tOR less than or equal to 0\n")
			}
		
		### ! is NOT, will be opposite of above result
		if (!( (number >= 1) || (number <= 0) ))
			{
				cat(number,"is between 0 and 1\n")
			}
	}
	
	
simple.nesting <- function(num.fam,num.child)
	{
		### this function writes a file of num.fam families
		### using cat() function
		### each with num.child children:
		### familyID, childID, x,
		### where x is sampled from a uniform distribution
		### between 10 and 20
		
		### outer loop is family
		for (i in 1:num.fam)
			{
				famID <- i
				
				### inner loop: for each family create num.child children
				for (j in 1:num.child)
					{
						kidID <- j
						
						### generate a random x value for each child
						### from a uniform distribution between 10 and 20
						x <- runif(1, min=10, max=20)
						
						### write this data to the next line of the file "nest.dat"
						cat(file="nest.dat",append=T,famID,kidID,x,"\n")
					
					} # end of inner loop j
			
			} # end of outer i
	
	} # end of function simple.nesting


simple.nesting2 <- function(num.fam,num.child)
	{
		### this function writes a file of num.fam families
		### using write.table() function
		### each with num.child children:
		### familyID, childID, x,
		### where x is sampled from a uniform distribution
		### between 10 and 20
		
		
		### create matrix for storing results
		n.cases <- num.fam * num.child
		result.mat <- matrix(0,ncol=3,nrow=n.cases)
		
		### line is pointer to file position
		line <- 0
		
		### outer loop is family
		for (i in 1:num.fam)
			{
				famID <- i
				
				### inner loop: for each family create num.child children
				for (j in 1:num.child)
					{
						kidID <- j
						
						### generate a random x value for each child
						### from a uniform distribution between 10 and 20
						x <- runif(1, min=10, max=20)
						
						### increment line pointer
						line <- line + 1
						
						### save this case into next row of result.mat
						result.mat[line,1] <- famID
						result.mat[line,2] <- kidID
						result.mat[line,3] <- x
					
					} # end of inner loop j
			
			} # end of outer i
		
		### write result to file
		write.table(result.mat,file="nest2.dat",row.names=F,col.names=F,quote=F)
		
	} # end of function simple.nesting2


simple.nesting3 <- function(num.fam,num.child)
	{
		### demonstration of appending to end of matrix
		### this function writes a file of num.fam families
		### each with num.child children:
		### familyID, childID, x,
		### where x is sampled from a uniform distribution
		### between 10 and 20
		
		
		### create empty object for storing results
		result.mat <- NULL
				
		### outer loop is family
		for (i in 1:num.fam)
			{
				famID <- i
				
				### inner loop: for each family create num.child children
				for (j in 1:num.child)
					{
						kidID <- j
						
						### generate a random x value for each child
						### from a uniform distribution between 10 and 20
						x <- runif(1, min=10, max=20)
												
						### save this case into next row of result.mat
						next.row <- c(famID,kidID,x)
						result.mat <- rbind(result.mat,next.row)
						
					} # end of inner loop j
			
			} # end of outer i
		
		### write result to file
		write.table(result.mat,file="nest3.dat",row.names=F,col.names=F,quote=F)
		
	} # end of function simple.nesting3


reg.data <- function(sample.size)
	{
		### this function writes some data
		### where y=b0 + b1x1 + b2x2 + error
		
		### put header on file
		cat(file="reg.dat",append=T,"id  x1  x2  y\n")
		
		for (i in 1:sample.size)
			{
			  ### x1 an integer 1 to 10
				x1 <- sample(1:10,1)
				
				### x2 is a real number between 50 and 100
				x2 <- runif(1,min=50,max=100)
				 
				### create y with b0=5, b1=2, b3=2, sigma=1				
				y <- 5 + 3 * x1 + 2 * x2 + rnorm(1,mean=0,sd=1)
				
				cat(file="reg.dat",append=T,i,x1,x2,y,"\n")
			}
	}


se.mean.basic <- function(num.samples=10)
{
	### This function repeatedly takes a sample of 100 
	### from a standard normal distribution,
	### and calculates the mean each time 
	### It returns the mean of these means 
	### as well as the standard deviation of these means
	
	
	## creates a vector of zeroes of the proper length 
	##   -- one place for each sample result
	
  mean.vec <- rep(0,num.samples)
  
  ## loop over number of samples
	
	for (count in 1:num.samples)
		{
			### sample of 100 from the std normal distribution 
			### and put results in vector x
			x <- rnorm(100,mean=0,sd=1)
			
			## get the mean of that sample and store it in the mean.vec
			mean.vec[count] <- mean(x)
		}
	
	### calculate the average of the means and sd of the means
	### (correcting so division by n, not n-1)
	avg.mean <- mean(mean.vec)
	sd.means <- sd(mean.vec)*((num.samples-1)/num.samples)
	
	### return results as a list
	return(list(num.samples=num.samples,theoretical.se=1/sqrt(100),
							mean.means=avg.mean,observed.se=sd.means))
}


look <- function(number,vec)
	{
		### finds first occurance of number in vector vec
		### returns position
		
		n <- length(vec)
		position <- 0
		
		for (i in 1:n)
			{
				if (vec[i] == number)
					{
						cat("found it!\n")
						position <- i
						
						### no reason to keep looking so drop out of loop
						break
					}
       } # end loop
       
     if ( position == 0 )
     	{
     	  cat("not found\n")
     		return(NULL)
     	}
     else 
     	{
     		return(position)
     	}
     	
  } # end function look


look2 <- function(number,vec)
	{
		### finds first occurance of number in vector vec
		### using a while loop
		### returns position
		
		n <- length(vec)
		position <- 0
		
		i <- 1
		while ( (i <= n) && (position == 0) )
			{
				if (vec[i] == number)
					{
						cat("found it!\n")
						position <- i
					} 
					
				### increment i
				i <- i + 1
				
       } # end while
       
     if ( position == 0 )
     	{
     	  cat("not found\n")
     		return(NULL)
     	}
     else 
     	{
     		return(position)
     	}
     	
  } # end function look2


