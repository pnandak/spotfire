add.even <- function(n)
	{
		### n is expected to be an integer
		### function returns the sum of the first n even integers
		
		## for storing running total
		sum.even <- 0 
		
		## for storing the next even integer
		even.int <- 0
		
		for (i in 1:n)
			{
			  ### even.int will contain the ith even integer
			  even.int <- even.int + 2
			  
			  ### add this even.int to the running total
				sum.even <- sum.even + even.int
			}
	
	  ### return sum
		return(sum.even)
	}


add.even.show <- function(n)
	{
		### n is expected to be an integer
		### function returns the sum of the first n even integers
		
		## for storing running total
		sum.even <- 0 
		
		## for storing the next even integer
		even.int <- 0
		
		cat("before loop starts: \n")
		cat("even.int=",even.int,"sum.even=",sum.even,"\n")
		
		for (i in 1:n)
			{
			  ### even.int will contain the ith even integer
			  even.int <- even.int + 2
			  
			  ### add this even.int to the running total
				sum.even <- sum.even + even.int
				
				### print out the results of each interation of the loop
				cat("i=",i,"even.int=",even.int,"sum.even=",sum.even,"\n")
			}
	
	  ### return sum
		return(sum.even)
	}


mean.sd.min.max <- function(x)
	{
		### x is expected to be a numeric vector
		### function returns the mean, sd, min, and max of the vector x
		
		the.mean <- mean(x)
		the.sd <- sd(x)
		the.min <- min(x)
		the.max <- max(x)
		
		return(list(average=the.mean,stand.dev=the.sd,minimum=the.min,
								maximum=the.max))
	}
	

recode.income <- function(filename)
	{
		### assumes file filename readable by read.table, no header
		### first column is id 
		### second column is annual income in thousands of dollars
		### no missing values
		### recodes income into five categories
		### 1:  income <= 10K or income >=100K
		### 2:  income between 10K  and 20K or 80K and 100K
		### 3:  income between 20K and 30K or 60K and 80K
		### 4:  income between 30K  and 40K or 50K and 60K
		### 5   income between 40K and 50K
		
		### read data
		dat <- read.table(file=filename,header=F)

    ### add column names
		colnames(dat) <- c("id","income")
		
		### get number of cases
		n <- nrow(dat)
		
		### create vector to store recodes in
		income.cat <- vector(mode="numeric",length=n)
		
		### loop thru all the cases recoding income
		for (i in 1:n)
			{
				if ( (dat$income[i] <= 10) || (dat$income[i] > 100) )
					{
						income.cat[i] <- 1
					}
					
				if ( ((dat$income[i] > 10) && (dat$income[i] <= 20)) 
					 || ((dat$income[i] > 80) && (dat$income[i] <= 100)) )
					{
						income.cat[i] <- 2
					}
				
				if ( ((dat$income[i] > 20) && (dat$income[i] <= 30)) 
					 || ((dat$income[i] > 60) && (dat$income[i] <= 80)) )
					{
						income.cat[i] <- 3
					}

				if ( ((dat$income[i] > 30) && (dat$income[i] <= 40)) 
					 || ((dat$income[i] > 50) && (dat$income[i] <= 60)) )
					{
						income.cat[i] <- 4
					}
					
				if ( (dat$income[i] > 40) && (dat$income[i] <= 50) ) 
					{
						income.cat[i] <- 5
					}
			}
		
		### add a new column to the dataframe
		dat$incat <- income.cat		
		
		### create a filename for the output file
		outfilename <- paste(filename,".recoded",sep="")
		
		### write an ascii text file with a header
		write.table(dat,file=outfilename,quote=F,row.names=F)
		
		### return the vector of recoded income data
		return(dat)					
	}