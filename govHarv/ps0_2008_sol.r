#Gov 2001
#Problem Set 0 Solutions

## 0.1 a

## A function which calculates the mean of a vector is

my.mean <- function(x){ #x is a vector
	out <- sum(x) / length(x)  #adds the elements of x and divides by
					      #the total number of elements in x
	return(out)			   #returns the result of the last line
}

## 0.1 b

## A function which calculates the median of a vector is

my.median <- function(x){ #x is a vector
	ifelse(length(x) %% 2, out <- x[(length(x)/2) + 1], out <- c(x[length(x)/2], x[length(x)/2 + 1]))
	return(out)
}
## Here the function returns the middle element if the number of elements in x is odd,
## and returns a range of the middle elements if the number of elements in x is even

## 0.2 a

## To create a 100 * 100 matrix full of zeros, we can write
mat <- matrix(data = 0, nrow = 100, ncol = 100)

## To fill each column with a random draw of 100 numbers between 1 and 1000, 
##   you can use a for loop

for (i in 1:ncol(mat)){
	mat[,i] <- sample(x = 1:1000, size = 100, replace = FALSE)
}

## 0.2 b

## To display the histograms of the first 25 columns of the matrix, first
##   set the plotting canvas to display 25 plots, 5 rows by 5 columms with

par(mfrow = c(5,5))

## Now use a for loop to plot the histograms of the columns

for (i in 1:25){
	hist(mat[,i])
}


	