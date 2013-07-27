

plot.within.aplot <- function()
{

par(pin=c(1.5,1.5))
par(mai=c(2.8,1.2,1,2.6))
par(ps=6)
plot(year,dollars,col="blue",type="l",xlab="",ylab="")
par(new=T)
par(ps=12)
par(pin=c(4.14,3.57))
par(mai=c(.956,.769,.769,.394))
plot(year,dollars,col="green",type="l")

}


try.pch <- function(pch.number)
{
  ## This function takes an integer and plots a line with
  ## the point character corresponding to that integer
  
	x <- 1:5
	y <- 2*x +3
	plot(x,y,pch=pch.number)
	lines(x,y,col="red")
	txt <- paste("pch =",pch.number)
	text(2,11,txt)
}



show.all.pch <- function()
{  
  ### demonstration of a function that calls a function
  ### series of graphs showing all of the different point characters
  ### in sequence
  
	for (i in 0:25) #loop over range of valid pch values
		{	
			try.pch(i)	#call function that does the graphing 
			cat("pch=",i,"\n")  #print number of pch
			par(ask=TRUE) #ask for user <return> before showing next graph
		} 
		
	par(ask=FALSE) # reset this graphing environment parameter back to its default	
}
