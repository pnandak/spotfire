Lecture1 <- function()
{
 # Part1()
 # Part2()
 # Part3()
 # Part4()
 # Part5()
 # Part6()
 # Part7() 
  
}

# ============================================================================================================================================
Part1 <- function()
{
 par(mfrow=c(2,2))

 FuncAEP(1)
 FuncAEP(0.5)
 FuncAEP(2)
 FuncAEP(0.25)

}

# -------------------------------------------------------------------------------------------------------------------------------------------

FuncAEP <- function(Period)
{
 xx <- seq(from=0,to=2*pi,length=100)
 yy <- sin(xx/Period)
 plot(xx,yy,type="l",lwd=2,lty=1)

}
# ============================================================================================================================================

Part2 <- function()
{
 print(ComputeCV(1,2))
 print(ComputeCV(1))

 xx <- rnorm(100,0,1)
 sumstat <- ComputeStat(xx)
 print(sumstat)
 print(sumstat$sd)
}

ComputeCV <- function(mean,var=1)
{
 CV <- sqrt(var)/mean
 return(CV)
}

ComputeStat <- function(xx)
{
 average <- mean(xx)
 var <- var(xx)
 cv <- ComputeCV(average,var)
 cat(average,var,cv,"\n")
 
 out <- NULL
 out$mean <- average
 out$sd <- sqrt(var)
 out$CV <- cv
 return(out)
}


# ============================================================================================================================================

Part3 <- function()
{
 sinFun("G:\\tst.csv",0.25)

}

sinFun <- function(FileName,Period)
{
 xx <- seq(from=0,to=2*pi,length=100)
 yy <- sin(xx/Period)
 out <- cbind(xx,yy)
 write.table(out,file=FileName,sep=',') 

}

# ============================================================================================================================================

Part4 <- function()
{
 GetHisto(c(1,2,3,4,5,6),c(9,1,2,9,9,1))

}

# -------------------------------------------------------------------------------------------------------------------------------------------

GetHisto <- function(xx,yy)
{
 ymax <- max(yy);  xmax <- max(xx)
 plot(0,0,type="n",xlim=c(0.5,xmax+0.5),ylim=c(0,ymax*1.05),ylab="frequency",xlab="x",yaxs="i")
 xxx <- NULL; yyy <- NULL
 for (ii in 1:length(xx))
  {
   xxx <- c(xxx,c(xx[ii]-0.5,xx[ii]-0.5,xx[ii]+0.5,xx[ii]+0.5))
   yyy <- c(yyy,c(0,yy[ii],yy[ii],0))
  }
 lines(xxx,yyy) 

}

# ============================================================================================================================================

Part5 <- function()
{
 for (i in seq(from=1,to=5,by=1)) print(i)
 for (i in c(1,4,5,8,8,8)) print(i)
 for (i in c("1","N","L")) print(i)
 
}

# ============================================================================================================================================

Part6 <- function()
{
 x <- c(1,2,3)
 y <- c(3,4,5,6)
 plotxy(x,y)
}

# --------------------------------------------------------------------------------------------------------------------------------------------

plotxy <- function(x,y)
{
 if (length(x) == length(y))
  plot(x,y)
 else
  print("Lengths of x and y are not the same") 
}


# ============================================================================================================================================

Part7 <- function()
{
 Primes <- 2
 for (i in 3:100)
  {
   found <- F
   Ipnt = 1
   while (found == F & Ipnt <= length(Primes))
    {
     if (i %% Primes[Ipnt] == 0) found = T
     Ipnt <- Ipnt + 1
    } 
   if (!found) Primes <- c(Primes,i) 
  }
 print(Primes) 

}



# ============================================================================================================================================

Lecture1()
