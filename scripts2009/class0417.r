## a function that will convert minutes to hours and minutes
convertFunction <- function(mins){
  hours <- floor(mins/60)
  leftOverMinutes <- round(mins - hours*60)
  out <- paste(hours,"h",
               ":",
               leftOverMinutes,"m",
               sep="")
  out
}


## to use this function, first cut-and-paste the above
## function definition into R
## then something like the following should work:
convertFunction(63)

## or try this
library(UsingR)
attach(nym.2002)
convertFunction(time)
