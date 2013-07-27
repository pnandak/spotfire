#################################################
## R commands for PS151B, Monday April 10, 2006
##
## simon jackman, dept of polisci, stanford univ
#################################################

## following p9 of Verzani

whales <- c(74, 122, 235, 111, 292, 111, 211, 133, 156, 79)

simpsons <- c("Homer","Marge","Bart","Lisa","Maggie")

## simple statistical stuff for quantitative variable, whales
## n.b., can't do this for qualitative variable, simpsons
S <- sum(whales)
n <- length(whales)
average <- S/n              ## compute the average "by hand"
mean(whales)                 ## use the built-in R function

sort(whales)                   ## from lowest to highest
min(whales)
max(whales)
range(whales)                 ## min and max
diff(whales)                    ## year-to-year change
cumsum(whales)            ## cumulative sum of whales

plot(whales,type="b")    ## plot a time series of whales

## Florida whale beachings
whales.fla <- c(89,254,306,292,274,233,294,204,204,90)

## difference between the two sets of whale beachings
d <- whales - whales.fla   
newVariable <- d
