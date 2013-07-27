# Entering the matrix of counts.
counts <- matrix(c(371,49,74,250,45,71,64,9,15,25,5,13),byrow=T,ncol=3)
counts
# Not needed, but it makes the output easier to read.
dimnames(counts) <- list(NULL,belief=c("yes","undecided","no"))
counts
# Note that multinom (the function we will use to fit the model) 
# treats the first level as the baseline (do we notice a pattern here?).
# So if we want to get the same coefficients as in the text, we need
# to rearrange the columns of our matrix of response counts.
counts <- counts[,c(3,2,1)]
counts
race <- factor(c("w","w","b","b"))
race
gender <- factor(c("f","m","f","m"))
gender
# To get the same dummy variables as used in the text we need to reorder
# the levels of this factor.  Note that we could have simply entered
# both race and gender as 0-1 variables to begin with.
gender <- relevel(gender,"m")
gender
# To fit these models we use the multinom function from the nnet library.
# For this you must have installed the VR package, which includes the
# nnet (neural networks) library.
library(nnet)
# The model fit in the text:
afterlife.logit <- multinom( counts ~ gender + race )
afterlife.logit
# The model for parts (b-d):
afterlife.logit.b <- multinom( counts ~ gender )
afterlife.logit.b
