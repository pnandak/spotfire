
# This rm command removes everything in the global environment. 
rm(list = ls())

# Constructing the data for the budworm experiment.
male.dead <- c(1,4,9,13,18,20)
female.dead <- c(0,2,6,10,12,16)
numdead <- c(male.dead,female.dead)

outcome <- cbind(numdead, numalive = 20 - numdead)

log.dose <- rep(0:5,2)
dose <- 2^log.dose
sex <- factor(rep(c("M","F"),c(6,6)))
budworm.data <- data.frame(sex,log.dose,outcome)
attr(budworm.data,"reference") <- "Venables & Ripley, MASS, page 190."

# Now that we have a data set, we can build the logistic regression
# model.   Notice that the response consists of two columns built together.

budworm.glm <- glm(cbind(numdead,numalive) ~ sex*log.dose,data = budworm.data, family = binomial)

# Now that we've fitted the model, try looking at it's structure with the 
# command.
str(budworm.glm)

# Notice that the budworm.glm object contains a lot of information.  To get
# more statistical insight into the model try the command.
summary(budworm.glm)


#  Using the coeficients from the model we can construct a fitted function


female.moth <- function(log.dose) 
      {
      1/(1 + exp( 2.9935  - 0.9060* log.dose))
      }

male.moth <- function(log.dose) 
      {
      1/(1 + exp( 2.8185 - 1.2589*log.dose))
      }


budworm.predict <- function(log.dose,sex)
    {
    result <- NA
	if(sex == "F")
	    {
		result <-female.moth(log.dose)
		}
	else if(sex == "M")
	    {
	    result <-male.moth(log.dose)
	    }
	return(result)
	}



#################################################
# A little bit of defensive programming. 
#    What happens if we call budworm.predict
# with value for sex other than "M" or "F"?
# We can use either the 'stopifnot' function or
# the 'warning' function.

budworm.predict <- function(log.dose,sex)
    {
    	
    if(!all(sex %in% c("M","F"))) warning("moth sex must be either F or M.")
    result <- NA
	if(sex == "F")
	    {
		result <-female.moth(log.dose)
		}
	else if(sex == "M")
	    {
	    result <-male.moth(log.dose)
	    }
	return(result)
	}

# Now try the commands
budworm.predict(2.3,"F")
# and
budworm.predict(2.3,"ASDF")

# Alternatively we can enforce even more discipline on the 
# budworm.predict function by requiring that the function
# halt and cause an error if sex is neither F or M.

budworm.predict <- function(log.dose,sex)
    {
    	
    stopifnot(sex %in% c("M","F"))
    result <- NA
	if(sex == "F")
	    {
		result <-female.moth(log.dose)
		}
	else if(sex == "M")
	    {
	    result <-male.moth(log.dose)
	    }
	return(result)
	}


# Now try the commands
budworm.predict(2.3,"F")
# and
budworm.predict(2.3,"ASDF")

########################################################################################
# Now we can use our new function to plot
# the data and the fitted logistic regression.

# First make a blank plot in R.  In the past we have used the plotting
# parameters type='p' or type = c('p','g') in lattice, in the basic
# plot command we have a similar parameter.   In plot, type='n'
# means make a new plot with nothing in it.

plot(c(1,32),c(0,1), type = "n",xlab = "dose", ylab = "prob",log = "x")

# Next we use the text command to place F/M labels
# on the plot at the fitted values for the
# saturated model.

text(2^log.dose,numdead/20,labels = as.character(sex))


# Now we can add the predictions from the fitted
# model.  
ld <- seq(0,5, by = 0.1)
lines(2^ld,budworm.predict(ld,"M"),col='blue',lwd=2)
lines(2^ld,budworm.predict(ld,"F"),col='green',lwd=2)



text(2^log.dose,numdead/20,labels = as.character(sex))
ld <- seq(0,5, by = 0.1)

lines(2^ld, predict(budworm.glm,data.frame(log.dose = ld, sex = factor(rep("M", length(ld)), levels = levels(sex))), type = "response"), col = "green")

# You can also use the built-in predict function with any glm model.
plot(c(1,32),c(0,1), type = "n",xlab = "dose", ylab = "prob",log = "x")
text(2^log.dose,numdead/20,labels = as.character(sex))

#####################################################################################################
# 
# Predictions for the female moths.
# 1. Make a new data frame & call predict for the female moths.
new.female.data <- data.frame(log.dose = ld, sex = factor(rep("F", length(ld)), levels = levels(sex)))
female.predictions <- predict(budworm.glm,new.female.data, type = "response")
# 2. Then plot the results
lines(2^ld, female.predictions,lwd=3,lty=3, col = "green")
# 3. Now repeat for the male moths.
new.male.data <- data.frame(log.dose = ld, sex = factor(rep("M", length(ld)), levels = levels(sex)))
male.predictions <- predict(budworm.glm,new.male.data, type = "response")
lines(2^ld, male.predictions,lwd=3, lty=3,col = "blue")




budworm.glmA <- update(budworm.glm, . ~ sex * I(log.dose - 3))
summary(budworm.glmA,cor=F)$coefficients

