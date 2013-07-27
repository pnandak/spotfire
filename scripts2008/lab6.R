# Lab #6 (May 3, 2007) Script file
# In this lab we will work general linear models, and
#  specifically the binomial family

setwd("C://courses//qerm514")
#######################################################################
# On to an example of a binomial GLM...........

# Remember to add this line if you're going to be dealing 
#  with any type of coefficient data in S+!
# options(contrasts=c("contr.treatment", "contr.poly"))

# According to V&R - "Collett(1991) reports on an experiment
#  on the toxicity to teh tobacco buworm of doses of the 
#  pyrethroid trans-cypmethrin to which the moths were beginning
#  to show resistance.  Batches of 20 moths of each sex were 
#  exposed for three days to the pyrethroid adn the number in 
#  each batch that were dead or knocked down was recorded."

# First download the data from the website and create the 
#  appropriate dataframe within your S-Plus session
budworm.df<-read.table("budworm.dat", header=T)

# Look at the data
budworm.df

# Now, since its mortality data, maybe a good model would be to use
#  a glm with a binomial family.  

# A couple comments on the format of the response data and on the
#  weights argument are needed.  

# - If the response vector is a numeric vector, it is assumed to hold
#  the data in ratio form yi = si/ai, in which case the ai's must be
#  given as a vector of weights using the weights argument.  
# - In this case, it is also expected that the response vector takes a 
#  value between 0 and 1.  si in the this example is therefore the
#  number that died and ai is 20 in every case.  

# First, look at the raw data...  A lot of ways to do this, but
#  one quick way is to create a new data frame:
male.bud.df<-budworm.df[budworm.df$Sex=="M",]
attach(male.bud.df)
plot(Dose, Mort/Samp, type="b", ylim=c(0,1))
detach()
female.bud.df<-budworm.df[budworm.df$Sex=="F",]
attach(female.bud.df)
points(Dose, Mort/Samp, type="b", pch=2, lty=2)
detach()

# Add a new column of data which is the log (base 2) of the dose
budworm.df$ldose<-log(budworm.df$Dose,  base=2)

# Thus, one way to code the glm function call is:
budworm.glm<-glm(Mort/20~Sex*ldose, family=binomial, 
   data=budworm.df, weights=Samp)

# and look at the results:
anova(budworm.glm, test="Chisq")

# So what does this tell you about the significance between the 
#  interaction between sex and dose?

# And now let's plot out this data
attach(budworm.df)

# Plot a blank screen with the right limits
plot(c(1,32), c(0,1), type="n", xlab="dose", ylab="prob", 
   log="x")

# Add points, as text, of the given data points
text(2^ldose, Mort/20, labels=as.character(Sex))

# ld is a temp variable which covers the range of our data
ld<-seq(0, 5, .1)

# use the predict() function to estimate data for each sex
#  See the help command for predict.glm for more information
#  about this function, but basically you pass a dataframe of 
#  new data and predict() returns the expected response using
#  the given model and associated parameters.  
male.df<-predict(budworm.glm, data.frame(ldose=ld, 
   Sex=factor(rep("M", length(ld)))), type="response")

# and draw a line on the plot connecting this data
lines(2^ld, male.df, col=3)

# again for the female data
female.df<-predict(budworm.glm, data.frame(ldose=ld, 
   Sex=factor(rep("F", length(ld)))), type="response")
lines(2^ld, female.df, lty=2, col=2)

# Note:  while this may be a useful plotting tool, make sure
#  to check your homework for exactly what type of plot 
#  is asked for!

#############################################################
# A couple of other quick things which may be useful towards
#  your homework:
# - to add a single interaction term to a model use:
#  glm(y ~ x1 + x2 + x3 + x1*x2)
#  where the interaction term between x1 and x2 is 
#  included along with all of the main effects (x1, x2 and x3) 

# Next week we'll start playing more with the update command and 
#  other functions to easily add and remove predictors from 
#  your models. 