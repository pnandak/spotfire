## Introduction to Statistics with R
## Grace Yang, Mũthoni Ngatia
## Social Sciences Statistical Laboratory, Yale University
## Sep.16, 2011

## NOTE: We will be copying and pasting these commands one at a time to demonstrate
## various features of R. To copy, highlight the line and press Ctrl-C (copy). Then go to the
## R window and press Ctrl-V or Shift-Ins (paste) at the ">" prompt.

#Getting help
help.start()	# Opens html help in web browser (if installed)
help(help)	# find help on how to use help
?help		# Same as above
help.search("help")	# Find all functions that include the word 'help'

# Reading in your data
getwd()		# What directory are we in?
setwd("C:/Documents and Settings/NetID/Desktop/IntroR2011")	
           # Set working directory to the directory where we put the data	
data <- read.table("remote_weight.txt", header=T, sep="", row.names=NULL)	# Read data including headers, data separated by spaces, no row names
ls()			# List all variables stored in memory

#Extracting data from the data frame
dim(data)	        # Find out how many rows and columns in the data set
names(data)	        # List all variable names in the dataset 
str(data)           # Look at the structure of your data
data		        # See the data frame on the screen
data[1:10,]	        # See the first 10 rows
data[,"weight"]	    # See only the weight column
data[,3]	        # Same as above
data$weight	        # Yet another way
data[1:10,"weight"]	# See only the first 10 values of the weight col.
data[,-1]	        # See all but the first column of data
data.o <- data	    # Backup data frame
ls()			# Now we have two variables: 'data' and 'data.o'

#Getting familiar with the data
summary(data)		# Generate summary statistics of data
sd(data)		# Calculate standard deviations of all variables
var(data)		# Variance on diagonal, covariance off diagonal
mean(data)		# Calculate the mean of all variables
pairs(data)	 	# A general view of data through scatter plots
pairs(data[,-1]) 	# See scatterplots for all pairs of variables except the first ('id') in the data frame
plot(data$weight, data$remote)	# Scatterplot of 'weight' vs. 'remote'

# Changing data type
class(data$gender)  		# What kind of variable is 'gender'?
data$gender <- factor(data$gender)	# Converts 'gender' from type integer to factor 
class(data$gender)		# Verify that gender is now indeed of type factor
data$gender			# See all data in column 'gender'; note "Levels: 0 1" at the bottom

# Attaching the data frame
attach(data)		# Attach the data frame
remote			# Now we can refer directly to the variable without using $

# Basic Graphics
hist(remote)			# Histogram of 'remote'
hist(weight)			# Histogram of 'weight'
boxplot(remote,weight)  	# Boxplot of 'remote' and 'weight' 
boxplot(remote ~ gender)  	# Boxplot of 'remote' conditioned on 'gender'
boxplot(weight ~ gender)	# Boxplot of 'weight' conditioned on 'gender'

# Inferential statistics
cor(remote,weight)		    # Run correlation coefficient
t.test(remote ~ gender)	    # Did frequency of remote use differ by gender?
rem.t <- t.test(remote ~ gender)    # Save results of last analysis
rem.t				    # Display analysis
names(rem.t)			    # See the names of variables in the object rem.t
rem.t$statistic		    # See the statistics variable in the object rem.t
mod1 <- lm(remote ~ gender)	    # Linear model, regressing 'remote' on 'gender'
anova(mod1)			    # ANOVA table of the previous model
anova(lm(remote ~ gender))	    # You can combine the two steps in to one line
mod2 <- lm(remote ~ weight)	    # Model 'remote' as a linear function of 'weight'
mod3 <- lm(remote ~ weight + gender)  # Model 'remote' as a linear function of 'weight' & 'gender'
mod4 <- lm(remote ~ weight*gender)  # Equivalent to all main effects and interaction: lm(remote ~ weight + gender + weight*gender)
summary(mod3)			# See regression table for model 3 (remote ~ weight + gender)
summary(mod4)			# See regression table for model 4 (remote ~ weight*gender)
anova(mod3,mod4)		# Prints ANOVA table comparing model 3 to model 4 (delta F)

# Regression diagnostics
mod3.1 <- lm(remote ~ weight + gender)	# Gives you regression diagnostics
par(mfrow=c(2,2))		# Set up plotting region for a 2x2 grid
plot(mod3.1)			# Plot the regression diagnostics (R knows automatically to do this)

# Saving the pretty graphs as postscript
postscript("prettygraph.ps")	# Turn on the postscript device and open a blank filed called "prettygraph.ps"
plot(mod3.1)			# Plot the model
dev.off()			# Turn off the postscript device
q()				# Quit R
