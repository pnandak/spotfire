# -------------------------------------------------------------------
# MPO1 Quantitative Research Methods
# Thilo Klein
# Lab Session 1: General Introduction to R; Descriptive Statistics

# Required libraries: Rcmdr, timeDate
  setwd("C:/Dokumente und Einstellungen/Thilo/Desktop/Lab_Sessions/Session1/Data")
  source("http://thiloklein.de/R/myfunctions.R")
  ls()
# -------------------------------------------------------------------




# --- Ex 1: find help; R Commander; install/load packages -------------------
 ?memory.size		    # help if exact command is known
 help.search("memory size")  # help if exact command is not knowm
 install.packages("Rcmdr")   # install package Rcmdr
 library(Rcmdr)		    # load package




# --- Ex 2: Load a dataset and save it with a different name ----------------
 dataset2 <- read.csv("http://thiloklein.de/R/dataset", header=T)  # read dataset from .csv file
 ls()   							# display active objects in workspace
 save("dataset2", file="dataset2.RData")		# save active object dataset2
 rm(dataset2) 						# clear object dataset2 from workspace
 rm(list=ls())						# clear workspace
 load("C:/.../dataset2.RData")   			# load object dataset2




# --- Ex 4: Provide description(s) for one or more variables ------------------
 str(dataset2)			# some characteristics of all variables 
 is.double(dataset2$rev) 	# is variable rev of type double?




# --- Ex 5: Inspect and modify the dataset ------------------
 attach(dataset2)					# directly call the variables, to undo: detach()
 edit(dataset2)					# open AND modify dataset
 edit(year)						# open AND modify single variable
 browse(dataset2)					# open dataset
 cbind(comp_name, year, level, capital)	
 cbind(comp_name, year, level, capital)[level > 3 & is.na(level)==FALSE]




# --- Ex 6: Create (generate) a new variable ------------------
# --- Ex 6: a) ---
# Calculate output per employee for all enterprises.

 totemp <- softemp + othemp
 productivity <- rev / totemp


# --- Ex 6: b) ---
# generate a variable containing revenue squared.

 revsq <- rev^2
 lrev <- log(rev)
 ltotemp <- log(totemp)




# --- Ex 7: Generate an indicator variable ------------------
 year2002 <- ifelse(year == 2002, 1, 0)
 cbind(year, year2002)
 compnameis <- paste("company name is:", comp_name, sep=" ")
 compnameis[1:10]




# --- Ex 8: Calculate summary statistics. Identify missing values and outliers. Note that the latter may be the result of mistakes. Calculate proportions of individuals presenting a certain characteristic. ------------------
 summary(dataset2)
 NA%in%level
 length(level[is.na(level)])




# --- Ex 9: Calculate additional summary statistics but only for a few variables, or a subset of observations. ------------------
 sd(rev)
 var(rev)
 help.search("Skewness"); library(timeDate); skewness(rev)
 sd(rev[level>2])




# --- Ex 10: Analysis of the frequency of discrete variables ------------------
 table(pub)




# --- Ex 11: Analysis of the frequency of a combination of discrete variables (two-way tables) ------------------
 table(pub, level)
 t<-table(pub, level)
 t/sum(t)




# --- Ex 12: Analysis of a variable conditioned on a discrete variable ------------------
 ?by
 by(data=totemp, INDICES=us, FUN=summary)
 by(totemp, us, summary) 
 by(totemp, india, summary); by(rev, india, summary) 




# --- Ex 13: Graphs ------------------
# --- Ex 13: a) ---
# Scatter plots

 plot(rev ~ totemp) # plot(totemp, rev)
 plot(lrev ~ ltotemp)


# --- Ex 13: b) ---
# Histograms

 hist(rev)
 hist(rev[rev<1000])
 hist(rev[rev<500], breaks=50)
 par(mfrow=c(2,1))
 hist(rev[rev<200 & uk==1], breaks=50); hist(rev[rev<200 & uk==0], breaks=50)




# --- Ex 14: Drop variables or observations ------------------
 dataset2$dom <- NULL




# --- Ex 15: Generate linear transformations ------------------
 othemp_lt <- 1 + othemp/100
 sum(othemp)
 mean(othemp)
 1 + mean(othemp)/100
 othemp_qt <- 1 + othemp/100 + 5 * othemp^2
 plot(othemp_lt ~ othemp)
 plot(othemp_qt ~ othemp)




# --- Ex 16: Some properties of the sample variance ------------------
 cor(othemp, softemp, use=" pairwise.complete.obs")
 cov(othemp, softemp, use=" pairwise.complete.obs")


# --- Ex 16: a) ---
# generate a variable which is the sum and  apply the variance. You have created the 
# variable (totemp), so you need to apply the variance.

 myvar <- function(x){var(x, na.rm=T)}
 myvar(totemp)


# --- Ex 16: b) ---
# Apply the formula: Var (X+Y) = Var (X) + Var (Y) + 2 * covariance (X, Y)

 myvar(othemp) + myvar(softemp) + 2*cov(othemp, softemp, use="pairwise.complete.obs")

 # the same applies to the mean:
 mymean <- function(x){mean(x,na.rm=T)}
 mymean(othemp) + mymean(softemp)




# --- Ex 19: T-tests (in your own time) ------------------
# --- Ex 19: b) ---
# Using R, perform this test and calculate the confidence interval at 99% 
# significance level.

 data <- scan("clipboard")
 mean(data) + c(-1,1)*sd(data)*qnorm(0.99)
 t.test(data, mu=60, alternative="less", conf.level=0.99)




# --- Digression: my function to count NAs per variable ---
 source("http://thiloklein.de/R/myfunctions.R")
 showNAs(dataset2)





# -------------------------------------------------------------------
# --- End of Session ------------------------------------------------

 save.image("dataset2_EndOfSession.RData")
 q("yes")




