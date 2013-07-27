

## Set the working directory (this should be the place on your computer  
## where you want to run the analysis and where the fishPassage.csv file is located

setwd("C:\\projects\\classes\\R_course\\Homework1")

## Read in the fishPassage.csv file
fishPassage <- read.csv("fishPassage.csv", stringsAsFactors = F)

## Look at the first six lines
head(fishPassage)

## Add a row to the data frame
fishPassage <-rbind(fishPassage,list(2008,'BON',640964,78365,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))

## Look at the last six rows of the data frame, you should see the new row you just added
tail(fishPassage)

## Make a new data frame that only has years 1980 and later
fishPassage1980 <- fishPassage[fishPassage$Year >= 1980,]

## Take the mean of Coho.Adult at every dam only for years 1980 and later
fishPassage1980mean <- aggregate(fishPassage1980$Coho.Adult, by = list(fishPassage1980$Dam), FUN = mean, na.rm=T)

## Take the mean of Coho.Adult at every dam only for all years
fishPassageMean <- aggregate(fishPassage$Coho.Adult, by = list(fishPassage$Dam), FUN = mean, na.rm=T)

## Change the column names for these two summary data frames
colnames(fishPassage1980mean) <- c("dam", "coho.adult.mean")
colnames(fishPassageMean) <- c("dam", "coho.adult.mean")

## Add a third column to the fishPassageMean data frame that has the mean adult coho for each dam for year 1980 and later
fishPassageMean$coho.adult.1980.mean <- fishPassage1980mean$coho.adult.mean

## Take the difference of the means at each dam between all years and only years 1980 and later
fishPassageMean$mean.diff <- fishPassageMean$coho.adult.1980.mean - fishPassageMean$coho.adult.mean

## Write this data frame to a .csv file
write.csv(fishPassageMean, "fishPassageMean.csv")

## Plot Adult versus Jack for only years 1980 and later
plot(fishPassage1980$Coho.Jack~fishPassage1980$Coho.Adult)

## Make a histogram of the difference of the means at each dam between all years and only years 1980 and later
hist(fishPassageMean$mean.diff)

## Load the lattice library
library(lattice)

## Use the lattice library function histogram to make histograms of Coho.Adult at each dam
histogram(~fishPassage1980$Coho.Adult | fishPassage1980$Dam)



##########################
##########################
####
#### Start of homework 2
####
####

## Take the log of Coho.Adult and Coho.Jack
fishPassage1980$Coho.Adult.log <- log(fishPassage1980$Coho.Adult)
fishPassage1980$Coho.Jack.log <- log(fishPassage1980$Coho.Jack)

fishPassage1980$Steelhead.log <- log(fishPassage1980$Steelhead)
fishPassage1980$Sockeye.log <- log(fishPassage1980$Sockeye)


## Make infinite values NA

fishPassage1980$Coho.Adult.log[is.infinite(fishPassage1980$Coho.Adult.log)] <- NA
fishPassage1980$Coho.Jack.log[is.infinite(fishPassage1980$Coho.Jack.log)] <- NA

fishPassage1980$Steelhead.log[is.infinite(fishPassage1980$Steelhead.log)] <- NA
fishPassage1980$Sockeye.log[is.infinite(fishPassage1980$Sockeye.log)] <- NA

## Create vector of colors for points

colrs <- c("red", "orange", "yellow", "lightblue", "blue", "turquoise", "green", "green4", "purple", "magenta", "grey", "black") 

## Make a additional column in fishPassage1980 data frame to specify a color for each point
fishPassage1980$colValue[fishPassage1980$Dam == "IHR"] <- colrs[1]
fishPassage1980$colValue[fishPassage1980$Dam == "IHR"] <- colrs[2]
fishPassage1980$colValue[fishPassage1980$Dam == "JDA"] <- colrs[3]
fishPassage1980$colValue[fishPassage1980$Dam == "LGR"] <- colrs[4]
fishPassage1980$colValue[fishPassage1980$Dam == "LGS"] <- colrs[5]
fishPassage1980$colValue[fishPassage1980$Dam == "LMN"] <- colrs[6]
fishPassage1980$colValue[fishPassage1980$Dam == "MCN"] <- colrs[7]
fishPassage1980$colValue[fishPassage1980$Dam == "PRD"] <- colrs[8]
fishPassage1980$colValue[fishPassage1980$Dam == "RIS"] <- colrs[9]
fishPassage1980$colValue[fishPassage1980$Dam == "RRH"] <- colrs[10]
fishPassage1980$colValue[fishPassage1980$Dam == "WEL"] <- colrs[11]
fishPassage1980$colValue[fishPassage1980$Dam == "WFA"] <- colrs[12]

#####
## Start creating the first graphic

png("homework2Coho.png", 1000,1000, pointsize = 30)

## Make a matrix and layout
mat <- matrix(c(2,0,1,3),2,2,byrow=TRUE)
lay2 <- layout(mat, c(5,1), c(1,5), TRUE)  

## Create histogram analysis objects
Coho.Adult.hist <- hist(fishPassage1980$Coho.Adult.log, plot = F)
Coho.Jack.hist <- hist(fishPassage1980$Coho.Jack.log, plot = F)

## Create linear regression model object
cohoAdultJack.lm <- lm(fishPassage1980$Coho.Jack.log~fishPassage1980$Coho.Adult.log)

## Change margin sizes for first plot on layout
par(mar=c(5,5,1,1))

## Make the first plot (scatter plot)
plot(fishPassage1980$Coho.Jack.log~fishPassage1980$Coho.Adult.log, main = "", xlab = "log(Adult Coho Count)", ylab = "log(Jack Coho Count)", pch = 21, bg = fishPassage1980$colValue)

## Add regression line to the scatter plot
abline(cohoAdultJack.lm, lwd = 2, lty = 2)          

## Add legend to the scatter plot
legend("topleft", legend = fishPassageMean$dam, pch = 21, pt.bg = colrs, ncol = 2)   

## Change the margins for the second plot on layout
par(mar=c(0,5,1,1))

## Draw second plot (bar plot histogram on top of scatter plot)
barplot(Coho.Adult.hist$counts, ylim = c(0,90))

## Change the margins for the third plot on layout
par(mar=c(5,0,1,1))

## Draw third plot (bar plot histogram to the right of the scatter plot)
barplot(Coho.Jack.hist$counts, xlim = c(0,90), horiz = T)


## finish creating the png (close the file)--aka turn the png device off
dev.off()


######
## Repeat this same thing for the second graphic, but do not add trend line

png("homework2_Steelhead_Sockeye.png", 1000,1000, pointsize = 30)


mat <- matrix(c(2,0,1,3),2,2,byrow=TRUE)
lay2 <- layout(mat, c(5,1), c(1,5), TRUE)  

Steelhead.hist <- hist(fishPassage1980$Steelhead.log, plot = F)
Sockeye.hist <- hist(fishPassage1980$Sockeye.log, plot = F)

steelheadSockeye.lm <- lm(fishPassage1980$Steelhead.log~fishPassage1980$Sockeye.log)

par(mar=c(5,5,1,1))

plot(fishPassage1980$Steelhead.log~fishPassage1980$Sockeye.log, main = "", xlab = "log(Sockeye Count)", ylab = "log(Steelhead Count)", pch = 21, bg = fishPassage1980$colValue)

#abline(steelheadSockeye.lm, lwd = 2, lty = 2)     

legend("bottomleft", legend = fishPassageMean$dam, pch = 21, pt.bg = colrs, ncol = 2)   
par(mar=c(0,5,1,1))
barplot(Sockeye.hist$counts, ylim = c(0,90))

par(mar=c(5,0,1,1))
barplot(Steelhead.hist$counts, xlim = c(0,90), horiz = T)

dev.off()


#####
##
## start cluster analysis


## aggregate data by taking the mean of Sockeye.log and Steelhead.log for each dam
SockSteel1980Mean <- aggregate(fishPassage1980[,c("Sockeye.log", "Steelhead.log")], by = list(fishPassage1980$Dam), FUN = mean,na.rm=T)

## Get rid of the row for the WFA dam (the 12th row) 
SockSteel1980Mean <- SockSteel1980Mean[-12, ]

## Load the necessary libraries
library(vegan)
library(cluster)

## Make the row names equal to the dam names
row.names(SockSteel1980Mean) <- SockSteel1980Mean$Group.1

## Get rid of the column of dam names (this is not necessary, but cleans up the data frame a bit)
SockSteel1980Mean <- SockSteel1980Mean[,-1]

## Calculate the jaccard distance between points in  terms of Sockeye.log and Steelhead.log
steelSock.jac <- vegdist(SockSteel1980Mean[,c("Sockeye.log", "Steelhead.log")], method = "jaccard")

## Run a cluster analysis on the jaccard distances using ward method
steelSock.jac.ward <- hclust(steelSock.jac, method = "ward")

#####
##
## Start creating the graphic for the cluster analysis

png("steelSockClust.png", 1500,1000, pointsize = 30)


## Plot the dendogram from the cluster analysis
plot(steelSock.jac.ward, xlab = "Dams", sub = "")

## Change the line weight and line type graphing parameters to make a wider and dashed line
par(lwd = 3, lty = 2)

## Draw rectangles around the three groups that are best clustered
rect.hclust(steelSock.jac.ward, k = 3)


## Turn off the png device
dev.off()

