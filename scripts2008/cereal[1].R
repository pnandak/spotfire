############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  9-2-03                                                            #
# UPDATE: 6-30-04                                                          #
# PURPOSE: Create plots in R                                               #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

#R IS CASE SENSITIVE

#Read in the data
cereal<-read.table(file = "C:\\chris\\UNL\\STAT873\\Chapter 3\\cereal.txt", header=TRUE)

#Simple scatter plot
plot(x = cereal$sugar, y = cereal$fat)


#Make the scatter plot nicer
plot(x = cereal$sugar, y = cereal$fat, xlab="Sugar", ylab="Fat", ylim=c(0,0.1), xlim=c(0,0.6), col="red", pch=1, cex=1, 
     main = "Fat vs. Sugar \n Each variable is adjusted for the serving size", panel.first=grid(col="gray", lty="dotted") )

#Shows current colors for number 1 to 8
palette() 
  


#Change other items on the plot
plot(x = cereal$sugar, y = cereal$fat, xlab="Sugar", ylab="Fat", type="n", 
     main = "Fat vs. Sugar \n Each variable is adjusted for the serving size", panel.first=grid(col="gray", lty="dotted"))
points(x = cereal$sugar[cereal$Shelf==1], y = cereal$fat[cereal$Shelf==1], pch = 1, col = 1, cex = 1)
points(x = cereal$sugar[cereal$Shelf==2], y = cereal$fat[cereal$Shelf==2], pch = 2, col = 2, cex = 1.5)
points(x = cereal$sugar[cereal$Shelf==3], y = cereal$fat[cereal$Shelf==3], pch = 3, col = 3, cex = 2)
points(x = cereal$sugar[cereal$Shelf==4], y = cereal$fat[cereal$Shelf==4], pch = 4, col = 4, cex = 2.5)
#legend(locator(1), legend=c("1", "2", "3", "4"), pch = c(1,2,3,4), col=c(1,2,3,4)) #Click on location in plot for legend
legend(x=0.5, y=0.06, legend=c("1", "2", "3", "4"), pch = c(1,2,3,4), col=c(1,2,3,4)) #Specify a location


#Bubble plot
symbols(x = cereal$sugar, y = cereal$fat, circles = cereal$sodium, xlab="Sugar", ylab="Fat",
        main = "Fat vs. Sugar with symbol proportional to sodium  \n Each variable is adjusted for the serving size", 
        panel.first=grid(col="gray", lty="dotted"))


#Rescale the plotting points
#do not need - plot(x = cereal$sugar, y = cereal$fat, xlab="Sugar", ylab="Fat", type="n")
symbols(x = cereal$sugar, y = cereal$fat, circles = cereal$sodium, xlab="Sugar", ylab="Fat", inches=0.5,
        main = "Fat vs. Sugar with symbol proportional to sodium  \n Each variable is adjusted for the serving size", 
        panel.first=grid(col="gray", lty="dotted")) 
#If inches is FALSE, the units are taken to be those of the x axis. If inches is TRUE, the symbols are scaled so that the largest symbol is one inch in height. If a number is given the symbols are scaled to make largest symbol this height in inches.
identify(x = cereal$sugar, y = cereal$fat, labels = cereal$Cereal) #By default, the row number in the data set will be the label



##################################################################################################################
#More complicated plots

#Open new plotting sheet
win.graph(width = 7, height = 7, pointsize = 9)

#2x2 plotting sheet
par(mfrow=c(2,2))

#Fat vs. Sugar - Bubble plot with different colors for the shelves
plot(x = cereal$sugar, y = cereal$fat, type="n", xlab="Sugar", ylab = "Fat", 
     main = "Fat vs. Sugar with symbol proportional to sodium  \n Each variable is adjusted for the serving size", 
     panel.first=grid(col="gray", lty="dotted"))     #Makes sure scale is correct
symbols(x = cereal$sugar[cereal$Shelf==1], y = cereal$fat[cereal$Shelf==1], circles = cereal$sodium[cereal$Shelf==1],
        inches=0.1, bg=1, add=TRUE) 
symbols(x = cereal$sugar[cereal$Shelf==2], y = cereal$fat[cereal$Shelf==2], circles = cereal$sodium[cereal$Shelf==2],
        inches=0.1, bg=2, add=TRUE) 
symbols(x = cereal$sugar[cereal$Shelf==3], y = cereal$fat[cereal$Shelf==3], circles = cereal$sodium[cereal$Shelf==3],
        inches=0.1, bg=3, add=TRUE) 
symbols(x = cereal$sugar[cereal$Shelf==4], y = cereal$fat[cereal$Shelf==4], circles = cereal$sodium[cereal$Shelf==4],
        inches=0.1, bg=4, add=TRUE) 
legend(locator(1), legend=c("1", "2", "3", "4"), pch=c(16,16,16,16), col=c(1,2,3,4)) 
#Can not get different plotting characters for the symbols using pch and can not get the legend to look correct



#Fat vs. Sodium - Bubble plot with different colors for the shelves
plot(x = cereal$sodium, y = cereal$fat, type="n", xlab="Sodium", ylab = "Fat", 
     main = "Fat vs. Sodium with symbol proportional to sugar  \n Each variable is adjusted for the serving size",
     panel.first=grid(col="gray", lty="dotted"))
symbols(x = cereal$sodium[cereal$Shelf==1], y = cereal$fat[cereal$Shelf==1], circles = cereal$sugar[cereal$Shelf==1],
        inches=0.1, bg=1, add=TRUE) 
symbols(x = cereal$sodium[cereal$Shelf==2], y = cereal$fat[cereal$Shelf==2], circles = cereal$sugar[cereal$Shelf==2],
        inches=0.1, bg=2, add=TRUE) 
symbols(x = cereal$sodium[cereal$Shelf==3], y = cereal$fat[cereal$Shelf==3], circles = cereal$sugar[cereal$Shelf==3],
        inches=0.1, bg=3, add=TRUE) 
symbols(x = cereal$sodium[cereal$Shelf==4], y = cereal$fat[cereal$Shelf==4], circles = cereal$sugar[cereal$Shelf==4],
        inches=0.1, bg=4, add=TRUE) 
legend(locator(1), legend=c("1", "2", "3", "4"), pch=c(16,16,16,16), col=c(1,2,3,4)) 



#Sugar vs. Sodium - Bubble plot with different colors for the shelves
plot(x = cereal$sodium, y = cereal$sugar, type="n", xlab="Sodium", ylab = "Sugar", 
     main = "Sugar vs. Sodium with symbol proportional to sugar  \n Each variable is adjusted for the serving size",
     panel.first=grid(col="gray", lty="dotted"))
symbols(x = cereal$sodium[cereal$Shelf==1], y = cereal$sugar[cereal$Shelf==1], circles = cereal$fat[cereal$Shelf==1],
        inches=0.1, bg=1, add=TRUE) 
symbols(x = cereal$sodium[cereal$Shelf==2], y = cereal$sugar[cereal$Shelf==2], circles = cereal$fat[cereal$Shelf==2],
        inches=0.1, bg=2, add=TRUE) 
symbols(x = cereal$sodium[cereal$Shelf==3], y = cereal$sugar[cereal$Shelf==3], circles = cereal$fat[cereal$Shelf==3],
        inches=0.1, bg=3, add=TRUE) 
symbols(x = cereal$sodium[cereal$Shelf==4], y = cereal$sugar[cereal$Shelf==4], circles = cereal$fat[cereal$Shelf==4],
        inches=0.1, bg=4, add=TRUE) 
legend(locator(1), legend=c("1", "2", "3", "4"), pch=c(16,16,16,16), col=c(1,2,3,4)) 



#Show how some symbols are being hidden with the above plots - why aren't the colors working???? 
par(mfrow=c(1,1))
#Fat vs. Sugar - Bubble plot with different colors for the shelves
plot(x = cereal$sugar, y = cereal$fat, type="n", xlab="Sugar", ylab = "Fat", 
     main = "Fat vs. Sugar with symbol proportional to sodium  \n Each variable is adjusted for the serving size", 
     panel.first=grid(col="gray", lty="dotted"))     #Makes sure scale is correct
symbols(x = cereal$sugar[cereal$Shelf==1], y = cereal$fat[cereal$Shelf==1], circles = cereal$sodium[cereal$Shelf==1],
        inches=0.1, add=TRUE, col = 1) 
symbols(x = cereal$sugar[cereal$Shelf==2], y = cereal$fat[cereal$Shelf==2], circles = cereal$sodium[cereal$Shelf==2],
        inches=0.1, add=TRUE, col = 2) 
symbols(x = cereal$sugar[cereal$Shelf==3], y = cereal$fat[cereal$Shelf==3], circles = cereal$sodium[cereal$Shelf==3],
        inches=0.1, add=TRUE, col = 3) 
symbols(x = cereal$sugar[cereal$Shelf==4], y = cereal$fat[cereal$Shelf==4], circles = cereal$sodium[cereal$Shelf==4],
        inches=0.1,  add=TRUE, col = 4) 
legend(locator(1), legend=c("1", "2", "3", "4"), pch=c(16,16,16,16), col=c(1,2,3,4)) 
#Can not get different plotting characters for the symbols using pch and can not get the legend to look correct



################################################################################
# Quicker way to so the above plot - write your own function - two possible functions are shown below

win.graph(width = 7, height = 7, pointsize = 9)
par(mfrow=c(2,2))

plot.it1<-function(x, y, z, title.x, title.y, title.z) {
  plot(x = x, y = y, type="n", xlab = title.x, ylab = title.y, main = 
  paste(title.y, "vs.", title.x, "with symbol proportional to", title.z, " \n Each variable is adjusted for the serving size"),
  panel.first=grid(col="gray", lty="dotted"))
  symbols(x = x[cereal$Shelf==1], y = y[cereal$Shelf==1], circles = z[cereal$Shelf==1],
          inches=0.1, bg=1, add=TRUE) 
  symbols(x = x[cereal$Shelf==2], y = y[cereal$Shelf==2], circles = z[cereal$Shelf==2],
          inches=0.1, bg=2, add=TRUE) 
  symbols(x = x[cereal$Shelf==3], y = y[cereal$Shelf==3], circles = z[cereal$Shelf==3],
          inches=0.1, bg=3, add=TRUE) 
  symbols(x = x[cereal$Shelf==4], y = y[cereal$Shelf==4], circles = z[cereal$Shelf==4],
          inches=0.1, bg=4, add=TRUE) 
  legend(locator(1), legend=c("1", "2", "3", "4"), pch=c(16,16,16,16), col=c(1,2,3,4)) 
}

plot.it1(x = cereal$sugar, y = cereal$fat, z = cereal$sodium, title.x = "Sugar", title.y = "Fat", title.z = "Sodium")
plot.it1(cereal$sodium, cereal$fat, cereal$sugar, "Sodium", "Fat", "Sugar")
plot.it1(cereal$sodium, cereal$sugar, cereal$fat, "Sugar", "Sodium", "Fat")



win.graph(width = 7, height = 7, pointsize = 9)
par(mfrow=c(2,2))

plot.it2<-function(x, y, z, title.x, title.y, title.z) {
  plot(x = x, y = y, type="n", xlab = title.x, ylab = title.y, main = 
  paste(title.y, "vs.", title.x, "with symbol proportional to", title.z, 
        " \n Each variable is adjusted for the serving size"),
  panel.first=grid(col="gray", lty="dotted"))
  
  for (i in 1:4) { 
    symbols(x = x[cereal$Shelf==i], y = y[cereal$Shelf==i], circles = z[cereal$Shelf==i],
            inches=0.1, bg=i, add=TRUE) 
  }
  legend(locator(1), legend=c("1", "2", "3", "4"), pch=c(16,16,16,16), col=c(1,2,3,4)) 

}

plot.it2(cereal$sugar, cereal$fat, cereal$sodium, "Sugar", "Fat", "Sodium")
plot.it2(cereal$sodium, cereal$fat, cereal$sugar, "Sodium", "Fat", "Sugar")
plot.it2(cereal$sodium, cereal$sugar, cereal$fat, "Sugar", "Sodium", "Fat")


#Need to tell R to go back to the 1x1 layout of plots
par(mfrow=c(1,1))





###################################################################################
#Chernoff faces
#   R does not do - see http://finzi.psych.upenn.edu/R/Rhelp01/archive/1085.html 
#   Below is S-Plus code for it 
#cereal<-read.table("C:\\chris\\UNL\\STAT873\\Chapter 3\\cereal.txt", header = TRUE)
#cereal.mat<-as.matrix.data.frame(cereal[,4:6])  #Data type needs to be converted from a data.frame to a matrix for faces()

#The face characteristics are plotted as sugar=area of face, fat=shape of face, and 
#  sodium=length of nose.  All other face characteristics are set to their midpoint value.  
#  See the help file for more information - type help(faces) in the command window.
#faces(cereal.mat, ncol = 10, nrow = 4, head = "Cereal data Chernoff faces plot", byrow = T)




###################################################################################
#Star Plots
stars(x = cereal[,4:6], nrow = 4, ncol = 10, key.loc = c(3, 11), main = "Cereal Star plot")

#Another version of the plot
stars(x = cereal[,4:6], nrow = 4, ncol = 10, key.loc = c(3, 11), draw.segments=TRUE, main = "Cereal Star plot")




###################################################################################
# Scatter plot matrix

temp<-data.frame(cereal$sugar, cereal$fat, cereal$sodium)
pairs(x = temp, pch=21, bg=cereal$Shelf, cex=2)
#Will need to put the legend in the lower right corner plot; otherwise, it will not be shown.
legend(locator(1), legend=c("1", "2", "3", "4"), pch=c(16,16,16,16), col=c(1,2,3,4), cex=0.5) 

#Also could do
#  pairs(data.frame(cereal$sugar, cereal$fat, cereal$sodium))



###################################################################################
# Parallel coordinate plot

library(MASS)

temp<-data.frame(cereal$ID, cereal$sugar, cereal$fat, cereal$sodium)
parcoord(x=temp, col=cereal$Shelf, main = "Parallel coordinate plot for cereal data")
legend(locator(1), legend=c("1", "2", "3", "4"), lty=c(1,1,1,1), col=c("black", "red", "green3", "blue"), cex=1) 
#I could have also used col = c(1,2,3,4)

#Helps to see how the colors were assigned
palette()



#Colors by shelf could have also been done this way:
shelf.colors<-ifelse(cereal$Shelf==1, "black", 
              ifelse(cereal$Shelf==2, "red", 
              ifelse(cereal$Shelf==3, "green3", "blue")))
parcoord(temp, col=shelf.colors, main = "Parallel coordinate plot for cereal data")
legend(locator(1), legend=c("1", "2", "3", "4"), lty=c(1,1,1,1), col=c("black", "red", "green3", "blue"), cex=1) 





###################################################################################
# Trellis plots

#Load package
library(lattice)

#Show default setting and a different theme
show.settings()
show.settings(col.whitebg())

trellis.par.set(col.whitebg()) #Set color theme for plots


#Need to restructure data set
sugar<-data.frame(ID = cereal$ID, Shelf = cereal$Shelf, value = cereal$sugar, variable = "Sugar")
fat<-data.frame(ID = cereal$ID, Shelf = cereal$Shelf, value = cereal$fat, variable = "Fat")
sodium<-data.frame(ID = cereal$ID, Shelf = cereal$Shelf, value = cereal$sodium, variable = "Sodium")
all.cereal<-rbind.data.frame(sugar, fat, sodium)


#Need to standardize the data by shelf and variable variable
#  NOTE: The SAS set of Chapter 3 notes does not standardize by shelf!  
#  The library nlme contains the gsummary function needed for BY processing (like SAS PROC MEANS with a BY statement)
library(nlme)
mean.group<-gsummary(object = all.cereal, FUN = mean, form = ~ value | Shelf / variable)
var.group<-gsummary(object = all.cereal, FUN = var, form = ~ value | Shelf / variable)
mean.var.group<-data.frame(mean.group, variance = var.group$value)

merge.set<-merge(x = all.cereal, y = mean.var.group, by = c("Shelf", "variable"))
stand.set<-data.frame(merge.set, stand = (merge.set$value.x-merge.set$value.y)/sqrt(merge.set$variance))

#Just check first 5 rows in the data set
merge.set[1:5,]
stand.set[1:5,]


#Various histogram plots
histogram(formula = ~ stand | variable + Shelf, data = stand.set, type = "percent", layout = c(3,4,1), 
          xlab = "Standardized value")

histogram(formula = ~ stand | variable + Shelf, data = stand.set, type = "density", layout = c(3,4),
          panel = function(x, ...) { panel.histogram(x, ...)
                                    panel.mathdensity(dmath = dnorm, args = list(mean=mean(x),sd=sd(x)), col=1)})

histogram(formula = ~ stand | variable + factor(Shelf), data = stand.set, type = "percent", layout = c(3,4), 
          xlab = "Standardized value", as.table=TRUE)

histogram(formula = ~ stand | variable + Shelf, data = stand.set, type = "percent", layout = c(3,4), 
          xlab = "Standardized value", as.table=TRUE)

densityplot(formula = ~ stand | variable + Shelf, data = stand.set, layout = c(3,4), 
          xlab = "Standardized value")



##################################################################################################################
#Other Trellis plots

xyplot(formula = sugar ~ fat | Shelf, data = cereal)
xyplot(formula = sugar ~ fat | factor(Shelf), data = cereal)
xyplot(formula = sugar ~ fat | factor(Shelf), data = cereal, panel = function(x, y) 
       {panel.xyplot(x,y)
        panel.lmline(x,y)})

xyplot(formula = sugar ~ fat | factor(Shelf), data = cereal, main="Scatter plots with fitted models", 
       scales = list(x=list(alternating=1), y=list(at=seq(0,0.4,0.1),alternating=1)),
       key = list(space="top", lines=list(lty=c(1,2), col=c(2,3)), lwd=c(1,2), 
                   text=list(lab=c("linear regression", "loess")), columns=2),
       panel = function(x, y) 
       {
        panel.xyplot(x,y, col=1, lwd=2)
        panel.lmline(x,y, col=2, lty=1, lwd=1)
        panel.loess(x,y, col=3, lty=2, lwd=2)
        panel.grid(h=-1, v=-1, lty=3, lwd=1, col=1) })    
#Warnings occur with the above code because of the small sample size
#For the panel.grid() function, I recommend using col="#808080" in the color statement to get light gray gridlines


#Continuous conditioning using shingles
sodium.new<-equal.count(cereal$sodium, number=3, overlap=0.1)
xyplot(formula = sugar ~ fat | sodium.new, data = cereal, groups = factor(Shelf), auto.key=TRUE)
#Note that sodium.new is NOT in the cereal data set so R looks for it elsewhere



#############################################################################
# 3D plots from the lattice library
cloud(sugar ~ fat * sodium, data = cereal,
      groups = factor(Shelf))
      
cloud(sugar ~ fat * sodium | Shelf, data = cereal)

cloud(sugar ~ fat * sodium, data = cereal,
      groups = factor(Shelf), screen = list(x = -90, y = 70),
      aspect = c(1, 1), distance = .4, zoom = .6,
      key = list(title = "Cereal Data", x = .1, y=.9,
                 corner = c(0,1),
                 border = TRUE, 
                 points = Rows(trellis.par.get("superpose.symbol"), 1:3),
                 text = list(levels(factor(cereal$Shelf)))))
