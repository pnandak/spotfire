############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  9-17-03                                                           #
# PURPOSE: Create plots in R for Chapter 7                                 #
#                                                                          #
# NOTES:                                                                   #
#                                                                          #
############################################################################


wheat<-read.table(file = "C:\\chris\\UNL\\STAT873\\Chapter 7\\wheat.dat", header=FALSE,
                  col.names = c("lot", "class", "type", "skden1", "skden2", "skden3", "skhard", "sksize", "skwt", "skmst"))

kernel<-1:nrow(wheat)
skden<-(wheat$skden1 + wheat$skden2 + wheat$skden3)/3
hrw<-ifelse(wheat$class=="hrw", 1, 0)
type1<-ifelse(wheat$type==1, "Healthy", ifelse(wheat$type==2, "Sprout", "Scab"))

wheat2<-data.frame(kernel, type1, skden, hrw, skhard = wheat$skhard, sksize = wheat$sksize, 
                   skwt = wheat$skwt, skmst = wheat$skmst)

###################################################################################
#Star plot
stars(x = wheat2[,(-1):(-2)], ncol = 20, key.loc = c(10, 0), draw.segments=TRUE, cex=0.5, main = "Wheat star plot", label = NULL)


#Figure out reason for differences in plot
wheat2[140:150,]

#Sort by type1 variable - note that order(wheat2$type1) gives the observation numbers for the sorted type1 variable
stars(x = wheat2[order(wheat2$type1),(-1):(-2)], ncol=20, key.loc = c(10, 0), draw.segments=TRUE, cex=0.5, main = "Wheat star plot")



#Open new plotting sheet
win.graph(width = 7, height = 7, pointsize = 9)

library(MASS)

#reorder variables since hrw is binary (may distort plot)
temp<-data.frame(wheat2$kernel, wheat2$skden, wheat2$skhard, wheat2$sksize, wheat2$skwt, wheat2$skmst, wheat2$hrw)

#Colors by type:
wheat.colors<-ifelse(wheat2$type1=="Healthy", "black", 
              ifelse(wheat2$type1=="Sprout", "red", "green3"))
parcoord(x = temp, col = wheat.colors, main = "Parallel coordinate plot for wheat data")
legend(locator(1), legend=c("Healthy", "Sprout", "Scab"), lty=c(1,1,1), col=c("black", "red", "green3"), cex=1, bty="n") 

#31
wheat.colors3<-ifelse(wheat2$type1=="Healthy", "black", 
               ifelse(wheat2$type1=="Sprout", "red", 
               ifelse(wheat2$type1=="Scab" & wheat2$kernel != 31, "green3", "purple")))
line.width<-ifelse(wheat2$kernel == 31, 2, 1)            
win.graph(width = 7, height = 7, pointsize = 9)         
parcoord(x = temp, col = wheat.colors3, lwd = line.width, main = "Parallel coordinate plot for wheat data - highlight kernel 31")
legend(locator(1), legend=c("Healthy", "Sprout", "Scab", "Kernel 31"), lty=c(1,1,1,1), 
       col=c("black", "red", "green3", "purple"), cex=1, bty="n") 

#Sort by wheat type
wheat.colors2<-ifelse(wheat2$type1=="Healthy", 1, 
               ifelse(wheat2$type1=="Sprout", 2, 3))
temp2<-data.frame(wheat.colors2, temp)     
win.graph(width = 7, height = 7, pointsize = 9)         
parcoord(x = temp2[order(wheat.colors2),], col = wheat.colors[order(wheat.colors2)], 
         main = "Parallel coordinate plot for wheat data - sort by type1")
legend(locator(1), legend=c("Healthy", "Sprout", "Scab"), lty=c(1,1,1), col=c("black", "red", "green3"), cex=1, bty="n") 




###################################################################################
#PCA

save<-princomp(formula = ~ skden + hrw + skhard + sksize + skwt + skmst, data = wheat2, cor = TRUE, scores = TRUE)
summary(save, loadings = TRUE, cutoff = 0.0)

#Plots
  par(pty = "s")
  par(mfrow = c(1,2))
  screeplot(x = save, type="barplot", main="Bar plot of eig. for wheat data")
  screeplot(x = save, type="lines", main="Scree plot for wheat data")

  par(mfrow = c(1,1))
  biplot(x = save, scale = 1, pc.biplot = FALSE, main = "Biplot for wheat data", panel.first=grid(col="gray", lty="dotted"))
      
  #Notice that circles can not take on a negative value!  
  symbols(x = save$scores[,1], y = save$scores[,2], circles = save$scores[,3]-min(save$scores[,3]), 
          inches=0.25, xlab = "PC1", ylab = "PC2", main = "Bubble plot for first three PCs \n Wheat data")
  abline(h = 0, lty = 1, lwd = 2)  
  abline(v = 0, lty = 1, lwd = 2)  
  text(x = save$scores[,1], y = save$scores[,2], col = 2, cex = 0.5)  


###################################################################################
#Discriminant Analysis

library(MASS)

#Using proportional priors and linear discriminant analysis
  save<-lda(formula = type1 ~ skden + hrw + skhard + sksize + skwt + skmst, data = wheat2, 
            CV = TRUE)
  summary(save) #Not useful
  names(save)      
  
  #first 10 observations - just to show some results
  results<-data.frame(wheat2$type1, save$class, save$posterior)
  results[1:10,]

  #Summarize crossvalidation
    class.table<-table(wheat2$type1, save$class)
    class.table
  
  

  
  #One could make the summary of the classifications nicer by writing a function
   summarize.class<-function(original, classify) {
    class.table<-table(original, classify)
    numb<-rowSums(class.table)
    prop<-round(class.table/numb,4)
    list(class.table = class.table, prop = prop) #return a list object type
  }

  summarize.class(original = wheat2$type1, classify = save$class)
  
  
  #This code is suppose to predict the classifications for a new data set
  #  Since we do not have a new data set for this problem, I just used wheat2 again.
  #  Unfortunately, R doesn't work here????
  #predict.lda(object = save, newdata = wheat2) 
  
#Some functions in MASS do not work?
#  plot.lda, ldahist - I got to work for only one variable at a time, predict
#  Note that the object from lda() is not of class "lda"!  


#Simple histogram by the grouping variable
par(mfrow=c(1,1))
ldahist(data = wheat2$skden, g = wheat2$type1)




#Using equal priors
  save<-lda(formula = type1 ~ skden + hrw + skhard + sksize + skwt + skmst, data = wheat2, 
            prior = c(1/3, 1/3, 1/3), CV = TRUE)
  #Could have also done the priors with: rep(1,nlevels(wheat2$type1))/nlevels(wheat2$type1)          
                     
  #first 10 observations
  results<-data.frame(original = wheat2$type1, class = save$class, save$posterior)
  results[1:10,]
  summarize.class(wheat2$type1, save$class)



#Quadratic discriminant analysis
  save<-qda(formula = type1 ~ skden + hrw + skhard + sksize + skwt + skmst, data = wheat2, 
                CV = TRUE)
  summarize.class(wheat2$type1, save$class)

  save<-qda(formula = type1 ~ skden + hrw + skhard + sksize + skwt + skmst, data = wheat2, 
                prior = c(1/3, 1/3, 1/3), CV = TRUE)
  summarize.class(wheat2$type1, save$class)
 
    
###############################################################################################
# K-nearest neighbor discriminant analysis

  #These functions are in the "class" package
  library(class)

  #remove the first column which contains the kernel number and the classifications
  wheat3<-data.frame(wheat2$skden, wheat2$hrw, wheat2$skhard, wheat2$sksize, wheat2$skwt, wheat2$skmst)
  
  #The formula method for specifying the variables does not work with this function.
  #  The use.all part tells R to use all experimental units which have the same distance.  For example,
  #  If the kth and (k+1)th closest experimental units to an experimental unit have the same distance to it,
  #  they both are used to determine the classification of the experimental unit.
  #  Ties in posterior probabilities are broken at random (I do not like this).  
  save<-knn.cv(train = wheat3, cl = wheat2$type1, k = 5, prob = TRUE, use.all = TRUE)
  names(save)  #Not helpful - why?
  summary(save)

  #In order to see the highest posterior probability for an experimental unit, use the command below.
  #  I think Venables and Ripley (authors of the function) are using some older form of syntax.
  attributes(save)

  
  #This function can be used with a test or validation data set
  #  knn(train = , test = , cl = , k = 1, l = 0, prob = FALSE, use.all = TRUE)
  
  #Note that this will return perfect classifications!  1 nearest neighbor is used on the same data set
  #  WITHOUT crossvalidation.  
  save<-knn(train = wheat3, test = wheat3,  cl = wheat2$type1, k = 1, prob = TRUE, use.all = TRUE)
  summarize.class(wheat2$type1, save)

  
  #
