############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  9-17-03                                                           #
# PURPOSE: CART in R                                                       #
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
                   
                   
#############################################################################################
library(rpart) 
#Find the classification tree using the default options
tree.wheat<-rpart(formula = type1~skhard+sksize+skwt+skmst+hrw+skden, data = wheat2)

#Examine the object
names(tree.wheat)

tree.wheat$frame

#See a summary of the results
summary(tree.wheat)
par(mfrow=c(1,1))
plot(tree.wheat) 
text(tree.wheat) 


##############################################################################################
# Different library
library(tree) 

#Find the classification tree using the default options
tree.wheat<-tree(type1~skhard+sksize+skwt+skmst+hrw+skden, data = wheat2)

#Examine the object
names(tree.wheat)

tree.wheat$frame

#See a summary of the results
summary(tree.wheat)
plot(tree.wheat) 
text(tree.wheat) 

#Prune the tree to make it less complex
wheat.prune<-prune.tree(tree.wheat) 
plot(wheat.prune) 
wheat.prune.auto<-prune.tree(tree.wheat, best=15)
plot(wheat.prune.auto)
text(wheat.prune.auto) 

summary(wheat.prune.auto)

# find out what kernel is where by clicking on plot
identify(tree.wheat) 
