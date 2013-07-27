##########################################
#          POLS 501, R 2, WEEK 7         #
#               2/18/2011                #
#           LOREN COLLINGWOOD            #
##########################################

#Set Directory
final <- "C:\\Users\\Loren\\Documents\\UW courses\\POLS501\\week7"
setwd(final)
list.files() #view directory

##########################################
#           DATA STRUCTURES              #
##########################################

###########################################
#Lists
###########################################
#Lists are collections of objects that can be iterated over and used to store information.
#Lists can be extremely helpful when manipulating data

mylist <- list(first = c(1,3,5), second=c('one', 'three', 'five'), third='end')
mylist # view the object
#Referencing a list
mylist[[1]] 
mylist[[2]]
mylist$third

library(foreign) # for stata files

dat_list <- list(wapoll =  read.dta("wapoll10_218.dta", convert.factors=FALSE), 
            mylist = mylist, vect_num = c(1,2,3,4,5,6), mymat2 = matrix(1:12, nrow=3, byrow=T))

#Can employ regular commands with list. Here we create a histogram of the sdo_scale variable
hist(dat_list$wapoll$sdo_scale)

library(ggplot2)
#install.packages("ggplot2") # http://had.co.nz/ggplot2/geom_histogram.html
qplot(sdo_scale, data=dat_list$wapoll, geom="histogram", binwidth=1)

###########################################
#               DATA FRAMES               #
###########################################

#Now let's create a separate object of the Washington Poll

wapoll <- dat_list$wapoll  # extracting a data frame from the list
fix(wapoll)                # look at it, do it!

class(wapoll)  #data.frame class
sapply(wapoll,class) # look at the class objects for each variable
names(wapoll)
#Indexing a data frame
wapoll[1:5,5:20]
wapll[1:5,] #view just first 5 rows
wapoll2 <- wapoll2[1:5,5:20] #examine a subset, oh my, for pedagogical purposes let's just look at the first 5 rows and some columns

reg_data <- data.frame(with(wapoll,cbind(sdo_scale ,income_r ,female ,married ,white ,region2 ,  # this with function says "with" dataset
                        region3 ,educ_r, age3 ,ideo_r ,dem ,rep, teastrong)))
reg_data <- na.omit(reg_data) # list-wise delete the missing data. Stata does this automatically.

sdo_reg <- lm(sdo_scale ~ ., data=reg_data) # the "." here says take all variables in the data frame

#Reading text files or delimted files, a very common way to read in files.
?read.delim
wp_delim <- read.delim("wapolltxt.txt", header=T, sep= '\t')
names(wp_delim)
