# This program will install all the R packages we use in the workshops
# The file "R Workshop Files 1-Day Version.zip" contains the PowerPoint
# slides with detailed instructions on how to install R and these packages.
# Once R is installed, choose "File> Open script..." to open this file.
# Then choose, "Edit> Run all" to run the program. The packages will
# then find the files on the Internet and install them.

myPackages <- c("car","ggplot2","gmodels","gplots","Hmisc","memisc",
  "prettyR","RGtk2","rattle", 
  "xtable")
install.packages(myPackages)
install.packages("Rcmdr", dependencies=TRUE)
