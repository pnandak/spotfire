library(ctest)  # Not required with R versions >= 0.99
tea <- matrix(c(3,1,1,3),ncol=2)  # entering the data as a matrix
fisher.test(tea,alternative="greater") # one-sided exact test
