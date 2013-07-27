#get cell counts
#first: year data
prop.left <- 342
prop.right <- 120
maj.left <- 86
maj.right <- 256
#second: gov't data (in parentheses in table)
prop.left.gov <- 8
prop.right.gov <- 1
maj.left.gov <- 0
maj.right.gov <- 8

#create matrix for stacked barplot
#note: since using horizontal barplot, need to invert matrices and labels
year.matrix <- 100* cbind(c(maj.left/(maj.left+maj.right), maj.right/(maj.left+maj.right)),
    c(prop.left/(prop.left+prop.right), prop.right/(prop.left+prop.right)))
    #looks like this --- rows are reversed compared to what appears in bar plot:
    #      [,1]     [,2]
    #[1,] 25.1462 74.02597
    #[2,] 74.8538 25.97403
country.matrix <- 100* cbind(c(maj.left.gov/(maj.left.gov+maj.right.gov), maj.right.gov/(maj.left.gov+maj.right.gov)),
    c(prop.left.gov/(prop.left.gov+prop.right.gov), prop.right.gov/(prop.left.gov+prop.right.gov)))
    #looks like this:
    #     [,1]     [,2]
    #[1,]    0 88.88889
    #[2,]  100 11.11111

#########################
#Mosaic plot

#1) left plot
#need to manipulate data into a contingency table for mosaicplot command

prop.vec.mosaic <- c(prop.left, prop.right, maj.left, maj.right)
prop.matrix.mosaic <- matrix(prop.vec.mosaic, nrow = 2, ncol =2, byrow = T,
     dimnames = list(c("Proportional", "Majoritarian"), c("Left", "Right")))
prop.table.mosaic  <- as.table(prop.matrix.mosaic)

#pdf("iversen_fig1.pdf", width = 8.5, height = 5.7) #use this command to create PDF 
#png("iversen_fig1.png", width = 820, height = 500) # use this command to create png
par(mfrow = c(1,2), mar=c(.5,3,6.6,1.2))#set margins for left plot

mosaicplot(prop.table.mosaic, color = c("gray60", "gray80"), #contrast rectangles with lighter/darker gray 
    main = "", shade = FALSE, xlab = "", 
    ylab = "", las = 1, cex.axis =.8)
text(.37,.1,"120") #add counts by hand (note: need to use trial and error for coordinates)
text(.37,.6,"342")
text(.8,.3,"256")
text(.8,.88,"86")
mtext("Electoral Systems and\nGovernment Partisanship,\n1945-1998", 3, line = 3, #add title to plot; 3 = top axis, line = .5 adjusts distance from top of plot
    cex = 1.2, font = 2) #cex reduces font size, font = 2 for bold text
mtext("Government Partisanship", 2, font = 2, line = 1, cex = 1.1)
mtext("Electoral System", 3, font = 2, line = 1, cex = 1.1)
#dev.off()

#2) left plot: by government

prop.vec.mosaic.gov <- c(prop.left.gov, prop.right.gov, maj.left.gov, maj.right.gov)
prop.matrix.mosaic.gov <- matrix(prop.vec.mosaic.gov, nrow = 2, ncol =2, byrow = T,
     dimnames = list(c("Proportional", "Majoritarian"), c("Yes", "No")))
prop.table.mosaic.gov  <- as.table(prop.matrix.mosaic.gov)

mosaicplot(prop.table.mosaic.gov, color = c("gray60", "gray80"), 
    main = "", shade = FALSE, xlab = "", 
    ylab = "", las = 1, cex.axis =.8)
text(.32,.03,"1") #add counts by hand (note: need to use trial and error for coordinates)
text(.32,.6,"8")
text(.8,.45,"8 (of 8)")
mtext("Electoral Systems and Percentage of\nCountries With Center-Left Governments\nat Least 50\% of Time, 1945-1998", 3, line = 3, #add title to plot; 3 = top axis, line = .5 adjusts distance from top of plot
    cex = 1.2, font = 2) #cex reduces font size, font = 2 for bold text
mtext("Electoral System", 3, font = 2, line = 1, cex  = 1.1)
mtext("At Least 50% of Time?", 2, font = 2, line = 1, cex = 1.1)

#dev.off() #use this to end creation of pdf or png
