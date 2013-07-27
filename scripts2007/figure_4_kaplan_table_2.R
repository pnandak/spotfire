library(foreign)#allow for import of Stata datasets

#Note 1: for these graphs we will use the complete data (obtained from David Park's website --
    #http://home.gwu.edu/~dkp/tpm.htm) to plot summary statistics, and save it into your R working
   #directory (or change your working directory to that where the data is located--e.g.
   #setwd("C:\\Documents and Settings\\Your Name\\My Documents\\....")
   
    #the data are available here:
    #http://svn.cluelessresearch.com/tables2graphs/kaplan_campaign.dta
    #http://svn.cluelessresearch.com/tables2graphs/kaplan_campaign_issue.dta
    #http://svn.cluelessresearch.com/tables2graphs/kaplan_issue.dta
    
#Note 2: You will need to install the "vioplot" package to run this script completely;
    #use the following command
 #install.packages("vioplot")
library(vioplot)
    
#TABLE 2 from Kaplan (2006)
#begin by create vectors for each column in table
    #use following for spreading text over 2-3 lines
var.names <- c("Issue\nConvergence\n(982)", "Competitiveness\n(65)", 
    "Total Spending/\nCapita \n(65)", "Difference\nSpending/Capita\n(65)",
     "State Voting Age\nPop. (ln) (65)", 
    "Percent\nNegative Ads\n(65)", "2000 Year\n(65)", "2002 Year\n(65)",
    "Consensual\nIssue\n(43)", "Issue Owned\n(43)", "Issue\nSalience\n(43)") 
    #use following for single line
#var.names <- c("Issue Convergence (982)", "Competitiveness (65)", 
#    "Total Spending/Capita  (65)", "Difference Spending/Capita (65)",
#     "State Voting Age Pop. (ln) (65)", 
#    "Percent Negative Ads (65)", "2000 Year (65)", "2002 Year (65)",
#    "Consensual Issue (43)", "Issue Owned (43)", "Issue Salience (43)") 
mean.vec <- c(24.85, 1.54, 3.47, 1.12, 1.2, 21.38, .38, .32, .28, .49, 2.86)
sd.vec <- c(34.73, 1.2, 2.71, 1.32, .85, 16.84, .49, .47, .45, .51, 6.38)
min.vec <- c(0, 0, .28, .03, -.65, 0, 0, 0, 0,0, 0)
max.vec <- c(99.98, 3, 13.39, 9.26, 3.13, 54.96, 1, 1, 1, 1, 35.63)

#Divide into groups:
    #Binary
    #millions (+competitiveness)
    #percents (+issue salience)

campaign.data <- read.dta("kaplan_campaign.dta")#data at the campaign level
campaign.issue.data <- read.dta("kaplan_campaign_issue.dta")#data at the campaign issue level
issue.data <- read.dta("kaplan_issue.dta") #data at the issue level

#begin graph

#pdf("kaplan_fig.pdf", height = 7.5, width =5) #open pdf device
#png("kaplan_fig.png", height = 600, width =450) #create png
#place graphs in single column
#use layout to make plot heights unequal, since bottom 2 need more room than 1st (this saves space overall)
layout(matrix(c(1,2,3)), heights = c(.8,1.2,1.4))#create 3x1 column of plot regions
#layout.show(3)

mar.left <- 8 #indicator for left margin size
point.size <- 1.1 #indicator for point size in violin plot
#1st plot: means of binary variables
keep <- 7:10 #pull out only binary vars, which happen to be the 7th-10th vars in our vectors
#create vectors for each; could also use mean.vec[7:10] etc.
mean.vec.binary <-  mean.vec[keep] 
min.vec.binary <- min.vec[keep]
max.vec.binary <- max.vec[keep]
var.names.binary<-var.names[keep]

#order vectors by mean values, in descending order
mean.vec.binary.order <- sort(mean.vec.binary, decreasing = TRUE) #first sort by means
var.names.binary.order <- var.names.binary[order(mean.vec.binary, decreasing = TRUE)]#variable names
y.axis.binary <- c(length(mean.vec.binary):1)# descending so that R orders vars from top to bottom on y-axis

par(mar=c(2.2,mar.left,2,1)) #adjust margins for  plot
plot(mean.vec.binary.order, y.axis.binary, type = "p", pch = 18, cex = 1.5, xlab="", ylab ="", #plot circular points; leave axis labels blank; we will fil in using axis command
    axes = F, xlim = c(0,1), #turn off axes so we can use fine control; set min/max of x-label according to min and maximums of var
    ylim = c(min(y.axis.binary - .1), max(y.axis.binary + .1)), # increase y-axis min/max by .1 so there no points and lines don't run into top and bottom of panels
     main = "Means of Binary Variables", xaxs="i") #add title; xaxs="i" uses internal axes, meaning axes are drawn right at xlim values 
box() #draw box around panel
axis(1, at = seq(0,1,by=.25), labels =  c(0,".25",".5",".75",1), tick = T, #draw x-axis and labels, tick marks and labels every .25,
     cex.axis = 1, mgp = c(2,.5,0))#shrink axis-label size to .8, use mgp to place labels closer to tick marks
axis(2, at = y.axis.binary, label = var.names.binary.order, las = 1, tick = T,#draw y-axis and labels, las = 1 makes y-labels perpendicular to y-axis instead of parallel
    cex.axis =1, mgp = c(2,3,0), hadj=.5) # hadj =.5 uses center-alignment for y-label; need to adjust
        #2nd command in "mgp" to account for centering
#############################################
#2ND PLOT: violin plots of percentage vars
    #pull out variables

#note: the following are ordered in terms of their median, from highest to lowest;
    #(issue convergence and issue salience both have medians of 0)
    #for many vectors, it would be easier to automate this process
percent.negative <- campaign.data$npercneg
issue.convergence <- campaign.issue.data$convf 
issue.salience <- issue.data$nsalnc
y.axis.percents <- c(3:1)


#create violin plot: 1 for each variable
par(mar = c(2.2,mar.left,2,1))
plot(issue.salience, xlim = c(0,100), ylim = c(.5,3.5), type = "n", axes = F, #ylim comes from fact that "vioplot" puts each plot at 1,2,3,etc. 
    main = "Percentage Variables", ylab = "", xlab = "", xaxs ="i")#call empty plot so we can
    #modify plot more easily than using vioplot command on its own
vioplot(issue.salience,  issue.convergence, percent.negative, #place in reverse order so they'r graphed from top to bottom
     horizontal = T, col = "dark gray", names = c("", "", ""), add = T, rectCol = "white")#use add = T to add to existig plot
#since medians for issue convergence and issue salience fall right on y-axis (i.e. at 0), create bigger points manually
    #so that they are easily seen
points(median(percent.negative), 3,  col = "black", cex = point.size, pch = 19)
points(median(issue.convergence), 2,  col = "black", cex = point.size, pch = 19, xpd = T)
points(median(issue.salience), 1,  col = "black", cex = point.size, pch = 19, xpd = T)

#Add variable labels manually since vioplot does not have axes = F option
axis(1, at = seq(0,100,by=25), labels =  c(0,25,50,75,"100%"), tick = T, #draw x-axis and labels, tick marks and labels every 25%,
    cex.axis = 1, mgp = c(2,.5,0))
segments(0,0,0,4)#use this to connect axis line where median dot divide it
axis(2, at = c(3:1), label = c(var.names[6], var.names[1], var.names[11]), las = 1, tick = T,  #draw y-axis and labels, las = 1 makes y-labels perpendicular to y-axis instead of parallel
    cex.axis =1,mgp = c(2,3.4,0), hadj=.5) # hadj =.5 uses center-alignment for y-label; need to adjust
        #2nd command in "mgp" to account for centering

#############################################
#3RD PLOT: violin plots of percentage vars
    #pull out variables

#note: the following are ordered in terms of their median, from highest to lowest
    #for many vectors, it would be easier to automate this process
total.spending <- campaign.data$nspendcap 
competitiveness <- campaign.data$cqnum
state.pop <- campaign.data$lnpop
difference.spending  <- campaign.data$ndifspendcap
#note: max of all vars = 13.4
    #min = -.6524

par(mar = c(2.2,mar.left,2,1))
plot(total.spending, xlim = c(-.66,14), ylim = c(.5,4.5), type = "n", axes = F, #ylim comes from fact that "vioplot" puts each plot at 1,2,3,etc. 
    main = "Variables Measured in Millions", ylab = "", xlab = "", xaxs ="i")#call empty plot so we can
    #modify plot more easily than using vioplot command on its own
vioplot(difference.spending,  state.pop, competitiveness, total.spending, #place in reverse order so they'r graphed from top to bottom
     horizontal = T, col = "dark gray",names = c("", "", "",""), add = T, rectCol = "white")#use add = T to add to existig plot
#use larger points to be consistent with 2nd plot
points(median(total.spending), 4,  col = "black", cex = point.size, pch = 19)
points(median(competitiveness), 3,  col = "black", cex = point.size, pch = 19)
points(median(state.pop), 2,  col = "black", cex = point.size, pch = 19)
points(median(difference.spending), 1,  col = "black", cex = point.size, pch = 19)

#Add variable labels manually since vioplot does not have axes = F option
axis(1, at = seq(0,14,by=2), labels =  seq(0,14,by=2), tick = T, las  = 1,#draw x-axis and labels, tick marks and labels every 2nd value,
    line =0, cex.axis = 1, mgp = c(2,.5,0)) #shrink axis-label size to .8, use mgp to place labels closer to tick marks
axis(2, at = c(4:1), label = c(var.names[3], var.names[2], var.names[5], var.names[4]), las = 1, tick = T,  #draw y-axis and labels, las = 1 makes y-labels perpendicular to y-axis instead of parallel
    cex.axis =1, mgp = c(2,4.2,0), hadj=.5) # hadj =.5 uses center-alignment for y-label; need to adjust
        #2nd command in "mgp" to account for centering

#dev.off()
