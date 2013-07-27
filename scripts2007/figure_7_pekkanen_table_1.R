#Create Vectors for coefs and standard errors for each model, and variable names
    #note that we  exclude "margin squared since it doesn't appear in either model

coef.vec.1<- c(0.18, -0.19,-0.39,-0.09, NA, 0.04,-0.86, 0.39,-3.76, -1.61,
    -0.34, -1.17, -1.15,-1.52, -1.66, -1.34,-2.89,-1.88,-1.08, 0.20)
se.vec.1 <-  c(0.22, 0.22, 0.18,.29, NA, 0.08,0.26,0.29,0.36,.19,0.19, 0.22,
    0.22,0.25,0.28,0.32,0.48, 0.43,0.41, 0.20)
coef.vec.2 <-  c(0.27,-0.19, NA, NA, 0.005, 0.04,-0.98,-.36,-3.66, -1.59,
     -0.45, -1.24, -1.04, -1.83, -1.82, -1.21, -2.77, -1.34, -0.94, 0.13)
se.vec.2 <- c(0.22,0.24, NA, NA, 0.004, 0.09 , .31 , .30 , .37 , .21 , .21 , .24 , .24,
     .29 , .32 , .33 , .49 , .46 , .49 , .26)
var.names <- c("Zombie" , "SMD Only", "PR Only", "Costa Rican in PR", 
    "Vote share margin", "Urban-Rural Index","No factional\nmembership",
    "Legal professional", "1st Term", "2nd Term", "4th Term",
    "5th Term","6th Term","7th Term","8th Term","9th Term","10th Term",
    "11th Term","12th Term", "Constant")

y.axis <- length(var.names):1#create indicator for y.axis, descending so that R orders vars from top to bottom on y-axis
adjust <- .2 #create object that we will use to adjust points and lines up and down to distinguish between models
    
#pdf("pekkanen_fig.pdf", height = 8, width = 7) #open pdf device
#png("pekkanen_fig.png", height = 600, width = 600)
layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
    widths = c(1.5, 5))#we use layout command, create a small second panel on left side.
                #using c(2,1) in matrix command tells R to create right panel 1st
#layout.show(2) #can use this command to check results of layout command (but it must be commented out when creating PDF).

par(mar=c(2,5,.5,1))#set margins for regression plot
plot(coef.vec.1, y.axis+adjust, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = .8, #plot model 1 coefs using black points (pch = 19, default = black), adding the "adjust amount" to the y.axis indicator to move points up
    xlim = c(min((coef.vec.1-qnorm(.975)*se.vec.1 -.1), (coef.vec.2-qnorm(.975)*se.vec.2 -.1), na.rm = T), #set xlims at mins and maximums (from both models) of confidence intervals, plus .1 to leave room at ends of plots
    max((coef.vec.1+qnorm(.975)*se.vec.1 -.1), (coef.vec.2+qnorm(.975)*se.vec.2 -.1), na.rm = T)),  #use na.rm=T since vectors have missing values
    ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,pretty(coef.vec.1, 3))#add x-axis and labels; "pretty" creates a sequence of  equally spaced nice values that cover the range of the values in 'x'-- in this case, integers
axis(2, at = y.axis, label = var.names, las = 1, tick = T)#add y-axis and labels; las = 1 makes labels perpendicular to y-axis
abline(h = y.axis, lty = 2, lwd = .5, col = "light grey")#draw light dotted line at each variable for dotplot effect
segments(coef.vec.1-qnorm(.975)*se.vec.1, y.axis+adjust, coef.vec.1+qnorm(.975)*se.vec.1, y.axis+adjust, lwd =  1.3)#draw lines connecting 95% confidence intervals
abline(v=0, lty = 2) # draw dotted line through 0 for reference line for null significance hypothesis testing

#add 2nd model
    #because we are using white points and do want the lines to go "through" points rather than over them
        #we draw lines first and the overlay points
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis-adjust, coef.vec.2+qnorm(.975)*se.vec.2, y.axis-adjust, lwd =  1.3)#draw lines connecting 95% confidence intervals
points(coef.vec.2, y.axis-adjust, pch = 21, cex = .8, bg = "white" ) #add point estimates for 2nd model; pch = 21 uses for overlay points, and "white" for white color

#add legend (manually) to identify which dots denote model 1 and which denote model 2
#legend(-4.5, 20, c("Model 1", "Model 2"), pch = c(19,21),bty = "n")
points(-4, 19.5, pch = 19)
text(-3.7, 19.5, "Model 1", adj = 0)#left-justify text using adj  = 0
points(-4, 18.5, pch = 21)
text(-3.7, 18.5, "Model 2", adj = 0)#left-justify text using adj  = 0

############################################################################

#Create Variable Categories and Braces to go in 2nd (left) plot

par(mar=c(2,0,.5,0)) #set margins--- bottom (1st number) and top (3rd number) must be the same as in 1st plot
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")#call empty plot using type="n"
    #use a sequence of length 20 so that x and y have same length

left.side <- .55#use this to manipulate how far segments are from y-axis
    #note:  getting braces and text in proper place requires much trial and error
segments(left.side,20.2,left.side,16.7) #add brackets around MP Type vars
segments(left.side,20.2,left.side+.15,20.2) #1 segment at a time
segments(left.side,16.7,left.side+.15,16.7)
text(.4, 18.5, "MP Type", srt = 90, font = 3)#Add text; "srt" rotates to 90 degrees, font = 3 == italics

#don't add "Electoral Strength" since it's only 1 variable

segments(left.side,15.6,left.side,12.3) #add brackets around "Misc Controls"
segments(left.side,15.6,left.side+.15,15.6) #one segment at a time
segments(left.side,12.3,left.side+.15,12.3)
text(.3, 14, "Misc\nControls", srt = 90, font = 3)#Add text; "srt" rotates to 90 degrees, font = 3 == italics

segments(left.side,12.15,left.side,1.8) #add brackets around "Seniority"
segments(left.side,12.15,left.side+.15,12.15)   #one segment at a time
segments(left.side,1.8,left.side+.15,1.8)
text(.4, 7, "Seniority", srt = 90, font = 3)#Add text; "srt" rotates to 90 degrees, font = 3 == italics

#dev.off()
