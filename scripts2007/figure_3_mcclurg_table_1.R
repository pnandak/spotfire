#create vectors for each column
var.names <- c("Size\n(1260)", "Political Talk\n(1253)", "Political Agreement\n(1154)",
    "Political Knowledge\n(1220)")
mean.vec <- c(3.13, 1.82, .43, 1.22)
sd.vec <- c(1.49, .61, .41, .42)
min.vec <- c(1, 0, 0, 0)
max.vec <- c(5,3,1,2)

#set up ordering, using means, from highest to lowest
mean.vec.order <- sort(mean.vec, decreasing = TRUE) #first order vars by means, from highest to lowest.
sd.vec.order <- sd.vec[order(mean.vec, decreasing=TRUE)] #then order other vectors according to sorted mean
min.vec.order <- min.vec[order(mean.vec, decreasing = TRUE)]
max.vec.order <- max.vec[order(mean.vec, decreasing = TRUE)]
var.names.order <- var.names[order(mean.vec, decreasing = TRUE)]

y.axis <- c(4:1)# #create indicator for y.axis, descending so that R orders vars from top to bottom on y-axis

#pdf("mcclurg_fig.pdf", height = 2.5, width =4.5) #open pdf device
#png("mcclurg_fig.png", height = 300, width = 440)#create png
par(mfrow=c(1,1), mar=c(3,6,1,1))#use single panel, with specified margins
plot(mean.vec.order, y.axis, type = "p", pch = 19, xlab="", ylab ="", #initiate plot commands with sorted mean vector displayed as points; don't label x or y-labels since they're selft explanotry
    axes = F, xlim = c(0, 5.1),#turn off axes so we can use fine control below. set xlims based on min and max values of variables
    ylim = c(min(y.axis - .1), max(y.axis + .1)), main = "", cex = .6, yaxs = "i", xaxs = "i")#increase y-axis min/max by .1 so there no points and lines don't run into top and bottom of panels
box(bty = "l") #connect axes together; we don't use box() -- which we create box around entire plot since it's a single plot
segments((mean.vec.order - sd.vec.order), y.axis, (mean.vec.order + sd.vec.order), y.axis)#draw lines connecting means +/- 1 st.dev
segments(min.vec.order, y.axis, max.vec.order, y.axis, lty = 2)#draw dotted lines (lty = 2) connecting minimum of variables to maximums
axis(1, at = seq(0,5,by=1), labels =  seq(0,5,by=1), tick = T, #draw x-axis and labels, at each integer from 0 to 5, and draw tick marks
   cex.axis = .7, mgp = c(2,.5,0))#reduce label size and move labels closer to tick mark (2nd part of mgp command
axis(2, at = y.axis, label = var.names.order, las = 1, tick = T, cex.axis = .7, #draw y-axis and labels, las = 1 makes y-labels perpendicular to y-axis instead of parallel
    mgp = c(2,3.2,0), hadj =.5)# hadj =.5 uses center-alignment for y-label; need to adjust 2nd command of mgp to account for centering.
    
#dev.off() #turn off pdf device. graph is created in working directory.
