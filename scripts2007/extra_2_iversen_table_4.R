#enter data as matrix
    #Majoritarian
    #1st column will be labels for graph
    
major <- cbind(c("Australia (Majority)", "Canada (SMP)", "France (Run-off)", 
    "Ireland (STV)", "Japan (SNTV)", "New Zealand (SMP)", "UK (SMP)", "USA (SMP)"), 
    c(2.5, 2.2, 3.8, 2.8, 2.7, 2.0, 2.1, 1.9), c(.19, .13, .16, .70, .61, 0, .16, .39))

#vectors instead
major.countries <- c("Australia (Majority)", "Canada (SMP)", "France (Run-off)", 
    "Ireland (STV)", "Japan (SNTV)", "New Zealand (SMP)", "UK (SMP)", "USA (SMP)")
major.parties <- c(2.5, 2.2, 3.8, 2.8, 2.7, 2.0, 2.1, 1.9) 
major.system <- c(.19, .13, .16, .70, .61, 0, .16, .39)

#proportional
    #matrix
prop <- cbind(c("Austria", "Belgium", "Denmark", "Finland", "Germany", "Italy", 
    "Netherlands", "Norway", "Sweden"), c(2.4,5.2, 4.4, 5.1, 2.6, 4, 4.6, 3.3, 3.3),
    c(.89, .86, 96, .87, .91, .91, 1, .76, .9))
    
#vectors
prop.countries <- c("Austria", "Belgium", "Denmark", "Finland", "Germany", "Italy", 
    "Netherlands", "Norway", "Sweden")
prop.parties <-  c(2.4,5.2, 4.4, 5.1, 2.6, 4, 4.6, 3.3, 3.3)
prop.system <- c(.89, .86, .96, .87, .91, .91, 1, .76, .9)


#homemade dotplots

    #create dummies for y-axis
y.axis.major <- c(1:length(major.countries))
y.axis.prop <- c(1:length(prop.countries))
#sort parties and labels by parties, low to high

sort.major.parties <- sort(major.parties)
sort.major.countries <- major.countries[order(major.parties)]
sort.prop.parties <- sort(prop.parties)
sort.prop.countries <- prop.countries[order(prop.parties)]

#pdf("iversen_fig4.pdf", height = 6, width = 6)
#png("iversen_fig4.png", height = 600, width = 650)

#Create 2x2 grid for figures; use layout command to only use labels for
    #left-hand plots
layout(rbind(c(1,2), c(3,4)), 
    widths = rbind(c(4,3.2,4,3.2)), heights=c(4,4),  respect = T)
#layout.show(4)
x.size <- .8 # dummy for size of x-axis labels

par(mar=c(1,6,3,0))
#plot parties for maj. systems 
plot(sort.major.parties, y.axis.major, xlim = c(min(major.parties,prop.parties)-.1, 
    max(major.parties,prop.parties)+.1), #draw points
    type = "p", axes = F, xlab = "", ylab = "", pch = 19, 
    main = "")
abline(h = y.axis.major, lty = "dotted") # draw lines
box(bty = "o")
axis(1, at=seq(2,5.5, by=1), labels = seq(2,5.5, by=1), 
    cex = 1, las = 1, tick = T, cex.axis = x.size,
    line = 0, mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis.major, labels = sort.major.countries, tick = T, las  = 1, 
    line =0, cex.axis = 1)  
mtext("Effective Number of\nLegislative Parties", 3, line = .6, cex = 1, font=2) # add title
mtext("Majoritarian", 2, font = 2, cex = 1, line = 9.5)#create label for Maj. countries
abline(v=mean(major.parties), lty =2)# drop dotted line through mean for comparison

#plot majoritarian system proportionality 
 
par(mar=c(1,3,3,0))   
sort.major.system <- major.system[order(major.parties)] #use same order as first plot

plot(sort.major.system, y.axis.major, xlim = c(min(major.system,prop.system)-.1, 
    max(major.system,prop.system)+.1), #draw points
    type = "p", axes = F, xlab = "", ylab = "", pch = 19)
abline(h = y.axis.major, lty = "dotted") # draw lines
box(bty = "o")
axis(1, at=seq(0,1, by=.25), labels = seq(0,1, by=.25), 
    cex = 1, las = 1, tick = T, cex.axis = x.size, 
    line = 0, mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis.major, labels = F, tick = T, las  = 1) # add tick marks
abline(v=mean(major.system), lty=2) # drop dotted line through mean for comparison
mtext("Proportionality of\nElectoral System", 3, line = .6, cex = 1, font=2) # add title

#plot parties for proportion

par(mar=c(3,6,2,0))
plot(sort.prop.parties, y.axis.prop, xlim = c(min(major.parties,prop.parties)-.1, 
    max(major.parties,prop.parties)+.1), #draw points
    type = "p", axes = F, xlab = "", ylab = "", pch = 19, 
    main = "")
abline(h = y.axis.prop, lty = "dotted") # draw lines
box(bty = "o")
axis(1, at=seq(2,5.5, by=1), labels = seq(2,5.5, by=1), 
    cex = 1, las = 1, tick = T, cex.axis = x.size,
    line = 0, mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis.prop, labels = sort.prop.countries, tick = T, las  = 1, 
    line =0, cex.axis =1)    
abline(v=mean(prop.parties), lty=2) # drop dotted line through mean for comparison
mtext("Proportional", 2, font = 2, cex = 1, line = 9.5) #create label for Prop. countries

#plot system proportionality for proportional systems
    
par(mar=c(3,3,2,0))      
sort.prop.system <- prop.system[order(prop.parties)] #use same order as first plot

plot(sort.prop.system, y.axis.prop, xlim = c(min(major.system,prop.system)-.1, 
    max(major.system,prop.system)+.1), #draw points
    type = "p", axes = F, xlab = "", ylab = "", pch = 19)
abline(h = y.axis.prop, lty = "dotted") # draw lines
box(bty = "o")
axis(1, at=seq(0,1, by=.25), labels = seq(0,1, by=.25), 
    cex = 1, las = 1, tick = T, cex.axis = x.size,
    line = 0, mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis.prop, labels = F, tick = T, las  = 1) # add tick marks
abline(v=mean(prop.system), lty = 2)#drop dotted line through mean for comparison
  
#dev.off()
