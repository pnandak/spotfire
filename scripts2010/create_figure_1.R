#note: this script makes two separate plots:
	#the left plot in Figure 1 (heat plots), and the right plot (histograms)

detach()
attach.all(roll.call.merged)

mean.opinion.sorted <- sort(mean.opinion, decreasing = F)
unique.nominee <- unique(factor(nominee))#they're in alpha order
nominee.labels <- c("Alito", "Bork", "Breyer","Ginsburg","O'Connor","Rehnquist", "Roberts","Souter", "Thomas")
nominees.sorted <- unique.nominee[order(mean.opinion,decreasing = F)]
nominees.sorted.names <- c(nominee.labels[order(mean.opinion, decreasing = F)], "All\nnominees")#order by increasing mean support
vote.tallies <- c("58-42",  "42-58", "87-9", "96-3", "99-0",  "65-33",   "78-22",  "90-9",  "52-48")#alpha order
vote.tallies.sorted <- c(vote.tallies[order(mean.opinion,decreasing =F)], "")


##plot 450 x 10 grid
x.max <- length(unique.nominee)
y.max <- length(unique(state))
axis.text.size <- .6
axis.text <- function(axis=1,labels,at,srt=45,tcl=-0.4) {
 axis(axis,at=at,labels=FALSE,tcl=tcl, line = 0)
 text(at,par("usr")[3]-1,srt=srt,adj=1,labels=labels,xpd=TRUE, cex = .8)
}
dem.offset <-.5
x.lim.addition <- 1
#for 1-3
#order states by increasing state liberalism
unique.state.ideology <- tapply(ideology, state, unique)
unique.state <- unique(state)
ordered.states <- unique.state[order(unique.state.ideology, decreasing = F)]

pdf("figure_1_heat_plots.pdf", height = 7, width = 2.5)

#1) heat plots
layout(cbind(c(1,2)), 
	heights =c(5,.5),
	widths = c(1,2))
#layout.show(2)
par(mar=c(3.5, 3, 0, 1))
#order in terms of increasing mean support (nominees) 
  #for nominees, put GOP and Dems separately, so Ginsburg and Breyer 
nominees.sorted <- c("bork",  "miers", "rehnquist", "alito", "thomas", "roberts", "souter",  
    "oconnor", "ginsburg", "breyer")
unique.nominee <- unique(factor(nominee))#they're in alpha order
nominees.sorted.names <-c("Bork",  "Miers", "Rehnquist", "Alito", "Thomas", "Roberts", "Souter",  
    "O'Connor", "Ginsburg", "Breyer")
temp.rescale.opinion <- 100 *(opinion - min(opinion))/#rescale so data goes from 0 to 100
  (max(opinion) - min(opinion))
range(temp.rescale.opinion)
plot(0,0, type = "n", xlim=c(0,x.max + x.lim.addition), ylim = c(1, y.max), 
  xlab = "", ylab="",axes = F, xaxs="i")
 for (i in 1:length(unique.nominee)){#loop over nominees
     keep1 <- factor(nominee) == nominees.sorted[i]
       for (j in 1:length(unique.state)){#loop over states
         #this is for whatever you want to plot
         keep2 <- state == ordered.states[j]
         plot.temp <- tapply(temp.rescale.opinion[keep1 & keep2], state[keep1 & keep2], unique)
         color.temp <- paste("gray", abs(100-round(plot.temp)), sep="")
      if (i <= 8)  points(i, j, pch =15, cex= 2,col = color.temp) #offset 2 Dem nominees
      if (i >= 9)  points(i + dem.offset, j, pch =15, cex= 2,col = color.temp)
       }}
axis(2, at = c(1:50), labels = ordered.states, las = 1, mgp=c(2,.5,0), cex.axis = axis.text.size)
x.axis.location <- c(1:8, 9+dem.offset, 10+dem.offset)
axis.text(1,at=x.axis.location,labels=nominees.sorted.names, srt=45)
segments(8.8, .5, 8.8,50.5, lty = 2)
mtext("States (Less Liberal to More Liberal)", 2, srt = 90, line = 2)
#mtext("All support", 3,  line = -1, cex = 1.2)


#add thermometer
  #note: this is set for black = high support; reverse color numbers if using white = high
par(mar=c(0, 0, 0, 0))
plot(0,0, type = "n", xlab = "", ylab = "", main = "", axes = F, xlim = c(-50,150), ylim = c(10,90))
end.therm <- 140
for (i in 0:140){
  color.temp <- paste("gray", abs(100-i), sep="")
  polygon(x=c(0 + i, end.therm, end.therm , 0+i), col = color.temp, y = c(60,60,80,80), border=F)#main box
  }
#segments(0,60,0,80)
text(12.5, 36, "Lower\nsupport",cex =.9)
text(120, 36, "Higher\nsupport",cex =.9)

dev.off()


######histograms


##we want to plot nominees in order of increasing mean opinion of all,
  #but Ginsburg and Breyer at bottom so Dems are separate
	#we want to plot nominees in order of increasing mean opinion of all,
	unique.nominee <- unique(factor(nominee))#they're in alpha order
	nominees.sorted <- c("bork",  "miers", "rehnquist", "alito", "thomas", "roberts", "souter",  
	    "oconnor", "ginsburg", "breyer")
	unique.nominee <- unique(factor(nominee))#they're in alpha order
	nominees.sorted.names <-c("Bork",  "Miers", "Rehnquist", "Alito", "Thomas", "Roberts", "Souter",  
	    "O'Connor", "Ginsburg", "Breyer")
	vote.tallies <- c("58-42",  "42-58", "87-9", "96-3", "(No vote)", "99-0", 
	 "65-33",   "78-22",  "90-9",  "52-48")#alpha order
	vote.tallies.sorted <- c("42-58", "(No vote)",  "65-33", "58-42",  "52-48",  "78-22", 
	  "90-9",  "99-0",  "96-3",  "87-9")
	nominee.labels <- c("Alito (R)", "Bork (R)", "Breyer (D)","Ginsburg (D)", "Miers (R)", "O'Connor (R)","Rehnquist (R)", "Roberts (R)","Souter (R)", "Thomas (R)")
	#nominees.sorted <- unique.nominee[order(mean.opinion.withop,decreasing = F)]
	nominees.sorted.names <- nominee.labels[order(mean.opinion, decreasing = F)]#order by increasing mean support
	nominees.sorted.names <- c("Bork (R)",  "Miers (R)", "Rehnquist (R)", "Alito (R)", 
	  "Thomas (R)", "Roberts (R)", "Souter (R)",  "O'Connor (R)", "Ginsburg (D)", "Breyer (D)", "All\nnominees")


pdf("figure_1_histograms.pdf", height =7, width = 3.5)

axis.text <- 1.2
axis.size <- 1.1
y.height <- .37
title.text <- 1.2
dem.color <- "blue"
gop.color <- "red"
all.color <- "dark green"
dot.size <- .8
dot.height <- .02
text.size <- 1
y.height <- 25

layout(cbind(
  c(2, 14:25), c(1, 3:13, 26)),
  heights = c(.4, rep(1,11), .5)
  , widths = c(5,1)
  )
#layout.show(26)
#add in row labels
par(mar = c(0,0,0,0))
plot(0, 0, axes = F, main = "", xlab = "", ylab = "", type = "n", xlim = c(35,100))
text(45, -.2, "Nominee\n(Vote)", cex = title.text, font = 2,  xpd = NA)
par(mar = c(0,0,0,0))
plot(0, 0, axes = F, main = "", xlab = "", ylab = "", type = "n", xlim = c(35,100))
text(65, -.2, "Support for Nominee", cex = title.text+.2, font = 2,  xpd = NA)#blank for now

#1st column sorted nominee names and vote tallies
for (i in 1:11){ #10 nominees plus pooled nominees
par(mar = c(0,0,0,0))
plot(0, 0, axes = F, main = "", xlab = "", ylab = "", type = "n", xlim = c(35,100), ylim = c(0,30))
text(45, 15, nominees.sorted.names[i], cex = 1.2, font = 2,  xpd = NA)
text(45, 7, vote.tallies.sorted[i], cex = 1.2, font = 2,  xpd = NA)
}

#now do histograms

for (i in 1:10){
  par(mar = c(0,0,1,0))
  keep <- factor(nominee) == nominees.sorted[i]
  opinion.temp <- tapply(opinion[keep], state[keep], unique)
  hist(opinion.temp, xlim = c(35,100), ylim = c(0,y.height),  axes = F, 
    main = "", ylab = "", xlab = "", xaxs ="i", breaks = 6)
  axis(1, at = seq(40,100,by=10), labels =  NA, #c(40,50,60,70,80,90,"100"), 
    tick = T, cex.axis = 1.3, mgp = c(2,.7,0))
  segments(mean(opinion.temp), 0, mean(opinion.temp), y.height-5, lty = 2) #vertical line for mean opinion
  }

#now pool all nominees
opinion.temp <- tapply(opinion, factor(nominee.state), unique)
hist(opinion.temp, xlim = c(35,100), axes = F, ylim = c(0,93),
    main = "", ylab = "", xlab = "", xaxs ="i",  breaks = 10)
axis(1, at = seq(40,100,by=10), tick = T, #draw x-axis and labels, tick marks and labels every 25%,
    cex.axis = 1.3, mgp = c(2,.5,0))
abline(v=mean(opinion.temp), lty = 2)

  
dev.off()








