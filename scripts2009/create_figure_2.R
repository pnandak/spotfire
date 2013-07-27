##we want to plot nominees in order of increasing mean.opion

mean.opinion <- tapply(opinion, factor(nominee), mean) 
mean.opinion.sorted <- sort(mean.opinion, decreasing = F)
unique.nominee <- unique(factor(nominee))#they're in alpha order
nominee.labels <- c("Alito", "Bork", "Breyer","Ginsburg","O'Connor","Rehnquist", "Roberts","Souter", "Thomas")
nominees.sorted <- factor(unique(nominee))[order(mean.opinion,decreasing = F)]
nominees.sorted.names <- c(nominee.labels[order(mean.opinion, decreasing = F)], "All\nnominees")#order by increasing mean support
vote.tallies <- c("58-42",  "42-58", "87-9", "96-3", "99-0",  "65-33",   "78-22",  "90-9",  "52-48")#alpha order
vote.tallies.sorted <- c(vote.tallies[order(mean.opinion,decreasing =F)], "")


pdf("figure_2.pdf", height = 10, width = 3)


axis.text <- 1.2
y.height <- 25
rug.size <- .1
layout(cbind(rep(11,10), c(1:10)), 
	heights = c(rep(1,9),1.4),
	widths =c(.1,2))
#layout.show(11)
par(mar = c(1,3,.5,1))

#now do logits
#1st, loop over each nominee to get logit results
for (i in 1:8){#exclude oconnor
      keep <- factor(nominee) == factor(nominees.sorted)[i]
      vote.keep <- vote[keep]
      opinion.keep <- opinion[keep]
      model <- glm(vote.keep~opinion.keep, family=binomial(link="logit"))
      coefs <- summary(model)$coef[,1] 
      plot(opinion[keep], vote[keep], axes = F, main = "", xlab = "", 
        ylab = "", xlim = c(35,100), ylim = c(0,1),type = "n")
    #create rug
    	keep.noes <- vote.keep==0 
    	keep.yesses <- vote.keep==1 
    	rug(opinion.keep[keep.noes], side  = 1, ticksize = rug.size) 
    	rug(opinion.keep[keep.yesses], side  = 3, ticksize = rug.size) 
    	box()
    #draw logit curve and axes
    	curve(invlogit(coef(model)[1] + coef(model)[2]*x), add=TRUE, lwd=2)#plot actual logit curve
    	axis(1, at = seq(40,100,by=10), labels =  NA)
    	axis(2, at = seq(0,1, by = .25), label = c(0, ".25",  ".5", ".75",  "1"), las =2,cex.axis = axis.text, mgp = c(2,.5,0))

		#add nominee labels
		text(93, .5, nominees.sorted.names[i], cex = 1.2, font = 2,  xpd = NA)
		text(93, .3, vote.tallies.sorted[i], cex = 1, font = 2,  xpd = NA)
      }

#add oconnor, but not logit curve
  i<-9
  keep <- factor(nominee) == nominees.sorted[i]
  vote.keep <- vote[keep]
  opinion.keep <- opinion[keep]
  plot(opinion[keep], vote[keep], axes = F, main = "", xlab = "", 
    ylab = "", xlim = c(35,100), ylim = c(0,1),type = "n")
#create rug
keep.yesses <- vote.keep==1#there are no "no" votes
rug(opinion.keep[keep.yesses], side  = 3, ticksize = rug.size) 
box()
axis(1, at = seq(40,100,by=10), labels =  NA, #c(40,50,60,70,80,90,"100"), 
tick = T, cex.axis = 1.3, mgp = c(2,.7,0))
 	axis(2, at = seq(0,1, by = .25), label = c(0, ".25",  ".5", ".75",  "1"), las =2,cex.axis = axis.text, mgp = c(2,.5,0))
	text(93, .5, nominees.sorted.names[i], cex = 1.2, font = 2,  xpd = NA)
	text(93, .3, vote.tallies.sorted[i], cex = 1, font = 2,  xpd = NA)

#NOW DO SINGLE PLOT FOR ALL NOMINEES POOLED
par(mar = c(4,3,0,1))
  model <- glm(vote~opinion, family=binomial(link="logit"))
  coefs <- summary(model)$coef[,1] 
  plot(opinion, axes = F, main = "", xlab = "", 
    ylab = "", xlim = c(35,100), ylim = c(0,1),type = "n")

#create rug
keep.noes <- vote==0 
keep.yesses <- vote==1 
rug(opinion[keep.noes], side  = 1, ticksize = rug.size) 
rug(opinion[keep.yesses], side  = 3, ticksize = rug.size) 
box()
curve(invlogit(coef(model)[1] + coef(model)[2]*x), add=TRUE, lwd=2)#plot actual logit curve
axis(1, at = seq(40,100,by=10), mgp=c(2,.5,0), cex.axis=1.3)
axis(2, at = seq(0,1, by = .25), label = c(0, ".25",  ".5", ".75",  "1"), las =2,cex.axis = axis.text, mgp = c(2,.5,0))
mtext("State support for nominee", 1, line =2.5, cex = 1.2)
text(93, .5, "All\nnominees", cex = 1.2, font = 2,  xpd = NA)

#add single label for y-axis
par(mar=c(0,0,0,0))
plot(0,0, type = "n", axes = F, xlim = c(0,10), ylim=c(0,10))
text(10,5, "Pr(Voting Yes)", cex = 2, srt =90, xpd=NA)


dev.off()

