
##########################################################################
############Graphs of opinion effects under diff conditions

#use model 8 (mlm.3) for graphs, but keeping opinion on original scale
mlm.for.graph <-  glmer(vote ~  opinion  + rescale(lack.quality) 
  + rescale(distance) + same.party + rescale(pres.approval) + (1 | nominee), 
  family=binomial(link="logit"))
display(mlm.for.graph)


pdf("figure_4.pdf", height = 12, width = 4.5) 

y <- c(1:length(opinion))# use for plots
axis.text <- 1.5
title.size <- 1.2
label.size <- 1.2

#use vertical lines to indicate 1 sd above and below opinion
sd.below <- mean(opinion) - sd(opinion)
sd.above <- mean(opinion) + sd(opinion)
sd.width <- .7
shade.color <- "grey80"

layout(cbind(c(1:5)), heights = c(4,4,4,4, .4))
par( mar = c(3, 5, 2, 1))

#1st plot:compares regular (avg lackqual) to lowlackqual (dotted) and highlackqual (dashed)
plot(opinion, y, type = "n", xlim = c(35, 95), ylim = c(0,1), axes = F, xlab = "", ylab = ""
  , xaxs ="i", yaxs="i")
polygon(x=c(35, 35, sd.below, sd.below), y=par()$usr[c(3,4,4,3)],col= shade.color, border=F)
polygon(x=c(sd.above, sd.above, 95,95), y=par()$usr[c(3,4,4,3)],col= shade.color, border=F)
axis(1, at = seq(40,90,10), mgp = c(2,.7,0), cex.axis = axis.text)
axis(2, at = seq(0,1, by = .25), label = c(0, ".25", ".5", ".75", "1"), las =2,  cex.axis = axis.text, mgp = c(2,.5,0))
curve(invlogit(fixef(mlm.for.graph)["(Intercept)"] + fixef(mlm.for.graph)["opinion"]*x), lwd=2,xlim=c(35,95),add = T)  # regular
curve(invlogit(fixef(mlm.for.graph)["(Intercept)"] + fixef(mlm.for.graph)["rescale(lack.quality)"]*(.5)+ fixef(mlm.for.graph)["opinion"]*x), lwd=2,xlim=c(35,95),add=TRUE ,lty=3) #highlackquak 
curve(invlogit(fixef(mlm.for.graph)["(Intercept)"] + fixef(mlm.for.graph)["rescale(lack.quality)"]*(-.5)+ fixef(mlm.for.graph)["opinion"]*x), lwd=2,xlim=c(35,95), add=TRUE,lty=2) #lowlackqual
mtext("Panel A: High vs. low quality", 3, line = .5, cex = title.size, font = 3)
text(48, .6, "High quality", cex = label.size)
text(64.5, .55, "Avg.\nqual.", cex = label.size)
text(76.5,.64, "Low quality", cex = label.size)
abline(v = sd.below, col = "dark grey", lwd = sd.width)
abline(v = sd.above, col = "dark grey", lwd = sd.width)
mtext("Pr(Voting Yes)", 2,line = 2.6, srt = 90, cex =1.5)
box()

#compares  opposingparty (dotted) and sameparty (dashed)
plot(opinion, y, type = "n", xlim = c(35, 95), ylim = c(0,1), axes = F, xlab = "", ylab = "", xaxs ="i", yaxs="i")
polygon(x=c(35, 35, sd.below, sd.below), y=par()$usr[c(3,4,4,3)],col= shade.color, border=F)
polygon(x=c(sd.above, sd.above, 95,95), y=par()$usr[c(3,4,4,3)],col= shade.color, border=F)
axis(1, at = seq(40,90,10), mgp = c(2,.7,0), cex.axis = axis.text)
axis(2, at = seq(0,1, by = .25), label = c(0, ".25", ".5", ".75", "1"), las =2,  cex.axis = axis.text, mgp = c(2,.5,0))
curve(invlogit(fixef(mlm.for.graph)["(Intercept)"] + fixef(mlm.for.graph)["same.party"]*(0)+ fixef(mlm.for.graph)["opinion"]*x), lwd=2,xlim=c(35,95),add=TRUE,lty=3) #opposing party
curve(invlogit(fixef(mlm.for.graph)["(Intercept)"] + fixef(mlm.for.graph)["same.party"]*(1)+ fixef(mlm.for.graph)["opinion"]*x), lwd=2,xlim=c(35,95), add=TRUE,lty=2) #sameparty
mtext("Panel B: Same vs. opposite party", 3, line = .5, cex = title.size, font = 3)
text(69, .54, "Opposite party", cex = label.size)
text(45, .76, "Same party", cex = label.size)
abline(v = sd.below, col = "dark grey", lwd = sd.width)
abline(v = sd.above, col = "dark grey", lwd = sd.width)
mtext("Pr(Voting Yes)", 2,line = 2.6, srt = 90, cex =1.5)
box()


#compares regular to lowdistance and fardistance (for opposite party, as assumed)
plot(opinion, y, type = "n", xlim = c(35, 95), ylim = c(0,1), axes = F, xlab = "", ylab = "", xaxs ="i", yaxs="i")
polygon(x=c(35, 35, sd.below, sd.below), y=par()$usr[c(3,4,4,3)],col= shade.color, border=F)
polygon(x=c(sd.above, sd.above, 95,95), y=par()$usr[c(3,4,4,3)],col= shade.color, border=F)
axis(1, at = seq(40,90,10), mgp = c(2,.7,0), cex.axis = axis.text)
axis(2, at = seq(0,1, by = .25), label = c(0, ".25", ".5", ".75", "1"), las =2,  cex.axis = axis.text, mgp = c(2,.5,0))
curve(invlogit(fixef(mlm.for.graph)["(Intercept)"] + fixef(mlm.for.graph)["opinion"]*x), lwd=2,xlim=c(35,95), add =T)  # regular -- all at means or zero
curve(invlogit(fixef(mlm.for.graph)["(Intercept)"] + fixef(mlm.for.graph)["rescale(distance)"]*(-.5)+ fixef(mlm.for.graph)["opinion"]*x), lwd=2,xlim=c(35,95),add=TRUE,lty=2) #near
curve(invlogit(fixef(mlm.for.graph)["(Intercept)"] + fixef(mlm.for.graph)["rescale(distance)"]*(.5)+ fixef(mlm.for.graph)["opinion"]*x), lwd=2,xlim=c(35,95), add=TRUE,lty=3) #far
mtext("Panel C: Ideological distance (opposite party)", 3, line = .5, cex = title.size, font = 3)
text(49, .8, "Close to\nnominee", cex = label.size)
text(70.8, .72, "Average\ndistance", cex = label.size)
text(73,.3, "Far from\nnominee", cex = label.size)
abline(v = sd.below, col = "dark grey", lwd = sd.width)
abline(v = sd.above, col = "dark grey", lwd = sd.width)
mtext("Pr(Voting Yes)", 2,line = 2.6, srt = 90, cex =1.5)
box()

#compares regular to lowdistance and fardistance (for SAME PARTY)
plot(opinion, y, type = "n", xlim = c(35, 95), ylim = c(0,1), axes = F, xlab = "", ylab = "", xaxs ="i", yaxs="i")
polygon(x=c(35, 35, sd.below, sd.below), y=par()$usr[c(3,4,4,3)],col= shade.color, border=F)
polygon(x=c(sd.above, sd.above, 95,95), y=par()$usr[c(3,4,4,3)],col= shade.color, border=F)
axis(1, at = seq(40,90,10), mgp = c(2,.7,0), cex.axis = axis.text)
axis(2, at = seq(0,1, by = .25), label = c(0, ".25", ".5", ".75", "1"), las =2,  cex.axis = axis.text, mgp = c(2,.5,0))
curve(invlogit(fixef(mlm.for.graph)["(Intercept)"] + fixef(mlm.for.graph)["same.party"] + fixef(mlm.for.graph)["opinion"]*x), lwd=2,xlim=c(35,95), add =T)  # regular -- all at means or zero
curve(invlogit(fixef(mlm.for.graph)["(Intercept)"] + fixef(mlm.for.graph)["same.party"] + fixef(mlm.for.graph)["rescale(distance)"]*(-.5)+ fixef(mlm.for.graph)["opinion"]*x), lwd=2,xlim=c(35,95),add=TRUE,lty=2) #near
curve(invlogit(fixef(mlm.for.graph)["(Intercept)"] + fixef(mlm.for.graph)["same.party"] + fixef(mlm.for.graph)["rescale(distance)"]*(.5)+ fixef(mlm.for.graph)["opinion"]*x), lwd=2,xlim=c(35,95), add=TRUE,lty=3) #far
mtext("Panel D: Ideological distance (same party)", 3, line = .5, cex = title.size, font = 3)
text(39.6, .92, "Close to\nnominee", cex = label.size)
text(54, .63, "Average\ndistance", cex = label.size)
text(72,.8, "Far from\nnominee", cex = label.size)
abline(v = sd.below, col = "dark grey", lwd = sd.width)
abline(v = sd.above, col = "dark grey", lwd = sd.width)
mtext("Pr(Voting Yes)", 2,line = 2.6, srt = 90, cex =1.5)
box()
#create x-axis label at bottom
par(mar = c(0,0,0,0))
plot(opinion[same.party == 0], distance[same.party == 0],type = "n", xlim = c(35,95), ylim = c(0,1), axes = F, xlab = "", ylab = "", xaxs ="r", yaxs="r")
text(67,.55, "State opinion", cex =2.5)


dev.off()




