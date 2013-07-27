#plot senator distance vs. state-level support

#x axis: opinion in home state
#y axis: distance betw nom and senator
#open dots for no votes, filled dots for yes votes.


logit.opdist1 <- glm(vote ~ +opinion +distance, family=binomial(link="logit"))
display(logit.opdist1)

#
#glm(formula = vote ~ +opinion + distance, family = binomial(link = "logit"))
#            coef.est coef.se
#(Intercept)  -3.74     0.82 
#opinion       0.11     0.01 
#distance    -11.82     0.90 
#---
#  n = 891, k = 3
#  residual deviance = 392.7, null deviance = 1004.8 (difference = 612.1)



logit.opdist2 <- glm(vote ~ +opinion +distance, family=binomial(link="logit"),subset= same.party == 1)
display(logit.opdist2)
#glm(formula = vote ~ +opinion + distance, family = binomial(link = "logit"), 
#    subset = same.party == 1)
#            coef.est coef.se
#(Intercept)  -4.99     2.01 
#opinion       0.16     0.04 
#distance    -16.66     4.26 
#---
#  n = 454, k = 3
#  residual deviance = 53.3, null deviance = 110.9 (difference = 57.6)



logit.opdist3 <- glm(vote ~ +opinion +distance, family=binomial(link="logit"),subset= same.party == 0)
display(logit.opdist3)
#
#
#glm(formula = vote ~ +opinion + distance, family = binomial(link = "logit"), 
#    subset = same.party == 0)
#            coef.est coef.se
#(Intercept) -8.36     1.61  
#opinion      0.15     0.02  
#distance    -6.82     1.11  
#---
#  n = 437, k = 3
#  residual deviance = 301.9, null deviance = 605.4 (difference = 303.5)
#
#
#



pdf("figure_3.pdf", height = 10, width = 4.5)


#par(mfrow = c(3,1), mar = c(4,3,3,1))
layout(cbind(c(1:4)), heights = c(4,4,4, .5))
par(mar = c(2,6.5,1,1))
circle.size <- 1.1
label.size <- 1.5
y.label.size <- 1.3
axis.size <- 1.3
yes.color <- "blue"
no.color <- "black"

#all senators
plot(opinion, distance,type = "n", xlim = c(35,95), ylim = c(0,1), axes = F, xlab = "", ylab = "", xaxs ="r", yaxs="r")
points(opinion[vote ==1], distance[vote == 1], pch = 19, col = yes.color, cex = circle.size)
points(opinion[vote ==0], distance[vote == 0], col = no.color, cex = circle.size)
points(82, .9, col = no.color, cex = circle.size)
text(82, .9, "= no vote", col = no.color, pos = 4, cex = label.size)
points(82, .8, col = yes.color, cex = circle.size, pch = 19)
curve((3.74  + (x * (-.11)))/(-11.82),
    add=T,col="red",lty=2)
text(82, .8, "= yes vote", col = yes.color, pos = 4, cex = label.size)
text(45, .94, "All senators",  cex = 2)
mtext("Ideological\nDistance", 2, line = 2.5,  cex = y.label.size, srt = 90)
axis(1, at = seq(40,90,10), mgp = c(2,.5,0), cex.axis = axis.size)
axis(2, at = seq(0,1,.25), labels = NA, tcl = F, las = 1, mgp = c(2,.5,0))
text(28, .15, "Near", xpd = T, cex = 1.6)
text(28, .85, "Far", xpd = T, cex = 1.6)
box()
#labels = c(0, ".25", ".5", ".75", 1)
text(88,.58, "50% chance\nof yes vote", srt = 23, cex = 1.3)

#In-party
plot(opinion[same.party == 1], distance[same.party == 1], type = "n", xlim = c(35,95), ylim = c(0,1), axes = F, xlab = "", ylab = "", xaxs ="r", yaxs="r")
points(opinion[vote == 1 & same.party == 1], distance[vote == 1 & same.party == 1], pch = 19, col = yes.color, cex = circle.size)
points(opinion[vote == 0 & same.party == 1], distance[vote == 0 & same.party == 1], col = no.color, cex = circle.size)
text(50, .94, "In-party senators",  cex = 2)
mtext("Ideological\nDistance", 2, line = 2.5,  cex =y.label.size, srt = 90)
points(82, .9, col = no.color, cex = circle.size)
text(82, .9, "= no vote", col = no.color, pos = 4, cex = label.size)
points(82, .8, col = yes.color, cex = circle.size, pch = 19)
curve((4.99  + (x * (-.16)))/(-16.66),
    add=T,col="red",lty=2)
text(82, .8, "= yes vote", col = yes.color, pos = 4, cex = label.size)
axis(1, at = seq(40,90,10), mgp = c(2,.5,0), cex.axis = axis.size)
axis(2, at = seq(0,1,.25), labels = NA, tcl = F, las = 1, mgp = c(2,.5,0))
text(28, .15, "Near", xpd = T, cex = 1.6)
text(28, .85, "Far", xpd = T, cex = 1.6)
box()

#Out-party
plot(opinion[same.party == 0], distance[same.party == 0],type = "n", xlim = c(35,95), ylim = c(0,1), axes = F, xlab = "", ylab = "", xaxs ="r", yaxs="r")
points(opinion[vote == 1 & same.party == 0], distance[vote == 1 & same.party == 0], pch = 19, col = yes.color, cex = circle.size)
points(opinion[vote == 0 & same.party == 0], distance[vote == 0 & same.party == 0], col = no.color, cex = circle.size)
text(50.5, .94, "Out-party senators",  cex = 2)
mtext("Ideological\nDistance", 2, line = 2.5,  cex =y.label.size, srt = 90)
points(82, .9, col = no.color, cex = circle.size)
text(82, .9, "= no vote", col = no.color, pos = 4, cex = label.size)
points(82, .8, col = yes.color, cex = circle.size, pch = 19)
curve((8.36  + (x * (-.15)))/(-6.82),
    add=T,col="red",lty=2,xlim=c(35,88))
text(82, .8, "= yes vote", col = yes.color, pos = 4, cex = label.size)
axis(1, at = seq(40,90,10), mgp = c(2,.5,0), cex.axis = axis.size)
axis(2, at = seq(0,1,.25), labels = NA, tcl = F, las = 1, mgp = c(2,.5,0))
text(28, .15, "Near", xpd = T, cex = 1.6)
text(28, .85, "Far", xpd = T, cex = 1.6)
box()
#create x-axis label at bottom
par(mar = c(0,0,0,0))
plot(opinion[same.party == 0], distance[same.party == 0],type = "n", xlim = c(35,95), ylim = c(0,1), axes = F, xlab = "", ylab = "", xaxs ="r", yaxs="r")
text(67,.75, "State opinion", cex =2)

dev.off()
