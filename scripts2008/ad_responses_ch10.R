############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  8-5-06                                                            #
# PURPOSE: Ad responses example for Chapter 10                             #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

ad.responses<-c(100, 400, 100, 300, 200, 400)
size<-c(1, 8, 3, 5, 6, 10)
circulation<-c(20000, 80000, 10000, 70000, 40000, 60000)

set1<-data.frame(ad.responses, size, circulation)
set1


mod.fit<-lm(formula = ad.responses ~ size + circulation, data = set1)
#summary(mod.fit)

h.ii<-hatvalues(model = mod.fit)
h.ii

mean(set1$circulation)
mean(set1$size)

plot(x = set1$circulation, y = set1$size, xlab = "Circulation", ylab = "Size", main = "Circulation vs. size",
     panel.first = grid(col = "gray", lty = "dotted"), lwd = 2, col = "red", xlim = c(0, 90000), ylim = c(1,11))
points(x = mean(set1$circulation), y = mean(set1$size), lwd = 2, pch = 2, col = "darkblue")
text(x = set1$circulation, y = set1$size+0.3, labels = round(h.ii,4)) 
text(x = mean(set1$circulation), y = mean(set1$size)+0.3, labels = "Mean") 








#
