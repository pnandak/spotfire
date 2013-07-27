############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-23-06                                                           #
# PURPOSE: Do all parts of Chapter 3 with respect to the GPA data set      #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

#One way to enter the data into R
set1<-data.frame(Y = c(95.71, 98.16, 99.52, 102.09, 103.79, 106.18, 108.14, 110.21),
                 X = c(96,    98,    100,   102,    104,    106,    108,    110))
set1


mod.fit<-lm(formula = Y ~ X, data = set1)
sum.fit<-summary(mod.fit)
sum.fit

Y.new<-104
alpha<-0.05
n<-nrow(set1)
X.hat.new<-(Y.new - mod.fit$coefficients[1])/mod.fit$coefficients[2]
as.numeric(X.hat.new)

sum.sq<-var(set1$X)*(n-1)
var.X.hat.new<-sum.fit$sigma^2 / mod.fit$coefficients[2]^2 * (1 + 1/n + (X.hat.new - mean(set1$X))^2/sum.sq)
save.ci<-X.hat.new-qt(p = c(1-alpha/2,alpha/2), df = mod.fit$df.residual)*sqrt(var.X.hat.new)
round(save.ci,2)

#pty = "s": Use square plot here since both variables are measured on the same scale - "m" is default
#xaxs and yaxs = "i" forces the x and y-axes to be exactly within the given range - "r" is default
par(pty = "s", xaxs = "i", yaxs = "i")  
plot(x = set1$X, y = set1$Y, xlab = "Water bath temperature (X)", ylab = "Thermometer temperature (Y)", 
     main = "Thermometer temperature vs. Water bath temperature", panel.first = grid(col = "gray", lty = "dotted"),
     xlim = c(95, 112), ylim = c(95, 112))
axis(side = 1, at = seq(from = 95, to = 112, by = 1), labels = FALSE, tcl = -0.25)  #Add tick marks without labels, side = 1 is x-axis, tcl is length of tick mark
axis(side = 2, at = seq(from = 95, to = 112, by = 1), labels = FALSE, tcl = -0.25)  #Add tick marks without labels, side = 2 is y-axis
curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x, 
      col = "red", lty = "solid", lwd = 1, add = TRUE, from = min(set1$X), to = max(set1$X))
segments(x0 = 0, y0 = Y.new, x1 = X.hat.new, y1 = Y.new, col = "darkgreen", lty = "dashed", lwd = 2)
segments(x0 = X.hat.new, y0 = Y.new, x1 = X.hat.new, y1 = 0, col = "darkgreen", lty = "dashed", lwd = 2)
points(x = save.ci, y = c(95.1, 95.1), pch = c("(", ")"), col = "darkgreen")  #Plot C.I.



#
