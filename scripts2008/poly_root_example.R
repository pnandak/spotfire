#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-21-06                                                   #
# PURPOSE: Roots of a polynomial                                    #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#Syntax for a AR(2): 1 - phi1*z - phi2*z 
#  phi1 = 0.5, phi2 = 0
polyroot(z = c(1, -0.5))  

#####################################################################
#Outside unit circle examples - causal

  #phi1 = 0, phi2 = 0.5 
  polyroot(z = c(1, 0, -0.5))  
  
  #phi1 = 0, phi2 = -0.5 
  polyroot(z = c(1, 0, 0.5))  

  #phi1 = -0.2, phi2 = -0.5
  polyroot(z = c(1, 0.2, 0.5))  
  abs(polyroot(z = c(1, 0.2, 0.5))) #Check if outside 
  sqrt((-0.2)^2+1.4^2) #Verify abs() function did it correctly
  
  #phi1 = -1, phi2 = -0.5
  polyroot(z = c(1, 1, 0.5))  
  abs(polyroot(z = c(1, 1, 0.5))) 

  #phi1 = -1.8, phi2 = -0.9
  polyroot(z = c(1, 1.8, 0.9))  
  abs(polyroot(z = c(1, 1.8, 0.9))) 

  #phi1 = 0.5, phi2 = 0.25 
  polyroot(z = c(1, -0.5, -0.25))  
  abs(polyroot(z = c(1, -0.5, -0.25))) 


#####################################################################
# Inside unit circle examples - not causal

  #phi1 = 1.8, phi2 = 0.9
  polyroot(z = c(1, -1.8, -0.9))  
  abs(polyroot(z = c(1, -1.8, -0.9))) 

  #phi1 = -1.2, phi2 = 0.8
  polyroot(z = c(1, 1.2, -0.8))  
  abs(polyroot(z = c(1, 1.2, -0.8))) 
  
  
####################################################################
# Region

  #By default, R goes 4% more on y and x-axis limits,
  #  These par() options make it stop at specified limits 
  par(xaxs = "i", yaxs = "i")
  #Dummy plot 
  plot(x = -2, y = -1, xlim = c(-2,2), ylim = c(-1,1), type = "n", frame.plot = FALSE,
       xlab = expression(phi1[1]), ylab = expression(phi1[2])) 
  #abline() draws y = mx + b   
  abline(a = 1, b = -1) #Draw line of phi2 = 1 - phi1
  abline(a = 1, b =  1) #Draw line of phi2 = 1 + phi1
  #Plot the phi1 and phi2 values
  points(x = 0, y = 0.5, pch = 1, col = "red") 
  points(x = 0, y = -0.5, pch = 2, col = "darkgreen")
  points(x = -0.2, y = -0.5, pch = 2, col = "darkgreen")
  points(x = -1, y = -0.5, pch = 2, col = "darkgreen")
  points(x = -1.8, y = -0.9, pch = 2, col = "darkgreen")
  points(x = 0.5, y = 0.25, pch = 1, col = "red")
  
  points(x = 1.8, y = 0.9, pch = 3, col = "blue")
  points(x = -1.2, y = 0.8, pch = 3, col = "blue")

  legend(locator(1), legend = c("Causal, real roots", "Causal, complex roots", "Not Casual"), pch = c(1,2,3), 
         col = c("red", "darkgreen", "blue"), cex = 0.75, bty = "n")
   



  
#
