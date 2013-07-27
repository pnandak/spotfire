############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  7-24-06                                                           #
# PURPOSE: Show the basics of matrix algebra in R                          #
#                                                                          #
# NOTES: 1)                                                                #
#                                                                          #
############################################################################

############################################################################
# Example #1

  A<-matrix(data = c(1, 2, 3,
                     4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE)
  class(A)
  B<-matrix(data = c(-1, 10, -1, 5, 5, 8), nrow = 2, ncol = 3, byrow = TRUE)

  A+B
  A-B

  #Show what happens with the byrow = TRUE option
  matrix(data = c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  
  
  #Vector example
  y<-matrix(data = c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE)
  y
  class(y)
  x<-c(1,2,3)
  x
  class(x)
  is.vector(x)

  #Transpose
  t(A)

  
#############################################################################
# Example #2

  A<-matrix(data = c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE)
  B<-matrix(data = c(3, 0, 1, 2, 0, 1), nrow = 3, ncol = 2, byrow = TRUE)
  
  C<-A%*%B
  D<-B%*%A

  C
  D

  #What is A*B?
  A*B
  
  #Show what happens with *
  E<-A
  A*E
  
  #Work with vectors
  x<-c(1,2,3)
  #Inner product
  x%*%x  
  A%*%x
  
  x%o%x  #outer product 
  
#############################################################################
# Example #3

  A<-matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)
  solve(A)
  A%*%solve(A)
  solve(A)%*%A
  
  round(solve(A)%*%A, 2)



















#
