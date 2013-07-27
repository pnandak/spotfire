
X = rnorm(50)
Y = 2 + 1.5 * rnorm(50)

# Squared error loss: 'least squares'

squared.error = function(b0, b1) {
return(sqrt(sum((Y - b0 - b1*X)^2)))
}

# Absolute value loss: 'absolute deviation'

absolute.loss = function(b0, b1) {
return(sum(abs(Y - b0 - b1*X)))
}


        png("squared_error.png", height=600, width=600)
        

# Plot the loss over a grid of values

plot.loss = function(loss, b0, b1) {
   l = matrix(0, length(b0), length(b1))
   for (i in 1:length(b0)) {
      for (j in 1:length(b1)) {
          l[i,j] = loss(b0[i], b1[j])
      }
   }
   image(b0, b1, l, col=rainbow(1000))
   return(l)
}

ll = plot.loss(squared.error, seq(0,4,length=100), seq(-2,2,length=100))

        dev.off()
        
        png("absolute_dev.png", height=600, width=600)
        
ll = plot.loss(absolute.loss, seq(0,4,length=100), seq(-2,2,length=100))
                       

        dev.off()
        