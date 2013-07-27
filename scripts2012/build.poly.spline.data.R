# build.poly.spline.data generates the data matrix needed for fitting a
# polynomial spline growth curve model
#
# USAGE:
#
#      build.poly.spline.data(timevec, p, knots, t0=0)
#
#
# ARGUMENTS:
#
#       timevec     n x 1 vector of time points
#
#       p           scalar giving the order of the polynomial
#
#       knots       m x 1 vector giving the location of the m knots.
#                   Locations of the knots are always assumed known
#                   and *must* be set by the user.
#
#       t0          scalar giving the reference time point
#
#
# VALUE:
#
#       build.poly.spline.data returns a n x (p+m) data matrix that
#       can be used to fit a p-order polynomial spline via any standard
#       least-squares-like procedure.
#
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# For a copy of the GNU General Public License
# write to the Free Software Foundation, Inc.,
# 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.
#
# Copyright (C) 2001 Kevin M. Quinn
#
# Kevin Quinn
# Assistant Professor
# Dept. of Political Science and CSSS
# Box 354322
# University of Washington
# Seattle, WA  98195-4322
# quinn@stat.washington.edu
# 
# 2/23/2001 
# Last updated 2/23/2001
#
build.poly.spline.data <- function(timevec, p, knots, t0=0){

  # error checking and determination of model specific quantities
  if (p < 1)
    stop("p < 1")
  
  if (is.matrix(timevec)){
     if (ncol(timevec)==1){
       n <- nrow(timevec)
    }
    else{
      stop("timevec not a vector or 1 column matrix type")
    }
  }
  else
    n <- length(timevec)
  
  if (is.matrix(knots)){
    if (ncol(knots)==1)
      m <- nrow(knots)
    else
      stop("knots not a vector or 1 column matrix type")
  }
  else
    m <- length(knots)
  

  # setup the matrix to be output
  outmat <- matrix(NA, n, p+m)

  # build the output matrix
  for (i in 1:p){
    outmat[,i] <- (timevec-t0)^i
  }

  for (i in 1:m){
    outmat[(timevec<=knots[i]),p+i] <- 0
    outmat[(timevec> knots[i]),p+i] <- (timevec[timevec>knots[i]] - knots[i])^p
  }

  return(outmat)
}



