# function to generate values from uniform  (equiprobable) distribution, within a given range of coefficient values.
# c.range = a list of two-elements vectors, that indicates the minimum and maximum value that the coefficient can take.
# c.steps = a vector with the same length of c.range, indicates the number of possible values that can takes the value sampled in c.range.
            # e.g., c.steps[1] = c(100), indicate the within the two values defined by c.range[[1]], 100 values are generated and one is sampled.

# this function can be used to generate random coefficients from regression (it is used in sim.norm.data.)


# Author: Giorgio Arcara (2023) v 1.0


sample.coef.unif = function(c.range=list(c(10, 12), c(-0.05, 0.1), c(0.04, 0.08), c(-0.01, 0.01)),
                        c.steps=c(100, 100, 100, 100)){
  


  if (length(c.range)!=length(c.steps)){
    stop("c.range must have same length of c.steps")
  }
  
  res_var=NULL
  for (iC in 1:length(c.range)){
    vlist = seq(c.range[[iC]][1], c.range[[iC]][2], length.out=c.steps[iC])
    res_var[[iC]]=sample(vlist, 1, replace=T)
  }
  
  return(res_var)
  
}