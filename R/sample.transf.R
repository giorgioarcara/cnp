## a function to sample a value of some transformations
# it could be used to define randomly the relationship between a predictor (e.g. age) and a score.
# the list of function used is:

# include_transf = the transformations to be included in the sampling. If NULL all transformation in the list are used. 
                  #  (See within the function for the list)
# exclude_transf = the transformations to be included in the sampling. IF NULL no transformation is excluded.


# Author: Giorgio Arcara (2023) v 1.0

sample.transf = function(include_transf= NULL, exclude_transf=NULL){
  
  #compute most common transformations for age and education
  cube = function(x){x^3}
  quadr = function(x){x^2}
  logm100 = function(x){log(100-x)}
  log10m100 = function(x){log10(100-x)}
  log10mAve = function(x){log10(mean(x)-x)} # not included now.
  inv = function(x){1/x}
  poly2 = function(x){poly(x,2)}
  # sqrt
  # log
  
  # you can add new functions here, creating them if necessary
  if(is.null(include_transf)){
    transf_list = c("identity",  "quadr", "log10", "logm100", "log", "log10m100", "inv", "sqrt", "cube", "zero") #"cube","log10mAve", 
  } else {
    transf_list = include_transf
  }
  
  
  transf_list=setdiff(transf_list, exclude_transf) 
  
  curr_transf = sample(transf_list, 1)
  
  return(curr_transf)
  
  
}