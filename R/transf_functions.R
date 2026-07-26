# a list  most common transformations assumed for age and education

# Author Giorgio Arcara (2023) v.1.0 

cube = function(x){x^3}
quadr = function(x){x^2}
logm100 = function(x){log(100-x)}
log10m100 = function(x){log10(100-x)}
log10mAve = function(x){log10(mean(x)-x)}
inv = function(x){1/x}
poly2 = function(x){poly(x,2)}
zero = function(x){0*x} # to model no effect.