# function to define outer tol Limits from adjusted scores.
# ad scores = adjusted scores

# author: Giorgio Arcara (2023) v. 1.0

tolLimits.adjscores = function(adjscores){
  dat = data.frame(AS = adjscores)
  dat$ranked_AS = rank(dat$AS)
  oTL.n= tolLimits.obs(dim(dat)[1])$oTL
  
  # order data.frame according to ran
  dat = dat[order(dat$ranked_AS), ]
  oTL = dat[oTL.n, "AS"]
  
  return(oTL)
  
}