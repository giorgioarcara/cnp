# function for identifying the observations corresponding to the outer and inner tolerance limits (oTL; iTL)
# this function is adapted version from Aiello & Depaoli, 2022.

# n = the numerosity of the normative sample.

# Author Giorgio Arcara (2023) v 1.0 (adapted from Aiello & Depaoli, 2022)

tolLimits.obs <- function(n){
  q <- 0.05
  r1 <- r2 <- 0
  p1 <- pbeta(q, shape1=r1, shape2=n-(r1+1), lower.tail=T)
  while(p1 >= 0.95){
    r1 <- r1+1
    p1 <- pbeta(q, shape1=r1, shape2=n-(r1+1), lower.tail=T)
  }
  r1 <- r1-1
  p1 <- pbeta(q, shape1=r1, shape2=n-(r1+1), lower.tail=T)
  
  p2 <- pbeta(1-q, shape1=n-(r2+1), shape2=r2, lower.tail=T)
  while(p2<=0.95){
    r2 <- r2+1
    p2 <- pbeta(1-q, shape1=n-(r2+1), shape2=r2, lower.tail=T)
  }
  if(r1 == 0) r1 <- 'not defined'
  
  res = data.frame(r1, r2, round(p1,5), round(p2,5))
  names(res)=c("oTL", "iTL", "p_oTL", "p_iTL")
  return(res)
}