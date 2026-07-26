# internal function to transform a model into an equation (to improve readibility)
# 

# Author: Giorgio Arcara (2023) v 1.0


model_text = function(mod, digits=3){
  coefs = coef(mod)
  model_res = NULL
  for (iC in 1:length(coefs)){
    # case intercept (i.e. first term)
    if (iC == 1){
      model_res = paste(signif(as.numeric(coefs[iC]),digits=digits), "+", sep=" ")}    
    # case middle terms
    if (iC != 1 & iC != length(coefs)){
      model_res = paste(model_res, names(coefs)[iC], "*", 
                        signif(as.numeric(coefs[iC]),digits=digits), "+", sep=" ")}
    # case last terms
    if (iC == length(coefs)){
      model_res = paste(model_res, names(coefs)[iC], "*", 
                        signif(as.numeric(coefs[iC]),digits=digits), sep=" ")
  }
}
return(model_res)
}
