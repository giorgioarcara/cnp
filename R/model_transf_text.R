# interal function to transform a model into an equation (to improve readibility, e.g. "12 + 0.8 * log(Age) + 0.002 * quadr(Edu) + 0.2 * Sex")
# this function can be used with the output of regressions model in which a transformation has been used
# (e.g., in adjscores_C1987 (Capitani, 1987) or adjscores_A2023 (Arcara, 2023))
# NOTE: the use of "transf.names" are necessary steps because adjscore_C1987, adjscores_Arcara2023 use these transformations)

## mod  = a linear model
# transfs = the transformations OF THE MODEL, to be substituted with transf.names
# transfs.names = the name of age function in the function
# new.names = the new labels used to generate the formula.


# eg. 
#  mod = mod_final, transfs = lm.model$transfs, transfs.names=c("age_funct", "edu_funct"); new.names=c("Age", "Edu");
# example of the output  "12 + 0.8 * log(Age) + 0.002 * quadr(Edu) + 0.2 * Sex"

# Author Giorgio Arcara (2023) v.1.0 

model_transf_text = function(mod, transfs=NULL, transfs.names =NULL, new.names=NULL,  digits=3){
  
  if (length(transfs.names)!=length(new.names)|length(transfs)!=length(transfs.names)){
    stop("length of transfs, transfs.names, and transfs.names, should be the same")
  }
  
  coefs = coef(mod)
  model_res = NULL
  
  if (length(coefs)>1){
    
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
      
      for (iN in 1:length(transfs.names))
        model_res=gsub(transfs.names[iN], 
                       paste(transfs[iN], "(",new.names[iN], ")", sep=""), model_res)
    }
  }
  if (length(coefs)==1){
    model_res = paste(signif(as.numeric(coefs[1]),digits=digits), sep=" ")    
  }
  return(model_res)
}
