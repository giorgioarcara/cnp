### internal function to create a text for running the linear model with the identified transformation

# transfs = character names of the transformations (e.g. c("quadr", "cube")
# pred.names = character names of the predictors (e.g. c("Age", "Edu")
# dap.name = name of the dependent variable 
# data.name = character with name of the dataset.

# Author Giorgio Arcara (2023) v.1.0 

formula_transf_text = function(transfs=NULL, pred.names =NULL, dep.name = NULL, data.name = NULL){
  
  if (length(transfs)!=length(pred.names)){
    stop("length of transfs and pred.names should be the same")
  }
  
  model_res = paste("lm(", dep.name, " ~ ", sep="")
  for (iC in 1:length(transfs)){
      model_res = paste(model_res, "+ ", transfs[iC], "(", pred.names[iC], ")")
  }
  
  model_res = paste(model_res, " , data = ", data.name, ")", sep="")
  return(model_res)
  
}


