## calculate adjusted scores according to Arcara (2024) method. - VERSION 2
# the function calculate adjusted scores via regression modeling. The model selection is made by fitting all possible models within a set of transformations
# (see the function) and performing a stepwise AIC selection from each starting model. AIC of all the reduced models is than compared to select the final best model.

# df  = the data.frame
# Dep = name of the dependent varaible
# dep.range = the range of the score of the dependent variable. It is used to remove ceiling effect and NOT correct them (as in ES original method)
# age = the name of the variable codifying age (numeric)
# edu = the name of the variable codifying education (numeric)
# sex = the name of the variable codifying Sex (factor or 0-1)

# returned results include: 
# new.df = the data.frame with the adjusted scores
# lm.model = the fitted lm.model
# transfs = the two best transformations identified for age and education.
# model_text = a text describing the best equation.

# Author: Giorgio Arcara (2024) v 1.1


adjscores_A2024 <- function(df = NULL, dep = "Dep", dep.range = c(0,30), age = "Age", edu="Education", sex="Sex"){
  
  # stepwise min AIC selection for all transformations and model combinations + stepwise elimination + drop of non significant terms
  
  dat = df
  
  dat$dep = dat[, dep]
  dat$age = dat[, age]
  dat$edu = dat[, edu]
  dat$sex = dat[, sex]
  
  dat = na.omit(dat)
  
  # convert sex to numeric
   if(!is.numeric(dat$sex)){
  dat$sex = factor(dat$sex)
  dat$sex.or = dat$sex
  
  dat$sex= ifelse(dat$sex==levels(dat$sex)[1], 1, 0)
  cat("Sex converted to numeric")
  }
  
  #compute most common transformations for age and education
  cube = function(x){x^3}
  quadr = function(x){x^2}
  logm100 = function(x){log(100-x)}
  log10m100 = function(x){log10(100-x)}
  inv = function(x){1/x}
  poly2 = function(x){res = poly(x,2); return(as.matrix(res))}
  
  # sqrt
  # log
  
  age_funct_list = c("identity",  "quadr", "log10", "logm100", "log", "log10m100", "inv", "sqrt", "cube")
  edu_funct_list =  c("identity",  "quadr", "log10", "logm100", "log", "log10m100", "inv", "sqrt",  "cube")
  # find transformation in which the univariate model lead to the highest R^2 for age
  
  all_models_AIC = NULL
  all_models_functs = list(NULL)
  length(all_models_functs) = length(age_funct_list)*length(edu_funct_list)
  all_models_coefs = list(NULL)
  length(all_models_coefs) = length(age_funct_list)*length(edu_funct_list)
  
  k = 1
  
  for (iAF in 1:length(age_funct_list)){
    for (iEF in 1:length(edu_funct_list)){
      curr_age_funct = eval(parse(text=age_funct_list[iAF]))
      curr_edu_funct = eval(parse(text=edu_funct_list[iEF]))
      curr_age_eff = curr_age_funct(dat$age)
      curr_edu_eff = curr_edu_funct(dat$edu)
      #curr_mod_ini = lm(dep~1, dat)
      #curr_mod_final = lm(dep~curr_age_eff+curr_edu_eff+sex, data = dat)
      curr_mod = step(lm(dep~curr_age_eff+curr_edu_eff+sex, data = dat), trace = F)
      #curr_mod = lm(dep~curr_age_eff+curr_edu_eff+sex, data = dat)
      all_models_coefs[[k]] = coef(curr_mod)[-1]
      all_models_AIC[k]=AIC(curr_mod)
      all_models_functs[[k]]=c(age_funct_list[iAF], edu_funct_list[iEF])
      k = k+1
    }
  }
  
  best_model_ind = which(all_models_AIC==min(all_models_AIC))
  
  if (length(best_model_ind)>1){
    best_model_ind = sample(best_model_ind, 1) # in case there are several best models. choose randomly
  }
  
  best_model_functs = all_models_functs[[best_model_ind]]
  
  best_age_funct = eval(parse(text=best_model_functs[1]))
  best_edu_funct = eval(parse(text=best_model_functs[2]))
  
  best_age_transf = best_model_functs[1]
  best_edu_transf = best_model_functs[2]
  
  
  #best_age_eff = best_age_funct(dat$age) # I have to calculate effects separately cause poly2 creates problems
  #best_edu_eff = best_edu_funct(dat$edu)
  
  ### FIT MODEL 
  
  dat$age_tr = best_age_funct(dat$age)
  dat$edu_tr = best_edu_funct(dat$edu)
  
  mod_formula_text = formula_transf_text(transfs = c(best_age_transf, best_edu_transf, ""), pred.names = c(age, edu, sex), dep.name = dep, data.name = "df")
  
  mod_formula = eval(parse(file="", text=mod_formula_text))
  
  mod_final = step(mod_formula, trace=F)
  
  ### DROP THE NON SIGNIFICANT TERMS
  mod = mod_final
  
  mod.anova=Anova(mod, type="III")[-c(1, dim(Anova(mod, type="III"))[1]), ] #recupero i risultati ANOVA (escluso l'ultimo, residuals)
  
  updated.mod=mod # at the first step updated mod is this
  
  p.crit = 0.05/dim(mod.anova)[1]
  
  if (dim(mod.anova)[1]>0){ # if there is only the Intercept the dimension would be 0.
    #
    mod.terms=as.data.frame(Anova(mod, type="III"))[-c(1, dim(Anova(mod, type="III"))[1]), ] 
    
    while (any(mod.terms[,"Pr(>F)"]>p.crit)){ #salto l'intercetta e check se c'è almeno un term > 0.05
      
      to.drop=rownames(mod.terms[mod.terms[,"Pr(>F)"]==max(mod.terms[,"Pr(>F)"]),]) # trovo il valore con p-value più alto.
      updated.mod=eval(parse(file="", text=paste("update(updated.mod, .~.-", to.drop, ")", sep="")))
      #cat("VARIABLE: ", to.drop, "dropped.\n")
      mod.terms = as.data.frame(Anova(updated.mod, type="III"))[-c(1, dim(Anova(updated.mod, type="III"))[1]), ] 
      
    } 
    mod_final =updated.mod 
  } else {
    mod_final = mod
    cat("only intercept in this model\n")
  }
  
  
  ## UPDATE BEST TRANSFORMATION STORED IN RESULTS
  if( length(grep(age, names(coef(mod_final)))) == 0){
    best_age_transf = "zero"
  }
  
  if( length(grep(edu, names(coef(mod_final)))) == 0){
    best_edu_transf = "zero"
  }
  
  if (length(grep(sex, names(coef(mod_final)))) == 0){
    best_sex_transf = "zero"
  } else {best_sex_transf = "identity"}
  
  
  # predict mean value to calculate adjusted score capitani way.
  age_m = mean(best_age_funct(dat$age))
  edu_m = mean(best_edu_funct(dat$edu))
  sex_m = 0.5
  
  coefs = coef(summary(mod_final))
  age_coef = ifelse(any(grepl(age, rownames(coefs))), coefs[grepl(age, rownames(coefs)), 1], 0)
  edu_coef = ifelse(any(grepl(edu, rownames(coefs))), coefs[grepl(edu, rownames(coefs)), 1], 0)
  sex_coef = ifelse(any(grepl(sex, rownames(coefs))), coefs[grepl(sex, rownames(coefs)), 1], 0)
  
  dat$ADJ_SCORES = dat[,dep] -(age_coef)*(best_age_funct(dat[,age])-age_m) - edu_coef*(best_edu_funct(dat[,edu])-edu_m) -sex_coef*(dat$sex-0.5) # Note it use "sex" cause it is numeric
  
  dat$RESIDUALS = residuals(mod_final)
  
  # uncorrect data above/equal maximum or below/equal minimum value
  dat[dat[, dep]>=dep.range[2], "ADJ_SCORES"] = dep.range[2]
  dat[dat[, dep]<=dep.range[1], "ADJ_SCORES"] = dep.range[1]
  dat[dat$ADJ_SCORES>=dep.range[2], "ADJ_SCORES"] = dep.range[2]
  dat[dat$ADJ_SCORES<=dep.range[1], "ADJ_SCORES"] = dep.range[1]
  
  # these two sets of corrections do not correct values that are initially already ad maximum or minimum, and
  # set threshold of adj score to dep range.
  
  
  # to improve readibility I define the returned model text here
  model_text_res = model_transf_text(mod_final,  transfs =c(best_age_transf, best_edu_transf), 
                                     transfs.names=c("age_tr", "edu_tr"), new.names = c("Age", "Edu"))
  
  return(list(new.df = dat, lm.model = mod_final, transfs =c(best_age_transf, best_edu_transf),
              model_text = model_text_res))
  
  return(list(new.df = dat, lm.model = mod_final, transfs =c(best_age_transf, best_edu_transf), model_text = model_text_res))
}



