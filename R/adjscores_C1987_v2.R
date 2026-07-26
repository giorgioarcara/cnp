## calculate adjusted scores according to E. Capitani's method (See Spinnler & Tognoni, 1987; Capitani 1997) method. - VERSION 2
# the function calculates adjusted scores via regression modeling. The model selection is made first by fitting all possible models within a set of transformations
# (see the function) in separate univariate model for age and education. Once the best transformation is selected a multivariate model is fitted, including also Sex.
# finally, terms are retained using Bonferroni corrected (0.05/3 = 0.017) threshold for significance of each tested coefficient.

# NOTE: this version is different from version 1 in the sense that terms are dropped iteratevely from the one with the highest p to the one with lowest p, until only significant terms
#, Bonferroni corrected, are kept.

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

# Author: Giorgio Arcara (2024) v 1.0

adjscores_C1987 <- function(df = NULL, dep = "Dep", dep.range = c(0,30), age = "Age", edu="Education", sex="Sex", ID = "ID"){
  
  ### stepwise elimination with F test from highest term
  
  dat = df
  
  dat$dep = dat[, dep]
  dat$age = dat[, age]
  dat$edu = dat[, edu]
  dat$sex = dat[, sex]
  
  dat = na.omit(dat)
  
  #
  # convert sex to numeric
  if(!is.numeric(dat$sex)){
    dat$sex = factor(dat$sex)
    dat$sex.or = dat$sex
    
    dat$sex= ifelse(dat$sex==levels(dat$sex)[1], 1, 0)
    cat("Sex converted to numeric")
  }
  
  #compute most common transformations for age and education
  #compute most common transformations for age and education
  cube = function(x){x^3}
  quadr = function(x){x^2}
  logm100 = function(x){log(100-x)}
  log10m100 = function(x){log10(100-x)}
  log10mAve = function(x){log10(mean(x)-x)} # not included now.
  inv = function(x){1/x}
  poly2 = function(x){poly(x,2)}
  # log
  
  age_funct_list = c("identity",  "quadr", "log10", "logm100", "log", "log10m100", "inv", "sqrt", "cube")
  edu_funct_list =  c("identity",  "quadr", "log10", "logm100", "log", "log10m100", "inv", "sqrt", "cube")

  # CAPITANI'S REGRESSION METHOD STARTS HERE
  
  ### FIND BEST AGE TRANSFORMATION
  age_R2 = NULL
  for (iF in 1:length(age_funct_list)){ # loop over transformations
    curr_funct = eval(parse(text=age_funct_list[iF]))
    curr_mod = lm(dep~curr_funct(age), data = dat)
    curr_R2 = summary(curr_mod)$r.squared
    if (is.null(curr_R2)){
      curr_R2=0
    }
    age_R2[iF] = curr_R2
  }
  
  best_age_funct_ind = which(age_R2==max(age_R2))
  
  if (length(best_age_funct_ind)>1){
    best_age_funct_ind = sample(best_age_funct_ind, 1) # in case there are several best models. choose randomly
  }
  
  best_age_funct= eval(parse(text = age_funct_list[best_age_funct_ind]))
  best_age_transf = age_funct_list[best_age_funct_ind]
  
  ### FIND BEST EDUCATION TRANSFORMATION
  
  edu_R2 = NULL
  for (iF in 1:length(edu_funct_list)){ # loop over transformations
    curr_funct = eval(parse(text=edu_funct_list[iF]))
    curr_mod = lm(dep~curr_funct(edu), data = dat)
    curr_R2 = summary(curr_mod)$r.squared
    if (is.null(curr_R2)){
      curr_R2=0
    }
    edu_R2[iF] = curr_R2
  }

  
  best_edu_funct_ind = which(edu_R2==max(edu_R2))
  
  if (length(best_edu_funct_ind)>1){
    best_edu_funct_ind = sample(best_edu_funct_ind, 1) # in case there are several best models. choose randomly
  }
  
  best_edu_funct= eval(parse(text = edu_funct_list[best_edu_funct_ind]))
  best_edu_transf = edu_funct_list[best_edu_funct_ind]
  
  ### FIT MODEL 
  
  dat$age_tr = best_age_funct(dat$age)
  dat$edu_tr = best_edu_funct(dat$edu)
  
  mod_formula_text = formula_transf_text(transfs = c(best_age_transf, best_edu_transf, ""), pred.names = c(age, edu, sex), dep.name = dep, data.name = "df")
  
  mod_formula = eval(parse(file="", text=mod_formula_text))
  
  ### DROP THE NON SIGNIFICANT TERMS
  p.crit = 0.05/3
  
  mod = mod_formula
  
  #mod_final = lm.drop.F(mod_ini, p.crit=0.05/3)
  
  mod.anova=Anova(mod, type="III")[-c(1, dim(Anova(mod, type="III"))[1]), ] #recupero i risultati ANOVA (escluso l'ultimo, residuals)
  
  updated.mod=mod # at the first step updated mod is this
  
  if (dim(mod.anova)[1]>0){ # if there is only the Intercept the dimension would be 0.
    #
    mod.terms=as.data.frame(Anova(mod, type="III"))[-c(1, dim(Anova(mod, type="III"))[1]), ] 
    
    while (any(mod.terms[,"Pr(>F)"]>p.crit)){ #salto l'intercetta e check se c'è almeno un termsf > 0.05
      
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
  
  
  ####
  # CAPITANI'S REGRESSION METHOD END HERE
  
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
  
  
  # NOTA GIORGIO : adjust the output: return regression and return new dataset
  # not strictly necessary for simulation, necessary for using the code
  
  # to improve readibility I define the returned model text here
  model_text_res = model_transf_text(mod_final,  transfs =c(best_age_transf, best_edu_transf), 
                                     transfs.names=c("age_tr", "edu_tr"), new.names = c("Age", "Edu"))
  
  return(list(new.df = dat, lm.model = mod_final, transfs =c(best_age_transf, best_edu_transf),
              model_text = model_text_res))
}