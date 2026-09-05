# Regression-based normative adjustment methods (Arcara 2024 / Capitani 1987)
#
# Each method has several versions, corresponding to different interpretations
# of the model-selection / term-dropping procedure (see the Arcara (2024)
# manuscript and its supplementary materials for a full comparison). All
# versions share the same argument names and return structure so they can be
# swapped for one another.

#' Adjusted scores via the Arcara (2024) method (version 1)
#'
#' Calculates adjusted scores via regression modeling. Model selection is
#' performed by fitting all possible age/education transformation
#' combinations (see Details) and selecting the model with the lowest AIC.
#' No stepwise elimination or term dropping is performed after that.
#'
#' @param df A data.frame with the raw data.
#' @param dep Character. Name of the dependent variable column.
#' @param dep.range Numeric vector of length 2. Range of the dependent
#'   variable's raw scores. Adjusted scores are not extrapolated beyond this
#'   range (ceiling/floor effects are left uncorrected, as in the original
#'   Equivalent Scores method).
#' @param age Character. Name of the numeric age column.
#' @param edu Character. Name of the numeric education column.
#' @param sex Character. Name of the sex column (factor/character with 2
#'   levels, or already-numeric 0/1).
#'
#' @details
#' Candidate transformations tested for both age and education: `identity`,
#' `quadr`, `log10`, `logm100`, `log`, `log10m100`, `inv`, `sqrt`, `cube`
#' (see [transformations]). The combination with the lowest AIC across a full
#' grid search is used to fit the final model.
#'
#' @return A list with:
#' \describe{
#'   \item{new.df}{The input data (NA-omitted) with `ADJ_SCORES` and
#'     `RESIDUALS` columns added.}
#'   \item{lm.model}{The fitted `lm` model.}
#'   \item{transfs}{Character vector with the two selected transformations
#'     for age and education.}
#'   \item{model_text}{A human-readable string describing the fitted
#'     equation.}
#' }
#' @export
adjscores_A2024_v1 <- function(df = NULL, dep = "Dep", dep.range = c(0,30), age = "Age", edu="Education", sex="Sex"){

  #  min AIC selection for all transformations and model combinations.

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
   # cat("Sex converted to numeric")
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
  edu_funct_list =  c("identity",  "quadr", "log10", "logm100", "log", "log10m100", "inv", "sqrt", "cube")
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
      #curr_mod = step(lm(dep~curr_age_eff+curr_edu_eff+sex, data = dat), trace = F)
      curr_mod = lm(dep~curr_age_eff+curr_edu_eff+sex, data = dat)
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

  #mod_final = lm(dep~age_tr+edu_tr+sex, data=dat)
  mod_formula_text = formula_transf_text(transfs = c(best_age_transf, best_edu_transf, ""), pred.names = c(age, edu, "sex"), dep.name = dep, data.name = "dat")

  mod_formula = eval(parse(file="", text=mod_formula_text))

  mod_final = mod_formula



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

  # to improve readibility I define the returned model text here
  model_text_res = model_transf_text(mod_final,  transfs =c(best_age_transf, best_edu_transf),
                                     transfs.names=c("age_tr", "edu_tr"), new.names = c("Age", "Edu"))

  return(list(new.df = dat, lm.model = mod_final, transfs =c(best_age_transf, best_edu_transf),
              model_text = model_text_res))
}

#' Adjusted scores via the Arcara (2024) method (version 2)
#'
#' Same transformation grid search as [adjscores_A2024_v1()], but each
#' candidate model is additionally reduced via stepwise AIC selection
#' ([stats::step()]) before comparing AIC across candidates.
#'
#' @inheritParams adjscores_A2024_v1
#' @inherit adjscores_A2024_v1 return
#' @export
adjscores_A2024_v2 <- function(df = NULL, dep = "Dep", dep.range = c(0,30), age = "Age", edu="Education", sex="Sex"){

  # stepwise min AIC selection for all transformations and model combinations + stepwise elimination

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
    #cat("Sex converted to numeric")
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

  mod_formula_text = formula_transf_text(transfs = c(best_age_transf, best_edu_transf, ""), pred.names = c(age, edu, "sex"), dep.name = dep, data.name = "dat")

  mod_formula = eval(parse(file="", text=mod_formula_text))

  mod_final = step(mod_formula, trace=F)


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

  # to improve readibility I define the returned model text here
  model_text_res = model_transf_text(mod_final,  transfs =c(best_age_transf, best_edu_transf),
                                     transfs.names=c("age_tr", "edu_tr"), new.names = c("Age", "Edu"))

  return(list(new.df = dat, lm.model = mod_final, transfs =c(best_age_transf, best_edu_transf),
              model_text = model_text_res))
}

#' Adjusted scores via the Arcara (2024) method (version 3)
#'
#' Same as [adjscores_A2024_v2()] (transformation grid search + stepwise AIC
#' selection), with an additional step that drops any remaining
#' non-significant terms (Bonferroni-corrected Type III F-tests, via
#' [car::Anova()]) iteratively, starting from the term with the highest
#' p-value, until only significant terms remain.
#'
#' @inheritParams adjscores_A2024_v1
#' @inherit adjscores_A2024_v1 return
#' @export
adjscores_A2024_v3 <- function(df = NULL, dep = "Dep", dep.range = c(0,30), age = "Age", edu="Education", sex="Sex"){

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

  mod_formula_text = formula_transf_text(transfs = c(best_age_transf, best_edu_transf, ""), pred.names = c(age, edu, "sex"), dep.name = dep, data.name = "dat")

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
}

#' Adjusted scores via the Capitani (1987) method (version 1)
#'
#' Calculates adjusted scores via regression modeling, following the method
#' described in Capitani's chapter in Spinnler & Tognoni (1987). The best
#' transformation for age and education is first selected independently
#' (highest univariate R-squared), a multivariate model with Sex is then
#' fitted, and non-significant terms (Bonferroni-corrected, alpha = 0.05/3)
#' are dropped **all at once, in a single step**.
#'
#' @param df A data.frame with the raw data.
#' @param dep Character. Name of the dependent variable column.
#' @param dep.range Numeric vector of length 2. Range of the dependent
#'   variable's raw scores. Adjusted scores are not extrapolated beyond this
#'   range (ceiling/floor effects are left uncorrected, as in the original
#'   Equivalent Scores method).
#' @param age Character. Name of the numeric age column.
#' @param edu Character. Name of the numeric education column.
#' @param sex Character. Name of the sex column (factor/character with 2
#'   levels, or already-numeric 0/1).
#' @param ID Character. Name of an ID column (currently unused; kept for
#'   interface compatibility).
#'
#' @details
#' Because terms are dropped all together in one step, a term initially kept
#' (p < 0.017) can end up in a model where it is no longer significant once
#' the other terms are removed. See [adjscores_C1987_v2()] for the iterative
#' alternative, which re-checks significance after each single-term removal.
#'
#' @return A list with:
#' \describe{
#'   \item{new.df}{The input data (NA-omitted) with `ADJ_SCORES` and
#'     `RESIDUALS` columns added.}
#'   \item{lm.model}{The fitted `lm` model.}
#'   \item{transfs}{Character vector with the two selected transformations
#'     for age and education.}
#'   \item{model_text}{A human-readable string describing the fitted
#'     equation.}
#' }
#' @export
adjscores_C1987_v1 <- function(df = NULL, dep = "Dep", dep.range = c(0,30), age = "Age", edu="Education", sex="Sex", ID = "ID"){

  # single drop of terms with p < p.crit (final model could have non-significant terms, as the overall model changes)

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
    #cat("Sex converted to numeric")
  }

  #compute most common transformations for age and education
  cube = function(x){x^3}
  quadr = function(x){x^2}
  logm100 = function(x){log(100-x)}
  log10m100 = function(x){log10(100-x)}
  log10mAve = function(x){log10(mean(x)-x)} # not included now.
  inv = function(x){1/x}
  poly2 = function(x){poly(x,2)}

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

  mod_formula_text = formula_transf_text(transfs = c(best_age_transf, best_edu_transf, ""), pred.names = c(age, edu, "sex"), dep.name = dep, data.name = "dat")

  mod_formula = eval(parse(file="", text=mod_formula_text))

  ### DROP THE NON SIGNIFICANT TERMS
  # to avoid problem with dropping terms based on T, I make the selection based on p-values associated to F test
  # as in the case of the model there are factors with more than two levels, this is mathematically equivalent to base
  # the selection on t (F = t^2), for covariates only

  p.crit = 0.05/3

  mod = mod_formula

  mod.anova=Anova(mod, type="III")[-dim(Anova(mod, type="III"))[1], ] # get results from Anova excluding the last (referred to residuals.)

  updated.mod=mod # at the first step updated mod is this
  #
  mod.terms=Anova(mod, type="III")[-dim(Anova(mod, type="III"))[1], ]

  to.drop=rownames(mod.terms[mod.terms[,"Pr(>F)"]>p.crit,]) # trovo il valore con p-value più alto.

  if (length(to.drop)>0){
    to.drop.text = paste(to.drop, collapse = " - ") # collapse into a single string so ALL flagged terms are dropped together, not just the last one
    updated.mod=eval(parse(file="", text=paste("update(updated.mod, .~. - ", to.drop.text, ")", sep="")))
  }

  mod_final =updated.mod


  ## UPDATE BEST TRANSFORMATION STORED IN RESULTS
  if( length(grep(age, names(coef(mod_final)))) == 0){
    best_age_transf = "zero"
  }

  if( length(grep(edu, names(coef(mod_final)))) == 0){
    best_edu_transf = "zero"
  }


  ####
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

  # NOTA GIORGIO : adjust the output: return regression and return new dataset
  # not strictly necessary for simulation, necessary for using the code

  # to improve readibility I define the returned model text here
  model_text_res = model_transf_text(mod_final,  transfs =c(best_age_transf, best_edu_transf),
                                     transfs.names=c("age_tr", "edu_tr"), new.names = c("Age", "Edu"))

  return(list(new.df = dat, lm.model = mod_final, transfs =c(best_age_transf, best_edu_transf),
              model_text = model_text_res))
}

#' Adjusted scores via the Capitani (1987) method (version 2)
#'
#' Same as [adjscores_C1987_v1()] (best transformation per predictor selected
#' independently, then a multivariate model with Sex), but non-significant
#' terms are dropped **iteratively**: at each step the single term with the
#' highest p-value is removed and Type III F-tests ([car::Anova()]) are
#' recomputed, until only Bonferroni-significant terms (alpha = 0.05/3)
#' remain.
#'
#' @inheritParams adjscores_C1987_v1
#' @inherit adjscores_C1987_v1 return
#' @export
adjscores_C1987_v2 <- function(df = NULL, dep = "Dep", dep.range = c(0,30), age = "Age", edu="Education", sex="Sex", ID = "ID"){

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

  mod_formula_text = formula_transf_text(transfs = c(best_age_transf, best_edu_transf, ""), pred.names = c(age, edu, "sex"), dep.name = dep, data.name = "dat")

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
