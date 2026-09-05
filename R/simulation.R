# Data-generation helpers for simulating normative datasets
#
# sample.coef.unif(), sample.demo.unif(), sample.demo.cond() and
# sample.transf() generate the building blocks (regression coefficients,
# demographic values, transformations) consumed by sim.norm.data(), the core
# function used to simulate normative data with a known ground-truth model.

#' Sample regression coefficients from a uniform grid
#'
#' Generates values from a uniform (equiprobable) distribution within given
#' ranges, one value per element of `c.range`. Used to generate random
#' regression coefficients (e.g. by [sim.norm.data()]).
#'
#' @param c.range A list of two-element numeric vectors, each giving the
#'   minimum and maximum value a coefficient can take.
#' @param c.steps Numeric vector, same length as `c.range`. Number of
#'   equally-spaced candidate values generated within each range before one
#'   is sampled (e.g. `c.steps[1] = 100` means 100 candidate values are
#'   generated within `c.range[[1]]` and one is drawn from them).
#'
#' @return A list of sampled coefficient values, one per element of
#'   `c.range`.
#' @export
sample.coef.unif = function(c.range=list(c(10, 12), c(-0.05, 0.1), c(0.04, 0.08), c(-0.01, 0.01)),
                        c.steps=c(100, 100, 100, 100)){



  if (length(c.range)!=length(c.steps)){
    stop("c.range must have same length of c.steps")
  }

  res_var=NULL
  for (iC in 1:length(c.range)){
    vlist = seq(c.range[[iC]][1], c.range[[iC]][2], length.out=c.steps[iC])
    res_var[[iC]]=sample(vlist, 1, replace=T)
  }

  return(res_var)

}

#' Sample demographic values independently from uniform distributions
#'
#' Generates age, education, and sex values for `n` hypothetical
#' participants, each sampled independently and uniformly from the supplied
#' candidate values.
#'
#' @param n Integer. Number of participants to generate.
#' @param age_values_t Numeric vector of candidate age values.
#' @param edu_values_t Numeric vector of candidate education values.
#' @param sex_values_t Numeric vector of candidate sex values (0/1).
#'
#' @return A data.frame with columns `age_values_o`, `edu_values_o`,
#'   `sex_values_o`.
#' @export
sample.demo.unif = function(n,
                            age_values_t = seq(18, 90, 1),
                            edu_values_t = seq(5, 20, 1),
                            sex_values_t = c(0, 1)){

  age_values_o = sample(age_values_t, n, replace=T)
  edu_values_o = sample(edu_values_t, n, replace=T)
  sex_values_o = sample(sex_values_t, n, replace=T)

  res = data.frame(age_values_o, edu_values_o, sex_values_o)
  names(res) = c("age_values_o", "edu_values_o", "sex_values_o")

  return(res)


}

#' Sample demographic values with age-conditioned probabilities
#'
#' Generates age values uniformly, then generates education and sex values
#' for each simulated participant using age-conditioned probability
#' distributions, so that (e.g.) younger simulated participants can be given
#' a different education/sex distribution than older ones.
#'
#' @param n Integer. Number of participants to generate.
#' @param age_values_t Numeric vector of candidate age values.
#' @param edu_values_t Numeric vector of candidate education values.
#' @param sex_values_t Numeric vector of candidate sex values (0/1).
#' @param cond.age.edu A list of two elements of equal length: (1) a list of
#'   two-element age-range vectors (e.g. `list(c(18,22), c(23,40), ...)`),
#'   and (2) a list of probability vectors (same length as `edu_values_t`,
#'   each summing to 1) giving the education probabilities for participants
#'   in the corresponding age range.
#' @param cond.age.sex Same structure as `cond.age.edu`, but for sex
#'   probabilities conditioned on age range.
#' @param details Logical. If `TRUE`, prints an additional diagnostic table
#'   of the age/education conditioning groups actually used.
#'
#' @return A data.frame with columns `age_values_o`, `edu_values_o`,
#'   `sex_values_o`.
#' @export
sample.demo.cond = function(n,
                            age_values_t = seq(18, 90, 1),
                            edu_values_t = c(5, 8, 13, 18, 20),
                            sex_values_t = c(0, 1),
                            cond.age.edu = list(
                              list(c(18, 22), c(23, 40), c(41, 50), c(51, 60), c(61, 70), c(71, 80), c(81, 90)),
                              list(c(0,0,1,0,0), c(0, 0, 0.5, 0.3, 0.2), c(0, 0, 0.55, 0.30, 0.15), c(0, 0.2, 0.6, 0.1, 0.1),
                                   c(0, 0.3, 0.5, 0.1, 0.1), c(0.4, 0.3, 0.2, 0.05, 0.05), c(0.3, 0.5, 0.15, 0.025, 0.025))
                            ),
                            cond.age.sex = list(list(c(18, 90)), list(c(0.5, 0.5))), details = F
){

  # internal function
  closest = function(x, yvec){which(abs(x-yvec)==min(abs(x-yvec)))[1]} # take only the first in case of ties.

  # consistency check
  check1 = all(length(cond.age.edu[[1]]) == length(cond.age.edu[[2]]))
  if (!check1){
    stop("cond.age.edu is a list of two elements with the same length.\n Open the function for examples and explanation")
  }


  check2 = all( unlist(lapply(cond.age.edu[[2]], length) == length(edu_values_t)))
  if (!check2){
    stop("cond.age.edu must be a list of k vectors with j elements each.\n Length of k should be equal to that of age_values t. \n
         Length of j should be the same of edu_values_t.\n Open the function for examples and explanation.")
  }

  check3 = all( unlist(lapply(cond.age.edu[[2]], sum)) == 1)
  if (!check3){
    stop("The sum of all elements in cond.age.edu[[2]] list are proportion and they should sum to 1.")
  }


  age_values_o = sample(age_values_t, n, replace=T)

  edu_values_o = rep(0, length(age_values_o)) #initialize edu vec
  sex_values_o = rep(0, length(age_values_o)) #initialize sex vec

  cond.age.edu.values = list(NULL)
  length(cond.age.edu.values)=length(cond.age.edu[[1]])
  for (iAE in 1:length(cond.age.edu[[1]])){
    cond.age.edu.values[[iAE]] = unlist(mapply(rep, edu_values_t, 100*cond.age.edu[[2]][[iAE]]))

  }

  cond.age.sex.values = list(NULL)
  length(cond.age.sex.values)=length(cond.age.sex[[1]])
  for (iAS in 1:length(cond.age.sex[[1]])){
    cond.age.sex.values[[iAS]] = unlist(mapply(rep, sex_values_t, 100*cond.age.sex[[2]][[iAS]]))

  }


  for (iA in 1:length(age_values_o)){
    curr_age = age_values_o[iA]

  #cond.age.edu.values = apply()

  age_edu_group = ceiling( closest (curr_age, unlist(cond.age.edu[[1]])) /2)
  edu_values_o[iA] = sample(cond.age.edu.values[[age_edu_group]], 1)
  # with celiling as it is a list of two-elements, it works to identify co
  # the couple in which the current age is within.

  age_sex_group = ceiling( closest (curr_age, unlist(cond.age.sex[[1]])) /2)
  sex_values_o[iA] = sample(cond.age.sex.values[[age_sex_group]], 1)
  }

  res = data.frame(age_values_o, edu_values_o, sex_values_o)
  names(res) = c("age_values_o", "edu_values_o", "sex_values_o")

  if(details){
    group_labels = unlist( lapply(cond.age.edu[[1]], function(x){paste(x, collapse="-")}) )
    group_labels = rep(group_labels, each=100)
    table(unlist(group_labels), unlist(cond.age.edu.values))

  }

  return(res)


}

#' Sample a random transformation function name
#'
#' Randomly samples the name of one transformation function, e.g. to define
#' the (unknown) generative relationship between a predictor (age or
#' education) and a simulated score.
#'
#' @param include_transf Character vector of transformation names to sample
#'   from. If `NULL` (default), all transformations in [transformations] are
#'   used (`identity`, `quadr`, `log10`, `logm100`, `log`, `log10m100`,
#'   `inv`, `sqrt`, `cube`, `zero`).
#' @param exclude_transf Character vector of transformation names to exclude
#'   from sampling. If `NULL` (default), none are excluded.
#'
#' @return A character string with the name of the sampled transformation.
#' @export
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

#' Simulate normative data with a known generative model
#'
#' Simulates normative data for `n` participants from a linear generative
#' model in age, education, and sex, plus normally-distributed noise. With
#' the default settings, a single female participant with age 50 and
#' education 15 is generated. This is the core function used to generate
#' ground-truth data for testing/comparing normative adjustment methods
#' (see [adjscores_A2024_v1()], [adjscores_C1987_v1()], etc.).
#'
#' @param n Integer. Number of participants to generate.
#' @param coefs A list of 4 values (Intercept, Age, Education, Sex
#'   coefficients) determining the generative model's coefficients. The
#'   final effect on the observed score also depends on `age_transf`/
#'   `edu_transf`. Defaults to a call to [sample.coef.unif()].
#' @param eff_mult Deprecated, unused.
#' @param P_score.sd Numeric. Standard deviation of the noise added to the
#'   demographic-predicted score to obtain each participant's "true"/observed
#'   score (i.e. the residual from the demographic regression model).
#' @param age_values_o Numeric vector (or scalar) of observed age values.
#' @param edu_values_o Numeric vector (or scalar) of observed education
#'   values.
#' @param sex_values_o Vector (or scalar) of observed sex values: either
#'   `"M"`/`"F"` character values, or numeric 0/1 values (e.g. as produced by
#'   [sample.demo.unif()] / [sample.demo.cond()]).
#' @param age_transf Character. Name of the transformation determining the
#'   relationship between age and the score (can be randomly generated with
#'   [sample.transf()]).
#' @param edu_transf Character. Name of the transformation determining the
#'   relationship between education and the score (can be randomly generated
#'   with [sample.transf()]).
#' @param details Logical. If `TRUE`, the returned data.frame also includes
#'   the separate age/education/sex contributions to the final score.
#'
#' @return A list with: a data.frame of simulated normative data (age,
#'   education, sex, demographic score, participant noise, observed score,
#'   ...), a human-readable string describing the generative model, the
#'   coefficients used, and the fitted `lm.model` used to derive the readable
#'   formula.
#' @export
sim.norm.data = function(n,
                         coefs =  sample.coef.unif(c.range = list(c(18, 20), c(0, -4), c(0, 4), c(-1, 1)),
                                                   c.steps=c(100,100,100,100)),
                         eff_mult = NULL, #deprecated.
                         P_score.sd = 1,
                         age_values_o = 50,
                         edu_values_o = 15,
                         sex_values_o = "F",
                         age_transf = NULL,
                         edu_transf = NULL,
                         details=FALSE
) {

  # NOTE! in c.range there are 5 values.
  # 3 are for Age, Education, and Sex, the last 2 are for 2-nd order polynomial part in case poly2 function is selected as transfr
  # in all the other cases they are simply not used.

  # version 2. updated on May 2023.

  vals = c(n, length(age_values_o), length(edu_values_o), length(sex_values_o))

#  if (length(c.range)!=length(eff_mult)){
#    stop("Inconsistency between coefficients and multipliers")
#  }

  if (length(unique(vals))>1){
    stop("Inconsistency between age, edu, sex, and n")
  }


  ### generate numeric values for sex
  # accepts either "M"/"F" character values or the numeric 0/1 values produced by sample.demo.cond/sample.demo.unif
  if (is.numeric(sex_values_o)){
    sex_values_o_n = sex_values_o
  } else {
    sex_values_o_n = ifelse(sex_values_o=="M", 0, 1) # numeric version of value
  }

  # create current data.frame
  curr_dat = data.frame(Age = age_values_o, Edu=edu_values_o, Sex=sex_values_o, Sex_n = sex_values_o_n)

  # generate multiple coefficients for Age and Edu in case of 2-nd order polynomials.
  names(coefs)=c("Intercept", "Age_coef", "Edu_coef", "Sex_coef")
  coefs = as.data.frame(t(unlist(coefs)))


  # select transformation function
  age_funct = eval(parse(text=age_transf))
  edu_funct = eval(parse(text=edu_transf))


  comment(curr_dat)=paste("age fun = ", age_transf, "; edu fun = ", edu_transf, sep="")


  ## determine score predicted by demographic variable

  # Intercept fix for Age and Edu offset

  # rescale to be compatible to linear, by anchoring to age, adu, and age range
  # basically it transform the range of the original scores, in the range as defined in
  # eff_mult vector.

  Age_coef = coefs$Age_coef / abs ( diff( age_funct(c(18,90)) ) )
  Edu_coef= coefs$Edu_coef / abs ( diff( edu_funct(c(5,20)) ) )
  Sex_coef = coefs$Sex / abs ( diff( (c(0,1)) ) )


  # Fix intercept
  #coefs$Intercept = coefs$Intercept - coefs$Age_coef*age_funct(18) - coefs$Edu_coef*edu_funct(5) - coefs$Sex_coef*(0)


  if(age_transf=="zero"){
    Age_coef = 0
  }

  if(edu_transf=="zero"){
    Edu_coef = 0
  }

  # TO BE CHECKED: PROBABLY POLINOMIALS LIKE THE LINES BELOW (but not working the code above)
  #Age2_score = Age2_eff/sd(Age2_eff)  - (mean(Age2_eff)/sd(Age2_eff)) * eff_mult[4]
  #Edu2_score = Edu2_eff/sd(Edu2_eff) - (mean(Edu2_eff)/sd(Edu2_eff)) * eff_mult[5]
  Age_score = Age_coef*age_funct(age_values_o)
  Edu_score = Edu_coef*edu_funct(edu_values_o)
  Sex_score = Sex_coef*(sex_values_o_n)


  if (details){

    curr_dat$Age_score = Age_score
    curr_dat$Edu_score = Edu_score
    curr_dat$Sex_score = Sex_score
    curr_dat$Intercept = coefs$Intercept

  }


  curr_dat$Demo_score = coefs$Intercept + Age_score + Edu_score + Sex_score

  ## readable formula ##
  ## create string for Age
  if (age_transf=="poly2"){
    Age_read_eff = paste(signif(coefs$Age_coef,2), "*Age +", signif(coefs$Age2_coef,2), "*Age^2 +", sep="")
  } else {
    Age_read_eff = paste(signif(coefs$Age_coef,2), "*", age_transf, "(Age) + ", sep="")
  }

  ## create string for Edu
  if (edu_transf=="poly2"){
    Edu_read_eff = paste(signif(coefs$Edu_coef,2), "*Edu +", signif(coefs$Edu2_coef,2), "*Edu^2 + ", sep="")
  } else {
    Edu_read_eff = paste(signif(coefs$Edu_coef,2), "*", edu_transf, "(Edu) + ", sep="")
  }

  age_tr = age_funct(curr_dat$Age)
  edu_tr = edu_funct(curr_dat$Edu)

  # Deprecated: get empirically the model from lm model
  Demo_mod = lm(Demo_score~age_tr+edu_tr+Sex_n, curr_dat )
  # to improve readibility I define the returned model text here
  #model_text_res = model_transf_text(Demo_mod,  transfs =c(age_transf, edu_transf),
                                     #transfs.names=c("age_tr", "edu_tr"), new.names = c("Age", "Edu"))

  model_text_res = paste(signif(coefs$Intercept, 2), "+", signif(Age_coef,2), "*", age_transf, "(Age) + ", signif(Edu_coef,2), "*", edu_transf, "(Edu) +", signif(Sex_coef,2), "*(Sex_n)")


  #read_formula = paste(deparse(round(coefs$Intercept,2)), "+",
  #                     Age_read_eff,
  #                     Edu_read_eff,
  #                     deparse(round(coefs$Sex_coef,2)), "*(Sex_n)")


  # generate the Participant Score, which in Capitani (1987) formulation
  # is the residual noise from regression with demographic variables as predictors. (also called "True Adjusted Score" Capitani & Laiacona, 1988)
  # to allow having proportional effect of noise, the sd is calculated, from the range of observed scores
  # and a multiplier (which is divided by the difference between score range)

  noise = rnorm(n, mean=0, sd=P_score.sd)

  curr_dat$P_score = noise # Note, here for P score I denote the "True" Participant score. It is not the True score in CCT sense, cause random measurement error is part of this P_score

  curr_dat$Obs_score = curr_dat$Demo_score + curr_dat$P_score

  #plot(curr_dat$Age, curr_dat$Obs_score)
  #plot(curr_dat$Edu, curr_dat$Obs_score)
  res = list(curr_dat, model_text_res, coefs, lm.model = Demo_mod)
  return(res)

}
