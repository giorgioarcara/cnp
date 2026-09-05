# Internal helpers and small user-facing utilities used across the package.

#' Build a model formula (as text) applying named transformations
#'
#' Internal function to create the text of a `lm()` call applying the
#' identified transformation to each predictor. Used by the `adjscores_*()`
#' functions to build their final model formula.
#'
#' @param transfs Character vector of transformation names (e.g.
#'   `c("quadr", "cube")`; `""` means no transformation is applied).
#' @param pred.names Character vector of predictor names (e.g.
#'   `c("Age", "Edu")`), same length as `transfs`.
#' @param dep.name Character. Name of the dependent variable.
#' @param data.name Character. Name of the data.frame (as it appears in the
#'   calling environment) to pass to `data = `.
#'
#' @return A character string with the full `lm(...)` call text, suitable for
#'   `eval(parse(text = ...))`.
#' @keywords internal
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

#' Turn a model's coefficients into a readable equation string
#'
#' Internal function to transform a model into an equation string, to
#' improve readability (e.g. printing/reporting a fitted model).
#'
#' @param mod A fitted model (e.g. from `lm()`).
#' @param digits Integer. Number of significant digits to keep for each
#'   coefficient.
#'
#' @return A character string, e.g. `"12.3 + 0.8 * Age + 0.2 * Sex"`.
#' @keywords internal
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

#' Turn a model's coefficients into a readable equation string, substituting transformation names
#'
#' Internal function to transform a model into an equation string (e.g.
#' `"12 + 0.8 * log(Age) + 0.002 * quadr(Edu) + 0.2 * Sex"`), substituting
#' the internal transformed-variable names (e.g. `age_tr`) used when fitting
#' the model with a readable `transformation(OriginalName)` form. Used by
#' [adjscores_A2024_v1()] and the other `adjscores_*()` functions.
#'
#' @param mod A fitted model (e.g. `lm.model` from an `adjscores_*()` call).
#' @param transfs Character vector of transformation names used in the model
#'   (to be substituted with `transfs.names`).
#' @param transfs.names Character vector of the internal variable names used
#'   when fitting the model (e.g. `c("age_tr", "edu_tr")`).
#' @param new.names Character vector of the readable labels to use in the
#'   output formula (e.g. `c("Age", "Edu")`).
#' @param digits Integer. Number of significant digits to keep for each
#'   coefficient.
#'
#' @return A character string with the readable equation.
#' @keywords internal
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

#' Bin a continuous variable into labeled groups
#'
#' Convenience function that assigns each value of a continuous variable to
#' a bin label, based on a supplied list of `c(min, max)` ranges.
#'
#' @param v Numeric vector, the original continuous variable.
#' @param v_bins A list of two-element numeric vectors, each giving the
#'   inclusive `c(min, max)` range of one bin.
#'
#' @return A character vector of bin labels (e.g. `"18-22"`), same length as
#'   `v`. Values not falling in any bin are `NA`. If bin ranges overlap, the
#'   last matching bin (in the order given) wins.
#'
#' @examples
#' edu = c(3, 7, 10, 16, 20)
#' group_v(edu, v_bins = list(c(0, 5), c(6, 8), c(9, 13), c(14, 18), c(19, Inf)))
#'
#' age = c(20, 35, 55, 75)
#' group_v(age, v_bins = list(c(18, 22), c(23, 40), c(41, 50), c(51, 60),
#'                             c(61, 70), c(71, 80), c(81, 90)))
#' @export
group_v = function(v=NULL, v_bins = NULL){
  v_group = rep(NA, length(v))
  for (iB in 1:length(v_bins)){
    bin_lab = paste(v_bins[[iB]], collapse="-")
    v_group[v>= v_bins[[iB]][1] & v <= v_bins[[iB]][2] ] = bin_lab

  }
  return(v_group)
}

#' Print a simple text progress bar
#'
#' Displays a simple progress bar made of dots in the console/prompt, meant
#' to be called from inside a loop.
#'
#' @param k Integer. The current iteration value (varies within the loop).
#' @param n Integer. The final iteration value (`k` ranges from 1 to `n`).
#' @param s Integer. Number of points shown in the progress bar (default 10).
#'
#' @return Invisibly `NULL`; called for its side effect of printing to the
#'   console.
#' @export
progress_bar = function(k, n, s = 10){


  crit.points = floor(seq(round(n/s), n, length.out=s))

  if (k %in% crit.points) {
    curr_point = which(k==crit.points)
    cat(rep(".", s - curr_point), "\n")
  }


}
