# Equivalent Scores methodology (tolerance limits + ES classification)
#
# Adapted from Aiello & Depaoli (2022), building on Capitani's normative data
# framework. tolLimits.obs() and tolLimits.adjscores() locate the outer/inner
# tolerance limit observations in a normative sample; ES() uses those to
# classify (adjusted) scores into the five Equivalent Score classes.

#' Locate the outer and inner tolerance limit observations
#'
#' Identifies the observation ranks corresponding to the outer (oTL) and
#' inner (iTL) tolerance limits for a normative sample of size `n`, following
#' Aiello & Depaoli (2022).
#'
#' @param n Numeric. Sample size of the normative sample.
#'
#' @return A one-row data.frame with columns `oTL`, `iTL`, `p_oTL`, `p_iTL`.
#'   `oTL` is the character string `"not defined"` when the sample is too
#'   small for an outer tolerance limit to exist.
#' @export
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

#' Outer tolerance limit value from a vector of adjusted scores
#'
#' Ranks a vector of adjusted scores and returns the value at the outer
#' tolerance limit rank (as identified by [tolLimits.obs()]).
#'
#' @param adjscores Numeric vector of adjusted scores.
#'
#' @return The adjusted score value at the outer tolerance limit rank.
#' @export
tolLimits.adjscores = function(adjscores){
  dat = data.frame(AS = adjscores)
  dat$ranked_AS = rank(dat$AS)
  oTL.n= tolLimits.obs(dim(dat)[1])$oTL

  # order data.frame according to ran
  dat = dat[order(dat$ranked_AS), ]
  oTL = dat[oTL.n, "AS"]

  return(oTL)

}

#' Calculate Equivalent Scores (ES) cut-offs
#'
#' Calculates the Equivalent Scores classification (ES0/oTL through ES4)
#' either as observation counts (ranks) for a normative sample of size `n`,
#' or directly as adjusted-score cut-off values when `adjscores` is supplied.
#' Adapted from Aiello & Depaoli (2022).
#'
#' @param n Numeric. Sample size. Ignored (and re-derived) if `adjscores` is
#'   supplied. Must have length 1.
#' @param adjscores Optional numeric vector of adjusted scores. If supplied,
#'   cut-off values are returned in addition to observation counts.
#' @param lower_tail Logical. If `TRUE` (default), lower adjusted scores are
#'   treated as more impaired (ES0 at the bottom of the distribution); if
#'   `FALSE`, the ranking is reversed.
#'
#' @return If `adjscores` is `NULL`, a named numeric vector of observation
#'   counts for the four ES cut-offs. If `adjscores` is supplied, a list with
#'   `Observations` (the same counts) and `Adjusted_Scores` (the
#'   corresponding cut-off values in the input scale).
#' @export
ES <- function(n=NULL, adjscores=NULL, lower_tail = TRUE){

  if(length(n)>1){
    stop("n must have length 1")
  }

  if (!is.null(adjscores)){
    dat = data.frame(AS = sort(adjscores)) # note that I sort here otherwise the reversing (below) will not work
    n = dim(dat)[1]
  }

  oTL = tolLimits.obs(n)$oTL

  if(oTL =="not defined"){
    stop("oTL is undefined: impossible to calculate ES")
  }

  cd1 <- oTL/n
  z1 <- qnorm(cd1)

  z1_3 <- z1/3
  z1_2 <- z1_3*2

  cd2 <- pnorm(z1_2)
  a <- (cd1-cd2)*n
  a_r <- round(a)
  ES1 <- -a_r+oTL


  cd3 <- pnorm(z1_3)
  b <- (cd3-cd2)*n
  b_r <- round(b)
  ES2 <- ES1+b_r


  cd4 <- pnorm(0)
  c <- (cd4-cd3)*n
  c_r <- round(c)
  ES3 <- ES2+c_r

  ES.n=c(oTL, ES1, ES2, ES3)
  names(ES.n)=c("ES0(oTL)-ES1", "ES1-ES2", "ES2-ES3", "ES3-ES4")

  if (!is.null(adjscores)){

    if (lower_tail){
      dat$sorted_AS = 1:nrow(dat)
    } else {
      dat$sorted_AS = nrow(dat):1
    }

    dat = dat[order(dat$sorted_AS), ]
    ES.s = unlist(dat[ES.n, "AS"])
    names(ES.s)=c("ES0(oTL)-ES1", "ES1-ES2", "ES2-ES3", "ES3-ES4")

    res = list(ES.n, ES.s)
    names(res) = c("Observations", "Adjusted_Scores")
    return(res)

  } else {

    res = ES.n

    return(res)

  }

}
