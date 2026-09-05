# Change-detection methods (Reliable Change Index family).
#
# SCAFFOLD / DRAFT -- see dev/DESIGN-change-methods.md.
# Nothing here is exported yet (@keywords internal). This file sketches the
# target structure: a pure computational core (section 1) plus a thin S3
# object layer (section 2) implementing the "estimate from norms -> apply to
# new data" split used elsewhere in the package.
#
# Folds in: 2026-ICC-RCI/R_functions/change_functions.R
#   RCI / mRCI / RCI_ICC / mRCI_ICC / CG2006reg / change_func()

## =====================================================================
## 1. Computational core -- pure functions, one calculation each.
##    These are the readable specification of each method and the unit
##    tested in tests/testthat/test-rci.R.
## =====================================================================

#' Standard error of the difference from paired normative data
#'
#' `sediff_pearson()` uses the Pearson test-retest correlation as the
#' reliability estimate (Jacobson & Truax, 1991):
#' \eqn{SE = SD_{t0}\sqrt{1 - r}}, \eqn{SE_{diff} = \sqrt{2\,SE^2}}.
#'
#' `sediff_icc()` uses the intraclass correlation coefficient instead
#' (two-way random effects, absolute agreement, single rater --
#' `psych::ICC()`'s "ICC2"), computing SEM separately at each timepoint:
#' \eqn{SEM_j = SD_j\sqrt{1 - ICC}}, \eqn{SE_{diff} = \sqrt{SEM_{t0}^2 + SEM_{t1}^2}}.
#'
#' @param xt0,xt1 Numeric vectors of equal length: the normative sample's
#'   baseline and follow-up scores (paired).
#' @return A single numeric value, the standard error of the difference.
#' @keywords internal
sediff_pearson <- function(xt0, xt1) {
  stopifnot(length(xt0) == length(xt1))
  r  <- stats::cor(xt0, xt1)
  se <- stats::sd(xt0) * sqrt(1 - r)
  sqrt(2 * se^2)
}

#' @rdname sediff_pearson
#' @keywords internal
sediff_icc <- function(xt0, xt1) {
  stopifnot(length(xt0) == length(xt1))
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Package 'psych' is required for ICC-based reliability. ",
         "Install it, or use reliability = \"pearson\".", call. = FALSE)
  }
  res <- suppressWarnings(psych::ICC(cbind(xt0, xt1), lmer = FALSE)$results)
  icc <- res$ICC[res$type == "ICC2"]
  sem0 <- stats::sd(xt0) * sqrt(1 - icc)
  sem1 <- stats::sd(xt1) * sqrt(1 - icc)
  sqrt(sem0^2 + sem1^2)
}

#' Average practice / retest effect in a normative sample
#'
#' The mean change from baseline to follow-up, subtracted from a patient's
#' observed change before standardizing (Chelune et al., 1993).
#'
#' @inheritParams sediff_pearson
#' @return A single numeric value.
#' @keywords internal
practice_mean <- function(xt0, xt1) {
  stopifnot(length(xt0) == length(xt1))
  mean(xt1 - xt0)
}

#' Standardized change score
#'
#' @param xt0_test,xt1_test Numeric (scalar or vector): the individual's
#'   baseline and follow-up score(s).
#' @param sediff Numeric(1). Standard error of the difference (see
#'   [sediff_pearson()]).
#' @param practice Numeric(1). Practice-effect correction to subtract from
#'   the observed change (0 = none).
#' @return Numeric, same length as `xt1_test - xt0_test`.
#' @keywords internal
rci_z <- function(xt0_test, xt1_test, sediff, practice = 0) {
  (xt1_test - xt0_test - practice) / sediff
}

#' Crawford & Garthwaite (2006) regression-based prediction for a new case
#'
#' Expected follow-up score and its standard error of prediction, both as a
#' function of the individual's baseline score.
#'
#' @param model An `lm` of follow-up on baseline (`xt1 ~ xt0`) fitted on the
#'   normative sample.
#' @param xt0_test Numeric (scalar or vector). Individual baseline score(s).
#' @return A list with `fit` (expected follow-up), `se` (standard error of
#'   prediction) and `df` (residual degrees of freedom, `n - 2`).
#' @keywords internal
cg_prediction <- function(model, xt0_test) {
  nd  <- data.frame(xt0 = xt0_test)
  pr  <- stats::predict(model, newdata = nd, se.fit = TRUE)
  s_e <- pr$residual.scale
  # se of prediction for a new observation = sqrt(se.fit^2 + sigma^2)
  se_pred <- sqrt(pr$se.fit^2 + s_e^2)
  list(fit = as.numeric(pr$fit), se = as.numeric(se_pred), df = pr$df)
}

## =====================================================================
## 2. Object layer -- S3, thin wrapper over the core.
## =====================================================================

#' Constructor for a fitted RCI model (internal, low-level)
#'
#' @param sediff,practice Numeric(1). `NA` for the regression method.
#' @param reliability,practice_method Character(1).
#' @param n Integer(1). Normative sample size.
#' @param model Optional `lm` (regression method only).
#' @param call The originating `match.call()`.
#' @return An object of class `c("cnp_rci", "cnp_normmodel")`.
#' @keywords internal
new_rci <- function(sediff, practice, reliability, practice_method,
                    n, model = NULL, call = NULL) {
  structure(
    list(
      sediff          = sediff,
      practice        = practice,
      reliability     = reliability,
      practice_method = practice_method,
      n               = n,
      model           = model,
      call            = call
    ),
    class = c("cnp_rci", "cnp_normmodel")
  )
}

#' Reliable Change Index: estimate from normative data
#'
#' Estimates the quantities needed to test whether an individual's change
#' between two assessments exceeds what is expected from measurement error
#' (and, optionally, practice effects) in a normative sample. Apply the
#' fitted model to a new case with [predict()][predict.cnp_rci] or
#' [classify()][classify.cnp_rci].
#'
#' The classic named methods map onto two orthogonal arguments:
#'
#' | method (literature)          | `reliability`  | `practice` |
#' | ---------------------------- | -------------- | ---------- |
#' | RCI (Jacobson & Truax 1991)  | `"pearson"`    | `"none"`   |
#' | mRCI (Chelune et al. 1993)   | `"pearson"`    | `"mean"`   |
#' | RCI-ICC                      | `"icc"`        | `"none"`   |
#' | mRCI-ICC                     | `"icc"`        | `"mean"`   |
#' | CG regression (Crawford & Garthwaite 2006) | `"regression"` | n/a |
#'
#' @param xt0,xt1 Numeric vectors of equal length: the normative sample's
#'   baseline and follow-up scores (paired).
#' @param reliability How to estimate reliability / expected follow-up.
#' @param practice Practice-effect correction. Ignored when
#'   `reliability = "regression"` (the regression already models it).
#' @return An object of class `c("cnp_rci", "cnp_normmodel")`.
#' @keywords internal
rci <- function(xt0, xt1,
                reliability = c("pearson", "icc", "regression"),
                practice    = c("none", "mean")) {
  reliability <- match.arg(reliability)
  practice    <- match.arg(practice)
  stopifnot(length(xt0) == length(xt1), length(xt0) > 2)

  if (reliability == "regression") {
    model <- stats::lm(xt1 ~ xt0, data = data.frame(xt0 = xt0, xt1 = xt1))
    return(new_rci(
      sediff = NA_real_, practice = NA_real_,
      reliability = reliability, practice_method = "regression",
      n = length(xt0), model = model, call = match.call()
    ))
  }

  sediff <- switch(reliability,
    pearson = sediff_pearson(xt0, xt1),
    icc     = sediff_icc(xt0, xt1)
  )
  k <- switch(practice,
    none = 0,
    mean = practice_mean(xt0, xt1)
  )
  new_rci(
    sediff = sediff, practice = k,
    reliability = reliability, practice_method = practice,
    n = length(xt0), call = match.call()
  )
}

#' Apply a fitted RCI model to a new case
#'
#' @param object An object from [rci()].
#' @param xt0_test,xt1_test Numeric (scalar or vector): the individual's
#'   baseline and follow-up score(s).
#' @param ... Unused.
#' @return Numeric standardized change score(s). For
#'   `reliability = "regression"` the value is t-distributed with `n - 2`
#'   df; otherwise it is treated as a z score.
#' @keywords internal
predict.cnp_rci <- function(object, xt0_test, xt1_test, ...) {
  if (object$reliability == "regression") {
    cg <- cg_prediction(object$model, xt0_test)
    return((xt1_test - cg$fit) / cg$se)
  }
  rci_z(xt0_test, xt1_test, object$sediff, object$practice)
}

#' Classify an individual's change as reliable decline / no change / improvement
#'
#' @param object An object from [rci()].
#' @param xt0_test,xt1_test Numeric: the individual's scores.
#' @param alpha Two-sided significance level.
#' @param higher_is_better Logical. If `TRUE` (default) an increase is
#'   "improvement"; set `FALSE` for scores where lower = better (e.g. error
#'   counts, reaction times).
#' @param ... Unused.
#' @return An ordered factor with levels
#'   `c("reliable decline", "no change", "reliable improvement")`, carrying
#'   the numeric change score as attribute `"score"`.
#' @keywords internal
classify.cnp_rci <- function(object, xt0_test, xt1_test,
                             alpha = 0.05, higher_is_better = TRUE, ...) {
  z <- predict(object, xt0_test, xt1_test)
  crit <- if (object$reliability == "regression") {
    stats::qt(1 - alpha / 2, df = object$n - 2)
  } else {
    stats::qnorm(1 - alpha / 2)
  }
  raw <- ifelse(z <= -crit, "decline",
         ifelse(z >=  crit, "improvement", "none"))
  if (!higher_is_better) {
    raw <- c(decline = "improvement", improvement = "decline",
             none = "none")[raw]
  }
  out <- factor(
    c(decline = "reliable decline", none = "no change",
      improvement = "reliable improvement")[raw],
    levels = c("reliable decline", "no change", "reliable improvement"),
    ordered = TRUE
  )
  attr(out, "score") <- z
  out
}

#' @keywords internal
print.cnp_rci <- function(x, ...) {
  cat("<cnp_rci> Reliable Change Index model\n")
  if (!is.null(x$call)) cat("  call        : ", deparse(x$call), "\n", sep = "")
  cat("  reliability : ", x$reliability, "\n", sep = "")
  cat("  practice    : ", x$practice_method,
      if (!is.na(x$practice)) sprintf(" (k = %.3f)", x$practice) else "",
      "\n", sep = "")
  if (!is.na(x$sediff)) cat("  SEdiff      : ", round(x$sediff, 4), "\n", sep = "")
  cat("  norm n      : ", x$n, "\n", sep = "")
  invisible(x)
}

## =====================================================================
## 3. Package generic + back-compat shim (draft).
## =====================================================================

#' Interpret a new case with a fitted `cnp` normative model
#'
#' The third verb in the package workflow, after *estimate* (the constructor,
#' e.g. [rci()]) and *apply* ([predict()]). `predict()` returns the continuous
#' statistic; `classify()` turns it into a categorical verdict by comparing it
#' to a critical value. Kept separate from `predict()` so the raw statistic is
#' always available and so classification-specific choices (`alpha`, one- vs
#' two-sided, direction of "better") stay explicit.
#'
#' Method per model class: [classify.cnp_rci()] returns reliable decline / no
#' change / reliable improvement; a future `classify.cnp_adjscores()` would
#' return the Equivalent Score class.
#'
#' @param object A fitted `cnp` model.
#' @param ... Passed to methods.
#' @keywords internal
classify <- function(object, ...) UseMethod("classify")

#' Deprecated wrapper mirroring the original `change_func()`
#'
#' Maps the old method names onto [rci()] + [predict()]. Kept only to ease
#' migration; new code should call [rci()] directly.
#'
#' @param xt0_test,xt1_test Individual scores.
#' @param xt0,xt1 Normative sample vectors.
#' @param method One of `"RCI"`, `"mRCI"`, `"RCI_ICC"`, `"mRCI_ICC"`,
#'   `"CG2006reg"`.
#' @keywords internal
change_func <- function(xt0_test, xt1_test, xt0, xt1, method) {
  spec <- switch(as.character(method),
    RCI       = list("pearson",    "none"),
    mRCI      = list("pearson",    "mean"),
    RCI_ICC   = list("icc",        "none"),
    mRCI_ICC  = list("icc",        "mean"),
    CG2006reg = list("regression", "none"),
    stop("Unknown method: ", method, call. = FALSE)
  )
  fit <- rci(xt0, xt1, reliability = spec[[1]], practice = spec[[2]])
  predict(fit, xt0_test = xt0_test, xt1_test = xt1_test)
}
