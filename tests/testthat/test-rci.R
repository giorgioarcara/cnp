# Tests for the RCI family (R/rci.R). See dev/DESIGN-change-methods.md.
#
# The core functions in section 1 are the readable spec, so they are tested
# against hand-computed values rather than against each other.

# --- fixtures ---------------------------------------------------------------

make_norms <- function(n = 200, seed = 1) {
  set.seed(seed)
  xt0 <- rnorm(n, mean = 10, sd = 3)
  xt1 <- xt0 + rnorm(n, mean = 0.5, sd = 2)  # +0.5 practice effect
  list(xt0 = xt0, xt1 = xt1)
}

# --- core: sediff_pearson -------------------------------------------------

test_that("sediff_pearson matches the closed-form formula", {
  d <- make_norms()
  r  <- cor(d$xt0, d$xt1)
  se <- sd(d$xt0) * sqrt(1 - r)
  expect_equal(sediff_pearson(d$xt0, d$xt1), sqrt(2 * se^2))
})

test_that("sediff_pearson errors on unequal lengths", {
  expect_error(sediff_pearson(1:5, 1:4))
})

# --- core: practice_mean ------------------------------------------------

test_that("practice_mean is the mean paired difference", {
  d <- make_norms()
  expect_equal(practice_mean(d$xt0, d$xt1), mean(d$xt1 - d$xt0))
  expect_gt(practice_mean(d$xt0, d$xt1), 0)  # we built in +0.5
})

# --- core: rci_z ------------------------------------------------------------

test_that("rci_z standardizes the (practice-corrected) change", {
  expect_equal(rci_z(10, 4, sediff = 2), -3)
  expect_equal(rci_z(10, 4, sediff = 2, practice = -1), -2.5)
  expect_equal(rci_z(c(10, 10), c(4, 16), sediff = 2), c(-3, 3))
})

# --- object: rci() + predict() --------------------------------------------

test_that("rci('pearson','none') reproduces the textbook RCI", {
  d <- make_norms()
  fit <- rci(d$xt0, d$xt1, reliability = "pearson", practice = "none")
  expect_s3_class(fit, "cnp_rci")
  expect_s3_class(fit, "cnp_normmodel")
  expect_equal(fit$practice, 0)
  expect_equal(
    predict(fit, xt0_test = 12, xt1_test = 8),
    (8 - 12) / sediff_pearson(d$xt0, d$xt1)
  )
})

test_that("rci('pearson','mean') subtracts the practice effect (mRCI)", {
  d <- make_norms()
  fit <- rci(d$xt0, d$xt1, reliability = "pearson", practice = "mean")
  k <- practice_mean(d$xt0, d$xt1)
  expect_equal(fit$practice, k)
  expect_equal(
    predict(fit, xt0_test = 12, xt1_test = 8),
    (8 - 12 - k) / sediff_pearson(d$xt0, d$xt1)
  )
})

test_that("predict() is vectorised over patients", {
  d <- make_norms()
  fit <- rci(d$xt0, d$xt1)
  out <- predict(fit, xt0_test = c(12, 10, 8), xt1_test = c(8, 10, 14))
  expect_length(out, 3)
  expect_lt(out[1], 0)
  expect_equal(out[2], 0)
  expect_gt(out[3], 0)
})

test_that("regression method: expected follow-up depends on baseline", {
  d <- make_norms()
  fit <- rci(d$xt0, d$xt1, reliability = "regression")
  expect_true(is.na(fit$sediff))
  expect_s3_class(fit$model, "lm")
  # two patients with identical raw change but different baselines get
  # different z (unlike the pearson/icc methods)
  z <- predict(fit, xt0_test = c(5, 15), xt1_test = c(5, 15) - 3)
  expect_false(isTRUE(all.equal(z[1], z[2])))
})

test_that("icc method is skipped when psych is absent", {
  skip_if_not_installed("psych")
  d <- make_norms()
  fit <- rci(d$xt0, d$xt1, reliability = "icc")
  expect_true(fit$sediff > 0)
})

# --- classify() -----------------------------------------------------------

test_that("classify() thresholds the change score at the critical value", {
  d <- make_norms()
  fit <- rci(d$xt0, d$xt1, reliability = "pearson", practice = "none")
  sediff <- fit$sediff
  crit <- qnorm(0.975)

  # construct a follow-up exactly past the decline threshold
  x0 <- 12
  x1_decline <- x0 - (crit + 0.01) * sediff
  x1_stable  <- x0
  x1_improve <- x0 + (crit + 0.01) * sediff

  cl <- classify(fit, xt0_test = rep(x0, 3),
                 xt1_test = c(x1_decline, x1_stable, x1_improve))
  expect_equal(as.character(cl),
               c("reliable decline", "no change", "reliable improvement"))
  expect_true(is.ordered(cl))
})

test_that("classify(higher_is_better = FALSE) flips decline/improvement", {
  d <- make_norms()
  fit <- rci(d$xt0, d$xt1)
  crit <- qnorm(0.975)
  x1 <- 12 - (crit + 0.01) * fit$sediff   # score went down
  expect_equal(as.character(classify(fit, 12, x1, higher_is_better = TRUE)),
               "reliable decline")
  expect_equal(as.character(classify(fit, 12, x1, higher_is_better = FALSE)),
               "reliable improvement")
})

# --- back-compat shim ---------------------------------------------------

test_that("change_func() maps old names onto rci() + predict()", {
  d <- make_norms()
  expect_equal(
    change_func(12, 8, d$xt0, d$xt1, method = "mRCI"),
    predict(rci(d$xt0, d$xt1, "pearson", "mean"), xt0_test = 12, xt1_test = 8)
  )
  expect_error(change_func(12, 8, d$xt0, d$xt1, method = "nope"))
})
