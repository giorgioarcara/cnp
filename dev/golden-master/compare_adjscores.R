# Golden-master check: compare the current (in-development) package's
# adjscores_*() and ES()/tolLimits.*() functions against ANY committed
# reference point (tag, branch, or commit SHA), to catch behavior changes
# introduced by ongoing refactoring.
#
# NOT part of the installed package (see .Rbuildignore) - depends on git
# history a built/installed package doesn't have.
#
# Run with:
#   Rscript dev/golden-master/compare_adjscores.R [ref]
# e.g.
#   Rscript dev/golden-master/compare_adjscores.R manus_bugfix
#   Rscript dev/golden-master/compare_adjscores.R HEAD~5
#   Rscript dev/golden-master/compare_adjscores.R 77d0c03
# [ref] defaults to DEFAULT_REF below if omitted. It can also be set via the
# GOLDEN_MASTER_REF environment variable, or by pre-defining a REF variable
# before source()-ing this script from an R session.
# (working directory can be anywhere inside the repo)

# clean workspace
rm(list=ls())

suppressPackageStartupMessages({
  library(car)   # needed by the old sourced adjscores_C1987_*/adjscores_A2024_v3
  library(waldo)
})

# find the repo root via git so the script works from any cwd inside the repo
REPO_ROOT = suppressWarnings(system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE))
# load build_old_env()/path_exists_at_ref()/git_repo_root(), used below to pull the old code
source(file.path(REPO_ROOT, "dev", "golden-master", "helpers.R"))

# load the current in-development package - this is the "new" side of every comparison below
devtools::load_all(REPO_ROOT, quiet = TRUE)

DEFAULT_REF = "manus_bugfix" # fallback reference point when none is supplied

# resolve which committed reference to compare against: CLI arg > env var > default.
# Pre-defining REF before source()-ing this script (e.g. from an R session) wins over all of these.
if (!exists("REF", inherits = FALSE)) {
  cli_args = commandArgs(trailingOnly = TRUE)
  REF = if (length(cli_args) >= 1) cli_args[1] else Sys.getenv("GOLDEN_MASTER_REF", unset = DEFAULT_REF)
}

# fail fast with a clear message if REF doesn't resolve to a real commit
ref_check = suppressWarnings(system2("git", c("-C", REPO_ROOT, "rev-parse", "--verify", "--quiet", paste0(REF, "^{commit}")),
                                      stdout = TRUE, stderr = TRUE))
if (!is.null(attr(ref_check, "status")) && attr(ref_check, "status") != 0){
  stop("'", REF, "' does not resolve to a commit in this repo (tag, branch, or SHA expected).")
}

## ---- adjscores_*: old file(s)/name -> new exported function name ----------
#
# Before the "Reorganize R/ into topical grouped files" commit, each variant
# lived in its own file and all 3 A2024 files (resp. both C1987 files) defined
# an identically-named adjscores_A2024()/adjscores_C1987() function. After
# that commit, all 5 variants live together in R/regr_methods.R already under
# their final distinct names. REF can be on either side of that commit, so
# detect which layout applies there instead of assuming one.

adjscores_uses_new_layout = path_exists_at_ref(REF, "R/regr_methods.R", REPO_ROOT)

adjscores_spec_old = list(
  adjscores_A2024_v1 = list(old_name = "adjscores_A2024", files = c(
    "R/adjscores_A2024_v1.R", "R/formula_transf_text.R", "R/model_transf_text.R")),
  adjscores_A2024_v2 = list(old_name = "adjscores_A2024", files = c(
    "R/adjscores_A2024_v2.R", "R/formula_transf_text.R", "R/model_transf_text.R")),
  adjscores_A2024_v3 = list(old_name = "adjscores_A2024", files = c(
    "R/adjscores_A2024_v3.R", "R/formula_transf_text.R", "R/model_transf_text.R")),
  adjscores_C1987_v1 = list(old_name = "adjscores_C1987", files = c(
    "R/adjscores_C1987_v1.R", "R/formula_transf_text.R", "R/model_transf_text.R")),
  adjscores_C1987_v2 = list(old_name = "adjscores_C1987", files = c(
    "R/adjscores_C1987_v2.R", "R/formula_transf_text.R", "R/model_transf_text.R"))
)

# resolve the actual (name, files) spec to use for a given function at REF
get_adjscores_spec = function(fn_name){
  if (adjscores_uses_new_layout) {
    list(old_name = fn_name, files = "R/regr_methods.R")
  } else {
    adjscores_spec_old[[fn_name]]
  }
}

extract_adjscores = function(res){
  list(
    ADJ_SCORES = res$new.df$ADJ_SCORES,
    RESIDUALS  = res$new.df$RESIDUALS,
    coefs      = coef(res$lm.model),
    transfs    = res$transfs,
    model_text = res$model_text
  )
}

make_test_data = function(seed, sex_type = c("character", "numeric"), n = 150){
  sex_type = match.arg(sex_type)
  set.seed(seed)
  sex_raw = sample(c("F", "M"), n, replace = TRUE)
  data.frame(
    Dep = rnorm(n, 20, 3),
    Age = round(runif(n, 20, 85)),
    Education = round(runif(n, 5, 18)),
    Sex = if (sex_type == "numeric") ifelse(sex_raw == "F", 1, 0) else sex_raw
  )
}

run_adjscores_comparison = function(fn_name, seed, sex_type){
  spec = get_adjscores_spec(fn_name)
  df = make_test_data(seed, sex_type)

  old_env = build_old_env(REF, spec$files, REPO_ROOT)
  old_fn  = get(spec$old_name, envir = old_env)
  new_fn  = match.fun(fn_name)

  set.seed(seed); old_res = old_fn(df)
  set.seed(seed); new_res = new_fn(df)

  # field-by-field diff (tolerance-based); empty list means old and new match
  diff = waldo::compare(extract_adjscores(old_res), extract_adjscores(new_res), tolerance = 1e-8)
  list(label = fn_name, seed = seed, sex_type = sex_type, ok = length(diff) == 0, diff = diff)
}

# every (function, seed, sex-encoding) combination to test
scenarios = expand.grid(
  fn_name  = names(adjscores_spec_old),
  seed     = c(1, 2),
  sex_type = c("character", "numeric"),
  stringsAsFactors = FALSE
)

# run the comparison for each row of the scenario grid, one result list per row
results = Map(run_adjscores_comparison, scenarios$fn_name, scenarios$seed, scenarios$sex_type)

## ---- ES() / tolLimits.obs(): unchanged names, direct comparison -----------
# ES() was never renamed, but its file moved from R/ES.R + R/tolLimits.obs.R
# to R/equivalent_scores.R in the same reorg commit - detect which applies.

es_files = if (path_exists_at_ref(REF, "R/equivalent_scores.R", REPO_ROOT)) {
  "R/equivalent_scores.R"
} else {
  c("R/ES.R", "R/tolLimits.obs.R")
}
old_es_env = build_old_env(REF, es_files, REPO_ROOT)

run_es_comparison = function(n){
  old_fn = get("ES", envir = old_es_env)
  old_res = old_fn(n = n)
  new_res = ES(n = n)
  diff = waldo::compare(old_res, new_res, tolerance = 1e-8)
  list(label = paste0("ES(n=", n, ")"), seed = NA, sex_type = NA, ok = length(diff) == 0, diff = diff)
}

results = c(results, lapply(c(100, 200, 500), run_es_comparison)) # n < ~70 leaves oTL undefined, see tolLimits.obs()

## ---- report ----------------------------------------------------------------

cat("\n==== Golden-master comparison vs ref:", REF, "====\n\n")
for (r in results) {
  status = if (r$ok) "PASS" else "FAIL"
  extra = if (is.na(r$seed)) "" else sprintf(" seed=%d sex=%s", r$seed, r$sex_type)
  cat(sprintf("[%s] %-20s%s\n", status, r$label, extra))
  if (!r$ok) print(r$diff)
}

n_fail = sum(!vapply(results, `[[`, logical(1), "ok"))
cat("\n", length(results) - n_fail, "/", length(results), " scenarios passed.\n", sep = "")
if (n_fail > 0) stop(n_fail, " scenario(s) diverged from ref ", REF, " - see diffs above.")
