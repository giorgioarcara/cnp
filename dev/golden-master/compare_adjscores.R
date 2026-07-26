# Golden-master check: compare the current (in-development) package's
# adjscores_*() and ES()/tolLimits.*() functions against the last
# known-good tagged version (manus_bugfix), to catch behavior changes
# introduced by ongoing refactoring.
#
# NOT part of the installed package (see .Rbuildignore) - depends on git
# history a built/installed package doesn't have.
#
# Run with:
#   Rscript dev/golden-master/compare_adjscores.R
# (working directory can be anywhere inside the repo)

suppressPackageStartupMessages({
  library(car)   # needed by the old sourced adjscores_C1987_*/adjscores_A2024_v3
  library(waldo)
})

REPO_ROOT = suppressWarnings(system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE))
source(file.path(REPO_ROOT, "dev", "golden-master", "helpers.R"))

devtools::load_all(REPO_ROOT, quiet = TRUE)

TAG = "manus_bugfix" # last tag with verified-correct (bug-fixed) behavior

## ---- adjscores_*: old file(s)/name -> new exported function name ----------

adjscores_spec = list(
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
  spec = adjscores_spec[[fn_name]]
  df = make_test_data(seed, sex_type)

  old_env = build_old_env(TAG, spec$files, REPO_ROOT)
  old_fn  = get(spec$old_name, envir = old_env)
  new_fn  = match.fun(fn_name)

  set.seed(seed); old_res = old_fn(df)
  set.seed(seed); new_res = new_fn(df)

  diff = waldo::compare(extract_adjscores(old_res), extract_adjscores(new_res), tolerance = 1e-8)
  list(label = fn_name, seed = seed, sex_type = sex_type, ok = length(diff) == 0, diff = diff)
}

scenarios = expand.grid(
  fn_name  = names(adjscores_spec),
  seed     = c(1, 2),
  sex_type = c("character", "numeric"),
  stringsAsFactors = FALSE
)

results = Map(run_adjscores_comparison, scenarios$fn_name, scenarios$seed, scenarios$sex_type)

## ---- ES() / tolLimits.obs(): unchanged names, direct comparison -----------

old_es_env = build_old_env(TAG, c("R/ES.R", "R/tolLimits.obs.R"), REPO_ROOT)

run_es_comparison = function(n){
  old_fn = get("ES", envir = old_es_env)
  old_res = old_fn(n = n)
  new_res = ES(n = n)
  diff = waldo::compare(old_res, new_res, tolerance = 1e-8)
  list(label = paste0("ES(n=", n, ")"), seed = NA, sex_type = NA, ok = length(diff) == 0, diff = diff)
}

results = c(results, lapply(c(100, 200, 500), run_es_comparison)) # n < ~70 leaves oTL undefined, see tolLimits.obs()

## ---- report ----------------------------------------------------------------

cat("\n==== Golden-master comparison vs tag:", TAG, "====\n\n")
for (r in results) {
  status = if (r$ok) "PASS" else "FAIL"
  extra = if (is.na(r$seed)) "" else sprintf(" seed=%d sex=%s", r$seed, r$sex_type)
  cat(sprintf("[%s] %-20s%s\n", status, r$label, extra))
  if (!r$ok) print(r$diff)
}

n_fail = sum(!vapply(results, `[[`, logical(1), "ok"))
cat("\n", length(results) - n_fail, "/", length(results), " scenarios passed.\n", sep = "")
if (n_fail > 0) stop(n_fail, " scenario(s) diverged from tag ", TAG, " - see diffs above.")
