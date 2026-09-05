# Design note — change-detection methods (RCI family) in `cnp`

Status: **draft for discussion**. Nothing here is implemented in the package yet
except the scaffold in `R/rci.R` (marked `@keywords internal`, not exported).

Source material being folded in:
`2026-ICC-RCI/R_functions/change_functions.R` — `RCI`, `mRCI`, `RCI_ICC`,
`mRCI_ICC`, `CG2006reg`, and the `change_func()` wrapper.

---

## 1. Goal

Two priorities, in order:

1. A **flexible** method layer that others can read, modify, and extend, with
   **transparent** computational code.
2. **Best-practice** R package design (idiomatic S3, `testthat`, roxygen, no
   `eval(parse(text=))`).

## 2. The recurring shape: *estimate* → *apply* → *interpret*

Every method in this package (regression adjustment, Equivalent Scores, and now
RCI) has the same structure:

| verb | R form | RCI example | regression-adjustment example |
|------|--------|-------------|-------------------------------|
| **estimate** from normative data | constructor `rci()` / `adjscores()` | SEdiff, reliability, practice effect `k` | transformations, `lm` coefficients, `dep.range` |
| **apply** to a new case (continuous) | `predict()` | standardized change score `Z` (or `t`) | adjusted score for a new patient |
| **interpret** as a verdict (categorical) | `classify()` | reliable decline / no change / reliable improvement | Equivalent Score class 0–4 |

The first two are exactly the `fit()` / `predict()` split. R's idiomatic
expression of it is an **S3 "model object" + a `predict()` method**, the same as
`lm()`, `glm()`, `prcomp()`. Adopt that here and keep it consistent across the
package.

### Why `classify()` is a third, separate verb

`predict()` returns the **continuous statistic** — a `Z` for the Pearson/ICC
methods, a `t` (df = n−2) for the regression method. That number is what you need
for plotting, meta-analysis, or applying a non-standard cut-off, so `predict()`
must always hand it back unmodified.

`classify()` takes that number and turns it into a **categorical clinical
verdict** by comparing it to a critical value (±1.96 for α = .05 two-sided;
±*t*(n−2) for the regression method). This is the same relationship as
`predict(glm, type = "response")` (a probability) versus thresholding it to a
0/1 label — or as the existing `ES()` turning a continuous adjusted score into an
ES class.

Keeping it separate rather than adding a `type =` argument to `predict()` is a
transparency/flexibility choice (priority 1): classification bundles in extra
decisions that deserve their own explicit arguments —

- `alpha` — significance level;
- one-sided vs two-sided threshold;
- `higher_is_better` — does an *increase* mean the patient improved, or (for
  error counts, reaction times, …) got *worse*? This flips which tail is
  "decline".

`classify()` is intended to be **one generic across the whole package**, declared
once (`classify <- function(object, ...) UseMethod("classify")`) with a method
per model class: `classify.cnp_rci()` → decline/stable/improvement;
`classify.cnp_adjscores()` → ES class 0–4 (wrapping the current `ES()` logic).
Shared plumbing lives on the parent class `cnp_normmodel`.

It is **optional**: a user who only wants the change score never calls it. The
exact return shape is an open question (§9) — current draft: an ordered factor
`c("reliable decline", "no change", "reliable improvement")` carrying the numeric
score as an attribute. Name alternatives considered: `interpret()`, `decision()`
— `classify()` chosen.

## 3. Two layers

### Layer 1 — computational core (pure functions)

Small exported functions, each doing **one** calculation: plain numbers/vectors
in, plain values out. No objects, no side effects. These functions *are* the
readable specification of each method and are the natural unit for `testthat`.

```
sediff_pearson(xt0, xt1)          -> numeric(1)
sediff_icc(xt0, xt1)              -> numeric(1)      # psych in Suggests
practice_mean(xt0, xt1)           -> numeric(1)
rci_z(xt0_test, xt1_test, sediff, practice = 0) -> numeric
cg_prediction(model, xt0_test)    -> list(fit, se)  # Crawford & Garthwaite 2006
```

### Layer 2 — object + methods (S3), a thin wrapper

```
rci(xt0, xt1, reliability = c("pearson","icc","regression"),
              practice    = c("none","mean"))   -> object of class c("cnp_rci","cnp_normmodel")

predict(object, xt0_test, xt1_test)             -> standardized change score(s)
print(object) / summary(object) / plot(object)
classify(object, xt0_test, xt1_test, ...)       -> "reliable decline"/"no change"/"reliable improvement"
```

## 4. The five source functions are a 2-D grid

| method (old name) | `reliability` | `practice` | notes |
|-------------------|---------------|------------|-------|
| `RCI`             | `"pearson"`   | `"none"`   | Jacobson & Truax 1991 |
| `mRCI`            | `"pearson"`   | `"mean"`   | Chelune et al. 1993 |
| `RCI_ICC`         | `"icc"`       | `"none"`   | ICC(2) instead of Pearson r |
| `mRCI_ICC`        | `"icc"`       | `"mean"`   | |
| `CG2006reg`       | `"regression"`| implied by the regression | Crawford & Garthwaite 2006 — expected follow-up **and** SE depend on `xt0_test`, so this is a separate branch in both `rci()` and `predict()` |

Four functions → two orthogonal arguments. One genuine special case (regression).

### Why `reliability = "regression"` cannot be flattened into the same code path

For Pearson/ICC, `SEdiff` is a single scalar estimated from the norms and reused
for every patient. For the Crawford–Garthwaite regression method, both the
expected follow-up score and its standard error are functions of the patient's
own baseline `xt0_test`:

```
y_hat  = b0 + b1 * xt0_test
se_pred = s_e * sqrt(1 + 1/n + (xt0_test - mean(xt0))^2 / Sxx)
t_obs  = (xt1_test - y_hat) / se_pred        # df = n - 2, t-distributed
```

So the fitted object stores the `lm` model, and `predict.cnp_rci()` has an
`if (object$reliability == "regression")` branch. Everything stays inspectable
(the stored object is a plain list; the `lm` is a normal `lm`).

## 5. S3 mechanics (brief, for reference)

- A class is just a `class` attribute on a list:
  `structure(list(...), class = c("cnp_rci", "cnp_normmodel"))`.
- Constructor / validator / helper split (Wickham, *Advanced R* ch. 13):
  - `new_rci()` — internal, fast, just stamps the class.
  - `validate_rci()` — optional, checks invariants.
  - `rci()` — user-facing, does `match.arg()`, calls core fns, then `new_rci()`.
- `predict`, `print`, `summary`, `plot` are **existing generics**; define
  `predict.cnp_rci` etc. and mark each with `#' @export` — roxygen writes
  `S3method(predict, cnp_rci)` into `NAMESPACE`.
- `classify()` is a **new generic owned by this package** — declare it once
  (`classify <- function(object, ...) UseMethod("classify")`) and give both
  `cnp_rci` and (later) `cnp_adjscores` a method. Put shared behaviour on the
  parent class `cnp_normmodel`.
- **Do not** use S4 (ceremony, no payoff here) or R6 (mutable reference
  semantics — wrong for an immutable fitted model). Every base-R model object is
  S3; `broom`/`marginaleffects`/`predict` all expect S3.

## 6. Extensibility for third parties

Preferred: `match.arg()` + good docs. Adding a method = one core function + one
`switch` branch + one doc paragraph. Fully greppable.

Only if out-of-package method registration is genuinely needed later:

```r
.rci_methods <- new.env(parent = emptyenv())
register_rci_method <- function(name, fit_fun) assign(name, fit_fun, .rci_methods)
```

Defer this until someone asks for it — indirection costs transparency.

## 7. Dependencies

- `psych` → `Suggests`. `sediff_icc()` guards with
  `if (!requireNamespace("psych", quietly = TRUE)) stop(...)`.
- Crawford–Garthwaite: either vendor `CGcut.off.bv` into `R/` (document its
  source), **or** depend on CRAN **`singcar`** (Rittmo & McIntosh), which
  implements these single-case tests and would remove vendored code. Decision
  pending — `singcar` adds a dependency but is well-tested and maintained.

## 8. Consistency back-port (bigger, later job)

`adjscores_*()` currently returns a bare list and has **no `predict()`** — you
can fit a norm model but cannot apply it to a fresh patient without re-running on
the whole sample. Same object pattern fixes it:

- `adjscores(df, dep, age, edu, sex, method = "A2024", select = "aic")`
  → `c("cnp_adjscores", "cnp_normmodel")`; the `_v1/_v2/_v3` / `_C1987_v1/v2`
  suffixes become `method` + `select` arguments.
- `predict.cnp_adjscores(object, newdata)` → adjusted scores for new rows.
- `classify.cnp_adjscores(object, newdata)` → ES class, via `es_cutoffs(object)`.

Do this behind `dev/golden-master/compare_adjscores.R`.

## 9. Open questions

1. `classify()` output — factor with ordered levels? include the numeric `Z`
   and a p-value as attributes?
2. `predict()` return — bare numeric `Z`, or a data.frame (`z`, `p`, `ci_lo`,
   `ci_hi`, `class`)? Leaning data.frame for the regression method (it has a
   real p-value / df), bare numeric for the others, but consistency argues for
   always returning the same shape.
3. Directionality: does a higher score mean *better* or *worse*? ES code has
   `lower_tail`; RCI needs the analogous switch for `classify()`.
4. One-sided vs two-sided thresholds; default alpha (1.645 vs 1.96).
5. Keep `change_func(method = "mRCI")` as a thin back-compat shim mapping old
   names → new `rci()` calls?
