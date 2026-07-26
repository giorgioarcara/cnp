# Clinical Neuropsychology Psychometrics (CNP) <img src="man/figures/logo.png" align="right" width="120" alt="cnp logo" />

`cnp` is an R package providing psychometric methods for normative data in clinical
neuropsychology. In its current version, it focuses on utility functions to build
regression-based norms and to test new normative methods.

## Features

- **Regression-based score adjustment** — two families of methods for adjusting raw
  test scores for demographic variables (age, education, sex):
  - `adjscores_A2024_v1/v2/v3()` — Arcara (2024) method
  - `adjscores_C1987_v1/v2()` — Capitani (1987) method
- **Equivalent Scores (ES) classification** — `ES()`, `tolLimits.obs()`,
  `tolLimits.adjscores()`, implementing the Equivalent Scores approach to
  score classification.
- **Data simulation** — `sim.norm.data()` and its `sample.*` helpers
  (`sample.coef.unif()`, `sample.demo.unif()`, `sample.demo.cond()`,
  `sample.transf()`) for generating synthetic normative data with a known
  ground-truth model, useful for testing and teaching these methods.
- **Supporting utilities** — common predictor transformations
  (`cube()`, `quadr()`, `logm100()`, `log10m100()`, `log10mAve()`, `inv()`,
  `poly2()`, `zero()`), plus helpers like `group_v()`, `progress_bar()`, and
  `shadenormal()` for plotting.

## Installation

The package is not yet on CRAN. Install the development version from GitHub:

```r
# install.packages("devtools")
pak::pak("giorgioarcara/cnp@v0.1")
```

## Usage

```r
library(cnp)

# simulate normative data with a known generative model
dat <- sim.norm.data(...)

# fit an adjustment model and compute adjusted scores
res <- adjscores_A2024_v1(df = dat, dep = "score", dep.range = c(0, 100),
                           age = "age", edu = "edu", sex = "sex")

# derive Equivalent Scores cut-offs
ES(...)
```

See the function help pages (`?adjscores_A2024_v1`, `?ES`, `?sim.norm.data`, ...)
for full argument details and examples.

## Status

This package is under active development on the `v0.1` branch. The API may
change between versions.

## References

- Arcara, G. (2024). *[reference for the Arcara adjustment method]*
- Capitani, E. (1987). *[reference for the Capitani adjustment method]*
- Aiello, E. N., & Depaoli, E. G. (2022). *[reference for the Equivalent Scores function]*

## License

MIT © Giorgio Arcara
