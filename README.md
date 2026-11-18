# Person-specific links between negative affect, coping, and suicidal ideation (PRECISE)

Data and analysis code for:

> Caudle, M., Moore, R. C., Depp, C. A., & Kuehn, K. S. (under review). *Person-specific links
> between negative affect, coping, and suicidal ideation.* Submitted to *Suicide and
> Life-Threatening Behavior*.

Thirty young adults (18–26) with active suicidal ideation completed ecological momentary
assessment (EMA) five times daily for six weeks during an open trial of PRECISE, a brief,
DBT-informed, EMA-guided intervention (ClinicalTrials.gov NCT07749391). Bayesian multilevel
models with person-varying slopes estimate within-person associations among negative affect,
maladaptive and adaptive emotion-regulation strategy use, and momentary suicidal ideation.

## Reproducing the results

```bash
# R >= 4.3 with a C++ toolchain for Stan (macOS: Xcode command-line tools; Windows: Rtools)
Rscript install.R        # brms, rstan, posterior, mclust, lavaan, dplyr, tidyr, readr, ggplot2
Rscript run_all.R        # full sampling (4 chains x 4,000 iterations); several hours
FAST=1 Rscript run_all.R # quick end-to-end check with short chains
PRECISE_SI=si_any Rscript run_all.R   # supplementary outcome: the screening item
```

Results are written to `output_public_intent_suicide_now/` (tables, figures, `sessionInfo.txt`).
Fitted models are cached in the output folder (`fits/`) and refit only when the formula, data, or priors change.
The published estimates were obtained with R 4.6.0, brms 2.23.0, rstan 2.32.7, and mclust 6.1.3.
Participant identifiers in the public files were randomly reassigned, which changes the order
of grouping levels in Stan; re-running therefore reproduces the published estimates up to
Monte Carlo error (typically in the second or third decimal), not digit for digit. The analysis
data themselves are identical to those used for the paper.

## Scripts

| Script | Produces | Manuscript |
|---|---|---|
| `R/02_prepare_data.R` | compliance, ICCs, centered/lagged/time variables (`S1`, `S2`) | Method; Results: sample |
| `R/03_core_models.R` | `T3`, `T3b` | Table 3; Results |
| `R/04_person_slopes.R` | `S4` person-specific slopes | Results; figures |
| `R/05_moderators.R` | `T4a` two-stage r, `T4b` draw-wise r, `T4c` cross-level interactions | Results: moderators |
| `R/06_lpa.R` | `S6` latent profile BIC table | Results: LPA |
| `R/07_sensitivity.R` | `T5` sensitivity and joint models, `T6` multilevel mediation | Results |
| `R/08_treatment_models.R` | `T7`–`T11`, `T13` change over treatment, compliance | Results: change over treatment |
| `R/09_lagged_models.R` | `T12` lagged models | Results: temporal precedence |
| `R/10_figures.R` | Figures 1–2 and S1–S3 | Figures |
| `R/11_reliability.R` | `S9` multilevel omega for negative affect | Method: Measures |
| `R/00_import_*.R` | private files from the REDCap exports (study team only) | Method: data processing |

## Model specification

Within-person predictors are person-mean centered (`*_w`); person means are grand-mean centered
(`*_b`). Each core model has a random intercept and a random slope for the within-person predictor:

```r
si  ~ neg_aff_w + neg_aff_b + (1 + neg_aff_w | pid)   # Bernoulli
si  ~ mal_w     + mal_b     + (1 + mal_w     | pid)   # Bernoulli
mal ~ neg_aff_w + neg_aff_b + (1 + neg_aff_w | pid)   # Gaussian
adp ~ neg_aff_w + neg_aff_b + (1 + neg_aff_w | pid)   # Gaussian
```

Bernoulli models use Normal(−3, 3) intercept, Normal(0, 1) slope, half-Student-t(3, 0, 1) SD,
and LKJ(2) correlation priors; Gaussian models use brms defaults.

## Data and privacy

`data/public/` contains the minimal de-identified data needed to reproduce the analyses; see
`data/README.md` for the codebook and the de-identification steps. Raw exports, response times,
demographics, and the identifier key are not shared. Please do not attempt to re-identify
participants or link these data to other sources.

## License

Code: MIT (see `LICENSE`).
