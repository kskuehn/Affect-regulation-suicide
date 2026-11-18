# =============================================================================
# 03_core_models.R — the four coupling models reported in the manuscript (Table 3)
# =============================================================================
# Each: random intercept + random (person-varying) slope for the within-person predictor,
# with the between-person (person-mean) component as a fixed covariate.
source("R/utils.R")
A <- readRDS(file.path(CONFIG$derived_dir, "analysis.rds")); d <- A$d

CORE <- list(
  m1_si_na = list(f = bf(si ~ neg_aff_w + neg_aff_b + (1 + neg_aff_w | pid)),
                  family = bernoulli(), prior = PRIORS_ST, pred = "neg_aff_w", exp = TRUE,
                  label = "ST ~ Negative Affect"),
  m2_si_mal = list(f = bf(si ~ mal_w + mal_b + (1 + mal_w | pid)),
                   family = bernoulli(), prior = PRIORS_ST, pred = "mal_w", exp = TRUE,
                   label = "ST ~ Maladaptive Strategy Use"),
  m3_mal_na = list(f = bf(mal ~ neg_aff_w + neg_aff_b + (1 + neg_aff_w | pid)),
                   family = gaussian(), prior = PRIORS_GAUSS, pred = "neg_aff_w", exp = FALSE,
                   label = "Maladaptive Use ~ Negative Affect"),
  m4_adp_na = list(f = bf(adp ~ neg_aff_w + neg_aff_b + (1 + neg_aff_w | pid)),
                   family = gaussian(), prior = PRIORS_GAUSS, pred = "neg_aff_w", exp = FALSE,
                   label = "Adaptive Use ~ Negative Affect")
)

fits <- list(); summ <- list(); diag <- list()
for (nm in names(CORE)) {
  m <- CORE[[nm]]
  if (!has_vars(d, all.vars(m$f$formula))) { message("Skipping ", nm, " (variables not in data)"); next }
  dd <- d[complete.cases(d[, all.vars(m$f$formula)]), ]
  message("Fitting ", nm, " (n = ", nrow(dd), ")")
  fits[[nm]] <- fit_model(nm, m$f, dd, m$family, m$prior)
  summ[[nm]] <- coupling_summary(fits[[nm]], m$label, m$pred, exponentiate = m$exp)
  diag[[nm]] <- diagnose(fits[[nm]], nm)
}

table3 <- bind_rows(summ)
save_csv(table3, "T3_core_coupling_models.csv")
save_csv(bind_rows(diag), "S3_convergence_core.csv")

# Same ST ~ NA model with NA per 10 points: same model on a more interpretable scale (the N(0,1) prior is slightly less informative per unit).
# (b = 0.10 per 1 slider point implies OR ~ 2.7 per 10 points — say which in the paper.)
m1_10 <- fit_model("m1_si_na_per10", bf(si ~ neg_aff_w10 + neg_aff_b10 + (1 + neg_aff_w10 | pid)),
                   d, bernoulli(), PRIORS_ST)
save_csv(coupling_summary(m1_10, "ST ~ Negative Affect (per 10 pts)", "neg_aff_w10", TRUE),
         "T3b_si_na_per10points.csv")

print(table3, digits = 3)
