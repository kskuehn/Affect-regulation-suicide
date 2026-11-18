# =============================================================================
# 07_sensitivity.R — robustness of the ST couplings
# =============================================================================
#  S1  ST ~ NA excluding the highest endorser            (reported in draft, Figure 3)
#  S2  ST ~ Maladaptive excluding the highest endorser   (NOT in draft — it is the headline
#                                                          effect, so it needs the same check)
#  S3  Joint model: ST ~ NA_w + Mal_w                    (unique contributions; is maladaptive
#                                                          coping anything more than a proxy for NA?)
#  S4  1-1-1 multilevel mediation, NA -> Mal -> ST       (the title says "pathways"; this is
#                                                          the model that actually tests one)
source("R/utils.R")
A <- readRDS(file.path(CONFIG$derived_dir, "analysis.rds")); d <- A$d
d_ex <- d |> filter(pid != A$top_endorser)

s1 <- fit_model("s1_si_na_excl_top", bf(si ~ neg_aff_w + neg_aff_b + (1 + neg_aff_w | pid)),
                d_ex, bernoulli(), PRIORS_ST)
HAS_MAL <- has_vars(d, "mal_w")
if (HAS_MAL) {
s2 <- fit_model("s2_si_mal_excl_top", bf(si ~ mal_w + mal_b + (1 + mal_w | pid)),
                d_ex |> filter(!is.na(mal_w)), bernoulli(), PRIORS_ST)
s3 <- fit_model("s3_si_joint", bf(si ~ neg_aff_w + mal_w + neg_aff_b + mal_b + (1 + neg_aff_w + mal_w | pid)),
                d |> filter(!is.na(mal_w)), bernoulli(), PRIORS_ST)
}

m1 <- readRDS(file.path(CONFIG$fits_dir, "m1_si_na.rds"))
sens <- bind_rows(
  coupling_summary(m1, "ST ~ NA | full sample", "neg_aff_w", TRUE),
  coupling_summary(s1, paste0("ST ~ NA | excl. PID ", A$top_endorser), "neg_aff_w", TRUE))
if (HAS_MAL) sens <- bind_rows(sens,
  coupling_summary(readRDS(file.path(CONFIG$fits_dir, "m2_si_mal.rds")), "ST ~ Mal | full sample", "mal_w", TRUE),
  coupling_summary(s2, paste0("ST ~ Mal | excl. PID ", A$top_endorser), "mal_w", TRUE),
  coupling_summary(s3, "ST ~ NA + Mal (joint) | NA", "neg_aff_w", TRUE),
  coupling_summary(s3, "ST ~ NA + Mal (joint) | Mal", "mal_w", TRUE)
)
save_csv(sens, "T5_sensitivity.csv")
print(sens, digits = 3)
if (!HAS_MAL) skip_script("No strategy items: mediation model skipped")

# ---- S4: multilevel mediation (Bayesian, multivariate brms) -------------------------
# a-path: mal ~ NA_w ; b-path: si ~ mal_w + NA_w ; person-varying a and b correlated
# through the shared |p| ID so the average indirect effect is E[a_i * b_i] = a*b + cov(a_i, b_i)
# (Bauer, Preacher & Gil, 2006). Indirect effect on the log-odds scale.
dm <- d |> filter(!is.na(mal_w))
f_a <- bf(mal ~ neg_aff_w + neg_aff_b + (1 + neg_aff_w | p | pid)) + gaussian()
f_b <- bf(si ~ mal_w + neg_aff_w + mal_b + neg_aff_b + (1 + mal_w | p | pid)) + bernoulli()
pri_med <- c(prior(normal(-3, 3), class = "Intercept", resp = "si"),
             prior(normal(0, 1), class = "b", resp = "si"),
             prior(student_t(3, 0, 1), class = "sd", resp = "si"))
s4 <- fit_model("s4_mediation", f_a + f_b + set_rescor(FALSE), dm, family = NULL, prior = pri_med)
dr <- as_draws_df(s4)
a <- dr$b_mal_neg_aff_w; b <- dr$b_si_mal_w
cov_ab <- dr$`cor_pid__mal_neg_aff_w__si_mal_w` * dr$sd_pid__mal_neg_aff_w * dr$sd_pid__si_mal_w
ind <- a * b + cov_ab
q <- function(x) quantile(x, c(.5, .025, .975))
save_csv(data.frame(quantity = c("a (NA->Mal)", "b (Mal->ST | NA)", "cov(a_i,b_i)", "avg indirect (log-odds)"),
                    rbind(q(a), q(b), q(cov_ab), q(ind)), check.names = FALSE),
         "T6_multilevel_mediation.csv")
