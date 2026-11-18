# =============================================================================
# 05_moderators.R — person-level moderators of coupling strength
# =============================================================================
# Three approaches, from what the draft currently reports to what reviewers will want:
#   (a) TWO-STAGE (as in the draft): correlate posterior-mean slopes with moderators,
#       now WITH 95% CIs (at N = 30, r = .30 has a CI that comfortably includes 0).
#   (b) TWO-STAGE, UNCERTAINTY-PROPAGATED: compute r within each posterior draw of the
#       person slopes, giving a distribution of r that carries slope uncertainty.
#   (c) ONE-STAGE CROSS-LEVEL INTERACTIONS (recommended for the paper): moderator x
#       within-person predictor inside the multilevel model. This is the defensible test
#       and answers the "fully joint model" limitation the draft currently concedes.
source("R/utils.R")
A <- readRDS(file.path(CONFIG$derived_dir, "analysis.rds")); d <- A$d; person <- A$person
wide  <- readRDS(file.path(CONFIG$derived_dir, "person_slopes_wide.rds"))
draws <- readRDS(file.path(CONFIG$derived_dir, "person_slope_draws.rds"))

MODS <- intersect(c("cssrs_life_sev", "cssrs_pm_sev", "sb_life", "sb_3mo", "connect", "lonely",
                    "dep", "anx", "inq_pb", "inq_tb", "fad", "cerq_maladaptive", "cerq_adaptive"), names(person))
MODS <- MODS[sapply(MODS, function(m) sum(!is.na(person[[m]])) >= 10)]
COUP <- intersect(c("si_na", "si_mal", "mal_na", "adp_na"), names(wide))
if (length(MODS) == 0) skip_script("No person-level moderators in data")
pw <- person |> select(pid, all_of(MODS)) |> inner_join(wide, by = "pid")


# ---- (a) two-stage with CIs -----------------------------------------------------------
# For ST couplings, also report the version EXCLUDING never-endorsers (their slopes are
# shrinkage-only and pull correlations toward whatever their moderator values are).
two_stage <- function(dat, tag) {
  expand.grid(coupling = COUP, moderator = MODS, stringsAsFactors = FALSE) |>
    rowwise() |>
    mutate(ct = list(suppressWarnings(cor.test(dat[[coupling]], dat[[moderator]]))),
           r = unname(ct$estimate), lo = ct$conf.int[1], hi = ct$conf.int[2], p = ct$p.value,
           n = sum(complete.cases(dat[[coupling]], dat[[moderator]])), sample = tag) |>
    select(-ct) |> ungroup()
}
ts_all <- two_stage(pw, "all")
ts_endorsers <- two_stage(pw |> filter(!pid %in% A$never_endorse), "SI endorsers only") |>
  filter(coupling %in% c("si_na", "si_mal"))
save_csv(bind_rows(ts_all, ts_endorsers) |> arrange(coupling, moderator, sample),
         "T4a_moderators_two_stage.csv")

# ---- (b) draw-wise correlations ---------------------------------------------------------
dw <- draws |> left_join(person |> select(pid, all_of(MODS)), by = "pid")
ts_draws <- lapply(MODS, function(m) {
  dw |> group_by(coupling, .draw) |>
    summarise(r = suppressWarnings(cor(slope, .data[[m]], use = "complete.obs")), .groups = "drop") |>
    group_by(coupling) |>
    summarise(moderator = m, r_med = median(r, na.rm = TRUE),
              r_lo = quantile(r, .025, na.rm = TRUE), r_hi = quantile(r, .975, na.rm = TRUE),
              .groups = "drop")
}) |> bind_rows()
save_csv(ts_draws, "T4b_moderators_drawwise.csv")

# ---- (c) one-stage cross-level interaction models -------------------------------------
# Focus on the moderator effects named in the abstract; add others as needed.
# Moderators are standardised (continuous) or 0/1 (behaviour history).
XLEVEL <- list(
  x_si_mal_sb3mo  = list(f = bf(si ~ mal_w * sb_3mo + mal_b + (1 + mal_w | pid)),
                         fam = bernoulli(), prior = PRIORS_ST, term = "mal_w:sb_3mo"),
  x_si_mal_sblife = list(f = bf(si ~ mal_w * sb_life + mal_b + (1 + mal_w | pid)),
                         fam = bernoulli(), prior = PRIORS_ST, term = "mal_w:sb_life"),
  x_si_na_sb3mo   = list(f = bf(si ~ neg_aff_w * sb_3mo + neg_aff_b + (1 + neg_aff_w | pid)),
                         fam = bernoulli(), prior = PRIORS_ST, term = "neg_aff_w:sb_3mo"),
  x_mal_na_lonely = list(f = bf(mal ~ neg_aff_w * lonely_z + neg_aff_b + (1 + neg_aff_w | pid)),
                         fam = gaussian(), prior = PRIORS_GAUSS, term = "neg_aff_w:lonely_z"),
  x_si_mal_lonely = list(f = bf(si ~ mal_w * lonely_z + mal_b + (1 + mal_w | pid)),
                         fam = bernoulli(), prior = PRIORS_ST, term = "mal_w:lonely_z"),
  x_si_mal_dep    = list(f = bf(si ~ mal_w * dep_z + mal_b + (1 + mal_w | pid)),
                         fam = bernoulli(), prior = PRIORS_ST, term = "mal_w:dep_z"),
  x_mal_na_connect = list(f = bf(mal ~ neg_aff_w * connect_z + neg_aff_b + (1 + neg_aff_w | pid)),
                          fam = gaussian(), prior = PRIORS_GAUSS, term = "neg_aff_w:connect_z")
)
res <- list(); diag <- list()
for (nm in names(XLEVEL)) {
  m <- XLEVEL[[nm]]
  if (!has_vars(d, all.vars(m$f$formula))) { message("Skipping ", nm); next }
  dd <- d[complete.cases(d[, all.vars(m$f$formula)]), ]
  message("Fitting ", nm)
  f <- fit_model(nm, m$f, dd, m$fam, m$prior)
  dr <- as_draws_df(f)[[paste0("b_", m$term)]]
  res[[nm]] <- data.frame(model = nm, term = m$term, b = median(dr),
                          lo = quantile(dr, .025), hi = quantile(dr, .975),
                          pd = max(mean(dr > 0), mean(dr < 0)), row.names = NULL)
  diag[[nm]] <- diagnose(f, nm)
}
if (length(res)) save_csv(bind_rows(res), "T4c_moderators_crosslevel.csv")
if (length(diag)) save_csv(bind_rows(diag), "S5_convergence_crosslevel.csv")
print(bind_rows(res), digits = 3)
