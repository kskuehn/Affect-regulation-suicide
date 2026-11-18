# =============================================================================
# 08_treatment_models.R — taking seriously that the EMA was collected DURING treatment
# =============================================================================
# Why this matters: in a 6-week open trial, negative affect, maladaptive coping and SI
# are all expected to change over time. Person-mean centring removes stable differences
# but NOT shared within-person trends. If NA and SI both drift down over treatment, part of
# the "within-person coupling" is just two declining time series (Curran & Bauer, 2011;
# Wang & Maxwell, 2015). The coupling may also *itself* change across treatment — which
# is the more interesting, mechanism-relevant question for PRECISE.
#
#  T1  Detrended core models: add study week (fixed + random) to each core model.
#  T1b Same, using predictors centred on each person's own linear trend (robustness).
#  T2  Does coupling change over treatment?  x_w * week
#  T3  Does coupling change with coaching-session dose? x_w * sessions_done (+ week)
#  T4  Does early coupling predict clinical change? (exploratory, two-stage, N = 30)
#  T5  EMA compliance over treatment and whether missingness tracks prior distress.
source("R/utils.R")
A <- readRDS(file.path(CONFIG$derived_dir, "analysis.rds")); d <- A$d
dm <- d |> filter(!is.na(mal_w))
HAS_MAL <- has_vars(d, "mal_w")
HAS_SESSIONS <- nrow(A$sessions) > 0
# Focal ST coupling for T3/T4: maladaptive -> ST when strategy items exist, otherwise NA -> ST.
FOCAL_X <- if (HAS_MAL) "mal_w" else "neg_aff_w"
FOCAL_T2 <- if (HAS_MAL) "t2_si_mal" else "t2_si_na"
FOCAL_B <- if (HAS_MAL) "mal_b" else "neg_aff_b"
dfocal <- if (HAS_MAL) dm else d

# ---- T1: detrended ----------------------------------------------------------------------
T1 <- list(
  t1_si_na  = list(f = bf(si ~ neg_aff_w + neg_aff_b + week + (1 + neg_aff_w + week | pid)), fam = bernoulli(), pr = PRIORS_ST, pred = "neg_aff_w", lab = "ST ~ NA", dat = d),
  t1_si_mal = list(f = bf(si ~ mal_w + mal_b + week + (1 + mal_w + week | pid)), fam = bernoulli(), pr = PRIORS_ST, pred = "mal_w", lab = "ST ~ Mal", dat = dm),
  t1_mal_na = list(f = bf(mal ~ neg_aff_w + neg_aff_b + week + (1 + neg_aff_w + week | pid)), fam = gaussian(), pr = PRIORS_GAUSS, pred = "neg_aff_w", lab = "Mal ~ NA", dat = dm),
  t1_adp_na = list(f = bf(adp ~ neg_aff_w + neg_aff_b + week + (1 + neg_aff_w + week | pid)), fam = gaussian(), pr = PRIORS_GAUSS, pred = "neg_aff_w", lab = "Adp ~ NA", dat = d |> filter(!is.na(adp)))
)
core <- readr::read_csv(file.path(CONFIG$tables_dir, "T3_core_coupling_models.csv"), show_col_types = FALSE)
t1 <- list(); trend <- list(); diag <- list()
for (nm in names(T1)) {
  m <- T1[[nm]]
  if (!has_vars(d, all.vars(m$f$formula))) { message("Skipping ", nm); next }
  message("Fitting ", nm)
  f <- fit_model(nm, m$f, m$dat, m$fam, m$pr)
  t1[[nm]] <- coupling_summary(f, paste(m$lab, "| + week"), m$pred, m$fam$family == "bernoulli")
  trend[[nm]] <- coupling_summary(f, paste(m$lab, "| week trend"), "week", m$fam$family == "bernoulli")
  diag[[nm]] <- diagnose(f, nm)
}
save_csv(bind_rows(core, bind_rows(t1)) |> arrange(predictor, model), "T7a_detrended_vs_core.csv")
save_csv(bind_rows(trend), "T7b_time_trends.csv")

# T1b: person-specific linear detrending of the predictor (no week term)
xdt <- if (HAS_MAL) "mal_wdt" else "neg_aff_wdt"
t1b <- fit_model(paste0("t1b_si_", xdt), bf(as.formula(sprintf("si ~ %s + %s + (1 + %s | pid)", xdt, FOCAL_B, xdt))),
                 dfocal, bernoulli(), PRIORS_ST)
save_csv(coupling_summary(t1b, paste("ST ~", xdt, "| person-detrended predictor"), xdt, TRUE), "T7c_detrended_predictor.csv")

# ---- T2: coupling x time -------------------------------------------------------------------
T2 <- list(
  t2_si_na  = list(f = bf(si ~ neg_aff_w * week + neg_aff_b + (1 + neg_aff_w + week | pid)), fam = bernoulli(), pr = PRIORS_ST, x = "neg_aff_w", dat = d),
  t2_si_mal = list(f = bf(si ~ mal_w * week + mal_b + (1 + mal_w + week | pid)), fam = bernoulli(), pr = PRIORS_ST, x = "mal_w", dat = dm),
  t2_mal_na = list(f = bf(mal ~ neg_aff_w * week + neg_aff_b + (1 + neg_aff_w + week | pid)), fam = gaussian(), pr = PRIORS_GAUSS, x = "neg_aff_w", dat = dm)
)
t2 <- list(); t2_curves <- list()
for (nm in names(T2)) {
  m <- T2[[nm]]
  if (!has_vars(d, all.vars(m$f$formula))) { message("Skipping ", nm); next }
  message("Fitting ", nm)
  f <- fit_model(nm, m$f, m$dat, m$fam, m$pr)
  dr <- as_draws_df(f)
  b0 <- dr[[paste0("b_", m$x)]]; bt <- dr[[paste0("b_", m$x, ":week")]]
  q <- function(x) quantile(x, c(.5, .025, .975), names = FALSE)
  t2[[nm]] <- data.frame(model = nm, coupling_week0 = q(b0)[1], c0_lo = q(b0)[2], c0_hi = q(b0)[3],
                         change_per_week = q(bt)[1], cpw_lo = q(bt)[2], cpw_hi = q(bt)[3],
                         pd_change = max(mean(bt > 0), mean(bt < 0)),
                         coupling_week6 = q(b0 + 6 * bt)[1], c6_lo = q(b0 + 6 * bt)[2], c6_hi = q(b0 + 6 * bt)[3])
  t2_curves[[nm]] <- lapply(seq(0, 6, by = .25), function(w) {
    v <- b0 + w * bt; data.frame(model = nm, week = w, est = median(v), lo = quantile(v, .025), hi = quantile(v, .975))
  }) |> bind_rows()
  diag[[nm]] <- diagnose(f, nm)
  if (nm == FOCAL_T2) {
    # person-specific coupling at the start of treatment ("early coupling") for T4
    early <- person_slope_summary(f, FOCAL_X) |> rename(early_coupling = est)
  }
}
save_csv(bind_rows(t2), "T8_coupling_change_over_treatment.csv")
save_csv(bind_rows(t2_curves), "T8b_coupling_curves.csv")

# ---- T3: coaching-session dose ---------------------------------------------------------------
# sessions_done is collinear with week; including both identifies the session effect from
# variation in WHEN people had sessions. Exploratory; interpret with care.
if (HAS_SESSIONS) {
  t3 <- fit_model(paste0("t3_si_", FOCAL_X, "_sessions"),
                  bf(as.formula(sprintf("si ~ %1$s * sessions_done + %1$s * week + %2$s + (1 + %1$s + week | pid)", FOCAL_X, FOCAL_B))),
                  dfocal, bernoulli(), PRIORS_ST)
  dr <- as_draws_df(t3)
  terms <- c(paste0(FOCAL_X, ":sessions_done"), paste0(FOCAL_X, ":week"), "sessions_done")
  save_csv(data.frame(term = terms,
                      t(sapply(paste0("b_", terms), function(v) quantile(dr[[v]], c(.5, .025, .975)))),
                      row.names = NULL, check.names = FALSE),
           "T9_session_dose.csv")
  diag$t3 <- diagnose(t3, "t3_sessions")
} else message("No session data: T3 skipped")

# ---- T4: early coupling -> clinical change (exploratory) -------------------------------------
# Outcomes: (i) change in EMA SI rate (last 2 wks minus first 2 wks);
#           (ii) post-treatment C-SSRS past-month severity, adjusting for baseline (if available).
si_change <- d |>
  mutate(window = case_when(study_day <= CONFIG$early_window_days ~ "early",
                            study_day > max(study_day) - CONFIG$late_window_days ~ "late")) |>
  filter(!is.na(window)) |>
  group_by(pid, window) |> summarise(rate = mean(si), .groups = "drop") |>
  pivot_wider(names_from = window, values_from = rate) |>
  mutate(si_rate_change = late - early)
p4 <- A$person |> left_join(early, by = "pid") |> left_join(si_change, by = "pid") |>
  filter(!pid %in% A$never_endorse)          # early SI coupling is uninformed for never-endorsers
out4 <- list(
  broom_like = function(fit, label) {
    s <- summary(fit)$coefficients; ci <- confint(fit)
    data.frame(outcome = label, term = rownames(s), b = s[, 1], lo = ci[, 1], hi = ci[, 2], p = s[, 4], n = nobs(fit), row.names = NULL)
  }
)
res4 <- list(out4$broom_like(lm(si_rate_change ~ early_coupling + early, data = p4),
                             paste("EMA SI-rate change | early", FOCAL_X, "coupling")))
# clinical outcomes at 6 weeks (_post) and 12 weeks (_fu), each adjusted for its baseline
for (v in c("cssrs_pm_sev", "dep", "anx", "inq_pb", "inq_tb", "lonely")) for (w in c("_post", "_fu")) {
  y <- paste0(v, w)
  if (all(c(y, v) %in% names(p4)) && sum(complete.cases(p4[, c(y, v, "early_coupling")])) >= 10)
    res4[[length(res4) + 1]] <- out4$broom_like(lm(as.formula(paste(y, "~ early_coupling +", v)), data = p4),
                                                paste0(y, " (adj. baseline)"))
}
save_csv(bind_rows(res4), "T10_early_coupling_predicts_outcome.csv")

# ---- Pre/post change in clinical measures (context for the coupling-over-time results) --------
pp <- list()
for (v in c("cssrs_pm_sev", "dep", "anx", "inq_pb", "inq_tb", "lonely", "connect")) for (w in c("_post", "_fu")) {
  y <- paste0(v, w)
  if (!all(c(v, y) %in% names(A$person))) next
  z <- A$person[complete.cases(A$person[, c(v, y)]), ]
  if (nrow(z) < 5) next
  dlt <- z[[y]] - z[[v]]; tt <- t.test(dlt)
  pp[[length(pp) + 1]] <- data.frame(measure = v, wave = sub("_", "", w), n = nrow(z),
                                     baseline_m = mean(z[[v]]), wave_m = mean(z[[y]]), change = mean(dlt),
                                     lo = tt$conf.int[1], hi = tt$conf.int[2],
                                     d_z = mean(dlt) / sd(dlt), d_av = mean(dlt) / mean(c(sd(z[[v]]), sd(z[[y]]))))
}
if (length(pp)) save_csv(bind_rows(pp), "T13_prepost_change.csv")

# ---- T5: compliance over treatment & missingness ----------------------------------------------
src <- CONFIG$src_dir
sched <- read_ema(src) |>
  filter(prompt_type == "scheduled") |>
  mutate(week = (study_day - 1) / 7) |>
  arrange(pid, study_day, beep) |>
  left_join(d |> select(pid, study_day, beep, neg_aff_w), by = c("pid", "study_day", "beep")) |>
  group_by(pid) |>
  # most recent completed prompt's NA (carried forward within day) as predictor of completing the next one
  mutate(prev_na_w = lag(neg_aff_w), same_day = study_day == lag(study_day)) |>
  tidyr::fill(prev_na_w, .direction = "down") |>
  ungroup() |>
  mutate(prev_na_w10 = ifelse(same_day, prev_na_w / 10, NA))
t5 <- fit_model("t5_compliance", bf(completed ~ week + prev_na_w10 + (1 + week | pid)),
                sched |> filter(!is.na(prev_na_w10)), bernoulli())
save_csv(bind_rows(coupling_summary(t5, "Completion ~ week", "week", TRUE),
                   data.frame(model = "Completion ~ prior NA (per 10)", predictor = "prev_na_w10",
                              b = median(as_draws_df(t5)$b_prev_na_w10),
                              b_lo = quantile(as_draws_df(t5)$b_prev_na_w10, .025),
                              b_hi = quantile(as_draws_df(t5)$b_prev_na_w10, .975), row.names = NULL)),
         "T11_compliance_over_treatment.csv")
diag$t5 <- diagnose(t5, "t5_compliance")

save_csv(bind_rows(diag), "S7_convergence_treatment.csv")
print(bind_rows(t2), digits = 3)
