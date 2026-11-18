# =============================================================================
# 09_lagged_models.R — temporal precedence
# =============================================================================
# The core models are CONCURRENT: SI ("since the last prompt") and NA ("past 10 minutes")
# are reported at the same prompt, and SI's reference window precedes NA's. So "NA predicts
# SI" in the draft is a statement of association, not prediction. These lagged models ask
# whether NA / maladaptive coping at t-1 predicts SI at t, controlling for SI at t-1
# (same day, <= CONFIG$max_lag_hours apart).
source("R/utils.R")
A <- readRDS(file.path(CONFIG$derived_dir, "analysis.rds")); d <- A$d

dl_na  <- d |> filter(!is.na(si_lag), !is.na(neg_aff_w_lag))
dl_mal <- d |> filter(!is.na(si_lag), !is.na(mal_w_lag))

l1 <- fit_model("l1_si_na_lag",  bf(si ~ neg_aff_w_lag + si_lag + neg_aff_b + week + (1 + neg_aff_w_lag | pid)),
                dl_na, bernoulli(), PRIORS_ST)
HAS_MAL <- has_vars(d, "mal_w")
if (HAS_MAL) {
l2 <- fit_model("l2_si_mal_lag", bf(si ~ mal_w_lag + si_lag + mal_b + week + (1 + mal_w_lag | pid)),
                dl_mal, bernoulli(), PRIORS_ST)
# reverse direction: does SI at t-1 predict maladaptive coping at t (beyond mal at t-1)?
d_rev <- d |> group_by(pid) |> arrange(timestamp, .by_group = TRUE) |>
  mutate(mal_lag = ifelse(lag_ok, lag(mal), NA)) |> ungroup() |>
  filter(!is.na(si_lag), !is.na(mal_lag), !is.na(mal))
l3 <- fit_model("l3_mal_si_lag", bf(mal ~ si_lag + mal_lag + week + (1 + si_lag | pid)), d_rev, gaussian(), PRIORS_GAUSS)
}
# reverse direction for NA: does SI at t-1 predict NA at t (beyond NA at t-1)?
d_rev_na <- d |> group_by(pid) |> arrange(timestamp, .by_group = TRUE) |>
  mutate(na_lag = ifelse(lag_ok, lag(neg_aff), NA)) |> ungroup() |>
  filter(!is.na(si_lag), !is.na(na_lag))
l4 <- fit_model("l4_na_si_lag", bf(neg_aff ~ si_lag + na_lag + week + (1 + si_lag | pid)), d_rev_na, gaussian(), PRIORS_GAUSS)

lag_tab <- bind_rows(
  coupling_summary(l1, "SI_t ~ NA_(t-1)", "neg_aff_w_lag", TRUE) |> mutate(n_pairs = nrow(dl_na)),
  coupling_summary(l4, "NA_t ~ SI_(t-1)", "si_lag", FALSE) |> mutate(n_pairs = nrow(d_rev_na)))
if (HAS_MAL) lag_tab <- bind_rows(lag_tab,
  coupling_summary(l2, "SI_t ~ Mal_(t-1)", "mal_w_lag", TRUE) |> mutate(n_pairs = nrow(dl_mal)),
  coupling_summary(l3, "Mal_t ~ SI_(t-1)", "si_lag", FALSE) |> mutate(n_pairs = nrow(d_rev))
)
save_csv(lag_tab, "T12_lagged_models.csv")
save_csv(bind_rows(diagnose(l1, "l1"), diagnose(l4, "l4"),
                   if (HAS_MAL) bind_rows(diagnose(l2, "l2"), diagnose(l3, "l3"))), "S8_convergence_lagged.csv")
print(lag_tab, digits = 3)
