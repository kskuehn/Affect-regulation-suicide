# =============================================================================
# 02_prepare_data.R — clean, score, centre, add treatment-time variables
# =============================================================================
# Input  (data/public, or private data/raw/...): ema_long.csv, person.csv, sessions.csv
# Output (data/derived):               analysis.rds, descriptives tables
source("R/utils.R")

src <- CONFIG$src_dir
message("Data source: ", CONFIG$data_source, " (", src, ")")
ema <- read_ema(src)            # handles both the repo format and the Inclivio export (see utils.R)
opt_csv <- function(f) {
  p <- file.path(src, f)
  if (file.exists(p)) readr::read_csv(p, show_col_types = FALSE, col_types = readr::cols(pid = "c")) else NULL
}
person   <- opt_csv("person.csv")
sessions <- opt_csv("sessions.csv")
if (is.null(person)) {
  message("No person.csv: moderator and outcome analyses will be skipped.")
  person <- tibble(pid = unique(ema$pid))
}
if (is.null(sessions)) {
  message("No sessions.csv: session-dose analyses will be skipped.")
  sessions <- tibble(pid = character(), session_number = integer(), session_day = integer())
}

NA_ITEMS  <- c("na_sad", "na_angry", "na_afraid", "na_ashamed", "na_guilty")
MAL_ITEMS <- c("mal_rumination", "mal_distraction", "mal_avoidance", "mal_suppression", "mal_selfinvalidation")
ADP_ITEMS <- c("adp_reappraisal", "adp_acceptance", "adp_problemsolving")
for (v in c(MAL_ITEMS, ADP_ITEMS)) if (!v %in% names(ema)) ema[[v]] <- NA_real_
if (all(is.na(ema$mal_rumination))) message("No strategy items: maladaptive/adaptive models will be skipped.")

# ---- Compliance / sample description (report these in Method) --------------------
sched <- ema |> filter(prompt_type == "scheduled")
compliance <- sched |>
  group_by(pid) |>
  summarise(n_scheduled = n(), n_completed = sum(completed == 1),
            compliance = n_completed / n_scheduled,
            si_rate = mean(si, na.rm = TRUE), n_si = sum(si == 1, na.rm = TRUE), .groups = "drop")
save_csv(compliance, "S1_compliance_by_person.csv")
save_csv(tibble(
  n_people = n_distinct(sched$pid),
  scheduled = nrow(sched),
  completed = sum(sched$completed == 1),
  compliance_mean = mean(compliance$compliance),
  compliance_sd = sd(compliance$compliance),
  compliance_min = min(compliance$compliance),
  compliance_max = max(compliance$compliance),
  si_rate_overall = mean(sched$si, na.rm = TRUE),
  n_never_endorse = sum(compliance$n_si == 0),
  max_person_si_rate = max(compliance$si_rate),
  event_contingent_surveys = sum(ema$prompt_type == "event")
), "S1_sample_summary.csv")

strat_score <- function(df) {
  m <- rowMeans(df, na.rm = TRUE)
  if (Sys.getenv("STRATEGY_SCORE", "sum") == "sum") m * ncol(df) else m   # prorated sum if an item is missing
}

# ---- Analysis sample: completed, scheduled prompts only -----------------------------
# Event-contingent surveys are triggered BY suicidal thoughts, so including them would
# inflate SI and its associations. They are excluded here (analyse separately if wanted).
d <- sched |>
  filter(completed == 1) |>
  mutate(neg_aff = rowMeans(across(all_of(NA_ITEMS)), na.rm = TRUE),
         # Strategy subscales: SUM of items (with Yes/No items = number of strategies used,
         # 0-5 maladaptive, 0-3 adaptive), which reproduces the manuscript's coefficient
         # scale. Set STRATEGY_SCORE=mean for the item mean (proportion for binary items).
         mal     = strat_score(across(all_of(MAL_ITEMS))),
         adp     = strat_score(across(all_of(ADP_ITEMS))),
         across(c(neg_aff, mal, adp), ~ ifelse(is.nan(.x), NA_real_, .x))) |>
  filter(!is.na(si), !is.na(neg_aff))

# ---- Person-mean centring (Curran & Bauer, 2011) -------------------------------------
d <- d |>
  group_by(pid) |>
  mutate(across(c(neg_aff, mal, adp), list(pm = ~ mean(.x, na.rm = TRUE)))) |>
  ungroup() |>
  mutate(neg_aff_w = neg_aff - neg_aff_pm,
         mal_w     = mal - mal_pm,
         adp_w     = adp - adp_pm,
         # grand-mean centre the between-person components
         neg_aff_b = neg_aff_pm - mean(neg_aff_pm),
         mal_b     = mal_pm - mean(mal_pm),
         adp_b     = adp_pm - mean(adp_pm),
         # rescaled NA: per 10 points on the 0–100 slider (easier to interpret ORs)
         neg_aff_w10 = neg_aff_w / 10, neg_aff_b10 = neg_aff_b / 10)

# ---- Treatment time ------------------------------------------------------------------
# weeks since EMA start (0 = first day). Used for detrending and coupling x time.
d <- d |> mutate(week = (study_day - 1) / 7)

# Number of coaching sessions completed BEFORE each prompt (time-varying dose).
# If you have session date-times, swap study_day for timestamp comparisons.
d$sessions_done <- mapply(function(p, day) sum(sessions$pid == p & sessions$session_day < day),
                          d$pid, d$study_day)

# Within-person components net of each person's linear trend over treatment.
# (Alternative to adding `week` as a covariate; used in a robustness check.)
detrend <- function(y, t) if (sum(!is.na(y)) > 3) as.numeric(y - fitted(lm(y ~ t, na.action = na.exclude))) else y - mean(y, na.rm = TRUE)
d <- d |>
  group_by(pid) |>
  mutate(neg_aff_wdt = detrend(neg_aff, week),
         mal_wdt     = detrend(mal, week)) |>
  ungroup()

# ---- Lags (same day, <= max_lag_hours apart) ----------------------------------------
# Private files carry response times; the public files carry instead each completed prompt's
# rank in time order (time_order) and a precomputed flag (lag_ok = previous completed prompt was
# on the same day and <= max_lag_hours earlier), so no clock times need to be shared.
if ("timestamp" %in% names(d)) {
  d <- d |>
    mutate(timestamp = as.POSIXct(timestamp, tz = "America/Los_Angeles")) |>
    arrange(pid, timestamp) |>
    group_by(pid) |>
    mutate(gap_h = as.numeric(difftime(timestamp, lag(timestamp), units = "hours")),
           lag_ok = !is.na(gap_h) & study_day == lag(study_day) & gap_h <= CONFIG$max_lag_hours) |>
    ungroup()
} else {
  d <- d |> arrange(pid, time_order) |> mutate(lag_ok = !is.na(lag_ok) & lag_ok == 1)
}
d <- d |>
  group_by(pid) |>
  mutate(si_lag        = ifelse(lag_ok, lag(si), NA),
         neg_aff_w_lag = ifelse(lag_ok, lag(neg_aff_w), NA),
         mal_w_lag     = ifelse(lag_ok, lag(mal_w), NA)) |>
  ungroup()

# ---- Person-level moderators (standardised) --------------------------------------------
person <- person |>
  mutate(across(any_of(c("cssrs_life_sev", "cssrs_pm_sev", "connect", "lonely", "dep", "anx", "inq_pb", "inq_tb")),
                ~ as.numeric(scale(.x)), .names = "{.col}_z"))
d <- d |> left_join(person, by = "pid")

# Highest endorser (used in sensitivity analyses)
top_endorser <- compliance$pid[which.max(compliance$si_rate)]
never_endorse <- compliance$pid[compliance$n_si == 0]

saveRDS(list(d = d, person = person, sessions = sessions, compliance = compliance,
             top_endorser = top_endorser, never_endorse = never_endorse),
        file.path(CONFIG$derived_dir, "analysis.rds"))

# Within- vs between-person variance (ICC-type decomposition) — useful for Method/Results
icc <- function(x, g) { v_b <- var(tapply(x, g, mean, na.rm = TRUE)); v_b / (v_b + mean(tapply(x, g, var, na.rm = TRUE))) }
sicc <- function(x, g) if (all(is.na(x))) NA_real_ else icc(x, g)
save_csv(tibble(variable = c("neg_aff", "mal", "adp", "si"),
                icc = c(icc(d$neg_aff, d$pid), sicc(d$mal, d$pid), sicc(d$adp, d$pid), icc(d$si, d$pid)),
                within_sd = c(sd(d$neg_aff_w), suppressWarnings(sd(d$mal_w, na.rm = TRUE)), suppressWarnings(sd(d$adp_w, na.rm = TRUE)), NA)),
         "S2_icc_within_sd.csv")

message(sprintf("Analysis sample: %d prompts from %d people | top endorser = %s | never endorse = %d",
                nrow(d), n_distinct(d$pid), top_endorser, length(never_endorse)))
