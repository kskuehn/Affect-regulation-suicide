# =============================================================================
# utils.R — shared helpers
# =============================================================================
suppressPackageStartupMessages({
  library(brms)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(posterior)
})
source("config.R")
options(mc.cores = CONFIG$cores, brms.backend = "rstan")

# --- Priors -------------------------------------------------------------------
# ST (Bernoulli) models: exactly as described in the Analytic Plan.
PRIORS_ST <- c(
  prior(normal(-3, 3), class = "Intercept"),
  prior(normal(0, 1), class = "b"),
  prior(student_t(3, 0, 1), class = "sd"),
  prior(lkj(2), class = "cor")
)
# Gaussian (strategy-use) models: the manuscript does not state priors for these.
# brms defaults are used unless you set them here — REPORT whichever you use.
PRIORS_GAUSS <- NULL

# --- Fitting with on-disk caching ---------------------------------------------
# brms' `file` argument re-uses a saved fit if the formula/data/priors are unchanged,
# so re-running the pipeline is cheap after the first pass.
fit_model <- function(name, formula, data, family, prior = NULL, ...) {
  invisible(gc())
  brm(
    formula = formula, data = data, family = family, prior = prior,
    chains = CONFIG$chains, iter = CONFIG$iter, warmup = CONFIG$warmup,
    cores = CONFIG$cores, seed = CONFIG$seed,
    control = list(adapt_delta = CONFIG$adapt_delta),
    file = file.path(CONFIG$fits_dir, name), file_refit = "on_change",
    refresh = 0, silent = 2, ...
  )
}

# --- Convergence check (R-hat / ESS) -------------------------------------------
diagnose <- function(fit, name) {
  s <- summarise_draws(as_draws_df(fit), "rhat", "ess_bulk", "ess_tail")
  s <- s[!grepl("^(r_|z_|L_|lp__|lprior)", s$variable), ]
  data.frame(
    model = name,
    max_rhat = max(s$rhat, na.rm = TRUE),
    min_ess_bulk = min(s$ess_bulk, na.rm = TRUE),
    min_ess_tail = min(s$ess_tail, na.rm = TRUE),
    divergences = sum(nuts_params(fit)$Value[nuts_params(fit)$Parameter == "divergent__"])
  )
}

# --- Fixed effect + random-slope SD summary for a within-person predictor ------
coupling_summary <- function(fit, name, pred, exponentiate = FALSE) {
  d <- as_draws_df(fit)
  b  <- d[[paste0("b_", pred)]]
  sd <- d[[paste0("sd_pid__", pred)]]
  q <- function(x) quantile(x, c(.025, .975), names = FALSE)
  out <- data.frame(
    model = name, predictor = pred,
    b = median(b), b_lo = q(b)[1], b_hi = q(b)[2],
    pd = max(mean(b > 0), mean(b < 0)),          # probability of direction
    sd = median(sd), sd_lo = q(sd)[1], sd_hi = q(sd)[2],
    # share of people whose true slope is > 0, implied by the population distribution
    prop_pos = mean(pnorm(0, b, sd, lower.tail = FALSE))
  )
  if (exponentiate) {
    out$OR <- exp(out$b); out$OR_lo <- exp(out$b_lo); out$OR_hi <- exp(out$b_hi)
  }
  out
}

# --- Person-specific slopes as full posterior draws ------------------------------
# Returns long draws (pid x draw), so downstream analyses can propagate uncertainty.
person_slope_draws <- function(fit, pred, ndraws = 1000) {
  arr <- coef(fit, summary = FALSE, ndraws = ndraws)$pid[, , pred]  # draws x pid
  as.data.frame(arr) |>
    mutate(.draw = row_number()) |>
    pivot_longer(-.draw, names_to = "pid", values_to = "slope")
}

person_slope_summary <- function(fit, pred) {
  cf <- coef(fit)$pid[, , pred]
  data.frame(pid = rownames(cf), est = cf[, "Estimate"],
             lo = cf[, "Q2.5"], hi = cf[, "Q97.5"], row.names = NULL)
}

# --- Read EMA in either format -------------------------------------------------
# (1) repo format (see data/README.md), or
# (2) the PRECISE Inclivio export: ema_id, study_day, ping, completed, anger, shame, sad,
#     fear, guilt, si_any, intent_*, selfharm_30m. That export has no clock times, so a
#     nominal timestamp is built from day/ping (pings ~3 h apart); lagged models then pair
#     consecutive pings within a day only.
read_ema <- function(src) {
  ema <- readr::read_csv(file.path(src, "ema_long.csv"), show_col_types = FALSE) |>
    mutate(across(any_of(c("pid", "ema_id")), as.character))
  if ("ema_id" %in% names(ema)) {
    ema <- ema |>
      rename(pid = ema_id, beep = ping, si = si_any,
             na_angry = anger, na_ashamed = shame, na_sad = sad, na_afraid = fear, na_guilty = guilt) |>
      mutate(prompt_type = "scheduled",
             timestamp = format(as.POSIXct("2000-01-01 00:00:00", tz = "UTC") +
                                  (study_day - 1) * 86400 + (9 + 3 * (beep - 1)) * 3600,
                                "%Y-%m-%d %H:%M:%S"))
  }
  # Which SI definition to analyse (PRECISE_SI env var). The intent_* items are only asked
  # when the SI screener = 1, so "> 0" on them is a stricter, nested definition.
  si_def <- CONFIG$si_definition
  if (si_def != "si_any") {
    item <- ema[[si_def]]
    if (is.null(item)) {
      stop("SI definition '", si_def, "' needs a column of that name in the data")
    } else {
      ema$si <- ifelse(is.na(ema$si), NA, as.integer(!is.na(item) & item > 0))
    }
  }
  ema
}

# Does the analysis data contain usable values for these variables?
has_vars <- function(d, vars) all(vars %in% names(d)) && all(sapply(vars, function(v) any(!is.na(d[[v]]))))

# Stop the current script cleanly (run_all.R catches this and moves on).
skip_script <- function(msg) {
  stop(structure(class = c("skip_script", "error", "condition"), list(message = msg, call = NULL)))
}

save_csv <- function(x, file) {
  readr::write_csv(x, file.path(CONFIG$tables_dir, file))
  invisible(x)
}

theme_paper <- function() {
  theme_minimal(base_size = 11, base_family = "serif") +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.title.position = "plot")
}
