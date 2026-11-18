# =============================================================================
# 00_import_redcap.R — convert the REDCap "All EMAs" labelled export to ema_long.csv
# =============================================================================
# Input : data/raw/redcap_ema_labels.csv   (REDCap > Export > CSV, labels)
# Output: data/raw/from_redcap/ema_long.csv  (repo format, see data/README.md)
#         data/raw/from_redcap/id_key.csv    (pid <-> Inclivio ID; KEEP PRIVATE)
#         data/raw/from_redcap/import_log.csv
#
# Run:   Rscript R/00_import_redcap.R
# Then:  PRECISE_DATA=redcap Rscript run_all.R
#
# Structure of the export (checked below): one row per submitted survey. Ping 1 is the
# morning survey (SI item: "since last night"); pings 2–5 are daytime surveys (SI item:
# "in the past few hours"). Each block has its own copy of the affect, strategy and SI
# items, which are merged here. Strategy items are Yes/No.
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
source("config.R")

in_file <- file.path(CONFIG$raw_dir, "redcap_ema_labels.csv")
out_dir <- file.path(CONFIG$raw_dir, "from_redcap")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

x <- read.csv(in_file, check.names = FALSE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
names(x)[1] <- sub("^﻿", "", names(x)[1])

# ---- Column positions (REDCap label exports have duplicate names, so use positions) ------
# Each entry: position -> expected label prefix. The script stops if the export layout changes.
COLS <- list(
  record = c(1, "Record ID"), inclivio = c(6, "Participant's ID"), day = c(7, "Day number"),
  ping = c(8, "Ping Number"), resp_time = c(9, "Response time stamp"), complete = c(139, "Complete?"),
  # morning block
  m_anger = c(10, "Anger"), m_shame = c(11, "Shame"), m_sad = c(12, "Sad"), m_fear = c(13, "Fear"), m_guilt = c(14, "Guilt"),
  m_joy = c(15, "Joy"), m_love = c(16, "Love"), m_calm = c(17, "Calm"), m_attentive = c(18, "Attentive"), m_confident = c(19, "Confident"),
  m_accept = c(20, "You decided to accept"), m_fix = c(21, "You did something to fix"), m_rum = c(22, "You were unable to stop"),
  m_reapp = c(23, "You tried to think of a problem"), m_distract = c(24, "You tried to keep your mind"),
  m_avoid = c(25, "You tried not to think"), m_suppress = c(26, "You tried to control"), m_selfcrit = c(27, "You criticized"),
  m_si = c(35, "Have you thought about hurting or killing yourself since last night"),
  m_int_sh30 = c(36, "Within the past 30 minutes, what has been your intention to harm"),
  m_int_su30 = c(37, "Within the past 30 minutes, how strong"), m_int_now = c(38, "What is your intention to kill"),
  m_sh30 = c(39, "Have you attempted suicide"),
  # daytime block
  d_anger = c(78, "Anger"), d_shame = c(79, "Shame"), d_sad = c(80, "Sad"), d_fear = c(81, "Fear"), d_guilt = c(82, "Guilt"),
  d_joy = c(83, "Joy"), d_love = c(84, "Love"), d_calm = c(85, "Calm"), d_attentive = c(86, "Attentive"), d_confident = c(87, "Confident"),
  d_accept = c(88, "You decided to accept"), d_fix = c(89, "You did something to fix"), d_rum = c(90, "You were unable to stop"),
  d_reapp = c(91, "You tried to think of a problem"), d_distract = c(92, "You tried to keep your mind"),
  d_avoid = c(93, "You tried not to think"), d_suppress = c(94, "You tried to control"), d_selfcrit = c(95, "You criticized"),
  d_si = c(96, "Have you thought about hurting or killing yourself in the past few hours"),
  d_int_sh30 = c(97, "Within the past 30 minutes, what has been your intention to harm"),
  d_int_su30 = c(98, "Within the past 30 minutes, how strong"), d_int_now = c(99, "What is your intention to kill"),
  d_sh30 = c(100, "Have you attempted suicide")
)
bad <- Filter(function(cc) !startsWith(trimws(names(x)[as.integer(cc[1])]), cc[2]), COLS)
if (length(bad)) stop("REDCap export layout differs from expected at: ",
                      paste(sapply(bad, `[`, 1), collapse = ", "), ". Update COLS.")
r <- as.data.frame(lapply(COLS, function(cc) x[[as.integer(cc[1])]]), stringsAsFactors = FALSE)

yes  <- function(v) ifelse(is.na(v), NA_integer_, as.integer(grepl("^(1\\. )?Yes", v)))
intent <- function(v) {               # None 0 / A little 1 / Moderate 2 / A lot 3 / Severe 4
  out <- rep(NA_integer_, length(v))
  out[grepl("^None", v)] <- 0L; out[grepl("^A little", v)] <- 1L; out[grepl("^Moderate", v)] <- 2L
  out[grepl("^A lot", v)] <- 3L; out[grepl("^Severe", v)] <- 4L
  out
}
co <- function(stem, f = identity) { a <- f(r[[paste0("m_", stem)]]); b <- f(r[[paste0("d_", stem)]]); ifelse(is.na(a), b, a) }
num <- function(v) suppressWarnings(as.numeric(v))

# ---- Build one row per submitted survey ------------------------------------------------------
sv <- tibble(
  inclivio = r$inclivio, study_day = as.integer(r$day), beep = as.integer(r$ping),
  timestamp_utc = as.POSIXct(r$resp_time, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT"),
  redcap_complete = r$complete,
  survey_window = ifelse(!is.na(r$m_si) | (!is.na(r$ping) & r$ping == 1), "morning", "daytime"),
  na_angry = co("anger", num), na_ashamed = co("shame", num), na_sad = co("sad", num),
  na_afraid = co("fear", num), na_guilty = co("guilt", num),
  pa_joy = co("joy", num), pa_love = co("love", num), pa_calm = co("calm", num),
  pa_attentive = co("attentive", num), pa_confident = co("confident", num),
  # maladaptive (manuscript): rumination, distraction, avoidance, suppression, self-invalidation
  mal_rumination = co("rum", yes), mal_distraction = co("distract", yes), mal_avoidance = co("avoid", yes),
  mal_suppression = co("suppress", yes), mal_selfinvalidation = co("selfcrit", yes),
  # adaptive: reappraisal, acceptance, problem-solving
  adp_reappraisal = co("reapp", yes), adp_acceptance = co("accept", yes), adp_problemsolving = co("fix", yes),
  si = co("si", yes),                                   # "thought about hurting OR killing yourself"
  intent_selfharm_30m = co("int_sh30", intent), intent_suicide_30m = co("int_su30", intent),
  intent_suicide_now = co("int_now", intent), selfharm_30m = co("sh30", yes)
)

log <- tibble(step = character(), n = integer())
add_log <- function(step, n) log <<- bind_rows(log, tibble(step = step, n = n))
add_log("rows in REDCap export", nrow(sv))
unlinked <- is.na(sv$inclivio) | is.na(sv$study_day) | is.na(sv$beep)
add_log("rows without Inclivio ID / day / ping (dropped; cannot be assigned to a person)", sum(unlinked))
sv <- sv[!unlinked, ]
# Separate enrollment runs: a gap of > 10 days between consecutive surveys from the same
# Inclivio ID means the ID restarted the protocol (day numbers reset). By default only the
# first run is kept; set SECOND_RUNS=separate to keep later runs as their own participant.
sv <- sv |> arrange(inclivio, timestamp_utc) |> group_by(inclivio) |>
  mutate(gap_days = as.numeric(difftime(timestamp_utc, lag(timestamp_utc), units = "days")),
         run = 1L + cumsum(!is.na(gap_days) & gap_days > 10)) |> ungroup()
runs <- sv |> group_by(inclivio, run) |>
  summarise(n = n(), from = min(timestamp_utc), to = max(timestamp_utc), .groups = "drop")
multi <- runs |> group_by(inclivio) |> filter(n() > 1) |> ungroup()
add_log("Inclivio IDs with more than one enrollment run", n_distinct(multi$inclivio))
if (Sys.getenv("SECOND_RUNS", "drop") == "separate") {
  sv <- sv |> mutate(inclivio = ifelse(run > 1, paste0(inclivio, "_run", run), inclivio))
} else {
  add_log("surveys from 2nd+ runs (dropped)", sum(sv$run > 1))
  sv <- sv |> filter(run == 1)
}
readr::write_csv(multi |> mutate(inclivio = substr(inclivio, 1, 4)), file.path(out_dir, "multi_run_ids.csv"))

# Re-submissions of the same ping (e.g., an Incomplete survey then a Complete one minutes
# later): keep the one with the SI item answered, then the latest.
sv <- sv |> group_by(inclivio, study_day, beep) |>
  arrange(is.na(si), desc(timestamp_utc), .by_group = TRUE)
add_log("duplicate person-day-ping submissions within a run (dropped, best kept)", sum(duplicated(sv[, c("inclivio", "study_day", "beep")])))
sv <- sv |> slice(1) |> ungroup() |> select(-gap_days, -run)
add_log("rows with no SI answer (counted as not completed)", sum(is.na(sv$si)))

# ---- De-identified participant IDs, ordered by first response ----------------------------
key <- sv |> group_by(inclivio) |> summarise(first = min(timestamp_utc, na.rm = TRUE), .groups = "drop") |>
  arrange(first) |> mutate(pid = sprintf("P%02d", row_number())) |> select(pid, inclivio)
readr::write_csv(key, file.path(out_dir, "id_key.csv"))
sv <- sv |> left_join(key, by = "inclivio") |> select(-inclivio)

# ---- Full schedule grid (missed pings = completed 0) ---------------------------------------
# Planned schedule: 42 days x 5 pings (PLANNED_DAYS env var), extended if anyone has more days.
# People who stopped early contribute missed pings after their last survey; see the
# last-day column of import_log / compliance tables and decide how to describe dropout.
planned_days <- as.integer(Sys.getenv("PLANNED_DAYS", "42"))
grid <- sv |> group_by(pid) |> summarise(last_day = max(planned_days, max(study_day)), .groups = "drop") |>
  rowwise() |> do(expand.grid(pid = .$pid, study_day = seq_len(.$last_day), beep = 1:5, stringsAsFactors = FALSE)) |>
  ungroup()
ema <- grid |>
  left_join(sv, by = c("pid", "study_day", "beep")) |>
  mutate(completed = as.integer(!is.na(si)),
         prompt_type = "scheduled",
         timestamp = format(timestamp_utc, "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")) |>
  arrange(pid, study_day, beep) |>
  select(pid, timestamp, study_day, beep, prompt_type, completed, survey_window, si,
         starts_with("na_"), starts_with("pa_"), starts_with("mal_"), starts_with("adp_"),
         intent_selfharm_30m, intent_suicide_30m, intent_suicide_now, selfharm_30m)
# Missed pings have no timestamp; give them a nominal one so the lag code orders them correctly.
ema$timestamp[is.na(ema$timestamp)] <- format(as.POSIXct("2000-01-01", tz = "UTC") +
  (ema$study_day[is.na(ema$timestamp)] - 1) * 86400 + (9 + 3 * (ema$beep[is.na(ema$timestamp)] - 1)) * 3600,
  "%Y-%m-%d %H:%M:%S")
# Mark that timestamps for missed pings are nominal (they are never used in models: completed == 0).

add_log("people", n_distinct(ema$pid))
lastobs <- sv |> group_by(pid) |> summarise(last_day = max(study_day))
add_log("people whose last survey was before day 35", sum(lastobs$last_day < 35))
add_log("scheduled prompts in grid", nrow(ema))
add_log("completed (SI answered)", sum(ema$completed))
add_log("SI = yes", sum(ema$si, na.rm = TRUE))
readr::write_csv(ema, file.path(out_dir, "ema_long.csv"))
readr::write_csv(log, file.path(out_dir, "import_log.csv"))
print(log, n = Inf)
