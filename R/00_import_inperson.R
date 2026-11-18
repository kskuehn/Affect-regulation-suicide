# =============================================================================
# 00_import_inperson.R — score the REDCap in-person assessments and build person.csv
# =============================================================================
# Input : data/raw/redcap_inperson_labels.csv   (REDCap labelled export; events Baseline,
#                                                6-Week, 12-Week; record IDs 201...)
#         data/raw/record_inclivio_key.tsv       study team's Record ID <-> Inclivio ID list
#         data/raw/from_redcap/id_key.csv        written by R/00_import_redcap.R (run that first)
# Output: data/raw/from_redcap/person.csv        (one row per EMA participant, pid = P01...)
#         data/raw/inperson_scored.csv           (all records x events, scored)
#
# Instruments found in the export (identified from item wording — confirm against protocol):
#   PROMIS Depression item bank (28 items) and Anxiety item bank (29 items), raw sums 1-5/item
#   INQ-15: perceived burdensomeness (items 1-6), thwarted belongingness (7-15; 7,8,10,13,14,15 reversed)
#   ACSS-FAD, 7 items (fearlessness about death; items 2, 3, 5 reversed)
#   CERQ, 36 items (9 subscales; items k, k+9, k+18, k+27)
#   Coping Self-Efficacy Scale (26 items, 0-10)
#   BIS/BAS (20 items)
#   NIH Toolbox Friendship (8 items) -> "connectedness";  NIH Toolbox Loneliness (5 items)
# NOT in this export: C-SSRS (ideation severity, suicidal behavior history). Add it as
# data/raw/cssrs.csv (record_id, cssrs_life_sev, cssrs_pm_sev, sb_life, sb_3mo) and it is merged.
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
source("config.R")

x <- read.csv(file.path(CONFIG$raw_dir, "redcap_inperson_labels.csv"), check.names = FALSE,
              na.strings = c("", "NA"), stringsAsFactors = FALSE)
names(x)[1] <- sub("^﻿", "", names(x)[1])

# ---- layout check (positions; label exports repeat column names) ----------------------
expect <- list(c(1, "Record ID"), c(2, "Event Name"), c(5, "I felt worthless"), c(32, "I felt emotionally exhausted"),
               c(35, "I felt fearful"), c(63, "I had difficulty calming down"), c(66, "1.  These days, the people"),
               c(80, "15. These days"), c(84, "The fact that I am going to die"), c(90, "I am not at all afraid"),
               c(93, "I feel that I am the one to blame"), c(128, "I feel that basically the cause lies with others"),
               c(131, "Keep from getting down"), c(156, "Resist the impulse"), c(159, "1. Even if something bad"),
               c(178, "20. I worry about making"), c(181, "1. I get invited"), c(189, "Score"),
               c(192, "1. I feel alone"), c(197, "Score"))
bad <- Filter(function(e) !startsWith(trimws(names(x)[as.integer(e[1])]), e[2]), expect)
if (length(bad)) stop("In-person export layout differs at columns: ", paste(sapply(bad, `[`, 1), collapse = ", "))

lik <- function(v, levels) { out <- match(trimws(v), levels); out }
num_lead <- function(v) suppressWarnings(as.numeric(sub("^\\s*(\\d+).*", "\\1", v)))
FREQ5   <- c("Never", "Rarely", "Sometimes", "Often", "Always")
# CERQ: the export labels are Almost Never / Sometimes / Often / Frequently / Almost Always.
# The ORDER of "Often" vs "Frequently" is assumed here — confirm in the REDCap data dictionary.
CERQ5   <- c("Almost Never", "Sometimes", "Often", "Frequently", "Almost Always")
AGREE4  <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
items <- function(cols, f, ...) sapply(cols, function(i) f(x[[i]], ...))
psum  <- function(m) ifelse(rowSums(!is.na(m)) == 0, NA, rowMeans(m, na.rm = TRUE) * ncol(m))   # prorated sum

dep  <- items(5:32,  lik, FREQ5)
anx  <- items(35:63, lik, FREQ5)
inq  <- items(66:80, num_lead)
inq_rev <- c(7, 8, 10, 13, 14, 15); inq[, inq_rev] <- 8 - inq[, inq_rev]
fad  <- items(84:90, num_lead); fad[, c(2, 3, 5)] <- 4 - fad[, c(2, 3, 5)]
cerq <- items(93:128, lik, CERQ5)
cerq_sub <- function(k) psum(cerq[, c(k, k + 9, k + 18, k + 27)])
cses <- items(131:156, num_lead)

sc <- tibble(
  record_id = x[[1]], event = x[[2]],
  dep = psum(dep), anx = psum(anx),
  inq_pb = psum(inq[, 1:6]), inq_tb = psum(inq[, 7:15]),
  fad = psum(fad),
  cerq_selfblame = cerq_sub(1), cerq_acceptance = cerq_sub(2), cerq_rumination = cerq_sub(3),
  cerq_posrefocus = cerq_sub(4), cerq_planning = cerq_sub(5), cerq_reappraisal = cerq_sub(6),
  cerq_perspective = cerq_sub(7), cerq_catastroph = cerq_sub(8), cerq_otherblame = cerq_sub(9),
  cses = psum(cses),
  connect = suppressWarnings(as.numeric(x[[189]])),    # NIH Toolbox Friendship raw score
  lonely  = suppressWarnings(as.numeric(x[[197]]))     # NIH Toolbox Loneliness raw score
) |>
  mutate(cerq_maladaptive = cerq_selfblame + cerq_rumination + cerq_catastroph + cerq_otherblame,
         cerq_adaptive = cerq_acceptance + cerq_posrefocus + cerq_planning + cerq_reappraisal + cerq_perspective)
readr::write_csv(sc, file.path(CONFIG$raw_dir, "inperson_scored.csv"))

# ---- wide person file: baseline predictors + 6/12-week outcomes ----------------------------
ev <- c(Baseline = "", `6-Week` = "_post", `12-Week` = "_fu")
wide <- sc |> filter(event %in% names(ev)) |> mutate(suffix = ev[event]) |>
  select(-event) |>
  pivot_longer(-c(record_id, suffix)) |>
  mutate(name = paste0(name, suffix)) |> select(-suffix) |>
  pivot_wider(names_from = name, values_from = value)

cssrs_file <- file.path(CONFIG$raw_dir, "cssrs.csv")
if (file.exists(cssrs_file)) {
  wide <- wide |> left_join(readr::read_csv(cssrs_file, show_col_types = FALSE), by = "record_id")
} else message("No data/raw/cssrs.csv: C-SSRS severity and suicidal-behavior moderators unavailable.")

# ---- link to EMA pid ------------------------------------------------------------------------
# data/raw/record_inclivio_key.tsv (Record ID / Screen ID / Inclivio ID, tab-separated, as kept by
# the study team) is joined to data/raw/from_redcap/id_key.csv (pid <-> Inclivio, written by
# 00_import_redcap.R). Stray characters in IDs (e.g., a trailing ")") are stripped.
kf <- file.path(CONFIG$raw_dir, "record_inclivio_key.tsv")
if (!file.exists(kf)) stop("Missing ", kf, " (record_id <-> Inclivio ID). See data/README.md.")
rk <- read.delim(kf, colClasses = "character") |>
  mutate(inclivio = gsub("[^A-Za-z0-9]", "", inclivio), record_id = as.integer(record_id)) |>
  filter(nchar(inclivio) == 20)
ek <- readr::read_csv(file.path(CONFIG$raw_dir, "from_redcap", "id_key.csv"), show_col_types = FALSE)
key <- inner_join(rk, ek, by = "inclivio") |> select(record_id, pid)
unl_ema <- setdiff(ek$pid, key$pid); unl_rec <- setdiff(rk$record_id[rk$record_id %in% wide$record_id], key$record_id)
if (length(unl_ema)) message("EMA participants with no record ID in the key: ", paste(unl_ema, collapse = ", "))
if (length(unl_rec)) message("Record IDs with assessments but no EMA match: ", paste(unl_rec, collapse = ", "))
readr::write_csv(key, file.path(CONFIG$raw_dir, "link_key.csv"))
person <- key |> left_join(wide, by = "record_id")
attr_key <- "verified"
dir.create(file.path(CONFIG$raw_dir, "from_redcap"), showWarnings = FALSE)
readr::write_csv(person, file.path(CONFIG$raw_dir, "from_redcap", "person.csv"))

summ <- sc |> group_by(event) |>
  summarise(n = sum(!is.na(dep)), across(c(dep, anx, inq_pb, inq_tb, connect, lonely),
                                         list(m = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))), .groups = "drop")
print(summ, width = Inf)
message("person.csv written for ", nrow(person), " EMA participants")
