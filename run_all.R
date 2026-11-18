# =============================================================================
# run_all.R — run the whole pipeline in order
# =============================================================================
#   Rscript run_all.R                         # public de-identified data, full sampling
#   FAST=1 Rscript run_all.R                  # quick pipeline check (short chains)
#   PRECISE_DATA=real Rscript run_all.R       # study data in data/raw/
#   Rscript R/00_import_redcap.R && PRECISE_DATA=redcap Rscript run_all.R
steps <- c(
  "R/02_prepare_data.R",
  "R/03_core_models.R",
  "R/04_person_slopes.R",
  "R/05_moderators.R",
  "R/06_lpa.R",
  "R/07_sensitivity.R",
  "R/08_treatment_models.R",
  "R/09_lagged_models.R",
  "R/10_figures.R",
  "R/11_reliability.R"
)
source("config.R")
for (s in steps) {
  message("\n==== ", s, " ====")
  t0 <- Sys.time()
  # each script runs in a fresh environment so they stay independent
  tryCatch(source(s, local = new.env()),
           skip_script = function(e) message("     skipped: ", conditionMessage(e)))
  message(sprintf("     done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
writeLines(capture.output(sessionInfo()), file.path(CONFIG$out_dir, "sessionInfo.txt"))
message("\nAll done. Results in ", CONFIG$out_dir, "/ (tables, figures, sessionInfo.txt)")
