# =============================================================================
# 04_person_slopes.R — person-specific coupling estimates (Figure 2 data)
# =============================================================================
source("R/utils.R")
A <- readRDS(file.path(CONFIG$derived_dir, "analysis.rds"))
fit <- function(nm) readRDS(file.path(CONFIG$fits_dir, paste0(nm, ".rds")))

MAP <- c(si_na = "m1_si_na", si_mal = "m2_si_mal", mal_na = "m3_mal_na", adp_na = "m4_adp_na")
PRED <- c(si_na = "neg_aff_w", si_mal = "mal_w", mal_na = "neg_aff_w", adp_na = "neg_aff_w")

summ <- list(); draws <- list()
for (k in names(MAP)) {
  if (!file.exists(file.path(CONFIG$fits_dir, paste0(MAP[k], ".rds")))) next
  f <- fit(MAP[k])
  summ[[k]]  <- person_slope_summary(f, PRED[k]) |> mutate(coupling = k)
  dr <- person_slope_draws(f, PRED[k])
  draws[[k]] <- dr[dr$.draw %in% sample(unique(dr$.draw), min(1000, n_distinct(dr$.draw))), ] |>
    mutate(coupling = k)
}
slopes <- bind_rows(summ) |>
  mutate(never_endorses_si = pid %in% A$never_endorse,
         # For people who never endorsed SI, the ST-model slope is NOT informed by their data:
         # it is the population mean plus shrinkage noise. Flag (and ideally exclude) them in
         # any figure or moderator analysis of ST couplings.
         si_slope_uninformed = never_endorses_si & coupling %in% c("si_na", "si_mal"),
         excludes_zero = lo > 0 | hi < 0)

save_csv(slopes, "S4_person_specific_slopes.csv")
saveRDS(bind_rows(draws), file.path(CONFIG$derived_dir, "person_slope_draws.rds"))

wide <- slopes |> select(pid, coupling, est) |> pivot_wider(names_from = coupling, values_from = est)
saveRDS(wide, file.path(CONFIG$derived_dir, "person_slopes_wide.rds"))

save_csv(slopes |> group_by(coupling) |>
           summarise(n_people = n(), n_ci_excludes_zero = sum(excludes_zero),
                     n_uninformed = sum(si_slope_uninformed), .groups = "drop"),
         "S4b_person_slope_counts.csv")
