# =============================================================================
# 06_lpa.R — latent profile analysis of standardized person-specific couplings
# =============================================================================
# NOTE for the paper: with N = 30 and 4 indicators this is badly underpowered, and
# the indicators are shrunken estimates (which compresses differences between people).
# Consider moving it to the supplement or dropping it.
source("R/utils.R")
suppressPackageStartupMessages(library(mclust))
wide <- readRDS(file.path(CONFIG$derived_dir, "person_slopes_wide.rds"))
cols <- intersect(c("si_na", "si_mal", "mal_na", "adp_na"), names(wide))
if (length(cols) < 2) skip_script("LPA needs at least two coupling estimates")
X <- scale(as.matrix(wide[, cols]))

bic <- mclustBIC(X, G = 1:5, verbose = FALSE)
bic_tab <- as.data.frame(unclass(bic)) |>
  mutate(G = 1:5) |>
  pivot_longer(-G, names_to = "covariance_model", values_to = "BIC")
save_csv(bic_tab, "S6_lpa_bic.csv")

best <- Mclust(X, x = bic, verbose = FALSE)
summ <- data.frame(best_G = best$G, best_model = best$modelName, BIC = best$bic,
                   # BIC difference between best 1- and best 2-profile solutions (mclust BIC: higher = better)
                   dBIC_2_vs_1 = max(bic_tab$BIC[bic_tab$G == 2], na.rm = TRUE) -
                                 max(bic_tab$BIC[bic_tab$G == 1], na.rm = TRUE))
save_csv(summ, "S6b_lpa_best.csv")
if (best$G > 1) save_csv(data.frame(pid = wide$pid, profile = best$classification,
                                    p_max = apply(best$z, 1, max)), "S6c_lpa_classes.csv")
print(summ)
