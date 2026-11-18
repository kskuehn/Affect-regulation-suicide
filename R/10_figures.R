# =============================================================================
# 10_figures.R — manuscript figures (1–2) and supplementary figures (S1–S3)
# =============================================================================
# Figures carry no titles (APA: captions go in the manuscript) and show no participant labels.
source("R/utils.R")
rd <- function(f) readr::read_csv(file.path(CONFIG$tables_dir, f), show_col_types = FALSE)
has_tab <- function(f) file.exists(file.path(CONFIG$tables_dir, f))
save_fig <- function(p, name, w = 7, h = 4) {
  ggsave(file.path(CONFIG$figures_dir, paste0(name, ".pdf")), p, width = w, height = h)
  ggsave(file.path(CONFIG$figures_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300)
}
ACCENT <- "#8B2E2E"; BLUE <- "#2E5A8B"
OUTCOME <- "suicidal ideation"
model_lab <- c("ST ~ Negative Affect"              = "Ideation ~ negative affect (per point)",
               "ST ~ Maladaptive Strategy Use"     = "Ideation ~ maladaptive strategy use",
               "Maladaptive Use ~ Negative Affect" = "Maladaptive use ~ negative affect",
               "Adaptive Use ~ Negative Affect"    = "Adaptive use ~ negative affect")
mod_lab <- c(lonely = "Loneliness", connect = "Connectedness", dep = "Depressive symptoms",
             anx = "Anxiety symptoms", inq_pb = "Perceived burdensomeness", inq_tb = "Thwarted belongingness",
             fad = "Fearlessness about death", cerq_maladaptive = "Trait maladaptive ER",
             cerq_adaptive = "Trait adaptive ER", cssrs_life_sev = "C-SSRS ideation, lifetime",
             cssrs_pm_sev = "C-SSRS ideation, past month", sb_life = "Suicidal behavior, lifetime",
             sb_3mo = "Suicidal behavior, past 3 months")

# ---- Figure 1: person-specific slopes for the two ideation couplings -------------------------
sl_all <- rd("S4_person_specific_slopes.csv")
t3 <- rd("T3_core_coupling_models.csv")
avg <- c(si_na = 10 * t3$b[t3$model == "ST ~ Negative Affect"],
         si_mal = t3$b[t3$model == "ST ~ Maladaptive Strategy Use"])
pan <- c(si_na = "A. Negative affect (per 10 points)", si_mal = "B. Maladaptive strategy use (per strategy)")
f1 <- sl_all |>
  filter(coupling %in% c("si_na", "si_mal")) |>
  mutate(k = ifelse(coupling == "si_na", 10, 1), est = est * k, lo = lo * k, hi = hi * k,
         panel = factor(pan[coupling], levels = pan),
         status = ifelse(si_slope_uninformed, "Never reported ideation (shrinkage only)", "Reported ideation at least once")) |>
  group_by(coupling) |> mutate(rank = rank(est, ties.method = "first")) |> ungroup()
avg_df <- data.frame(panel = factor(pan, levels = pan), b = avg[names(pan)])
p1 <- ggplot(f1, aes(est, rank, colour = status)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_vline(data = avg_df, aes(xintercept = b), linetype = 3, colour = ACCENT) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, alpha = .7) +
  geom_point(size = 1.8) +
  facet_wrap(~ panel, scales = "free_x") +
  scale_colour_manual(values = c("Reported ideation at least once" = ACCENT,
                                 "Never reported ideation (shrinkage only)" = "grey65"), name = NULL) +
  labs(x = paste0("Person-specific association with ", OUTCOME, " (log-odds), 95% credible interval"),
       y = "Participants (ordered by estimate)") +
  theme_paper() +
  theme(legend.position = "bottom", axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())
save_fig(p1, "fig1_person_specific_ideation", 8, 5)

# ---- Figure 2: person-level moderators (two-stage correlations) ------------------------------
if (has_tab("T4a_moderators_two_stage.csv")) {
cp_lab <- c(si_na = "A. Negative affect\u2013ideation\n(endorsers, n = 18)",
            si_mal = "B. Maladaptive use\u2013ideation\n(endorsers, n = 18)",
            mal_na = "C. Negative affect\u2013\nmaladaptive use (N = 30)",
            adp_na = "D. Negative affect\u2013\nadaptive use (N = 30)")
t4 <- rd("T4a_moderators_two_stage.csv") |>
  filter((coupling %in% c("si_na", "si_mal") & sample != "all") | (coupling %in% c("mal_na", "adp_na") & sample == "all")) |>
  mutate(coupling = factor(cp_lab[coupling], levels = cp_lab),
         moderator = factor(mod_lab[moderator], levels = rev(mod_lab)),
         ci_excl = lo > 0 | hi < 0)
p2 <- ggplot(t4, aes(r, moderator)) +
  geom_vline(xintercept = 0, colour = "grey40") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, colour = "grey60") +
  geom_point(aes(shape = ci_excl, colour = r > 0), size = 2.2) +
  scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 16), guide = "none") +
  scale_colour_manual(values = c(`FALSE` = ACCENT, `TRUE` = BLUE), guide = "none") +
  facet_wrap(~ coupling, nrow = 1) +
  coord_cartesian(xlim = c(-1, 1)) +
  labs(x = "Correlation with person-specific coupling, 95% CI", y = NULL) + theme_paper()
save_fig(p2, "fig2_moderators", 10, 5)
}

# ---- Figure S1: average effects and heterogeneity, by model ---------------------------------
f_s1 <- bind_rows(
  t3 |> transmute(model, panel = "Average (fixed) effect", est = b, lo = b_lo, hi = b_hi),
  t3 |> transmute(model, panel = "Person-to-person SD", est = sd, lo = sd_lo, hi = sd_hi)) |>
  mutate(model = factor(model_lab[model], levels = model_lab))
ps1 <- ggplot(f_s1, aes(est, panel, colour = panel)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = .8) +
  geom_point(size = 2.4) +
  facet_wrap(~ model, scales = "free_x", ncol = 2) +
  scale_colour_manual(values = c("black", ACCENT), guide = "none") +
  labs(x = "Estimate with 95% credible interval (model units)", y = NULL) + theme_paper()
save_fig(ps1, "figS1_average_and_heterogeneity", 8, 4.5)

# ---- Figure S2: sensitivity to the highest-endorsing participant ----------------------------
if (has_tab("T5_sensitivity.csv")) {
t5 <- rd("T5_sensitivity.csv") |> filter(!grepl("joint", model)) |>
  mutate(sample = ifelse(grepl("excl", model), "Excluding highest-endorsing participant", "Full sample"),
         predictor = recode(predictor, neg_aff_w = "Ideation ~ negative affect (per point)",
                            mal_w = "Ideation ~ maladaptive strategy use"))
ps2 <- ggplot(t5, aes(sd, sample)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_errorbarh(aes(xmin = sd_lo, xmax = sd_hi), height = 0, linewidth = .8) +
  geom_point(size = 2.4) +
  facet_wrap(~ predictor, scales = "free_x", ncol = 1) +
  labs(x = "Random-slope SD (log-odds), 95% credible interval", y = NULL) + theme_paper()
save_fig(ps2, "figS2_sensitivity", 7, 4)
}

# ---- Figure S3: average coupling across the treatment period --------------------------------
if (has_tab("T8b_coupling_curves.csv")) {
cv <- rd("T8b_coupling_curves.csv") |>
  mutate(model = recode(model, t2_si_na = "Negative affect\u2013ideation\n(log-odds per point)",
                        t2_si_mal = "Maladaptive use\u2013ideation\n(log-odds)",
                        t2_mal_na = "Negative affect\u2013\nmaladaptive use"),
         model = factor(model, levels = c("Negative affect\u2013ideation\n(log-odds per point)",
                                          "Maladaptive use\u2013ideation\n(log-odds)",
                                          "Negative affect\u2013\nmaladaptive use")))
ps3 <- ggplot(cv, aes(week, est)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = ACCENT, alpha = .2) +
  geom_line(colour = ACCENT, linewidth = 1) +
  facet_wrap(~ model, scales = "free_y") +
  labs(x = "Weeks since start of EMA", y = "Average coupling (95% CrI)") + theme_paper()
save_fig(ps3, "figS3_coupling_over_treatment", 10, 3.4)
}
message("Figures written to ", CONFIG$figures_dir)
