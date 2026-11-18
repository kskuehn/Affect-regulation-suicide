# =============================================================================
# 10_figures.R — manuscript figures (reproduces Figures 1–4, adds Figure 5)
# =============================================================================
source("R/utils.R")
A <- readRDS(file.path(CONFIG$derived_dir, "analysis.rds"))
rd <- function(f) readr::read_csv(file.path(CONFIG$tables_dir, f), show_col_types = FALSE)
has_tab <- function(f) file.exists(file.path(CONFIG$tables_dir, f))
save_fig <- function(p, name, w = 7, h = 4) {
  ggsave(file.path(CONFIG$figures_dir, paste0(name, ".pdf")), p, width = w, height = h)
  ggsave(file.path(CONFIG$figures_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300)
}
ACCENT <- "#8B2E2E"; BLUE <- "#2E5A8B"

# ---- Figure 1: average effects and heterogeneity --------------------------------------
t3 <- rd("T3_core_coupling_models.csv") |> mutate(model = factor(model, levels = rev(model)))
f1 <- bind_rows(
  t3 |> transmute(model, panel = "A. Average (fixed) effect", est = b, lo = b_lo, hi = b_hi),
  t3 |> transmute(model, panel = "B. Person-to-person heterogeneity (SD)", est = sd, lo = sd_lo, hi = sd_hi)
)
p1 <- ggplot(f1, aes(est, model, colour = panel)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = .8) +
  geom_point(size = 2.4) +
  facet_wrap(~ panel, scales = "free_x") +
  scale_colour_manual(values = c("black", ACCENT), guide = "none") +
  labs(x = "Estimate with 95% credible interval", y = NULL) + theme_paper()
save_fig(p1, "fig1_average_and_heterogeneity", 8, 3.5)

# ---- Figure 2: person-specific ST ~ maladaptive slopes -----------------------------------
sl_all <- rd("S4_person_specific_slopes.csv")
FIG2 <- if ("si_mal" %in% sl_all$coupling) "si_mal" else "si_na"
sl <- sl_all |> filter(coupling == FIG2) |>
  mutate(pid = reorder(as.character(pid), est),
         status = ifelse(si_slope_uninformed, "Never endorsed SI (estimate = shrinkage only)", "Endorsed SI at least once"))
t3c <- rd("T3_core_coupling_models.csv")
b_avg <- t3c$b[t3c$model == ifelse(FIG2 == "si_mal", "ST ~ Maladaptive Strategy Use", "ST ~ Negative Affect")]
p2 <- ggplot(sl, aes(est, pid, colour = status)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_vline(xintercept = b_avg, linetype = 3, colour = ACCENT) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, alpha = .7) +
  geom_point(size = 2) +
  scale_colour_manual(values = c(ACCENT, "grey65"), name = NULL) +
  labs(title = paste("Person-specific", ifelse(FIG2 == "si_mal", "ST ~ maladaptive-strategy", "ST ~ negative-affect"), "coupling"),
       subtitle = sprintf("Dotted line = population average (b = %.2f)", b_avg),
       x = "Person-specific slope (log-odds), 95% credible interval", y = "Participant (sorted)") +
  theme_paper() + theme(legend.position = "bottom", axis.text.y = element_text(size = 7))
save_fig(p2, paste0("fig2_person_specific_", FIG2), 6.5, 7.5)

# ---- Figure 3: sensitivity -----------------------------------------------------------------
if (has_tab("T5_sensitivity.csv")) {
t5 <- rd("T5_sensitivity.csv") |> filter(!grepl("joint", model)) |>
  mutate(model = factor(model, levels = rev(model)))
p3 <- ggplot(t5, aes(sd, model)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_errorbarh(aes(xmin = sd_lo, xmax = sd_hi), height = 0, linewidth = .8) +
  geom_point(size = 2.4) +
  facet_wrap(~ predictor, scales = "free", ncol = 1) +
  labs(title = "Sensitivity of heterogeneity estimates to the highest-endorsing participant",
       x = "Random-slope SD, 95% credible interval", y = NULL) + theme_paper()
save_fig(p3, "fig3_sensitivity", 7, 4.5)
}

# ---- Figure 4: moderators WITH uncertainty ---------------------------------------------------
if (has_tab("T4a_moderators_two_stage.csv")) {
lab <- c(si_na = "NA -> SI", si_mal = "Maladaptive -> SI", mal_na = "NA -> maladaptive", adp_na = "NA -> adaptive")
t4 <- rd("T4a_moderators_two_stage.csv") |> filter(sample == "all") |>
  mutate(coupling = lab[coupling])
p4 <- ggplot(t4, aes(r, moderator)) +
  geom_vline(xintercept = 0, colour = "grey40") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, colour = "grey55") +
  geom_point(aes(colour = r > 0), size = 2.4) +
  scale_colour_manual(values = c(ACCENT, BLUE), guide = "none") +
  facet_wrap(~ coupling, nrow = 1) +
  coord_cartesian(xlim = c(-1, 1)) +
  labs(title = "Person-level moderators of coupling strength (exploratory, N = 30)",
       x = "Correlation with person-specific coupling, 95% CI", y = NULL) + theme_paper()
save_fig(p4, "fig4_moderators", 9, 3.5)
}

# ---- Figure 5 (new): coupling across treatment ------------------------------------------------
if (has_tab("T8b_coupling_curves.csv")) {
cv <- rd("T8b_coupling_curves.csv") |>
  mutate(model = recode(model, t2_si_na = "NA -> SI (log-odds)", t2_si_mal = "Maladaptive -> SI (log-odds)",
                        t2_mal_na = "NA -> maladaptive"))
p5 <- ggplot(cv, aes(week, est)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = ACCENT, alpha = .2) +
  geom_line(colour = ACCENT, linewidth = 1) +
  facet_wrap(~ model, scales = "free_y") +
  labs(title = "Average within-person coupling across the treatment period",
       x = "Weeks since start of EMA / treatment", y = "Coupling (95% CrI)") + theme_paper()
save_fig(p5, "fig5_coupling_over_treatment", 9, 3.2)
}
message("Figures written to ", CONFIG$figures_dir)
