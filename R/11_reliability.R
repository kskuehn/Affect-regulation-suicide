# =============================================================================
# 11_reliability.R — multilevel omega (Geldhof et al., 2014) for the negative-affect composite
# =============================================================================
# Two-level CFA (one factor within, one between) in lavaan; writes S9_reliability_omega.csv.
suppressMessages(library(lavaan))
source("config.R")
d <- read.csv(file.path(CONFIG$src_dir, "ema_long.csv"))
d <- subset(d, completed == 1 & prompt_type == "scheduled")
items <- c("na_sad", "na_angry", "na_afraid", "na_ashamed", "na_guilty")
d[items] <- d[items] / 10                      # rescale 0-100 sliders for estimation
d <- d[rowSums(!is.na(d[items])) > 0, ]
m <- '
level: 1
  fw =~ lw1*na_sad + lw2*na_angry + lw3*na_afraid + lw4*na_ashamed + lw5*na_guilty
  na_sad ~~ tw1*na_sad; na_angry ~~ tw2*na_angry; na_afraid ~~ tw3*na_afraid
  na_ashamed ~~ tw4*na_ashamed; na_guilty ~~ tw5*na_guilty
level: 2
  fb =~ lb1*na_sad + lb2*na_angry + lb3*na_afraid + lb4*na_ashamed + lb5*na_guilty
  na_sad ~~ tb1*na_sad; na_angry ~~ tb2*na_angry; na_afraid ~~ tb3*na_afraid
  na_ashamed ~~ tb4*na_ashamed; na_guilty ~~ tb5*na_guilty
omega_w := (lw1+lw2+lw3+lw4+lw5)^2 / ((lw1+lw2+lw3+lw4+lw5)^2 + tw1+tw2+tw3+tw4+tw5)
omega_b := (lb1+lb2+lb3+lb4+lb5)^2 / ((lb1+lb2+lb3+lb4+lb5)^2 + tb1+tb2+tb3+tb4+tb5)
'
f <- sem(m, data = d, cluster = "pid", std.lv = TRUE, missing = "ML",
         optim.method = "em", em.iter.max = 20000)
pe <- parameterEstimates(f)
out <- pe[pe$op == ":=", c("label", "est", "ci.lower", "ci.upper")]
fm <- fitMeasures(f, c("cfi", "rmsea", "srmr_within", "srmr_between"))
for (k in names(fm)) out[[k]] <- round(as.numeric(fm[k]), 3)
print(out)
write.csv(out, file.path(CONFIG$tables_dir, "S9_reliability_omega.csv"), row.names = FALSE)
