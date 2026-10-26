############################################################
# 12_dca.R - DECISION CURVE ANALYSIS (EXTERNAL, 2023)
#
# Net benefit across threshold probabilities for two decisions:
#   1. Figure 8: refer to a non-DRG funded facility rather than a
#      DRG funded one (model = Stage 1 P(non-DRG))
#   2. Identify a Rural/District referral (model = combined
#      hierarchical P(Rural/District)); the class with the
#      poorest discrimination
# Model-guided strategies (hierarchical RF and CatBoost) are
# compared with refer all, refer none and the naive baseline.
#   NB = TP/n - FP/n * pt / (1 - pt)
############################################################

if (!exists("CFG")) source("00_config.R")
library(patchwork)

naive <- read_rds_out("preds", "pred_naive.rds")$external
hrf   <- read_rds_out("preds", "pred_hier_rf.rds")$external
hcb   <- read_rds_out("preds", "pred_hier_cb.rds")$external
stopifnot(identical(naive$actual, hrf$actual), identical(hrf$actual, hcb$actual))

dca_strategies <- c("Hierarchical RF", "Hierarchical CatBoost", "Naive baseline", "Refer all", "Refer none")

run_dca <- function(y, p_rf, p_cb, naive_pos, thresholds) {
  bind_rows(
    net_benefit_curve(p_rf, y, thresholds, "Hierarchical RF"),
    net_benefit_curve(p_cb, y, thresholds, "Hierarchical CatBoost"),
    net_benefit_fixed(naive_pos, y, thresholds, "Naive baseline"),
    net_benefit_reference(y, thresholds)
  ) %>% mutate(strategy = factor(strategy, levels = dca_strategies))
}

plot_dca <- function(d, title, prevalence, ymin) {
  ggplot(d, aes(threshold, net_benefit, colour = strategy, linetype = strategy)) +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(values = model_colours) +
    scale_linetype_manual(values = c("Hierarchical RF" = "solid", "Hierarchical CatBoost" = "solid",
                                     "Naive baseline" = "longdash", "Refer all" = "dashed", "Refer none" = "dotted")) +
    coord_cartesian(ylim = c(ymin, prevalence * 1.1)) +
    theme_ms() +
    labs(title = title, subtitle = sprintf("External validation (2023); prevalence = %.1f%%", 100 * prevalence),
         x = "Threshold probability", y = "Net benefit", colour = NULL, linetype = NULL)
}

nb_at <- function(d, cut_points) {
  d %>% filter(round(threshold, 3) %in% round(cut_points, 3)) %>%
    pivot_wider(names_from = strategy, values_from = net_benefit)
}

# --- 1. Non-DRG referral decision (Figure 8) -------------------
y_nondrg <- as.integer(hrf$actual %in% non_drg_classes)
thr1 <- seq(0.01, 0.50, by = 0.01)
dca_nondrg <- run_dca(y_nondrg, hrf$stage1_p_nondrg, hcb$stage1_p_nondrg,
                      naive$pred %in% non_drg_classes, thr1)
save_table(dca_nondrg, "dca_non_drg.csv")
save_table(nb_at(dca_nondrg, c(0.05, 0.10, 0.15, 0.20, 0.30)), "dca_non_drg_summary.csv")
fig8 <- plot_dca(dca_nondrg, "Decision curve analysis: non-DRG referral decision", mean(y_nondrg), -0.02)
save_fig(fig8, "figure8_dca_non_drg.png", width = 9, height = 6.5)

# --- 2. Rural/District identification ---------------------------
y_rural <- as.integer(hrf$actual == "Rural/District")
thr2 <- seq(0.005, 0.20, by = 0.005)
dca_rural <- run_dca(y_rural, hrf[["Rural/District"]], hcb[["Rural/District"]],
                     naive$pred == "Rural/District", thr2)
save_table(dca_rural, "dca_rural_district.csv")
save_table(nb_at(dca_rural, c(0.01, 0.02, 0.05, 0.10)), "dca_rural_district_summary.csv")
fig_rural <- plot_dca(dca_rural, "Decision curve analysis: Rural/District referral", mean(y_rural), -0.005)
save_fig(fig_rural, "figure_dca_rural_district.png", width = 9, height = 6.5)

save_fig(fig8 + fig_rural + plot_layout(guides = "collect") & theme(legend.position = "bottom"),
         "figure_dca_combined.png", width = 15, height = 6.5)

rm(naive, hrf, hcb); invisible(gc())
cat("Step 12 complete.\n")
