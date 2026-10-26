############################################################
# 11_fig_calibration.R - CALIBRATION FIGURES (EXTERNAL, 2023)
#
#   (a) Stage 1 calibration: predicted vs observed probability of
#       a non-DRG destination, RF vs CatBoost (deciles + 95% CI,
#       with distribution of predictions underneath)
#   (b) Stage 1 probability for Rural/District referrals: how
#       actual Rural/District referrals are routed by Stage 1
#       (share falling below 0.5 is sent to the DRG branch and
#       can no longer be predicted correctly)
#   (c) Figure 5: calibration of the combined hierarchical
#       probability for Rural/District (Stage 1 x Stage 3)
#
# Brier score, calibration intercept and slope (logistic
# recalibration) are saved alongside.
############################################################

if (!exists("CFG")) source("00_config.R")
library(patchwork)

ext <- list(
  "Random Forest" = read_rds_out("preds", "pred_hier_rf.rds")$external,
  "CatBoost"      = read_rds_out("preds", "pred_hier_cb.rds")$external
)

calib_panel <- function(get_p, get_y, title, xlab, axis_max = 1) {
  stats <- imap_dfr(ext, ~ calibration_stats(get_p(.x), get_y(.x), .y))
  bins  <- imap_dfr(ext, ~ calibration_bins(get_p(.x), get_y(.x), .y, n_bins = 10, type = "quantile"))
  dist  <- imap_dfr(ext, ~ tibble(p = get_p(.x), model = .y))
  # "auto": zoom the axes to the range covered by the calibration bins
  if (identical(axis_max, "auto")) axis_max <- min(1, max(c(bins$mean_predicted, bins$upper)) * 1.15)
  ann <- stats %>%
    mutate(txt = sprintf("%s: Brier = %.4f, intercept = %.3f, slope = %.3f", model, brier, cal_intercept, cal_slope)) %>%
    pull(txt) %>% paste(collapse = "\n")
  top <- ggplot(bins, aes(mean_predicted, observed_freq, colour = model)) +
    geom_abline(linetype = "dashed", colour = "grey55") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, alpha = 0.6) +
    geom_line(linewidth = 0.7) + geom_point(size = 2.4) +
    annotate("label", x = 0, y = axis_max, label = ann, hjust = 0, vjust = 1, size = 3,
             fill = "white", label.size = 0.15) +
    scale_colour_manual(values = model_colours) +
    coord_equal(xlim = c(0, axis_max), ylim = c(0, axis_max)) +
    theme_ms() +
    labs(title = title, subtitle = "External validation (2023); points = deciles of predicted probability, bars = 95% CI",
         x = "Mean predicted probability", y = "Observed proportion", colour = NULL)
  bottom <- ggplot(dist, aes(p, fill = model)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
    scale_fill_manual(values = model_colours) +
    coord_cartesian(xlim = c(0, axis_max)) +
    theme_ms() + theme(legend.position = "none") +
    labs(x = xlab, y = "Count")
  list(plot = top / bottom + plot_layout(heights = c(3, 1)), stats = stats, bins = bins)
}

# (a) Stage 1 -------------------------------------------------
cal1 <- calib_panel(function(d) d$stage1_p_nondrg,
                    function(d) as.integer(d$stage1_actual == "NON_DRG"),
                    "Calibration: Stage 1 probability of a non-DRG funded destination",
                    "Predicted probability (non-DRG)")
save_fig(cal1$plot, "figure_calibration_stage1.png", width = 7.5, height = 9)
save_table(cal1$stats, "calibration_stage1_stats.csv")
save_table(cal1$bins,  "calibration_stage1_bins.csv")

# (b) Stage 1 probability among actual Rural/District referrals
rural_s1 <- imap_dfr(ext, ~ .x %>% filter(actual == "Rural/District") %>%
                       transmute(model = .y, stage1_p_nondrg))
rural_s1_summary <- rural_s1 %>% group_by(model) %>%
  summarise(n = n(), median_p = median(stage1_p_nondrg),
            pct_routed_to_drg = 100 * mean(stage1_p_nondrg < 0.5), .groups = "drop")
save_table(rural_s1_summary, "stage1_probability_rural_district_summary.csv")

p_rural_s1 <- ggplot(rural_s1, aes(stage1_p_nondrg, fill = model)) +
  geom_histogram(binwidth = 0.025, boundary = 0, position = "identity", alpha = 0.55, colour = NA) +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_text(data = rural_s1_summary %>% mutate(y = Inf, lab = sprintf("%s: %.1f%% below 0.5 (routed to DRG branch)", model, pct_routed_to_drg)),
            aes(x = 0.52, y = y, label = lab, colour = model), inherit.aes = FALSE,
            hjust = 0, vjust = c(1.5, 3.2), size = 3.2, show.legend = FALSE) +
  scale_fill_manual(values = model_colours) + scale_colour_manual(values = model_colours) +
  scale_x_continuous(limits = c(0, 1)) +
  theme_ms() +
  labs(title = "Stage 1 probability of a non-DRG destination for Rural/District referrals",
       subtitle = sprintf("Actual Rural/District referrals, external validation (2023), n = %d", sum(rural_s1$model == "CatBoost")),
       x = "Stage 1 predicted probability (non-DRG)", y = "Number of referrals", fill = NULL)
save_fig(p_rural_s1, "figure_stage1_probability_rural_district.png", width = 9, height = 6)

# (c) Figure 5: combined hierarchical Rural/District probability
y_rural <- function(d) as.integer(d$actual == "Rural/District")
p_rural <- function(d) d[["Rural/District"]]
cal5 <- calib_panel(p_rural, y_rural,
                    "Calibration: combined hierarchical probability for Rural/District referrals",
                    "Predicted probability (Stage 1 x Stage 3, Rural/District)",
                    axis_max = "auto")
save_fig(cal5$plot, "figure5_calibration_rural_district.png", width = 7.5, height = 9)
save_table(cal5$stats, "calibration_rural_district_stats.csv")
save_table(cal5$bins,  "calibration_rural_district_bins.csv")

rm(ext); invisible(gc())
cat("Step 11 complete.\n")
