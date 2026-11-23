############################################################
# 11b_recalibration.R - RECALIBRATION (FIT 2022 Q4, APPLY 2023)
#
# Class weighting inflates the predicted probabilities of the rarer
# destinations. Recalibration models are FITTED on the internal
# validation period (October-December 2022) and APPLIED UNCHANGED to
# the 2023 external validation cohort:
#   - Stage 1 P(non-DRG): logistic recalibration (intercept + slope)
#   - Five-class probabilities: multinomial logistic recalibration on
#     the log-probabilities (nnet::multinom)
# Calibration (Brier, intercept, slope, mean predicted vs observed)
# is reported before and after recalibration, and the effect on the
# 0.70 confidence threshold used by the prototype and concordance
# analysis is shown.
#
# Outputs:
#   output/predictions/pred_<model>_recalibrated.rds (external, recalibrated probabilities)
#   output/tables/recalibration_stage1_stats.csv
#   output/tables/recalibration_by_class_stats.csv
#   output/tables/recalibration_classification_metrics.csv
#   output/tables/recalibration_threshold_070.csv
#   output/figures/figure_recalibration_stage1.png, figure_recalibration_rural_district.png
############################################################

if (!exists("CFG")) source("00_config.R")
if (!requireNamespace("nnet", quietly = TRUE)) install.packages("nnet")

preds <- read_all_preds(setdiff(model_names, "Naive baseline"))
file_stub <- c("Flat RF" = "flat_rf", "Weighted flat RF" = "flat_rf_weighted",
               "Weighted flat CatBoost" = "flat_cb_weighted", "Flat CatBoost" = "flat_cb",
               "Hierarchical RF" = "hier_rf", "Hierarchical CatBoost" = "hier_cb")

stats_class <- list(); stats_s1 <- list(); metr <- list(); thr <- list(); bins_s1 <- list(); bins_rd <- list()

for (model in names(preds)) {
  int <- preds[[model]]$internal; ext <- preds[[model]]$external
  P_int <- prob_matrix(int); P_ext <- prob_matrix(ext)

  fit <- fit_multiclass_recalibration(P_int, int$actual)
  P_rec <- apply_multiclass_recalibration(fit, P_ext)
  saveRDS(fit, out_path("models", paste0("recalibration_", file_stub[[model]], ".rds")))

  # per-class calibration before/after
  for (cl in class_order) {
    y <- as.integer(ext$actual == cl)
    stats_class[[length(stats_class) + 1]] <- bind_rows(
      calibration_stats(P_ext[, cl], y, model) %>% mutate(version = "Original"),
      calibration_stats(P_rec[, cl], y, model) %>% mutate(version = "Recalibrated")) %>%
      mutate(class = cl, .after = model)
  }
  y_rd <- as.integer(ext$actual == "Rural/District")
  bins_rd[[model]] <- bind_rows(
    calibration_bins(P_ext[, "Rural/District"], y_rd, model) %>% mutate(version = "Original"),
    calibration_bins(P_rec[, "Rural/District"], y_rd, model) %>% mutate(version = "Recalibrated"))

  # classification after recalibration (argmax)
  pred_rec <- prob_to_class(P_rec, class_order)
  metr[[model]] <- bind_rows(
    classification_metrics(ext$actual, ext$pred) %>% mutate(version = "Original"),
    classification_metrics(ext$actual, pred_rec) %>% mutate(version = "Recalibrated")) %>%
    mutate(model = model, .before = 1)

  # 0.70 threshold before/after
  conf_o <- apply(P_ext, 1, max); conf_r <- apply(P_rec, 1, max)
  thr[[model]] <- tibble(
    model = model, version = c("Original", "Recalibrated"),
    pct_referrals_top_prob_ge_070 = 100 * c(mean(conf_o >= CFG$confidence_threshold), mean(conf_r >= CFG$confidence_threshold)),
    pct_concordant_among_ge_070 = 100 * c(mean((ext$pred == ext$actual)[conf_o >= CFG$confidence_threshold]),
                                         mean((pred_rec == ext$actual)[conf_r >= CFG$confidence_threshold])))

  out <- ext %>% mutate(pred_recalibrated = pred_rec) %>%
    bind_cols(as_tibble(P_rec, .name_repair = ~ paste0(class_order, "_recal")))

  # stage 1 (hierarchical models only)
  if ("stage1_p_nondrg" %in% names(ext)) {
    y1_int <- as.integer(int$stage1_actual == "NON_DRG"); y1_ext <- as.integer(ext$stage1_actual == "NON_DRG")
    f1 <- fit_binary_recalibration(int$stage1_p_nondrg, y1_int)
    p1_rec <- apply_binary_recalibration(f1, ext$stage1_p_nondrg)
    stats_s1[[model]] <- bind_rows(
      calibration_stats(ext$stage1_p_nondrg, y1_ext, model) %>% mutate(version = "Original"),
      calibration_stats(p1_rec, y1_ext, model) %>% mutate(version = "Recalibrated (stage 1 logistic)"),
      calibration_stats(rowSums(P_rec[, non_drg_classes]), y1_ext, model) %>%
        mutate(version = "Recalibrated (sum of recalibrated non-DRG classes)"))
    bins_s1[[model]] <- bind_rows(
      calibration_bins(ext$stage1_p_nondrg, y1_ext, model) %>% mutate(version = "Original"),
      calibration_bins(p1_rec, y1_ext, model) %>% mutate(version = "Recalibrated"))
    out <- out %>% mutate(stage1_p_nondrg_recal = p1_rec)
  }
  saveRDS(out, out_path("preds", paste0("pred_", file_stub[[model]], "_recalibrated.rds")))
}

stats_class <- bind_rows(stats_class); stats_s1 <- bind_rows(stats_s1)
save_table(stats_class, "recalibration_by_class_stats.csv")
save_table(stats_s1, "recalibration_stage1_stats.csv")
save_table(bind_rows(metr), "recalibration_classification_metrics.csv")
save_table(bind_rows(thr), "recalibration_threshold_070.csv")
print(stats_s1, width = Inf)
print(stats_class %>% filter(class == "Rural/District"), width = Inf)
print(bind_rows(thr))

plot_bins <- function(b, title, axis_max = 1) {
  ggplot(b, aes(mean_predicted, observed_freq, colour = version)) +
    geom_abline(linetype = "dashed", colour = "grey55") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, alpha = 0.5) +
    geom_line() + geom_point(size = 2) +
    facet_wrap(~ model) +
    coord_equal(xlim = c(0, axis_max), ylim = c(0, axis_max)) +
    scale_colour_manual(values = c("Original" = "grey55", "Recalibrated" = "black")) +
    theme_ms(11) +
    labs(title = title, subtitle = "Recalibration fitted on internal validation (Oct-Dec 2022), applied to 2023; deciles, 95% CI",
         x = "Mean predicted probability", y = "Observed proportion", colour = NULL)
}
if (length(bins_s1)) save_fig(plot_bins(bind_rows(bins_s1), "Stage 1 probability of a non-DRG destination, before and after recalibration"),
                              "figure_recalibration_stage1.png", width = 11, height = 6)
b_rd <- bind_rows(bins_rd)
save_fig(plot_bins(b_rd, "Rural/District probability, before and after recalibration",
                   axis_max = min(1, max(0.1, max(b_rd$mean_predicted, b_rd$upper) * 1.1))),
         "figure_recalibration_rural_district.png", width = 13, height = 7)

rm(preds); invisible(gc())
cat("Step 11b complete.\n")
