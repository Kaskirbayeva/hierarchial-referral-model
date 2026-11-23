############################################################
# 16_sensitivity_no_surgery_type.R - WITHOUT SURGERY TYPE
# (predictor timing: surgery type is recorded at referral but may be revised)
#
# sur_type (main / additional operation) is entered by the referring
# facility as the proposed treatment plan, together with the referral
# diagnosis. The receiving facility may revise this plan after its own
# investigations, and the HES record holds the final coding. The hierarchical RF
# (and, if CFG$sens_no_surgery_catboost, the hierarchical CatBoost)
# are refitted WITHOUT sur_type, with the same hyperparameters,
# weights and data, and compared with the main models.
# Descriptives of sur_type are saved to help document the field.
#
# Outputs (output/tables):
#   surgery_type_descriptives.csv
#   sens_no_surgery_type_performance.csv
#   output/predictions/pred_hier_rf_no_surgery.rds (, pred_hier_cb_no_surgery.rds)
#   output/models/rf_stage*_no_surgery.rds (models without surgery type)
############################################################

if (!exists("CFG")) source("00_config.R")
library(ranger)

coh <- read_rds_out("data", "cohorts.rds")
train_main <- coh$train_main; val_internal <- coh$val_internal; df_extval_2023 <- coh$df_extval_2023
rm(coh)

# --- descriptives ---------------------------------------------------
desc <- bind_rows(
  df_extval_2023 %>% count(by = "Accepting facility level", group = as.character(referral_destination), sur_type),
  df_extval_2023 %>% count(by = "Referring facility level", group = refer_level, sur_type),
  df_extval_2023 %>% mutate(group = if_else(str_starts(toupper(icd10_ref), "O"), "Obstetric referral diagnosis", "Other referral diagnosis")) %>%
    count(by = "Referral diagnosis", group, sur_type)) %>%
  group_by(by, group) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup()
save_table(desc, "surgery_type_descriptives.csv")

predictor_vars_main <- predictor_vars
predictor_vars <- setdiff(predictor_vars_main, "sur_type")   # global: used by model_formula() and build_pool()
cat("Predictors without sur_type:", length(predictor_vars), "\n")
sens <- tryCatch({

stage_data <- function() {
  s2 <- train_main %>% filter(referral_destination %in% drg_classes) %>%
    mutate(destination_drg = factor(as.character(referral_destination), levels = drg_classes))
  s3 <- train_main %>% filter(referral_destination %in% non_drg_classes) %>%
    mutate(destination_non_drg = factor(as.character(referral_destination), levels = non_drg_classes))
  list(s2 = s2, s3 = s3)
}
sd <- stage_data()
w1 <- inverse_freq_weights(train_main$destination_broad)
w3 <- inverse_freq_weights(sd$s3$destination_non_drg)

# --- hierarchical RF without sur_type -----------------------------
best_params <- read_rds_out("models", "rf_best_params.rds")
m1 <- fit_rf_tuned(train_main, "destination_broad", best_params, unname(w1[as.character(train_main$destination_broad)]), importance = "none")
m2 <- fit_rf_tuned(sd$s2, "destination_drg", best_params, importance = "none")
m3 <- fit_rf_tuned(sd$s3, "destination_non_drg", best_params, unname(w3[as.character(sd$s3$destination_non_drg)]), importance = "none")
pred_rf <- map(list(internal = val_internal, external = df_extval_2023), function(df) {
  h <- combine_hierarchical(rf_prob(m1, df), rf_prob(m2, df), rf_prob(m3, df))
  make_pred_table(df, h$pred, h$prob)
})
saveRDS(pred_rf, out_path("preds", "pred_hier_rf_no_surgery.rds"))
saveRDS(m1, out_path("models", "rf_stage1_drg_vs_nondrg_no_surgery.rds"))
saveRDS(m2, out_path("models", "rf_stage2_city_vs_regional_no_surgery.rds"))
saveRDS(m3, out_path("models", "rf_stage3_nondrg_no_surgery.rds"))
# Calibrator matched to the no-surgery model (fit only on 2022 Q4).
if (!requireNamespace("nnet", quietly = TRUE)) install.packages("nnet")
cal_no_surgery <- fit_multiclass_recalibration(prob_matrix(pred_rf$internal), pred_rf$internal$actual)
saveRDS(cal_no_surgery, out_path("models", "recalibration_hier_rf_no_surgery.rds"))
rm(m1, m2, m3); invisible(gc())
sens_list <- list("Hierarchical RF" = pred_rf)

# --- hierarchical CatBoost without sur_type -----------------------
if (isTRUE(CFG$sens_no_surgery_catboost)) {
  library(catboost)
  bp <- read_rds_out("models", "catboost_best_params.rds")
  c1 <- cb_train(train_main, "destination_broad", bp, class_weights = w1)
  c2 <- cb_train(sd$s2, "destination_drg", bp)
  c3 <- cb_train(sd$s3, "destination_non_drg", bp, class_weights = w3)
  pred_cb <- map(list(internal = val_internal, external = df_extval_2023), function(df) {
    h <- combine_hierarchical(cb_prob(c1, df, c("DRG", "NON_DRG")), cb_prob(c2, df, drg_classes),
                              cb_prob(c3, df, non_drg_classes))
    make_pred_table(df, h$pred, h$prob)
  })
  saveRDS(pred_cb, out_path("preds", "pred_hier_cb_no_surgery.rds"))
  sens_list[["Hierarchical CatBoost"]] <- pred_cb
  rm(c1, c2, c3)
}
sens_list
}, finally = { predictor_vars <- predictor_vars_main })   # always restore the full predictor set
stopifnot("sur_type" %in% predictor_vars)

# --- compare with main models -------------------------------------
main <- read_all_preds(names(sens))
res <- map_dfr(names(sens), function(model) map_dfr(c("internal", "external"), function(cohort) bind_rows(
  bootstrap_metrics(main[[model]][[cohort]]$actual, main[[model]][[cohort]]$pred, n_boot = 200) %>% mutate(version = "With surgery type (main)"),
  bootstrap_metrics(sens[[model]][[cohort]]$actual, sens[[model]][[cohort]]$pred, n_boot = 200) %>% mutate(version = "Without surgery type")) %>%
  mutate(model = model, cohort = cohort, .before = 1)))
print(res %>% filter(metric %in% c("accuracy", "balanced_accuracy", "macro_precision")), n = Inf, width = Inf)
save_table(res, "sens_no_surgery_type_performance.csv")

rm(train_main, val_internal, df_extval_2023, sens, main, sd); invisible(gc())
cat("Step 16 complete.\n")
