############################################################
# 06b_weighted_flat_models.R - FLAT COMPARATORS WITH CLASS WEIGHTS
# (comparators: is the gain from the hierarchy or from class weighting?)
#
#   Weighted flat RF       five classes, inverse-frequency case weights,
#                          tuned flat-RF hyperparameters (script 03), 1000 trees
#   Weighted flat CatBoost five classes, inverse-frequency class weights,
#                          tuned CatBoost hyperparameters (script 05)
#   Flat CatBoost          (optional, CFG$fit_unweighted_flat_catboost)
#
# The weights are the same inverse class-frequency weights
# n / (K * n_k) used in the hierarchical models, applied to the five
# destinations. Comparing weighted flat with hierarchical models
# separates the effect of the DRG/non-DRG structure from the effect
# of class weighting.
#
# Outputs:
#   output/predictions/pred_flat_rf_weighted.rds, pred_flat_cb_weighted.rds (, pred_flat_cb.rds)
#   output/models/rf_flat_weighted.rds, cb_flat_weighted.cbm
#   output/tables/importance_flat_rf_weighted.csv
#   output/tables/hierarchy_vs_weighting.csv  (decomposition of the gain)
############################################################

if (!exists("CFG")) source("00_config.R")
library(ranger)

coh <- read_rds_out("data", "cohorts.rds")
train_main <- coh$train_main; val_internal <- coh$val_internal; df_extval_2023 <- coh$df_extval_2023
rm(coh)

w5 <- inverse_freq_weights(train_main$referral_destination)
cat("Five-class inverse-frequency weights:\n"); print(round(w5, 3))

# --- Weighted flat RF ---------------------------------------------
best_params <- read_rds_out("models", "rf_best_params.rds")
rf_w <- fit_rf_tuned(train_main, "referral_destination", best_params,
                     weights = unname(w5[as.character(train_main$referral_destination)]))
print(rf_w)
saveRDS(rf_w, out_path("models", "rf_flat_weighted.rds"))

pred_rf_w <- map(list(internal = val_internal, external = df_extval_2023), function(df) {
  p <- rf_prob(rf_w, df)[, class_order]
  make_pred_table(df, prob_to_class(p, class_order), p)
})
saveRDS(pred_rf_w, out_path("preds", "pred_flat_rf_weighted.rds"))
save_table(tibble(variable = names(rf_w$variable.importance),
                  importance = as.numeric(rf_w$variable.importance)) %>%
             mutate(label = label_var(variable)) %>% arrange(desc(importance)),
           "importance_flat_rf_weighted.csv")
rm(rf_w); invisible(gc())

# --- Flat CatBoost (weighted; optionally unweighted) ---------------
library(catboost)
best_params_cb <- read_rds_out("models", "catboost_best_params.rds")

fit_flat_cb <- function(weights, model_file, pred_file) {
  m <- cb_train(train_main, "referral_destination", best_params_cb, class_weights = weights)
  catboost.save_model(m, out_path("models", model_file))
  pr <- map(list(internal = val_internal, external = df_extval_2023), function(df) {
    p <- cb_prob(m, df, class_order)
    make_pred_table(df, prob_to_class(p, class_order), p)
  })
  saveRDS(pr, out_path("preds", pred_file))
  pr
}
pred_cb_w <- fit_flat_cb(w5, "cb_flat_weighted.cbm", "pred_flat_cb_weighted.rds")
if (isTRUE(CFG$fit_unweighted_flat_catboost)) fit_flat_cb(NULL, "cb_flat.cbm", "pred_flat_cb.rds")

# --- Decomposition: weighting vs hierarchy ------------------------
preds <- read_all_preds(setdiff(model_names, "Naive baseline"))
perf <- imap_dfr(preds, function(p, model) {
  map_dfr(c("internal", "external"), function(cohort) {
    classification_metrics(p[[cohort]]$actual, p[[cohort]]$pred) %>%
      mutate(model = model, cohort = cohort, .before = 1)
  })
})
ba <- perf %>% select(model, cohort, balanced_accuracy) %>%
  pivot_wider(names_from = model, values_from = balanced_accuracy)
get_col <- function(df, nm) if (nm %in% names(df)) df[[nm]] else NA_real_
decomp <- ba %>% transmute(
  cohort,
  flat_rf = get_col(ba, "Flat RF"), weighted_flat_rf = get_col(ba, "Weighted flat RF"),
  hierarchical_rf = get_col(ba, "Hierarchical RF"),
  weighted_flat_catboost = get_col(ba, "Weighted flat CatBoost"),
  hierarchical_catboost = get_col(ba, "Hierarchical CatBoost"),
  gain_weighting_rf_pp       = 100 * (weighted_flat_rf - flat_rf),
  gain_hierarchy_rf_pp       = 100 * (hierarchical_rf - weighted_flat_rf),
  gain_hierarchy_catboost_pp = 100 * (hierarchical_catboost - weighted_flat_catboost),
  gain_total_catboost_vs_flat_rf_pp = 100 * (hierarchical_catboost - flat_rf)
)
print(perf, n = Inf, width = Inf); print(decomp, width = Inf)
save_table(perf, "flat_vs_hierarchical_performance.csv")
save_table(decomp, "hierarchy_vs_weighting.csv")

rm(train_main, val_internal, df_extval_2023, preds); invisible(gc())
cat("Step 06b complete.\n")
