############################################################
# 06_hierarchical_catboost.R - MODEL 4: HIERARCHICAL CATBOOST
#
# Identical three-stage structure, training data, class weights
# and probability combination as the hierarchical RF (script 04),
# fitted with CatBoost (native categorical features, no one-hot
# encoding) using the tuned hyperparameters from script 05 in all
# three stages.
#
# Everything that needs a trained CatBoost model (predictions,
# feature importance) is computed HERE and saved as plain R
# objects, because CatBoost model/pool objects (external C++
# pointers) do not reliably survive saveRDS()/load() across R
# sessions. Models are also saved in CatBoost's own .cbm format.
#
# Outputs:
#   output/models/cb_stage1_drg_vs_nondrg.cbm, cb_stage2_city_vs_regional.cbm,
#                 cb_stage3_nondrg.cbm
#   output/predictions/pred_hier_cb.rds
#   output/tables/importance_hier_cb.csv
############################################################

if (!exists("CFG")) source("00_config.R")
library(catboost)

coh <- read_rds_out("data", "cohorts.rds")
train_main <- coh$train_main; val_internal <- coh$val_internal; df_extval_2023 <- coh$df_extval_2023
rm(coh)
best_params_cb <- read_rds_out("models", "catboost_best_params.rds")
cat("CatBoost hyperparameters: iterations =", best_params_cb$iterations, "| depth =",
    best_params_cb$depth, "| learning_rate =", best_params_cb$learning_rate, "\n")

# --- Stage 1 ----------------------------------------------------
cb_stage1 <- cb_train(train_main, "destination_broad", best_params_cb,
                      class_weights = inverse_freq_weights(train_main$destination_broad))

# --- Stage 2 ----------------------------------------------------
train_stage2 <- train_main %>%
  filter(referral_destination %in% drg_classes) %>%
  mutate(destination_drg = factor(as.character(referral_destination), levels = drg_classes))
cb_stage2 <- cb_train(train_stage2, "destination_drg", best_params_cb)

# --- Stage 3 ----------------------------------------------------
train_stage3 <- train_main %>%
  filter(referral_destination %in% non_drg_classes) %>%
  mutate(destination_non_drg = factor(as.character(referral_destination), levels = non_drg_classes))
cb_stage3 <- cb_train(train_stage3, "destination_non_drg", best_params_cb,
                      class_weights = inverse_freq_weights(train_stage3$destination_non_drg))

catboost.save_model(cb_stage1, out_path("models", "cb_stage1_drg_vs_nondrg.cbm"))
catboost.save_model(cb_stage2, out_path("models", "cb_stage2_city_vs_regional.cbm"))
catboost.save_model(cb_stage3, out_path("models", "cb_stage3_nondrg.cbm"))

# --- Combined hierarchical predictions ---------------------------
predict_hier_cb <- function(df) {
  p1 <- cb_prob(cb_stage1, df, c("DRG", "NON_DRG"))
  p2 <- cb_prob(cb_stage2, df, drg_classes)
  p3 <- cb_prob(cb_stage3, df, non_drg_classes)
  h  <- combine_hierarchical(p1, p2, p3)
  stopifnot(max(abs(rowSums(h$prob) - 1)) < 1e-6)
  make_pred_table(df, h$pred, h$prob,
                  extra = tibble(stage1_actual   = df$destination_broad,
                                 stage1_p_nondrg = p1[, "NON_DRG"]))
}

pred_hier_cb <- list(internal = predict_hier_cb(val_internal), external = predict_hier_cb(df_extval_2023))
saveRDS(pred_hier_cb, out_path("preds", "pred_hier_cb.rds"))

cat("\nHierarchical CatBoost - internal:\n"); print(classification_metrics(pred_hier_cb$internal$actual, pred_hier_cb$internal$pred))
cat("Hierarchical CatBoost - external:\n");   print(classification_metrics(pred_hier_cb$external$actual, pred_hier_cb$external$pred))

# --- Feature importance (PredictionValuesChange) -----------------
cb_imp <- function(model, df, target, stage) {
  v <- catboost.get_feature_importance(model, build_pool(df, label = df[[target]]))
  tibble(model = "CatBoost", stage = stage, variable = predictor_vars, importance = as.numeric(v))
}
save_table(bind_rows(cb_imp(cb_stage1, train_main,   "destination_broad",   "Stage 1"),
                     cb_imp(cb_stage2, train_stage2, "destination_drg",     "Stage 2"),
                     cb_imp(cb_stage3, train_stage3, "destination_non_drg", "Stage 3")) %>%
             mutate(label = label_var(variable)),
           "importance_hier_cb.csv")

rm(cb_stage1, cb_stage2, cb_stage3, train_stage2, train_stage3); invisible(gc())
cat("Step 06 complete.\n")
