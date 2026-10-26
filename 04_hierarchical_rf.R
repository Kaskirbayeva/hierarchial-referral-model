############################################################
# 04_hierarchical_rf.R - MODEL 3: HIERARCHICAL RANDOM FOREST
#
#                   REFERRAL DESTINATION
#                           |
#          Stage 1: DRG vs NON-DRG (class-weighted)
#              |                         |
#     Stage 2: City vs Regional   Stage 3: Republican/National vs
#     (trained on DRG cases)      Rural/District vs Specialised
#                                 (trained on NON-DRG cases,
#                                  class-weighted)
#
# Final P(class) = P(stage-1 branch) x P(class | branch), computed
# for EVERY observation from the predicted stage-1 probability,
# so stage-1 errors propagate exactly as they would in practice.
# The actual destination is never used to choose stage 2 or 3.
#
# Same tuned hyperparameters (script 03) and 1000 trees in all
# three stages. Inverse-frequency class weights are applied in
# Stage 1 (and in Stage 3, where Republican/National dominates).
#
# Outputs:
#   output/models/rf_stage1_drg_vs_nondrg.rds, rf_stage2_city_vs_regional.rds,
#                 rf_stage3_nondrg.rds
#   output/predictions/pred_hier_rf.rds  (final + stage probabilities)
#   output/tables/importance_hier_rf.csv
############################################################

if (!exists("CFG")) source("00_config.R")
library(ranger)

coh <- read_rds_out("data", "cohorts.rds")
train_main <- coh$train_main; val_internal <- coh$val_internal; df_extval_2023 <- coh$df_extval_2023
rm(coh)
best_params <- read_rds_out("models", "rf_best_params.rds")

fit_rf_stage <- function(data, target, weights = NULL) {
  ranger(model_formula(target), data = data, num.trees = CFG$rf_final_trees,
         mtry = best_params$mtry, min.node.size = best_params$min_node_size,
         sample.fraction = best_params$sample_fraction,
         case.weights = weights, importance = "permutation", probability = TRUE,
         respect.unordered.factors = "order", seed = CFG$seed)
}

# --- Stage 1: DRG vs NON-DRG ----------------------------------
w1 <- inverse_freq_weights(train_main$destination_broad)
cat("\nStage 1 class weights:\n"); print(w1)
rf_stage1 <- fit_rf_stage(train_main, "destination_broad",
                          weights = unname(w1[as.character(train_main$destination_broad)]))
print(rf_stage1)

# --- Stage 2: City vs Regional (DRG training cases) -----------
train_stage2 <- train_main %>%
  filter(referral_destination %in% drg_classes) %>%
  mutate(destination_drg = factor(as.character(referral_destination), levels = drg_classes))
rf_stage2 <- fit_rf_stage(train_stage2, "destination_drg")
print(rf_stage2)

# --- Stage 3: non-DRG destinations (NON-DRG training cases) ---
train_stage3 <- train_main %>%
  filter(referral_destination %in% non_drg_classes) %>%
  mutate(destination_non_drg = factor(as.character(referral_destination), levels = non_drg_classes))
w3 <- inverse_freq_weights(train_stage3$destination_non_drg)
cat("\nStage 3 class weights:\n"); print(w3)
rf_stage3 <- fit_rf_stage(train_stage3, "destination_non_drg",
                          weights = unname(w3[as.character(train_stage3$destination_non_drg)]))
print(rf_stage3)

saveRDS(rf_stage1, out_path("models", "rf_stage1_drg_vs_nondrg.rds"))
saveRDS(rf_stage2, out_path("models", "rf_stage2_city_vs_regional.rds"))
saveRDS(rf_stage3, out_path("models", "rf_stage3_nondrg.rds"))

# --- Combined hierarchical predictions -------------------------
predict_hier_rf <- function(df) {
  p1 <- rf_prob(rf_stage1, df); p2 <- rf_prob(rf_stage2, df); p3 <- rf_prob(rf_stage3, df)
  h  <- combine_hierarchical(p1, p2, p3)
  stopifnot(max(abs(rowSums(h$prob) - 1)) < 1e-6)
  make_pred_table(df, h$pred, h$prob,
                  extra = tibble(stage1_actual   = df$destination_broad,
                                 stage1_p_nondrg = p1[, "NON_DRG"]))
}

pred_hier_rf <- list(internal = predict_hier_rf(val_internal), external = predict_hier_rf(df_extval_2023))
saveRDS(pred_hier_rf, out_path("preds", "pred_hier_rf.rds"))

cat("\nHierarchical RF - internal:\n"); print(classification_metrics(pred_hier_rf$internal$actual, pred_hier_rf$internal$pred))
cat("Hierarchical RF - external:\n");   print(classification_metrics(pred_hier_rf$external$actual, pred_hier_rf$external$pred))

# --- Permutation importance (stage 1 and stage 3) --------------
imp <- function(m, stage) tibble(model = "Random Forest", stage = stage,
                                 variable = names(m$variable.importance),
                                 importance = as.numeric(m$variable.importance))
save_table(bind_rows(imp(rf_stage1, "Stage 1"), imp(rf_stage2, "Stage 2"), imp(rf_stage3, "Stage 3")) %>%
             mutate(label = label_var(variable)),
           "importance_hier_rf.csv")

rm(rf_stage1, rf_stage2, rf_stage3, train_stage2, train_stage3); invisible(gc())
cat("Step 04 complete.\n")
