############################################################
# 03_naive_and_flat_rf.R - MODEL 1 (NAIVE) AND MODEL 2 (FLAT RF)
#
# Model 1 - Naive baseline: approximation of current referral
#   practice. The modal destination for each referring facility
#   level in the TRAINING data is predicted for everyone at that
#   level (no patient or clinical information). Unseen levels get
#   the overall training mode.
#
# Model 2 - Flat random forest: classifies directly into the five
#   destination categories. Hyperparameters (mtry, min.node.size,
#   sample.fraction) are chosen by grid search on a training
#   subsample using out-of-bag BALANCED ACCURACY, then the best
#   configuration is refitted on the full training data with
#   1000 trees. These tuned values are reused for every stage of
#   the hierarchical RF (script 04).
#
# Outputs:
#   output/models/rf_flat.rds, rf_best_params.rds
#   output/predictions/pred_naive.rds, pred_flat_rf.rds
#   output/tables/naive_baseline_lookup.csv, rf_tuning_grid_results.csv,
#                 importance_flat_rf.csv
############################################################

if (!exists("CFG")) source("00_config.R")
library(ranger)

coh <- read_rds_out("data", "cohorts.rds")
train_main <- coh$train_main; val_internal <- coh$val_internal; df_extval_2023 <- coh$df_extval_2023
rm(coh)


############################################################
# 1. NAIVE BASELINE
############################################################

naive_lookup <- train_main %>%
  count(refer_level, referral_destination, name = "n") %>%
  group_by(refer_level) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(refer_level, naive_prediction = referral_destination)

overall_mode <- train_main %>%
  count(referral_destination, name = "n") %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  pull(referral_destination) %>% as.character()

predict_naive <- function(df) {
  df %>%
    select(refer_level) %>%
    left_join(naive_lookup %>% mutate(naive_prediction = as.character(naive_prediction)), by = "refer_level") %>%
    mutate(naive_prediction = coalesce(naive_prediction, overall_mode)) %>%
    pull(naive_prediction) %>%
    factor(levels = class_order)
}

print(naive_lookup)
cat("Overall fallback mode:", overall_mode, "\n")
save_table(naive_lookup, "naive_baseline_lookup.csv")

pred_naive <- list(
  internal = make_pred_table(val_internal,   predict_naive(val_internal)),
  external = make_pred_table(df_extval_2023, predict_naive(df_extval_2023))
)
saveRDS(pred_naive, out_path("preds", "pred_naive.rds"))
saveRDS(list(lookup = naive_lookup, overall_mode = overall_mode), out_path("models", "naive_baseline.rds"))

cat("\nNaive - internal:\n"); print(classification_metrics(pred_naive$internal$actual, pred_naive$internal$pred))
cat("Naive - external:\n");   print(classification_metrics(pred_naive$external$actual, pred_naive$external$pred))


############################################################
# 2. RF HYPERPARAMETER TUNING (OOB balanced accuracy, subsample)
############################################################

formula_flat <- model_formula("referral_destination")
p <- length(predictor_vars)

tune_n <- min(round(nrow(train_main) * CFG$tune_subsample_frac), CFG$tune_subsample_max_n)
set.seed(CFG$seed)
train_tune <- train_main %>% slice_sample(n = tune_n)

tuning_grid <- expand_grid(
  mtry            = unique(pmax(2, round(c(sqrt(p) - 2, sqrt(p), sqrt(p) + 2, p / 3)))),
  min_node_size   = CFG$rf_min_node_grid,
  sample_fraction = CFG$rf_sample_frac_grid
)
cat("\nRF tuning:", nrow(tuning_grid), "configurations on", tune_n, "training rows\n")

tuning_results <- pmap_dfr(tuning_grid, function(mtry, min_node_size, sample_fraction) {
  fit <- ranger(formula_flat, data = train_tune, num.trees = CFG$rf_tune_trees,
                mtry = mtry, min.node.size = min_node_size, sample.fraction = sample_fraction,
                probability = TRUE, respect.unordered.factors = "order",
                importance = "none", seed = CFG$seed)
  m <- classification_metrics(train_tune$referral_destination,
                              prob_to_class(fit$predictions, levels(train_tune$referral_destination)))
  tibble(mtry, min_node_size, sample_fraction,
         oob_accuracy = m$accuracy, oob_balanced_accuracy = m$balanced_accuracy, oob_macro_F1 = m$macro_F1)
}) %>% arrange(desc(oob_balanced_accuracy))

print(tuning_results, n = Inf)
save_table(tuning_results, "rf_tuning_grid_results.csv")

best_params <- tuning_results %>% slice(1)
saveRDS(best_params, out_path("models", "rf_best_params.rds"))
cat("Selected: mtry =", best_params$mtry, "| min.node.size =", best_params$min_node_size,
    "| sample.fraction =", best_params$sample_fraction, "\n")


############################################################
# 3. FINAL FLAT RF (full training data, 1000 trees)
############################################################

rf_flat <- ranger(formula_flat, data = train_main, num.trees = CFG$rf_final_trees,
                  mtry = best_params$mtry, min.node.size = best_params$min_node_size,
                  sample.fraction = best_params$sample_fraction,
                  importance = "permutation", probability = TRUE,
                  respect.unordered.factors = "order", seed = CFG$seed)
print(rf_flat)
saveRDS(rf_flat, out_path("models", "rf_flat.rds"))

prob_int <- rf_prob(rf_flat, val_internal)[, class_order]
prob_ext <- rf_prob(rf_flat, df_extval_2023)[, class_order]

pred_flat_rf <- list(
  internal = make_pred_table(val_internal,   prob_to_class(prob_int, class_order), prob_int),
  external = make_pred_table(df_extval_2023, prob_to_class(prob_ext, class_order), prob_ext)
)
saveRDS(pred_flat_rf, out_path("preds", "pred_flat_rf.rds"))

cat("\nFlat RF - internal:\n"); print(classification_metrics(pred_flat_rf$internal$actual, pred_flat_rf$internal$pred))
cat("Flat RF - external:\n");   print(classification_metrics(pred_flat_rf$external$actual, pred_flat_rf$external$pred))

save_table(
  tibble(variable = names(rf_flat$variable.importance),
         importance = as.numeric(rf_flat$variable.importance)) %>%
    mutate(label = label_var(variable)) %>% arrange(desc(importance)),
  "importance_flat_rf.csv"
)

rm(rf_flat, train_tune, prob_int, prob_ext); invisible(gc())
cat("Step 03 complete.\n")
