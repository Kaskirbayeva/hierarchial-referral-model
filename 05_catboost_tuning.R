############################################################
# 05_catboost_tuning.R - CATBOOST HYPERPARAMETER GRID SEARCH
#
# Mirrors the RF tuning: grid over iterations x depth x
# learning rate on the same-sized training subsample, selected
# by BALANCED ACCURACY. CatBoost has no out-of-bag estimate, so
# candidates are evaluated on a held-out validation split
# (30%) within the subsample. Tuned on Stage 1 (DRG vs NON-DRG,
# class-weighted); the selected values are then used for all
# three hierarchical stages (script 06).
#
# Outputs:
#   output/models/catboost_best_params.rds
#   output/tables/catboost_tuning_grid_results.csv
############################################################

if (!exists("CFG")) source("00_config.R")
library(catboost)

train_main <- read_rds_out("data", "cohorts.rds")$train_main

set.seed(CFG$seed)
tune_n <- min(round(nrow(train_main) * CFG$tune_subsample_frac), CFG$tune_subsample_max_n)
train_tune_cb <- train_main %>% slice_sample(n = tune_n)
inner_idx   <- sample(seq_len(nrow(train_tune_cb)), size = round(CFG$cb_inner_train_frac * nrow(train_tune_cb)))
inner_train <- train_tune_cb[inner_idx, ]
inner_val   <- train_tune_cb[-inner_idx, ]
cat("CatBoost tuning:", nrow(inner_train), "inner-train /", nrow(inner_val), "inner-validation rows\n")

tuning_grid_cb <- expand_grid(
  iterations    = CFG$cb_iterations_grid,
  depth         = CFG$cb_depth_grid,
  learning_rate = CFG$cb_learning_rate_grid
)

w_inner <- inverse_freq_weights(inner_train$destination_broad)

tuning_results_cb <- pmap_dfr(tuning_grid_cb, function(iterations, depth, learning_rate) {
  fit <- cb_train(inner_train, "destination_broad",
                  params = list(iterations = iterations, depth = depth, learning_rate = learning_rate),
                  class_weights = w_inner, verbose = 0)
  pred <- prob_to_class(cb_prob(fit, inner_val, c("DRG", "NON_DRG")))
  m <- classification_metrics(inner_val$destination_broad, pred)
  cat(sprintf("  iter=%d depth=%d lr=%.2f  BA=%.4f\n", iterations, depth, learning_rate, m$balanced_accuracy))
  tibble(iterations, depth, learning_rate,
         val_accuracy = m$accuracy, val_balanced_accuracy = m$balanced_accuracy, val_macro_F1 = m$macro_F1)
}) %>% arrange(desc(val_balanced_accuracy))

print(tuning_results_cb, n = Inf)
save_table(tuning_results_cb, "catboost_tuning_grid_results.csv")

best_params_cb <- tuning_results_cb %>% slice(1)
saveRDS(best_params_cb, out_path("models", "catboost_best_params.rds"))
cat("Selected: iterations =", best_params_cb$iterations, "| depth =", best_params_cb$depth,
    "| learning_rate =", best_params_cb$learning_rate, "\n")

rm(train_main, train_tune_cb, inner_train, inner_val); invisible(gc())
cat("Step 05 complete.\n")
