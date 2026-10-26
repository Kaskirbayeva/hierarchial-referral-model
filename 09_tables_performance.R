############################################################
# 09_tables_performance.R - MODEL PERFORMANCE TABLES
#
#   Table 3  Per-category recall by destination, model, cohort
#   Table 4  Overall performance by cohort (95% bootstrap CI):
#            accuracy, balanced accuracy, macro precision,
#            macro recall, macro-F1
#   Confusion matrices for every model x cohort (+ figure)
#   Table S2 / Figure S4  Stage 1 (DRG vs non-DRG) performance
#            incl. AUC (95% DeLong CI) and ROC curves
#
# Uses only the saved predictions (scripts 03, 04, 06).
############################################################

if (!exists("CFG")) source("00_config.R")
library(pROC)

preds <- list(
  "Naive baseline"        = read_rds_out("preds", "pred_naive.rds"),
  "Flat RF"               = read_rds_out("preds", "pred_flat_rf.rds"),
  "Hierarchical RF"       = read_rds_out("preds", "pred_hier_rf.rds"),
  "Hierarchical CatBoost" = read_rds_out("preds", "pred_hier_cb.rds")
)
combos <- expand_grid(model = names(preds), cohort = c("external", "internal"))


############################################################
# TABLE 3 - PER-CATEGORY RECALL
############################################################

recall_long <- pmap_dfr(combos, function(model, cohort) {
  d <- preds[[model]][[cohort]]
  per_class_recall(d$actual, d$pred) %>% mutate(model = model, cohort = cohort, .before = 1)
})
save_table(recall_long, "per_class_recall_long.csv")

table3 <- recall_long %>%
  mutate(row = paste0(model, ", ", cohort_labels[cohort]),
         model = factor(model, levels = model_names),
         cohort = factor(cohort, levels = c("external", "internal"))) %>%
  arrange(model, cohort) %>%
  select(row, class, recall) %>%
  mutate(recall = sprintf("%.3f", recall)) %>%
  pivot_wider(names_from = class, values_from = recall) %>%
  select(`Model, cohort` = row, all_of(class_order))

print(table3, width = Inf)
save_table(table3, "table3_per_category_recall.csv")


############################################################
# TABLE 4 - OVERALL PERFORMANCE WITH 95% BOOTSTRAP CI
############################################################

cat("\nBootstrapping 95% CIs (", CFG$n_boot, "resamples per model x cohort) ...\n")
perf_ci <- pmap_dfr(combos, function(model, cohort) {
  d <- preds[[model]][[cohort]]
  bootstrap_metrics(d$actual, d$pred) %>% mutate(model = model, cohort = cohort, .before = 1)
})
save_table(perf_ci, "model_performance_bootstrap_long.csv")

table4 <- perf_ci %>%
  mutate(value = fmt_ci(estimate, lower, upper)) %>%
  select(model, cohort, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(model = factor(model, levels = model_names),
         cohort = factor(cohort, levels = c("internal", "external"))) %>%
  arrange(model, cohort) %>%
  transmute(Model = as.character(model),
            Evaluation = if_else(cohort == "internal", "Internal", "External"),
            Accuracy = accuracy, `Balanced accuracy` = balanced_accuracy,
            `Macro - F1` = macro_F1, `Macro precision` = macro_precision, `Macro recall` = macro_recall)

print(table4, width = Inf)
save_table(table4, "table4_overall_performance_ci.csv")

# internal -> external change (temporal drift)
drift <- perf_ci %>%
  select(model, cohort, metric, estimate) %>%
  pivot_wider(names_from = cohort, values_from = estimate) %>%
  mutate(drop_internal_to_external = internal - external)
save_table(drift, "performance_drift.csv")


############################################################
# CONFUSION MATRICES
############################################################

cm_long <- pmap_dfr(combos, function(model, cohort) {
  d <- preds[[model]][[cohort]]
  as_tibble(confusion(d$actual, d$pred), n = "n") %>%
    group_by(Observed) %>% mutate(pct_of_observed = 100 * n / sum(n)) %>% ungroup() %>%
    mutate(model = model, cohort = cohort, .before = 1)
})
save_table(cm_long, "confusion_matrices_all_models.csv")

cm_plot <- cm_long %>%
  filter(cohort == "external", model %in% c("Hierarchical RF", "Hierarchical CatBoost")) %>%
  mutate(Observed = factor(Observed, levels = rev(class_order)),
         Predicted = factor(Predicted, levels = class_order),
         model = factor(model, levels = model_names)) %>%
  ggplot(aes(Predicted, Observed, fill = pct_of_observed)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%s\n(%.1f%%)", scales::comma(n), pct_of_observed)), size = 2.9, lineheight = 0.9) +
  scale_fill_gradient(low = "white", high = "grey35", limits = c(0, 100), name = "% of observed\ndestination") +
  facet_wrap(~ model) +
  theme_ms(11) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "right") +
  labs(title = "Confusion matrices, external validation (2023)",
       x = "Predicted destination", y = "Observed destination")
save_fig(cm_plot, "figure_confusion_matrices_hierarchical_external.png", width = 13, height = 6)


############################################################
# TABLE S2 / FIGURE S4 - STAGE 1 (DRG vs NON-DRG) PERFORMANCE
############################################################

stage1_models <- c("Hierarchical RF" = "Random Forest", "Hierarchical CatBoost" = "CatBoost")

stage1_tab <- pmap_dfr(expand_grid(model = names(stage1_models), cohort = c("internal", "external")),
  function(model, cohort) {
    d <- preds[[model]][[cohort]]
    pred1 <- factor(if_else(d$stage1_p_nondrg >= 0.5, "NON_DRG", "DRG"), levels = c("DRG", "NON_DRG"))
    m <- classification_metrics(d$stage1_actual, pred1)
    r <- roc(response = as.integer(d$stage1_actual == "NON_DRG"), predictor = d$stage1_p_nondrg,
             levels = c(0, 1), direction = "<", quiet = TRUE)
    ci <- as.numeric(ci.auc(r, method = "delong"))
    cm <- table(d$stage1_actual, pred1)
    tibble(model = stage1_models[[model]], cohort = cohort_labels[[cohort]],
           accuracy = m$accuracy, balanced_accuracy = m$balanced_accuracy,
           sensitivity_nondrg = cm["NON_DRG", "NON_DRG"] / sum(cm["NON_DRG", ]),
           specificity_nondrg = cm["DRG", "DRG"] / sum(cm["DRG", ]),
           macro_F1 = m$macro_F1, auc = ci[2], auc_lower = ci[1], auc_upper = ci[3],
           `AUC (95% CI)` = fmt_ci(ci[2], ci[1], ci[3]))
  })
print(stage1_tab, width = Inf)
save_table(stage1_tab, "tableS2_stage1_performance_auc.csv")

roc_df <- pmap_dfr(expand_grid(model = names(stage1_models), cohort = c("internal", "external")),
  function(model, cohort) {
    d <- preds[[model]][[cohort]]
    r <- roc(as.integer(d$stage1_actual == "NON_DRG"), d$stage1_p_nondrg, levels = c(0, 1), direction = "<", quiet = TRUE)
    keep <- unique(round(seq(1, length(r$specificities), length.out = min(3000, length(r$specificities)))))
    tibble(fpr = 1 - r$specificities[keep], tpr = r$sensitivities[keep],
           model = stage1_models[[model]], auc = as.numeric(auc(r)),
           cohort = cohort_labels[[cohort]])
  })

auc_labels <- roc_df %>% distinct(model, cohort, auc) %>%
  group_by(cohort) %>% mutate(y = 0.05 + 0.07 * (row_number() - 1)) %>% ungroup()

roc_plot <- ggplot(roc_df, aes(fpr, tpr, colour = model)) +
  geom_abline(linetype = "dashed", colour = "grey60") +
  geom_path(linewidth = 0.8) +
  geom_text(data = auc_labels, aes(x = 0.98, y = y, label = sprintf("%s AUC = %.3f", model, auc)),
            hjust = 1, size = 3.3, show.legend = FALSE) +
  facet_wrap(~ cohort) +
  coord_equal() +
  scale_colour_manual(values = model_colours) +
  theme_ms() +
  labs(title = "Stage 1 ROC curves: non-DRG vs DRG-funded destination",
       x = "1 - specificity", y = "Sensitivity", colour = NULL)
save_fig(roc_plot, "figureS4_stage1_roc.png", width = 11, height = 6)

rm(preds); invisible(gc())
cat("Step 09 complete.\n")
