############################################################
# 19_tableS2_figureS1.R - SUPPLEMENTARY TABLE S2 AND FIGURE S1
#
# Table S2: class-specific precision, recall, F1 (95% CI from a
#   multinomial bootstrap of the confusion matrix, 2,000 resamples)
#   and one-vs-rest AUC (95% DeLong CI) for the hierarchical models,
#   external validation (2023).
# Figure S1: ROC curves, external validation (2023): stage 1 (DRG vs
#   non-DRG) and one-vs-rest curves for the five destinations,
#   hierarchical CatBoost (A, B) and hierarchical RF (C, D).
#
# Uses the uncalibrated hierarchical predictions (as Tables 3-4).
# Outputs: output/tables/tableS2_perclass_external.csv
#          output/figures/figureS1_roc_external.png
# Run time: about 1-2 minutes.
############################################################

if (!exists("CFG")) source("00_config.R")
library(pROC)
library(patchwork)

models <- c("Hierarchical CatBoost" = "pred_hier_cb.rds", "Hierarchical RF" = "pred_hier_rf.rds")
n_boot <- 2000

fmt <- function(e, l, u) sprintf("%.3f (%.3f-%.3f)", e, l, u)

perclass <- function(cm) {
  tp <- diag(cm); prec <- tp / colSums(cm); rec <- tp / rowSums(cm)
  f1 <- 2 * prec * rec / (prec + rec)
  cbind(precision = prec, recall = rec, f1 = f1)
}

tab <- list(); roc_rows <- list()
for (m in names(models)) {
  d <- read_rds_out("preds", models[[m]])$external
  cm <- unclass(confusion(d$actual, d$pred))
  point <- perclass(cm)
  # multinomial bootstrap of the confusion-matrix cells
  set.seed(CFG$seed)
  probs <- as.vector(cm) / sum(cm)
  boot <- replicate(n_boot, {
    b <- matrix(rmultinom(1, sum(cm), probs), nrow(cm))
    perclass(b)
  })
  for (k in seq_along(class_order)) {
    cl <- class_order[k]
    ci <- apply(boot[k, , , drop = FALSE], 2, quantile, c(0.025, 0.975), na.rm = TRUE)
    y <- as.integer(d$actual == cl)
    r <- roc(y, d[[cl]], levels = c(0, 1), direction = "<", quiet = TRUE)
    a <- as.numeric(ci.auc(r, method = "delong"))
    tab[[length(tab) + 1]] <- tibble(
      Model = m, Class = cl, `Observed n` = format(sum(y), big.mark = ","),
      Precision = fmt(point[k, "precision"], ci[1, 1], ci[2, 1]),
      Recall    = fmt(point[k, "recall"],    ci[1, 2], ci[2, 2]),
      F1        = fmt(point[k, "f1"],        ci[1, 3], ci[2, 3]),
      AUC       = fmt(a[2], a[1], a[3]),
      auc = a[2])
    keep <- unique(round(seq(1, length(r$specificities), length.out = min(2000, length(r$specificities)))))
    roc_rows[[length(roc_rows) + 1]] <- tibble(model = m, curve = cl, fpr = 1 - r$specificities[keep],
                                               tpr = r$sensitivities[keep], auc = a[2])
  }
  r1 <- roc(as.integer(d$stage1_actual == "NON_DRG"), d$stage1_p_nondrg, levels = c(0, 1), direction = "<", quiet = TRUE)
  keep <- unique(round(seq(1, length(r1$specificities), length.out = min(2000, length(r1$specificities)))))
  roc_rows[[length(roc_rows) + 1]] <- tibble(model = m, curve = "Stage 1 (non-DRG vs DRG)",
                                             fpr = 1 - r1$specificities[keep], tpr = r1$sensitivities[keep],
                                             auc = as.numeric(auc(r1)))
}
tab <- bind_rows(tab)
print(tab, width = Inf)
cat("\nOne-vs-rest AUC range:\n")
print(tab %>% group_by(Model) %>% summarise(min_auc = round(min(auc), 3), max_auc = round(max(auc), 3)))
save_table(tab %>% select(-auc), "tableS2_perclass_external.csv")

rd <- bind_rows(roc_rows)
panel <- function(m, stage1, title) {
  x <- rd %>% filter(model == m, if (stage1) curve == "Stage 1 (non-DRG vs DRG)" else curve != "Stage 1 (non-DRG vs DRG)")
  lab <- x %>% distinct(curve, auc) %>% mutate(curve_lab = sprintf("%s (AUC %.3f)", curve, auc))
  x <- x %>% left_join(lab %>% select(curve, curve_lab), by = "curve")
  ggplot(x, aes(fpr, tpr, colour = curve_lab)) +
    geom_abline(linetype = "dashed", colour = "grey60") + geom_path(linewidth = 0.7) +
    coord_equal() + theme_ms(10) + theme(legend.position = c(0.62, 0.22), legend.title = element_blank(),
                                         legend.background = element_rect(fill = "white", colour = NA)) +
    labs(title = title, x = "1 - specificity", y = "Sensitivity")
}
fig <- (panel("Hierarchical CatBoost", TRUE,  "A. Hierarchical CatBoost: stage 1") |
        panel("Hierarchical CatBoost", FALSE, "B. Hierarchical CatBoost: one-vs-rest")) /
       (panel("Hierarchical RF", TRUE,  "C. Hierarchical RF: stage 1") |
        panel("Hierarchical RF", FALSE, "D. Hierarchical RF: one-vs-rest"))
save_fig(fig, "figureS1_roc_external.png", width = 12, height = 12)
cat("Step 19 complete.\n")
