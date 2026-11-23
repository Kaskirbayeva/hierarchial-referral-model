############################################################
# 09b_precision_recall.R - PER-CLASS PRECISION, PR CURVES AND
# COMPARISON AT MATCHED MACRO-PRECISION
#
# A flat model can trade precision for recall by shifting its
# decision rule, so the hierarchical and flat models are compared:
#   (a) per-class precision and recall at the default rule (argmax)
#   (b) one-vs-rest precision-recall curves and average precision
#   (c) along an "operating curve": class probabilities are reweighted
#       by w^alpha (w = inverse training frequency; alpha = 0 is the
#       fitted model, alpha > 0 favours rarer destinations) and the
#       balanced accuracy of each flat model is read off at the
#       macro-precision of each hierarchical model.
#
# Outputs (output/tables, output/figures):
#   per_class_precision_recall.csv
#   pr_average_precision.csv, figure_pr_curves_external.png
#   operating_points_external.csv, figure_operating_curves_external.png
#   matched_macro_precision_comparison.csv
############################################################

if (!exists("CFG")) source("00_config.R")

preds <- read_all_preds()
train_y <- read_rds_out("data", "cohorts.rds")$train_main$referral_destination
w5 <- inverse_freq_weights(train_y)

# (a) per-class precision / recall --------------------------------
pcpr <- imap_dfr(preds, function(p, model) map_dfr(c("internal", "external"), function(cohort)
  per_class_pr(p[[cohort]]$actual, p[[cohort]]$pred) %>% mutate(model = model, cohort = cohort, .before = 1)))
save_table(pcpr, "per_class_precision_recall.csv")
print(pcpr %>% filter(cohort == "external") %>% select(model, class, recall, precision) %>%
        pivot_wider(names_from = class, values_from = c(recall, precision)), width = Inf)

prob_models <- preds[map_lgl(preds, ~ has_probs(.x$external))]

# (b) PR curves ---------------------------------------------------
pr <- imap(prob_models, function(p, model) {
  d <- p$external
  map(set_names(class_order), function(cl) pr_curve_ovr(d[[cl]], as.integer(d$actual == cl)))
})
ap_tab <- imap_dfr(pr, function(x, model) imap_dfr(x, ~ tibble(model = model, class = .y, average_precision = .x$ap,
                                                              prevalence = NA_real_)))
prev <- prob_models[[1]]$external %>% count(actual) %>% mutate(prevalence = n / sum(n)) %>% select(class = actual, prevalence) %>%
  mutate(class = as.character(class))
ap_tab <- ap_tab %>% select(-prevalence) %>% left_join(prev, by = "class")
print(ap_tab %>% pivot_wider(names_from = class, values_from = average_precision, id_cols = model), width = Inf)
save_table(ap_tab, "pr_average_precision.csv")

pr_df <- imap_dfr(pr, function(x, model) imap_dfr(x, ~ .x$curve %>% mutate(model = model, class = .y)))
fig_pr <- ggplot(pr_df, aes(recall, precision, colour = model)) +
  geom_path(linewidth = 0.7) +
  facet_wrap(~ factor(class, levels = class_order), nrow = 2) +
  scale_colour_manual(values = model_colours) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) + theme_ms(11) +
  labs(title = "One-vs-rest precision-recall curves by destination, external validation (2023)",
       x = "Recall", y = "Precision", colour = NULL)
save_fig(fig_pr, "figure_pr_curves_external.png", width = 12, height = 8)

# (c) operating curves and matched macro-precision ----------------
ops <- imap_dfr(prob_models, function(p, model)
  operating_points(prob_matrix(p$external), p$external$actual, w5, CFG$pr_alpha_grid) %>% mutate(model = model, .before = 1))
save_table(ops, "operating_points_external.csv")

fitted_pts <- ops %>% filter(abs(alpha) < 1e-9)
fig_op <- ggplot(ops, aes(macro_precision, balanced_accuracy, colour = model)) +
  geom_path(linewidth = 0.5, alpha = 0.7) +
  geom_point(data = fitted_pts, size = 3) +
  scale_colour_manual(values = model_colours) + theme_ms() +
  labs(title = "Balanced accuracy versus macro-precision across decision rules",
       subtitle = "External validation (2023). Points = models as fitted (argmax); lines = class probabilities reweighted by w^alpha",
       x = "Macro-precision", y = "Balanced accuracy", colour = NULL)
save_fig(fig_op, "figure_operating_curves_external.png", width = 9, height = 6.5)

# Best balanced accuracy a comparator reaches at (approximately) the same
# macro-precision as the reference model: max over operating points within
# +/- tol of the target. The operating curves are not monotone, so this is
# the comparator's most favourable point (conservative for the hierarchy).
ba_at_precision <- function(curve, target, tol = 0.01) {
  near <- curve %>% filter(abs(macro_precision - target) <= tol)
  if (nrow(near) == 0) return(NA_real_)
  max(near$balanced_accuracy)
}
hier <- intersect(c("Hierarchical RF", "Hierarchical CatBoost"), names(prob_models))
flat <- setdiff(names(prob_models), hier)
matched <- expand_grid(reference = hier, comparator = flat) %>%
  rowwise() %>%
  mutate(reference_macro_precision = fitted_pts$macro_precision[fitted_pts$model == reference],
         reference_balanced_accuracy = fitted_pts$balanced_accuracy[fitted_pts$model == reference],
         comparator_ba_at_same_macro_precision = ba_at_precision(filter(ops, model == comparator), reference_macro_precision)) %>%
  ungroup() %>%
  mutate(difference_pp = 100 * (reference_balanced_accuracy - comparator_ba_at_same_macro_precision))
print(matched, width = Inf)
save_table(matched, "matched_macro_precision_comparison.csv")

rm(preds, prob_models, pr, pr_df, ops); invisible(gc())
cat("Step 09b complete.\n")
