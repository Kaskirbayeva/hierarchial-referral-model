############################################################
# 14_concordance.R - OUTCOME CONCORDANCE ANALYSIS (2023)
#
# Compares length of stay and in-hospital complication rates
# between referrals whose actual destination matched the model's
# top-ranked prediction (concordant) and those that did not
# (discordant), in the external validation cohort:
#   - all referrals
#   - high-confidence predictions only (top probability >= threshold)
#   - stratified by ICD-10 chapter of the referral diagnosis
#
# Retrospective and non-randomised: hypothesis generating only,
# NOT evidence that following the model improves outcomes
# (confounding by indication).
############################################################

if (!exists("CFG")) source("00_config.R")
library(patchwork)

pred_file <- c("Hierarchical RF" = "pred_hier_rf.rds", "Hierarchical CatBoost" = "pred_hier_cb.rds")[[CFG$analysis_model]]
d   <- read_rds_out("preds", pred_file)$external
ext <- read_rds_out("data", "cohorts.rds")$df_extval_2023

df_conc <- ext %>%
  select(los, complications, icd10_ref) %>%
  mutate(
    predicted  = d$pred,
    actual     = d$actual,
    confidence = apply(as.matrix(d[, class_order]), 1, max),
    concordant = factor(if_else(d$pred == d$actual, "Concordant", "Discordant"),
                        levels = c("Concordant", "Discordant")),
    chapter    = icd10_chapter(icd10_ref),   # "Not specified" kept as its own group
    high_conf  = confidence >= CFG$confidence_threshold
  )

summarise_outcomes <- function(x) {
  x %>%
    group_by(concordant, .add = TRUE) %>%
    summarise(n = n(),
              mean_los = mean(los, na.rm = TRUE), sd_los = sd(los, na.rm = TRUE),
              median_los = median(los, na.rm = TRUE),
              q1_los = quantile(los, 0.25, na.rm = TRUE), q3_los = quantile(los, 0.75, na.rm = TRUE),
              n_complications = sum(complications, na.rm = TRUE),
              complication_rate_pct = 100 * mean(complications, na.rm = TRUE),
              .groups = "drop")
}

test_outcomes <- function(x) {
  if (n_distinct(x$concordant) < 2) return(tibble(p_los_wilcoxon = NA_real_, p_complications = NA_real_))
  tab <- table(x$concordant, factor(x$complications, levels = c(0, 1)))
  p_c <- if (any(suppressWarnings(chisq.test(tab)$expected) < 5)) fisher.test(tab)$p.value else chisq.test(tab)$p.value
  tibble(p_los_wilcoxon = wilcox.test(los ~ concordant, data = x)$p.value, p_complications = p_c)
}

# --- overall and high-confidence -------------------------------
concordance_rate <- df_conc %>% count(concordant) %>% mutate(pct = 100 * n / sum(n))
by_confidence <- df_conc %>%
  mutate(confidence_band = cut(confidence, c(0, 0.5, 0.7, 0.9, 1), include.lowest = TRUE,
                               labels = c("<0.50", "0.50-0.69", "0.70-0.89", ">=0.90"))) %>%
  group_by(confidence_band) %>% summarise(n = n(), pct_concordant = 100 * mean(concordant == "Concordant"))

outcomes_main <- bind_rows(
  summarise_outcomes(df_conc) %>% mutate(subset = "All referrals", .before = 1),
  summarise_outcomes(filter(df_conc, high_conf)) %>%
    mutate(subset = sprintf("High-confidence predictions (>= %.2f)", CFG$confidence_threshold), .before = 1)
) %>%
  left_join(bind_rows(test_outcomes(df_conc) %>% mutate(subset = "All referrals"),
                      test_outcomes(filter(df_conc, high_conf)) %>%
                        mutate(subset = sprintf("High-confidence predictions (>= %.2f)", CFG$confidence_threshold))),
            by = "subset")

# --- stratified by ICD-10 chapter (all and high-confidence) ----
by_chapter <- bind_rows(
  df_conc %>% group_by(chapter) %>% summarise_outcomes() %>% mutate(subset = "All referrals"),
  df_conc %>% filter(high_conf) %>% group_by(chapter) %>% summarise_outcomes() %>% mutate(subset = "High-confidence")
) %>%
  group_by(subset, chapter) %>% filter(all(n >= CFG$min_stratum_n), n() == 2) %>% ungroup() %>%
  arrange(subset, chapter, concordant)

chapter_diff <- by_chapter %>%
  select(subset, chapter, concordant, n, mean_los, complication_rate_pct) %>%
  pivot_wider(names_from = concordant, values_from = c(n, mean_los, complication_rate_pct)) %>%
  mutate(los_difference_disc_minus_conc = mean_los_Discordant - mean_los_Concordant,
         complication_difference_pp     = complication_rate_pct_Discordant - complication_rate_pct_Concordant)

cat("\nModel used:", CFG$analysis_model, "\n")
print(concordance_rate); print(by_confidence); print(outcomes_main, width = Inf)

save_table(concordance_rate, "concordance_rate.csv")
save_table(by_confidence,    "concordance_by_confidence.csv")
save_table(outcomes_main,    "concordance_outcomes_overall_and_high_confidence.csv")
save_table(by_chapter,       "concordance_outcomes_by_icd10_chapter.csv")
save_table(chapter_diff,     "concordance_differences_by_icd10_chapter.csv")

# --- figure -----------------------------------------------------
plot_d <- bind_rows(df_conc %>% mutate(subset = "All referrals"),
                    df_conc %>% filter(high_conf) %>% mutate(subset = "High-confidence"))
los_cap <- quantile(df_conc$los, 0.99, na.rm = TRUE)

pa <- ggplot(filter(plot_d, los <= los_cap), aes(concordant, los, fill = concordant)) +
  geom_boxplot(outlier.size = 0.3) + facet_wrap(~ subset) +
  scale_fill_manual(values = c(Concordant = "grey55", Discordant = "white"), guide = "none") +
  theme_ms(11) + labs(title = "A. Length of stay", subtitle = "Top 1% trimmed for display", x = NULL, y = "Days")
pb <- outcomes_main %>%
  mutate(subset = if_else(subset == "All referrals", subset, "High-confidence")) %>%
  ggplot(aes(concordant, complication_rate_pct / 100, fill = concordant)) +
  geom_col(colour = "black") + facet_wrap(~ subset) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  scale_fill_manual(values = c(Concordant = "grey55", Discordant = "white"), guide = "none") +
  theme_ms(11) + labs(title = "B. In-hospital complications", x = NULL, y = "Complication rate")

save_fig(pa + pb + plot_annotation(title = paste0("Outcomes by model-concordant referral, external validation (2023) - ", CFG$analysis_model)),
         "figure_concordance_outcomes.png", width = 12, height = 5.5)

rm(d, ext, df_conc, plot_d); invisible(gc())
cat("Step 14 complete.\n")
