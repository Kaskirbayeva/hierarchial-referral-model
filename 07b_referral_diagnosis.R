############################################################
# 07b_referral_diagnosis.R - REFERRAL DIAGNOSIS ("NOT SPECIFIED")
#
# The predictor is the referral diagnosis (icd10_ref, entered by the
# referring facility). About one-fifth is missing and was recoded to
# its own category "Not specified" during data preparation (kept
# as a category, not imputed). This script reports:
#   (a) "Not specified" overall, by referring region and by level
#   (b) agreement of referral vs final diagnosis ICD-10 chapter
#   (c) relationship with regional model performance (2023)
#   (d) model performance for specified vs not specified diagnoses
#
# Outputs (output/tables, output/figures):
#   referral_dx_not_specified_overall.csv / _by_region.csv / _by_level.csv
#   referral_vs_final_chapter_agreement.csv
#   referral_dx_not_specified_vs_regional_ba.csv (+ _correlation.csv)
#   performance_by_referral_dx_specified.csv
#   figure_not_specified_vs_regional_ba.png
############################################################

if (!exists("CFG")) source("00_config.R")

coh <- read_rds_out("data", "cohorts.rds")
d <- bind_rows(bind_rows(coh$train_main, coh$val_internal) %>% mutate(year = 2022L),
               coh$df_extval_2023 %>% mutate(year = 2023L)) %>%
  mutate(ref_chapter   = icd10_chapter(icd10_ref),
         final_chapter = icd10_chapter(icd3),
         not_specified = ref_chapter == "Not specified")

ns <- function(x) x %>% summarise(n = n(), n_not_specified = sum(not_specified),
                                  pct_not_specified = 100 * mean(not_specified), .groups = "drop")
overall   <- d %>% group_by(year) %>% ns()
by_region <- d %>% group_by(year, refer_region) %>% ns()
by_level  <- d %>% group_by(year, refer_level) %>% ns()
print(overall); print(by_level)
save_table(overall,   "referral_dx_not_specified_overall.csv")
save_table(by_region, "referral_dx_not_specified_by_region.csv")
save_table(by_level,  "referral_dx_not_specified_by_level.csv")

agreement <- d %>% filter(!not_specified) %>% group_by(year) %>%
  summarise(n_specified = n(), pct_same_chapter_as_final = 100 * mean(ref_chapter == final_chapter), .groups = "drop")
save_table(agreement, "referral_vs_final_chapter_agreement.csv")

# --- regional performance vs share not specified (2023) -----------
preds <- read_all_preds(c("Hierarchical RF", "Hierarchical CatBoost"))
ext <- coh$df_extval_2023
reg_ba <- imap_dfr(preds, function(p, model) {
  p$external %>% mutate(refer_region = ext$refer_region) %>%
    group_by(refer_region, actual) %>% summarise(recall = mean(pred == actual), .groups = "drop") %>%
    group_by(refer_region) %>% summarise(balanced_accuracy = mean(recall), .groups = "drop") %>%
    mutate(model = model)
})
rel <- reg_ba %>% left_join(by_region %>% filter(year == 2023) %>% select(refer_region, pct_not_specified, n),
                            by = "refer_region")
cor_tab <- rel %>% group_by(model) %>%
  summarise(n_regions = n(),
            spearman_rho = suppressWarnings(cor(pct_not_specified, balanced_accuracy, method = "spearman")),
            p_value = suppressWarnings(cor.test(pct_not_specified, balanced_accuracy, method = "spearman", exact = FALSE)$p.value),
            .groups = "drop")
print(cor_tab)
save_table(rel, "referral_dx_not_specified_vs_regional_ba.csv")
save_table(cor_tab, "referral_dx_not_specified_vs_regional_ba_correlation.csv")

fig <- ggplot(rel, aes(pct_not_specified, balanced_accuracy, colour = model)) +
  geom_point(size = 2.4) + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.6) +
  geom_text(aes(label = label_facility_region(refer_region)), size = 2.4, vjust = -0.8, show.legend = FALSE) +
  scale_colour_manual(values = model_colours) + scale_y_continuous(labels = scales::percent) +
  theme_ms() +
  labs(title = "Referral diagnosis 'Not specified' and regional model performance",
       subtitle = "External validation (2023), by referring region",
       x = "Referrals with referral diagnosis 'Not specified' (%)", y = "Balanced accuracy", colour = NULL)
save_fig(fig, "figure_not_specified_vs_regional_ba.png", width = 9, height = 6.5)

# --- performance for specified vs not specified ---------------------
perf_ns <- imap_dfr(preds, function(p, model) {
  map_dfr(c("internal", "external"), function(cohort) {
    df <- if (cohort == "external") coh$df_extval_2023 else coh$val_internal
    grp <- if_else(icd10_chapter(df$icd10_ref) == "Not specified", "Not specified", "Specified")
    map_dfr(c("Specified", "Not specified"), function(g) {
      i <- grp == g
      classification_metrics(p[[cohort]]$actual[i], p[[cohort]]$pred[i]) %>%
        mutate(model = model, cohort = cohort, referral_dx = g, n = sum(i), .before = 1)
    })
  })
})
print(perf_ns, width = Inf)
save_table(perf_ns, "performance_by_referral_dx_specified.csv")

rm(coh, d, ext, preds); invisible(gc())
cat("Step 07b complete.\n")
