############################################################
# 18_sensitivity_obstetric.R - OBSTETRIC VS NON-OBSTETRIC REFERRALS
# (case mix: obstetric vs non-obstetric referral diagnoses)
#
# About a third of episodes are obstetric. Performance is reported
# separately for obstetric and non-obstetric referrals, defined by
# the REFERRAL diagnosis (ICD-10 chapter O; main definition, known
# at referral) and, as a check, by the FINAL diagnosis (icd3).
#
# Output: output/tables/sens_obstetric_performance.csv,
#         sens_obstetric_per_class_recall.csv
############################################################

if (!exists("CFG")) source("00_config.R")

coh <- read_rds_out("data", "cohorts.rds")
groups <- function(df) list(
  referral = if_else(str_starts(toupper(df$icd10_ref), "O"), "Obstetric", "Non-obstetric"),
  final    = if_else(str_starts(toupper(coalesce(df$icd3, "")), "O"), "Obstetric", "Non-obstetric"))
g <- list(internal = groups(coh$val_internal), external = groups(coh$df_extval_2023))

preds <- read_all_preds()
grid <- expand_grid(model = names(preds), cohort = c("internal", "external"),
                    definition = c("referral", "final"), group = c("Obstetric", "Non-obstetric"))
res <- pmap_dfr(grid, function(model, cohort, definition, group) {
  d <- preds[[model]][[cohort]]; i <- g[[cohort]][[definition]] == group
  bootstrap_metrics(d$actual[i], d$pred[i], n_boot = 200) %>%
    mutate(model = model, cohort = cohort, definition = paste(definition, "diagnosis"), group = group, n = sum(i), .before = 1)
})
rec <- pmap_dfr(grid, function(model, cohort, definition, group) {
  d <- preds[[model]][[cohort]]; i <- g[[cohort]][[definition]] == group
  per_class_pr(d$actual[i], d$pred[i]) %>%
    mutate(model = model, cohort = cohort, definition = paste(definition, "diagnosis"), group = group, .before = 1)
})
print(res %>% filter(metric == "balanced_accuracy", cohort == "external", definition == "referral diagnosis"), n = Inf, width = Inf)
save_table(res, "sens_obstetric_performance.csv")
save_table(rec, "sens_obstetric_per_class_recall.csv")

rm(coh, preds); invisible(gc())
cat("Step 18 complete.\n")
