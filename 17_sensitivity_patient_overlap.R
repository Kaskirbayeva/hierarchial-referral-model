############################################################
# 17_sensitivity_patient_overlap.R - PATIENTS IN BOTH TRAINING
# AND VALIDATION DATA
#
# The unit of analysis is the referral episode, so a patient can
# contribute episodes to training (2022 Jan-Sep) and validation.
# Performance is recomputed on validation episodes from patients
# whose pseudonymised identifier does not appear in the training
# data (and, for 2023, in any 2022 data).
# Requires the pseudonymised patient_id in the analysis-ready data.
#
# Output: output/tables/sens_patient_overlap.csv, sens_patient_overlap_counts.csv
############################################################

if (!exists("CFG")) source("00_config.R")

coh <- read_rds_out("data", "cohorts.rds")
if (!"patient_id" %in% names(coh$train_main))
  stop("patient_id is not in the data (see docs/data_dictionary.md); rerun step 02 with data that include it.")

train_ids <- unique(na.omit(coh$train_main$patient_id))
all22_ids <- unique(na.omit(c(coh$train_main$patient_id, coh$val_internal$patient_id)))
is_known_id <- function(x) !is.na(x) & nzchar(x)
flags <- list(
  internal = tibble(known_id = is_known_id(coh$val_internal$patient_id),
                    new_vs_training = is_known_id(coh$val_internal$patient_id) & !coh$val_internal$patient_id %in% train_ids,
                    new_vs_all_2022 = NA),
  external = tibble(known_id = is_known_id(coh$df_extval_2023$patient_id),
                    new_vs_training = is_known_id(coh$df_extval_2023$patient_id) & !coh$df_extval_2023$patient_id %in% train_ids,
                    new_vs_all_2022 = is_known_id(coh$df_extval_2023$patient_id) & !coh$df_extval_2023$patient_id %in% all22_ids))

counts <- imap_dfr(flags, ~ tibble(cohort = .y, n_episodes = nrow(.x),
                                   n_new_vs_training = sum(.x$new_vs_training),
                                   pct_new_vs_training = 100 * mean(.x$new_vs_training),
                                   n_new_vs_all_2022 = sum(.x$new_vs_all_2022),
                                   pct_new_vs_all_2022 = 100 * mean(.x$new_vs_all_2022),
                                   n_missing_patient_id = sum(!.x$known_id)))
print(counts, width = Inf)
save_table(counts, "sens_patient_overlap_counts.csv")

preds <- read_all_preds()
res <- imap_dfr(preds, function(p, model) map_dfr(c("internal", "external"), function(cohort) {
  d <- p[[cohort]]; f <- flags[[cohort]]
  subsets <- list("All episodes" = rep(TRUE, nrow(d)), "Patients not in training data" = f$new_vs_training)
  if (cohort == "external") subsets[["Patients not in any 2022 data"]] <- f$new_vs_all_2022
  imap_dfr(subsets, function(i, nm) bootstrap_metrics(d$actual[i], d$pred[i], n_boot = 200) %>%
             mutate(model = model, cohort = cohort, subset = nm, n = sum(i), .before = 1))
}))
print(res %>% filter(metric == "balanced_accuracy"), n = Inf, width = Inf)
save_table(res, "sens_patient_overlap.csv")

rm(coh, preds); invisible(gc())
cat("Step 17 complete.\n")
