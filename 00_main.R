############################################################
# 00_main.R - RUN THE WHOLE REFERRAL PATHWAY PIPELINE
#
# Predicting hospital referral destination in Kazakhstan
# (2022 development, 2023 external validation), starting from
# analysis-ready cohorts:
#   naive baseline, flat RF, weighted flat RF and CatBoost,
#   hierarchical RF, hierarchical CatBoost
#
# HOW TO RUN
#   1. Open referral-pathway-ml.Rproj (or setwd() to the repository root)
#   2. Optionally edit 00_config.R (default: the synthetic data in data/synthetic)
#   3. source("00_main.R")
#   4. Results: output/tables, output/figures (see README.md)
#   5. Shiny prototype afterwards:  shiny::runApp()
#
# Every step saves its results to output/, so steps can be
# switched off below and re-run later without refitting models
# (e.g. set the modelling steps to FALSE to only redraw figures).
############################################################

source("00_config.R")

run_steps <- c(
  # --- analysis cohorts -------------------------------------------
  "02_cohorts_split.R"               = TRUE,   # outcome, temporal split, DRG/non-DRG
  # --- model development ---------------------------------------------
  "03_naive_and_flat_rf.R"           = TRUE,   # models 1-2, RF tuning
  "04_hierarchical_rf.R"             = TRUE,   # hierarchical RF
  "05_catboost_tuning.R"             = TRUE,
  "06_hierarchical_catboost.R"       = TRUE,   # hierarchical CatBoost
  "06b_weighted_flat_models.R"       = TRUE,   # weighted flat RF and CatBoost (comparators)
  # --- tables and figures --------------------------------------------
  "07_table1_characteristics.R"      = TRUE,   # Table 1
  "07b_referral_diagnosis.R"         = TRUE,   # referral diagnosis "Not specified" (Table S9)
  "08_table2_destinations.R"         = TRUE,   # Table 2
  "09_tables_performance.R"          = TRUE,   # Tables 3, 4; confusion matrices; stage-1 ROC
  "09b_precision_recall.R"           = TRUE,   # Tables S4, S7; Figure S2 (precision, PR curves)
  "10_fig_variable_importance.R"     = TRUE,   # Figures 2-3 (variable importance, stages 1 and 3)
  "11_fig_calibration.R"             = TRUE,   # Figures 4-5 (calibration)
  "11b_recalibration.R"              = TRUE,   # recalibration (Table S8); needed by steps 12-15
  "12_dca.R"                         = TRUE,   # Figures 8-9 (decision curves, recalibrated)
  "12b_aggregate_volumes.R"          = TRUE,   # Table 6, Table S10 (predicted vs observed volumes)
  "13_regional_maps.R"               = TRUE,   # Figures 6-7, Table S6
  "14_concordance.R"                 = TRUE,   # Table 5, Table S5 (outcome concordance)
  "15_shiny_inputs.R"                = TRUE,   # inputs for app.R
  # --- sensitivity analyses ------------------------------------------
  "16_sensitivity_no_surgery_type.R" = TRUE,   # refit without surgery type
  "17_sensitivity_patient_overlap.R" = TRUE,   # patients not in the training data
  "18_sensitivity_obstetric.R"       = TRUE,   # obstetric vs non-obstetric referrals
  "19_tableS2_figureS1.R"            = TRUE    # Table S2, Figure S1 (class-specific metrics, ROC)
)

log_file <- file.path("output", paste0("run_log_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt"))
timings <- tibble::tibble(step = character(), minutes = numeric())

for (step in names(run_steps)) {
  if (!isTRUE(run_steps[[step]])) { cat("\n--- skipping", step, "\n"); next }
  cat("\n============================================================\n")
  cat("RUNNING", step, "  (", format(Sys.time(), "%H:%M:%S"), ")\n")
  cat("============================================================\n")
  t0 <- Sys.time()
  source(step, echo = FALSE, local = globalenv())
  mins <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  timings <- dplyr::bind_rows(timings, tibble::tibble(step = step, minutes = round(mins, 2)))
  cat(sprintf("Finished %s in %.1f min\n", step, mins))
}

print(timings)
writeLines(c(paste("Run finished:", Sys.time()), capture.output(print(timings)),
             capture.output(sessionInfo())), log_file)

cat("\nPIPELINE COMPLETE. Tables: output/tables | Figures: output/figures\n")
