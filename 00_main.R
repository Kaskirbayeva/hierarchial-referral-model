############################################################
# 00_main.R - RUN THE WHOLE REFERRAL PATHWAY PIPELINE
#
# Predicting hospital referral destination in Kazakhstan
# (2022 development, 2023 external validation), starting from
# analysis-ready cohorts:
#   naive baseline, flat RF, hierarchical RF, hierarchical CatBoost
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
  "02_cohorts_split.R"            = TRUE,   # outcome, temporal split, DRG/non-DRG
  # --- model development ---------------------------------------------
  "03_naive_and_flat_rf.R"        = TRUE,   # models 1-2, RF tuning
  "04_hierarchical_rf.R"          = TRUE,   # model 3
  "05_catboost_tuning.R"          = TRUE,
  "06_hierarchical_catboost.R"    = TRUE,   # model 4
  # --- tables and figures --------------------------------------------
  "07_table1_characteristics.R"   = TRUE,   # Table 1
  "08_table2_destinations.R"      = TRUE,   # Table 2
  "09_tables_performance.R"       = TRUE,   # Tables 3, 4, S2; confusion matrices; Figure S4
  "10_fig_variable_importance.R"  = TRUE,   # importance stage 1 & 3
  "11_fig_calibration.R"          = TRUE,   # calibration, stage-1 prob for rural/district, Figure 5
  "12_dca.R"                      = TRUE,   # Figure 8 (+ rural/district DCA)
  "13_regional_maps.R"            = TRUE,   # Figure 6 (+ referral concentration map)
  "14_concordance.R"              = TRUE,   # outcome concordance
  "15_shiny_inputs.R"             = TRUE    # inputs for app.R
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
