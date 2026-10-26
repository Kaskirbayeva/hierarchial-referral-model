############################################################
# 00_config.R - SETTINGS SHARED BY EVERY SCRIPT
#
# Referral pathway prediction (Kazakhstan HES, 2022-2023)
#
# This is the ONLY file you should need to edit before running
# the pipeline. Every numbered script sources it automatically
# (if it has not been loaded yet), so each script can be run on
# its own as well as from 00_main.R.
############################################################

# ---------------------------------------------------------
# 1. LOCATION
# ---------------------------------------------------------

# Folder that contains all the pipeline scripts (the repository
# root). By default this is the current working directory - open
# referral-pathway-ml.Rproj in RStudio, or setwd() to the repo.
project_dir <- getwd()

CFG <- list(

  # ---------------------------------------------------------
  # 2. ANALYSIS-READY DATA
  # ---------------------------------------------------------
  # One file per year, one row per referral episode, with the
  # variables described in docs/data_dictionary.md. The repository
  # ships SYNTHETIC data; to use other data with the same structure,
  # point these paths to your own files (never commit them).
  data_files = c(
    "2022" = "data/synthetic/df_model_22_synthetic.rds",
    "2023" = "data/synthetic/df_model_23_synthetic.rds"
  ),

  # ---------------------------------------------------------
  # 3. ANALYSIS SETTINGS
  # ---------------------------------------------------------
  seed         = 20260818,

  # Temporal split of 2022: months 1..train_last_month = training,
  # later months = internal temporal validation; 2023 = external.
  train_last_month = 9,

  # Hyperparameter tuning (grid search on a training subsample)
  tune_subsample_frac  = 0.15,
  tune_subsample_max_n = 30000,
  rf_tune_trees        = 200,        # trees per grid candidate (ranking only)
  rf_final_trees       = 1000,       # trees in every final RF model
  rf_min_node_grid     = c(1, 5, 15),
  rf_sample_frac_grid  = c(0.7, 1.0),
  cb_iterations_grid   = c(200, 500, 800),
  cb_depth_grid        = c(4, 6, 8),
  cb_learning_rate_grid = c(0.05, 0.1, 0.2),
  cb_inner_train_frac  = 0.7,        # inner train/validation split for CatBoost tuning

  # Bootstrap for 95% CIs in Table 4
  n_boot = 1000,

  # Hierarchical model used for the regional map (Figure 6),
  # and outcome-concordance analysis:
  # "Hierarchical RF" or "Hierarchical CatBoost"
  analysis_model = "Hierarchical RF",

  # Shiny prototype: recommendation threshold
  confidence_threshold = 0.70,

  # Concordance analysis: minimum stratum size reported
  min_stratum_n = 100
)

# ---------------------------------------------------------
# 4. FIXED DEFINITIONS (do not change unless the study changes)
# ---------------------------------------------------------

class_order     <- c("City", "Regional", "Republican/National", "Rural/District", "Specialised")
drg_classes     <- c("City", "Regional")
non_drg_classes <- c("Republican/National", "Rural/District", "Specialised")

# Predictors: information available to the referring clinician
# before the referral decision. EXCLUDED on purpose:
#   accepting-facility attributes (accept_region, accept_ownership,
#   accept_urban, accept_level) - post-decision / outcome-related;
#   icd3 (final/discharge diagnosis) - leakage;
#   age_cat, gender - redundant;
#   los, complications - reserved for the concordance analysis.
predictor_vars <- c(
  "age", "female", "residence", "employment_group", "employment_region",
  "benefit_group", "refer_level", "refer_region", "icd10_ref", "sur_type",
  "doctors_per10k", "beds_per10k", "wage", "unemployment_rate",
  "log_population", "log_grp_pc", "log_investment_pc",
  "admission_day_of_week", "admission_month"
)

model_names <- c("Naive baseline", "Flat RF", "Hierarchical RF", "Hierarchical CatBoost")

# ---------------------------------------------------------
# 5. OUTPUT FOLDERS (created automatically)
# ---------------------------------------------------------

if (dir.exists(project_dir)) setwd(project_dir)
if (!file.exists("01_functions.R")) stop("Run the scripts from the repository root (the folder containing 00_config.R).")

out_dirs <- c(
  data    = "output/data",
  models  = "output/models",
  preds   = "output/predictions",
  tables  = "output/tables",
  figures = "output/figures",
  shiny   = "output/shiny"
)
for (d in out_dirs) dir.create(d, showWarnings = FALSE, recursive = TRUE)

out_path <- function(type, file) file.path(out_dirs[[type]], file)

# ---------------------------------------------------------
# 6. PACKAGES
# ---------------------------------------------------------

cran_packages <- c("tidyverse", "lubridate", "ranger",
                   "pROC", "patchwork", "scales", "sf", "shiny", "remotes")
missing_pkgs <- cran_packages[!cran_packages %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0) {
  # A CRAN mirror must be set first, otherwise install.packages() opens
  # an interactive menu that swallows the following lines of a sourced script.
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages(missing_pkgs)
}
if (!requireNamespace("catboost", quietly = TRUE)) {
  message(
    "Package 'catboost' is not installed (it is not on CRAN). Install it once with:\n",
    "  remotes::install_url('https://github.com/catboost/catboost/releases/download/",
    "v1.2.8/catboost-R-windows-x86_64-1.2.8.tgz', INSTALL_opts = c('--no-multiarch', '--no-test-load'))\n",
    "(Linux / macOS: use the matching catboost-R-<os> file from the same release page;\n",
    " see https://catboost.ai/docs/en/installation/r-installation-binary-installation)"
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

options(warn = 1, dplyr.summarise.inform = FALSE)

# Shared modelling / evaluation functions
source("01_functions.R")

cat("Config loaded. Working directory:", getwd(), "\n")
