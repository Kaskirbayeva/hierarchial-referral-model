############################################################
# 02_cohorts_split.R - OUTCOME, TEMPORAL SPLIT, HIERARCHY
#
#   referral_destination = accepting facility level (5 classes)
#   2022 Jan-Sep  -> training (train_main)
#   2022 Oct-Dec  -> internal temporal validation (val_internal)
#   2023          -> external temporal validation (df_extval_2023)
#   destination_broad = DRG (City, Regional) vs NON_DRG
#
# Output: output/data/cohorts.rds (list of the three data sets)
############################################################

if (!exists("CFG")) source("00_config.R")

read_cohort <- function(f) {
  if (!file.exists(f)) stop("Data file not found: ", f, " (see CFG$data_files in 00_config.R)")
  readRDS(f)
}
df_dev_2022    <- add_destination(read_cohort(CFG$data_files[["2022"]]))
df_extval_2023 <- add_destination(read_cohort(CFG$data_files[["2023"]]))

missing_cols <- setdiff(c(predictor_vars, "accept_level"), names(df_dev_2022))
if (length(missing_cols) > 0) stop("Variables missing from the data: ", paste(missing_cols, collapse = ", "))

# ranger cannot handle NA predictors
drop_incomplete <- function(df, nm) {
  cc <- complete.cases(df[, predictor_vars])
  if (any(!cc)) warning(nm, ": removing ", sum(!cc), " rows with missing predictors")
  df[cc, ]
}
df_dev_2022    <- drop_incomplete(df_dev_2022, "2022")
df_extval_2023 <- drop_incomplete(df_extval_2023, "2023")

# The data hold admission month (no exact date), so the split uses month
df_dev_2022 <- df_dev_2022 %>%
  mutate(admission_month_num = match(as.character(admission_month), month.name))
if (anyNA(df_dev_2022$admission_month_num)) stop("admission_month could not be matched to month.name")

train_main   <- df_dev_2022 %>% filter(admission_month_num <= CFG$train_last_month)
val_internal <- df_dev_2022 %>% filter(admission_month_num >  CFG$train_last_month)

train_main     <- add_hierarchical_targets(train_main)
val_internal   <- add_hierarchical_targets(val_internal)
df_extval_2023 <- add_hierarchical_targets(df_extval_2023)

cat("\nTraining (2022 Jan-Sep):        ", nrow(train_main), "\n")
cat("Internal validation (2022 Oct-Dec):", nrow(val_internal), "\n")
cat("External validation (2023):      ", nrow(df_extval_2023), "\n")
print(train_main %>% count(destination_broad) %>% mutate(pct = round(100 * n / sum(n), 2)))

saveRDS(list(train_main = train_main, val_internal = val_internal, df_extval_2023 = df_extval_2023),
        out_path("data", "cohorts.rds"))

rm(df_dev_2022); invisible(gc())
cat("Step 02 complete.\n")
