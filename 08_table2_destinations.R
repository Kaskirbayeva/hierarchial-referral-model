############################################################
# 08_table2_destinations.R - TABLE 2
# Distribution of referral destinations across training,
# internal validation and external temporal validation cohorts.
#
# Output: output/tables/table2_destinations.csv
############################################################

if (!exists("CFG")) source("00_config.R")

coh <- read_rds_out("data", "cohorts.rds")

col_for <- function(df) {
  tab <- table(factor(df$referral_destination, levels = class_order))
  sprintf("%d (%.1f%%)", as.integer(tab), 100 * as.numeric(tab) / nrow(df))
}

table2 <- tibble(
  `Referral destination number (%)` = class_order,
  !!sprintf("Training (2022 January - September), n = %d", nrow(coh$train_main))     := col_for(coh$train_main),
  !!sprintf("Internal validation (2022 October - December), n = %d", nrow(coh$val_internal)) := col_for(coh$val_internal),
  !!sprintf("External validation (2023), n = %d", nrow(coh$df_extval_2023))         := col_for(coh$df_extval_2023)
)

print(table2, width = Inf)
save_table(table2, "table2_destinations.csv")
rm(coh)
cat("Step 08 complete.\n")
