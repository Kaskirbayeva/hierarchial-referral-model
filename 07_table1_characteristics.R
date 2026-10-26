############################################################
# 07_table1_characteristics.R - TABLE 1
# Demographic and clinical characteristics of HES cohorts,
# 2022 and 2023 (analytical cohorts, polyclinic destinations
# excluded; polyclinic as a REFERRING facility is retained).
#
# Output: output/tables/table1_characteristics.csv
############################################################

if (!exists("CFG")) source("00_config.R")

coh <- read_rds_out("data", "cohorts.rds")
df_22 <- bind_rows(coh$train_main, coh$val_internal)
df_23 <- coh$df_extval_2023
rm(coh)

n_22 <- nrow(df_22); n_23 <- nrow(df_23)

fmt_n    <- function(n, total) sprintf("%d (%.1f%%)", as.integer(n), 100 * n / total)
fmt_mean <- function(x) sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))

cat_block <- function(x22, x23, label, level_order = NULL) {
  x22 <- as.character(x22); x23 <- as.character(x23)
  lev <- if (is.null(level_order)) sort(unique(na.omit(c(x22, x23)))) else level_order
  if (anyNA(c(x22, x23))) lev <- c(lev, "Missing")
  x22[is.na(x22)] <- "Missing"; x23[is.na(x23)] <- "Missing"
  tibble(
    Characteristic = c(label, rep("", length(lev) - 1)),
    Category       = lev,
    y2022          = map_chr(lev, ~ fmt_n(sum(x22 == .x), n_22)),
    y2023          = map_chr(lev, ~ fmt_n(sum(x23 == .x), n_23))
  )
}
num_row <- function(v, label) {
  tibble(Characteristic = label, Category = "", y2022 = fmt_mean(df_22[[v]]), y2023 = fmt_mean(df_23[[v]]))
}

agecat_labels <- c(
  child        = "Child (age: 5 - 17 y.o)",
  middle_adult = "Middle adult (age: 45 - 72 y.o)",
  newborn      = "Newborn (age: 0 - 4 y.o)",
  senior       = "Senior (age: 72+ y.o)",
  young_adult  = "Young adult (age: 18 - 44 y.o)"
)
lab_agecat  <- function(x) unname(agecat_labels[x])
lab_benefit <- function(x) recode(x, "Benefit" = "MSHI benefit receiver", "No benefit" = "Not MSHI benefit receiver")
lab_surgery <- function(x) recode(x, "Additional_operation" = "Additional operation", "Primary_operation" = "Primary operation")
lab_compl   <- function(x) if_else(x == 1, "yes", "no")

table1 <- bind_rows(
  num_row("age", "Age, mean (SD) years"),
  cat_block(df_22$gender, df_23$gender, "Sex, number (%)", c("F", "M")),
  cat_block(lab_agecat(df_22$age_cat), lab_agecat(df_23$age_cat), "Age category, number (%)", unname(sort(agecat_labels))),
  cat_block(label_residence(df_22$residence), label_residence(df_23$residence), "Area of patient residence, number (%)"),
  cat_block(df_22$employment_group, df_23$employment_group, "Employment status, number (%)"),
  cat_block(lab_benefit(df_22$benefit_group), lab_benefit(df_23$benefit_group), "MSHI benefit category, number (%)",
            c("MSHI benefit receiver", "Not MSHI benefit receiver")),
  cat_block(df_22$admission_day_of_week, df_23$admission_day_of_week, "Day of week of admission, number (%)",
            c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
  cat_block(df_22$admission_month, df_23$admission_month, "Month of admission, number (%)", month.name),
  cat_block(icd10_chapter(df_22$icd3), icd10_chapter(df_23$icd3),
            "Clinical diagnosis ICD - 10 chapter, number (%)", icd10_chapter_order),
  cat_block(lab_compl(df_22$complications), lab_compl(df_23$complications), "In - hospital complications number (%)", c("no", "yes")),
  cat_block(lab_surgery(df_22$sur_type), lab_surgery(df_23$sur_type), "Surgery type, number (%)"),
  cat_block(df_22$refer_level, df_23$refer_level, "Referring facility level number (%)"),
  cat_block(label_facility_region(df_22$refer_region), label_facility_region(df_23$refer_region), "Referring facility region, number (%)"),
  cat_block(df_22$referral_destination, df_23$referral_destination, "Accepting facility level, number (%)", class_order),
  cat_block(label_facility_region(df_22$accept_region), label_facility_region(df_23$accept_region), "Region accepting facility, number (%)"),
  num_row("doctors_per10k",    "Doctors per 10,000 population, mean (SD)"),
  num_row("beds_per10k",       "Hospital beds per 10,000 population, mean (SD)"),
  num_row("wage",              "Average wage, mean (SD)"),
  num_row("unemployment_rate", "Unemployment rate, mean (SD)"),
  num_row("log_population",    "Population (log), mean (SD)"),
  num_row("log_grp_pc",        "Gross regional product per capita (log), mean (SD)")
)

names(table1) <- c("Characteristic", "Category",
                   sprintf("2022 year (n = %d)", n_22), sprintf("2023 year (n = %d)", n_23))

print(table1, n = Inf, width = Inf)
save_table(table1, "table1_characteristics.csv")
rm(df_22, df_23)
cat("Step 07 complete.\n")
