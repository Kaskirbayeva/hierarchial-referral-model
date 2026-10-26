############################################################
# 15_shiny_inputs.R - INPUTS FOR THE SHINY PROTOTYPE (app.R)
#
# Saves output/shiny/shiny_inputs.rds with:
#   - the category levels seen in training for every categorical
#     predictor (dropdowns only offer values the model knows)
#   - the regional indicator lookup (2022 data, the same values
#     joined to the model data), so indicators are auto-filled
#     from the selected region
#   - class order, non-DRG classes and the confidence threshold
#
# The app uses the hierarchical RF stage models in output/models.
############################################################

if (!exists("CFG")) source("00_config.R")

train_main <- read_rds_out("data", "cohorts.rds")$train_main

categorical_predictors <- c("residence", "employment_group", "benefit_group", "refer_level",
                            "refer_region", "icd10_ref", "sur_type")
category_levels <- map(set_names(categorical_predictors), ~ sort(unique(as.character(train_main[[.x]]))))

regional_cols <- c("doctors_per10k", "beds_per10k", "wage", "employment_region",
                   "unemployment_rate", "log_population", "log_grp_pc", "log_investment_pc")

# The regional indicators are attached to each episode by `region`;
# take exactly the values the models were trained on.
regional_lookup <- train_main %>%
  distinct(region, across(all_of(regional_cols))) %>%
  filter(!is.na(region))
dups <- regional_lookup %>% count(region) %>% filter(n > 1)
if (nrow(dups) > 0) stop("Regions with more than one set of indicators: ", paste(dups$region, collapse = ", "))

no_lookup <- setdiff(category_levels$refer_region, regional_lookup$region)
if (length(no_lookup) > 0) warning("No regional indicators for referring region(s): ", paste(no_lookup, collapse = ", "))

# admission day/month are (ordered) factors in the training data;
# the app rebuilds them with exactly these levels
date_factor_levels <- list(
  admission_day_of_week = levels(train_main$admission_day_of_week),
  admission_month       = levels(train_main$admission_month)
)

shiny_inputs <- list(
  category_levels      = category_levels,
  date_factor_levels   = date_factor_levels,
  regional_lookup      = regional_lookup,
  class_order          = class_order,
  non_drg_classes      = non_drg_classes,
  confidence_threshold = CFG$confidence_threshold
)
saveRDS(shiny_inputs, out_path("shiny", "shiny_inputs.rds"))
print(regional_lookup, n = Inf)

rm(train_main)
cat("Step 15 complete. Launch the prototype with: shiny::runApp('", getwd(), "')\n", sep = "")
