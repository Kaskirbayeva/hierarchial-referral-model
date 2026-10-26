############################################################
# 01_functions.R - SHARED MODELLING & EVALUATION FUNCTIONS
#
# Defined once here and used by every modelling, table and
# figure script, so all models are evaluated identically.
# (Sourced automatically by 00_config.R.)
############################################################


############################################################
# 1. COHORT HELPERS
############################################################

# referral_destination = accepting facility level (5 classes, fixed order)
add_destination <- function(df) {
  df <- df %>%
    filter(accept_level != "Polyclinic") %>%
    mutate(referral_destination = factor(as.character(accept_level), levels = class_order))
  n_bad <- sum(is.na(df$referral_destination))
  if (n_bad > 0) {
    stop(n_bad, " rows have an accept_level outside class_order: ",
         paste(unique(df$accept_level[is.na(df$referral_destination)]), collapse = ", "))
  }
  df
}

# Hierarchical targets: Stage 1 DRG (City, Regional) vs NON_DRG
add_hierarchical_targets <- function(df) {
  df %>%
    mutate(
      destination_broad = factor(
        if_else(referral_destination %in% drg_classes, "DRG", "NON_DRG"),
        levels = c("DRG", "NON_DRG")
      )
    )
}

# Inverse class-frequency weights: n / (K * n_k)
inverse_freq_weights <- function(y) {
  counts <- table(y)
  w <- length(y) / (length(counts) * counts)
  setNames(as.numeric(w), names(counts))
}

model_formula <- function(target) {
  as.formula(paste(target, "~", paste(predictor_vars, collapse = " + ")))
}


############################################################
# 2. PERFORMANCE METRICS
#
# accuracy, balanced accuracy (= mean per-class recall) and
# macro-averaged precision / recall / F1. Per-class values that
# are undefined (class never observed / never predicted) are NA
# and dropped from the macro average - this is the definition
# used for every reported number.
############################################################

metrics_from_cm <- function(cm) {
  # cm: K x K matrix, rows = observed, cols = predicted
  tp   <- diag(cm)
  obs  <- rowSums(cm)
  prd  <- colSums(cm)
  recall    <- ifelse(obs == 0, NA, tp / obs)
  precision <- ifelse(prd == 0, NA, tp / prd)
  f1 <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0, NA,
               2 * precision * recall / (precision + recall))
  c(
    accuracy          = sum(tp) / sum(cm),
    balanced_accuracy = mean(recall, na.rm = TRUE),
    macro_precision   = mean(precision, na.rm = TRUE),
    macro_recall      = mean(recall, na.rm = TRUE),
    macro_F1          = mean(f1, na.rm = TRUE)
  )
}

confusion <- function(actual, predicted) {
  lev <- levels(actual)
  table(Observed = factor(actual, levels = lev), Predicted = factor(predicted, levels = lev))
}

classification_metrics <- function(actual, predicted) {
  as_tibble(as.list(metrics_from_cm(unclass(confusion(actual, predicted)))))
}

per_class_recall <- function(actual, predicted) {
  cm <- confusion(actual, predicted)
  tibble(
    class    = rownames(cm),
    n_actual = as.integer(rowSums(cm)),
    recall   = ifelse(rowSums(cm) == 0, NA_real_, diag(cm) / rowSums(cm))
  )
}

# Percentile bootstrap 95% CI for all metrics (resampling cases)
bootstrap_metrics <- function(actual, predicted, n_boot = CFG$n_boot, seed = CFG$seed) {
  lev <- levels(actual)
  K   <- length(lev)
  a   <- as.integer(factor(actual, levels = lev))
  p   <- as.integer(factor(predicted, levels = lev))
  n   <- length(a)
  cell <- a + K * (p - 1L)
  set.seed(seed)
  boot <- t(vapply(seq_len(n_boot), function(b) {
    idx <- sample.int(n, n, replace = TRUE)
    metrics_from_cm(matrix(tabulate(cell[idx], K * K), K, K))
  }, numeric(5)))
  point <- metrics_from_cm(matrix(tabulate(cell, K * K), K, K))
  tibble(
    metric   = names(point),
    estimate = as.numeric(point),
    lower    = apply(boot, 2, quantile, probs = 0.025, na.rm = TRUE),
    upper    = apply(boot, 2, quantile, probs = 0.975, na.rm = TRUE)
  )
}

fmt_ci <- function(est, lo, hi, digits = 3) {
  sprintf(paste0("%.", digits, "f (%.", digits, "f - %.", digits, "f)"), est, lo, hi)
}


############################################################
# 3. PREDICTION HELPERS
############################################################

prob_to_class <- function(prob, levels = colnames(prob)) {
  prob <- as.matrix(prob)
  factor(colnames(prob)[max.col(prob, ties.method = "first")], levels = levels)
}

# --- ranger ---------------------------------------------------
rf_prob <- function(model, df) {
  predict(model, data = df)$predictions
}

# --- CatBoost -------------------------------------------------
# Character predictors are converted to factors so CatBoost's
# data.frame auto-detection treats them as categorical (plain
# character columns can be mistaken for text features, which
# caused the "Dictionary size is 0" error). Category values are
# hashed from their string representation, so pools built
# separately for train / validation sets are consistent.
build_pool <- function(df, label = NULL) {
  X <- as.data.frame(df[, predictor_vars, drop = FALSE])
  X[] <- lapply(X, function(col) if (is.character(col)) factor(col) else col)
  if (is.null(label)) {
    catboost::catboost.load_pool(data = X)
  } else {
    catboost::catboost.load_pool(data = X, label = as.integer(label) - 1L)  # 0-indexed
  }
}

cb_prob <- function(model, df, level_labels) {
  p <- catboost::catboost.predict(model, build_pool(df), prediction_type = "Probability")
  p <- as.matrix(p)
  if (ncol(p) == 1) p <- cbind(1 - p, p)   # safety for binary output
  colnames(p) <- level_labels
  p
}

cb_train <- function(df, target, params, class_weights = NULL, verbose = 100) {
  y <- df[[target]]
  prm <- list(
    loss_function = "MultiClass",
    iterations    = params$iterations,
    depth         = params$depth,
    learning_rate = params$learning_rate,
    random_seed   = CFG$seed,
    verbose       = verbose
  )
  if (!is.null(class_weights)) prm$class_weights <- as.list(unname(class_weights[levels(y)]))
  catboost::catboost.train(build_pool(df, label = y), params = prm)
}

# --- Hierarchical combination ---------------------------------
# Final P(class) = P(stage-1 branch) x P(class | branch), computed
# for EVERY observation from the PREDICTED stage-1 probabilities,
# never from the actual destination, so stage-1 errors propagate.
combine_hierarchical <- function(p1, p2, p3) {
  prob <- cbind(
    p1[, "DRG"]     * p2[, drg_classes, drop = FALSE],
    p1[, "NON_DRG"] * p3[, non_drg_classes, drop = FALSE]
  )
  colnames(prob) <- c(drg_classes, non_drg_classes)
  prob <- prob[, class_order, drop = FALSE]
  list(prob = prob, pred = prob_to_class(prob, class_order))
}

# Standard prediction table saved for every model x cohort
make_pred_table <- function(df, pred, prob = NULL, extra = NULL) {
  out <- tibble(
    row_id = seq_len(nrow(df)),
    actual = df$referral_destination,
    pred   = factor(as.character(pred), levels = class_order)
  )
  if (!is.null(prob)) out <- bind_cols(out, as_tibble(as.data.frame(prob[, class_order, drop = FALSE]), .name_repair = "minimal"))
  if (!is.null(extra)) out <- bind_cols(out, as_tibble(extra))
  out
}


############################################################
# 4. CALIBRATION & DECISION CURVES
############################################################

calibration_stats <- function(p, y, label) {
  eps <- 1e-6
  lp  <- qlogis(pmin(pmax(p, eps), 1 - eps))
  fit <- glm(y ~ lp, family = binomial())
  int_fit <- glm(y ~ 1, offset = lp, family = binomial())   # calibration-in-the-large
  tibble(
    model          = label,
    brier          = mean((p - y)^2),
    cal_intercept  = unname(coef(fit)[1]),       # from logistic recalibration y ~ logit(p)
    cal_in_the_large = unname(coef(int_fit)[1]), # y ~ offset(logit(p))
    cal_slope      = unname(coef(fit)[2]),
    mean_predicted = mean(p),
    observed_rate  = mean(y)
  )
}

calibration_bins <- function(p, y, label, n_bins = 10, type = c("quantile", "fixed")) {
  type <- match.arg(type)
  d <- tibble(p = p, y = y)
  d <- if (type == "quantile") mutate(d, bin = ntile(p, n_bins)) else
    mutate(d, bin = cut(p, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE))
  d %>%
    group_by(bin) %>%
    summarise(mean_predicted = mean(p), observed_freq = mean(y), n = n(),
              lower = binom.test(sum(y), n())$conf.int[1],
              upper = binom.test(sum(y), n())$conf.int[2], .groups = "drop") %>%
    mutate(model = label)
}

# Net benefit (Vickers & Elkin 2006): NB = TP/n - FP/n * pt/(1-pt)
net_benefit_curve <- function(p, y, thresholds, label) {
  n <- length(y)
  tibble(threshold = thresholds) %>%
    mutate(
      tp = map_dbl(threshold, ~ sum(p >= .x & y == 1)),
      fp = map_dbl(threshold, ~ sum(p >= .x & y == 0)),
      net_benefit = tp / n - fp / n * threshold / (1 - threshold),
      strategy = label
    ) %>% select(threshold, net_benefit, strategy)
}

net_benefit_fixed <- function(pred_pos, y, thresholds, label) {
  n <- length(y); tp <- sum(pred_pos & y == 1); fp <- sum(pred_pos & y == 0)
  tibble(threshold = thresholds,
         net_benefit = tp / n - fp / n * thresholds / (1 - thresholds),
         strategy = label)
}

net_benefit_reference <- function(y, thresholds) {
  prev <- mean(y)
  bind_rows(
    tibble(threshold = thresholds,
           net_benefit = prev - (1 - prev) * thresholds / (1 - thresholds),
           strategy = "Refer all"),
    tibble(threshold = thresholds, net_benefit = 0, strategy = "Refer none")
  )
}


############################################################
# 5. LABELS FOR TABLES AND FIGURES
############################################################

variable_labels <- c(
  age                   = "Age",
  female                = "Sex (female)",
  residence             = "Patient residence region",
  employment_group      = "Employment status",
  employment_region     = "Regional employment",
  benefit_group         = "MSHI benefit category",
  refer_level           = "Referring facility level",
  refer_region          = "Referring facility region",
  icd10_ref             = "Diagnosis at referral (ICD-10)",
  sur_type              = "Surgery type",
  doctors_per10k        = "Doctors per 10,000 population",
  beds_per10k           = "Hospital beds per 10,000 population",
  wage                  = "Average regional wage",
  unemployment_rate     = "Regional unemployment rate",
  log_population        = "Regional population (log)",
  log_grp_pc            = "Gross regional product per capita (log)",
  log_investment_pc     = "Regional investment per capita (log)",
  admission_day_of_week = "Day of week of admission",
  admission_month       = "Month of admission"
)

label_var <- function(x) unname(ifelse(x %in% names(variable_labels), variable_labels[x], x))

# ICD-10 chapter (manuscript wording). Codes that are missing or
# "Not specified" are labelled "Not specified".
icd10_chapter <- function(code) {
  code   <- toupper(trimws(as.character(code)))
  valid  <- !is.na(code) & str_detect(code, "^[A-Z][0-9]{2}")
  letter <- substr(code, 1, 1)
  num    <- suppressWarnings(as.numeric(substr(code, 2, 3)))
  case_when(
    !valid ~ "Not specified",
    letter %in% c("A", "B") ~ "A00 - B99: Certain infectious and parasitic diseases",
    letter == "C" | (letter == "D" & num <= 48) ~ "C00 - D48: Neoplasms",
    letter == "D" ~ "D50 - D89: Diseases of the blood and blood - forming organs",
    letter == "E" ~ "E00 - E89: Endocrine nutritional and metabolic diseases",
    letter == "F" ~ "F00 - F99: Mental and behavioural disorders",
    letter == "G" ~ "G00 - G99: Diseases of the nervous system",
    letter == "H" & num <= 59 ~ "H00 - H59: Diseases of the eye and adnexa",
    letter == "H" ~ "H60 - H95: Diseases of the ear and mastoid process",
    letter == "I" ~ "I00 - I99: Diseases of the circulatory system",
    letter == "J" ~ "J00 - J99: Diseases of the respiratory system",
    letter == "K" ~ "K00 - K95: Diseases of the digestive system",
    letter == "L" ~ "L00 - L98: Diseases of the skin and subcutaneous tissue",
    letter == "M" ~ "M00 - M99: Diseases of the musculoskeletal system and connective tissue",
    letter == "N" ~ "N00 - N99: Diseases of the genitourinary system",
    letter == "O" ~ "O00 - O99: Pregnancy childbirth and the puerperium",
    letter == "P" ~ "P00 - P96: Certain conditions originating in the perinatal period",
    letter == "Q" ~ "Q00 - Q99: Congenital malformations deformations and chromosomal abnormalities",
    letter == "R" ~ "R00 - R99: Symptoms signs and abnormal clinical and laboratory findings",
    letter %in% c("S", "T") ~ "S00 - T98: Injury poisoning and certain other consequences of external causes",
    letter %in% c("V", "W", "X", "Y") ~ "V01 - Y98: External causes of morbidity and mortality",
    letter == "Z" ~ "Z00 - Z99: Factors influencing health status and contact with health services",
    letter == "U" ~ "U00 - U99: Codes for special purposes",
    TRUE ~ "Not specified"
  )
}

icd10_chapter_order <- c(
  "A00 - B99: Certain infectious and parasitic diseases",
  "C00 - D48: Neoplasms",
  "D50 - D89: Diseases of the blood and blood - forming organs",
  "E00 - E89: Endocrine nutritional and metabolic diseases",
  "F00 - F99: Mental and behavioural disorders",
  "G00 - G99: Diseases of the nervous system",
  "H00 - H59: Diseases of the eye and adnexa",
  "H60 - H95: Diseases of the ear and mastoid process",
  "I00 - I99: Diseases of the circulatory system",
  "J00 - J99: Diseases of the respiratory system",
  "K00 - K95: Diseases of the digestive system",
  "L00 - L98: Diseases of the skin and subcutaneous tissue",
  "M00 - M99: Diseases of the musculoskeletal system and connective tissue",
  "N00 - N99: Diseases of the genitourinary system",
  "O00 - O99: Pregnancy childbirth and the puerperium",
  "P00 - P96: Certain conditions originating in the perinatal period",
  "Q00 - Q99: Congenital malformations deformations and chromosomal abnormalities",
  "R00 - R99: Symptoms signs and abnormal clinical and laboratory findings",
  "S00 - T98: Injury poisoning and certain other consequences of external causes",
  "V01 - Y98: External causes of morbidity and mortality",
  "Z00 - Z99: Factors influencing health status and contact with health services",
  "U00 - U99: Codes for special purposes",
  "Not specified"
)

# Region display names: patient residence (Table 1 wording)
label_residence <- function(x) {
  case_when(
    x == "Almaty/Zhetisu" ~ "Almaty region*",
    x == "Zhetisu"        ~ "Zhetisu region*",
    x == "Almaty City"    ~ "Almaty City",
    x == "Astana"         ~ "Astana city",
    x == "Shymkent"       ~ "Shymkent city",
    is.na(x)              ~ NA_character_,
    TRUE                  ~ paste(x, "region")
  )
}
# Facility regions (referring / accepting)
label_facility_region <- function(x) if_else(x == "Almaty/Zhetisu", "Almaty region", x)

cohort_labels <- c(
  internal = "Internal validation (2022, October - December)",
  external = "External validation (2023)"
)

model_colours <- c(
  "Naive baseline"        = "#E45756",
  "Flat RF"               = "#9D9D9D",
  "Hierarchical RF"       = "#4C72B0",
  "Hierarchical CatBoost" = "#DD8452",
  "Random Forest"         = "#4C72B0",
  "CatBoost"              = "#DD8452",
  "Refer all"             = "grey40",
  "Refer none"            = "grey70"
)

theme_ms <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          plot.title.position = "plot")
}

save_fig <- function(plot, file, width = 8, height = 6) {
  ggsave(out_path("figures", file), plot, width = width, height = height, dpi = 300, bg = "white")
  cat("  saved figure:", file, "\n")
}

save_table <- function(df, file) {
  write_csv(df, out_path("tables", file))
  cat("  saved table:", file, "\n")
}

read_rds_out <- function(type, file) {
  path <- out_path(type, file)
  if (!file.exists(path)) stop("Missing ", path, " - run the earlier pipeline step that creates it.")
  readRDS(path)
}
