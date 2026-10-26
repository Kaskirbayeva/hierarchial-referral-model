############################################################
# data-raw/make_synthetic_data.R - GENERATE SYNTHETIC COHORTS
#
# Creates data/synthetic/df_model_22_synthetic.rds and
# df_model_23_synthetic.rds: artificial referral episodes with the
# SAME STRUCTURE (columns, types, category labels) as the
# analysis-ready data used in the study, so the whole pipeline and
# the Shiny app can be run without access to the restricted
# Hospital Episode Statistics (HES) data.
#
# NO REAL RECORDS ARE USED. Every value is simulated:
#   - marginal distributions follow the PUBLISHED aggregate
#     percentages in Table 1 of the manuscript;
#   - regional indicators are public 2022 statistics (Bureau of
#     National Statistics), stored in data-raw/regional_indicators_2022.csv;
#   - the referral destination is simulated from an invented
#     multinomial model (referring level, diagnosis, region, age)
#     tuned only to reproduce the published destination shares.
# Model performance on these data says NOTHING about the real study.
#
# Run from the repository root:  source("data-raw/make_synthetic_data.R")
############################################################

suppressPackageStartupMessages(library(tidyverse))
regional_data <- read_csv("data-raw/regional_indicators_2022.csv", show_col_types = FALSE)

set.seed(2026)

n_2022 <- 24000
n_2023 <- 20000

classes <- c("City", "Regional", "Republican/National", "Rural/District", "Specialised")
target_share <- c(0.339, 0.524, 0.066, 0.007, 0.063)   # Table 1, accepting facility level 2022

# ---- published marginal distributions (Table 1, 2022) -------------------
p_refer_level <- c("City" = 6.4, "Polyclinic" = 51.2, "Regional" = 1.8,
                   "Republican/National" = 0.3, "Rural/District" = 40.1, "Specialised" = 0.1)
p_refer_region <- c("Abai" = 1.5, "Akmola" = 4.5, "Aktobe" = 3.9, "Almaty City" = 0.05,
                    "Almaty/Zhetisu" = 21.3, "Astana" = 10.9, "Atyrau" = 2.1, "East Kazakhstan" = 3.0,
                    "Karaganda" = 4.4, "Kostanay" = 5.3, "Kyzylorda" = 5.0, "Mangystau" = 4.3,
                    "North Kazakhstan" = 5.1, "Pavlodar" = 5.6, "Shymkent" = 3.2, "Turkistan" = 7.8,
                    "Ulytau" = 0.7, "West Kazakhstan" = 4.9, "Zhambyl" = 6.4)
p_chapter <- c(A = 0.5, C = 3.3, D = 1.3, E = 2.5, F = 0.04, G = 0.6, H1 = 4.2, H2 = 0.9, I = 13.0,
               J = 3.7, K = 6.6, L = 0.7, M = 3.1, N = 5.5, O = 37.5, P = 0.3, Q = 2.2, R = 0.2,
               S = 4.5, V = 1.1, Z = 8.2, U = 0.1)
p_dow   <- c(Monday = 22.5, Tuesday = 18.5, Wednesday = 19.6, Thursday = 18.7, Friday = 11.6, Saturday = 4.7, Sunday = 4.4)
p_month <- setNames(c(6.3, 8.5, 8.6, 8.9, 8.9, 9.5, 8.1, 8.7, 8.8, 8.5, 9.1, 5.9), month.name)
p_employment_adult <- c("Employed" = 32.5, "Homemaker" = 26.2, "Unemployed" = 4.9, "Other" = 6.0,
                        "Disabled" = 3.2, "Military" = 0.02, "Prisoner" = 0.03)

# ICD-10 code pools: each chapter gets a fixed set of invented-but-valid-format
# 3-character codes with skewed (Zipf-like) frequencies
chapter_codes <- list(
  A = c("A", 0, 99), C = c("C", 0, 97), D = c("D", 50, 89), E = c("E", 0, 89), F = c("F", 0, 99),
  G = c("G", 0, 99), H1 = c("H", 0, 59), H2 = c("H", 60, 95), I = c("I", 0, 99), J = c("J", 0, 99),
  K = c("K", 0, 95), L = c("L", 0, 98), M = c("M", 0, 99), N = c("N", 0, 99), O = c("O", 0, 99),
  P = c("P", 0, 96), Q = c("Q", 0, 99), R = c("R", 0, 99), S = c("S", 0, 99), V = c("W", 0, 99),
  Z = c("Z", 0, 99), U = c("U", 0, 99)
)
code_pool <- imap(chapter_codes, function(x, ch) {
  nums <- sample(as.integer(x[2]):as.integer(x[3]), min(25, as.integer(x[3]) - as.integer(x[2]) + 1))
  codes <- sprintf("%s%02d", x[1], nums)
  list(codes = codes, w = 1 / seq_along(codes)^0.9)
})
all_codes <- unlist(map(code_pool, "codes"))

# Invented code- and region-level effects on each destination (fixed across years)
code_effect   <- matrix(rnorm(length(all_codes) * 5, 0, 0.8), ncol = 5, dimnames = list(all_codes, classes))
region_effect <- matrix(rnorm(length(p_refer_region) * 5, 0, 0.5), ncol = 5, dimnames = list(names(p_refer_region), classes))

# ---- one cohort -------------------------------------------------------------
simulate_cohort <- function(n, year, drift = 0) {

  chapter <- sample(names(p_chapter), n, TRUE, p_chapter)
  icd3 <- map_chr(chapter, ~ sample(code_pool[[.x]]$codes, 1, prob = code_pool[[.x]]$w))

  # age consistent with diagnosis chapter
  age_cat <- case_when(
    chapter == "O" ~ "young_adult",
    chapter == "P" ~ "newborn",
    TRUE ~ sample(c("newborn", "child", "young_adult", "middle_adult", "senior"), n, TRUE,
                  c(4.3, 7.2, 35, 41, 12.5))
  )
  age <- case_when(
    age_cat == "newborn"      ~ sample(0:4, n, TRUE),
    age_cat == "child"        ~ sample(5:17, n, TRUE),
    age_cat == "young_adult"  ~ sample(18:44, n, TRUE),
    age_cat == "middle_adult" ~ sample(45:71, n, TRUE),
    TRUE                      ~ sample(72:95, n, TRUE)
  )
  female <- if_else(chapter == "O", 1, rbinom(n, 1, 0.55))

  employment_group <- case_when(
    age < 18 ~ "Student/Child",
    age >= 63 & runif(n) < 0.9 ~ "Pensioner",
    age < 23 & runif(n) < 0.4 ~ "Student/Child",
    TRUE ~ sample(names(p_employment_adult), n, TRUE, p_employment_adult)
  )
  benefit_group <- if_else(
    runif(n) < case_when(employment_group %in% c("Student/Child", "Pensioner", "Disabled") ~ 0.7,
                         chapter == "O" ~ 0.15, TRUE ~ 0.08),
    "Benefit", "No benefit")

  refer_region <- sample(names(p_refer_region), n, TRUE, p_refer_region)
  # patient residence: mostly the referring region (Almaty/Zhetisu split into the two oblasts)
  residence <- if_else(runif(n) < 0.85, refer_region, sample(names(p_refer_region), n, TRUE))
  residence <- if_else(residence == "Almaty/Zhetisu" & runif(n) < 0.5, "Zhetisu", residence)
  refer_level <- sample(names(p_refer_level), n, TRUE, p_refer_level)

  # ICD-10 at referral: ~20% missing ("Not specified"), otherwise mostly the final code
  icd10_ref <- if_else(runif(n) < 0.75, icd3, map_chr(chapter, ~ sample(code_pool[[.x]]$codes, 1)))
  icd10_ref <- paste0(icd10_ref, ".", sample(0:9, n, TRUE))
  icd10_ref[runif(n) < 0.20] <- "Not specified"

  dow   <- sample(names(p_dow), n, TRUE, p_dow)
  month <- sample(month.name, n, TRUE, p_month)

  df <- tibble(
    age, female, gender = if_else(female == 1, "F", "M"), age_cat, residence,
    region = residence, employment_group, benefit_group, refer_level, refer_region,
    icd10_ref, icd3,
    sur_type = if_else(runif(n) < 0.178, "Additional_operation", "Primary_operation"),
    admission_day_of_week = factor(dow, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), ordered = TRUE),
    admission_month = factor(month, levels = month.name, ordered = TRUE)
  ) %>%
    left_join(regional_data, by = "region")

  # ---- destination: invented multinomial model ----------------------------
  ch <- chapter; rl <- refer_level
  lp <- cbind(
    City     = 1.6 * (rl %in% c("Polyclinic", "City")) + 0.9 * (refer_region %in% c("Almaty City", "Astana", "Shymkent")) +
               0.4 * (ch == "O"),
    Regional = 1.6 * (rl == "Rural/District") + 1.0 * (rl == "Regional") + 0.3 * (ch %in% c("S", "K")),
    `Republican/National` = 1.6 * (ch %in% c("C", "I", "Q")) + 2.0 * (rl %in% c("Regional", "Republican/National")) +
               0.8 * (refer_region == "Astana") + 0.02 * (df$doctors_per10k - 45),
    `Rural/District` = 1.0 * (rl == "Rural/District") + 0.8 * (ch %in% c("O", "Z")) + 0.5 * (ch == "J") -
               0.05 * (df$beds_per10k - 53),
    Specialised = 2.5 * (ch %in% c("H1", "H2")) + 1.5 * (ch == "Q") + 1.0 * (ch %in% c("C", "P")) + 0.8 * (age < 18)
  )
  code_key <- substr(if_else(icd10_ref == "Not specified", icd3, icd10_ref), 1, 3)
  # scale up so that destinations are reasonably (not perfectly) predictable
  lp <- 2.2 * (lp + code_effect[code_key, ] + region_effect[refer_region, ]) + rnorm(n * 5, 0, 0.3)
  list(df = df, lp = lp, chapter = chapter)
}

# calibrate class intercepts so the 2022 destination shares match Table 1
draw_destination <- function(lp, intercept) {
  eta <- sweep(lp, 2, intercept, "+")
  p <- exp(eta - apply(eta, 1, max)); p <- p / rowSums(p)
  list(p = p, y = classes[apply(p, 1, function(r) sample.int(5, 1, prob = r))])
}
cal <- simulate_cohort(n_2022, 2022)
intercept <- setNames(rep(0, 5), classes)
for (i in 1:200) {
  p <- { eta <- sweep(cal$lp, 2, intercept, "+"); e <- exp(eta - apply(eta, 1, max)); e / rowSums(e) }
  intercept <- intercept + 0.5 * (log(target_share) - log(colMeans(p)))
}

finish <- function(sim, year, shift = 0) {
  icpt <- intercept + shift
  y <- draw_destination(sim$lp, icpt)$y
  n <- length(y)
  ch <- sim$chapter
  sim$df %>%
    mutate(
      accept_level = y,
      accept_region = case_when(
        accept_level %in% c("Republican/National", "Specialised") ~
          sample(c("Astana", "Almaty City", "Almaty/Zhetisu", "Karaganda"), n, TRUE, c(0.5, 0.35, 0.1, 0.05)),
        TRUE ~ refer_region),
      los = pmax(0, round(rgamma(n, shape = if_else(ch == "O", 3, 2.2), scale = if_else(ch == "O", 1.2, 3)))),
      complications = rbinom(n, 1, if_else(accept_level %in% c("Republican/National", "Specialised"), 0.004, 0.0018))
    ) %>%
    # a few polyclinic destinations, which the pipeline excludes (as in the real data)
    mutate(accept_level = if_else(runif(n) < 0.003, "Polyclinic", accept_level)) %>%
    relocate(accept_level, .after = refer_region)
}

df_22 <- finish(cal, 2022)
# 2023: same mechanism with small changes in practice (temporal drift)
df_23 <- finish(simulate_cohort(n_2023, 2023), 2023,
                shift = c(0, -0.05, 0.10, 0.05, 0.08))

dir.create("data/synthetic", recursive = TRUE, showWarnings = FALSE)
saveRDS(df_22, "data/synthetic/df_model_22_synthetic.rds")
saveRDS(df_23, "data/synthetic/df_model_23_synthetic.rds")
write_csv(df_22, "data/synthetic/df_model_22_synthetic.csv.gz")
write_csv(df_23, "data/synthetic/df_model_23_synthetic.csv.gz")

cat("Synthetic destination shares 2022:\n"); print(round(prop.table(table(df_22$accept_level)), 3))
cat("Synthetic destination shares 2023:\n"); print(round(prop.table(table(df_23$accept_level)), 3))
