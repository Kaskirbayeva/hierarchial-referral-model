############################################################
# 13_regional_maps.R - REGIONAL VARIATION (MAPS)
#
#   Figure 6: balanced accuracy of the hierarchical model by
#             referring region, external validation (2023)
#             (model set by CFG$analysis_model; CSV has all models)
#   Supplementary: referral concentration (Herfindahl-Hirschman
#             index of destination shares) by referring region,
#             full analytical cohort
#
# Boundaries: {geokz} (installed from GitHub if missing). The
# data use a combined "Almaty/Zhetisu" region, so the Almaty and
# Zhetisu polygons are merged.
############################################################

if (!exists("CFG")) source("00_config.R")
library(sf)
if (!requireNamespace("geokz", quietly = TRUE)) remotes::install_github("arodionoff/geokz")
library(geokz)

coh <- read_rds_out("data", "cohorts.rds")
pred_files <- c("Naive baseline" = "pred_naive.rds", "Flat RF" = "pred_flat_rf.rds",
                "Hierarchical RF" = "pred_hier_rf.rds", "Hierarchical CatBoost" = "pred_hier_cb.rds")

# --- balanced accuracy by referring region (all models) ---------
# Recall is averaged over destinations present in that region.
regional_perf <- imap_dfr(pred_files, function(f, model) {
  d <- read_rds_out("preds", f)$external
  d %>%
    mutate(refer_region = coh$df_extval_2023$refer_region) %>%
    filter(!is.na(refer_region)) %>%
    group_by(refer_region, actual) %>%
    summarise(n_actual = n(), recall = mean(pred == actual), .groups = "drop") %>%
    group_by(refer_region) %>%
    summarise(n_referrals = sum(n_actual), n_classes_present = n(),
              balanced_accuracy = mean(recall), .groups = "drop") %>%
    mutate(model = model, .before = 1)
})
save_table(regional_perf, "regional_balanced_accuracy_external.csv")

# --- HHI by referring region (full cohort) ------------------------
regional_hhi <- bind_rows(coh$train_main, coh$val_internal, coh$df_extval_2023) %>%
  filter(!is.na(refer_region)) %>%
  count(refer_region, referral_destination) %>%
  group_by(refer_region) %>%
  summarise(n_referrals = sum(n), hhi = sum((n / sum(n))^2), .groups = "drop")
save_table(regional_hhi, "regional_hhi_full_cohort.csv")

# --- map ---------------------------------------------------------
kaz <- kaz_adm1_sf
is_almaty  <- grepl("almaty", kaz$ADM1_EN, ignore.case = TRUE) & !grepl("city|qalasy|shahar", kaz$ADM1_EN, ignore.case = TRUE)
is_zhetisu <- grepl("zhetisu|jetisu|zhetysu", kaz$ADM1_EN, ignore.case = TRUE)
crosswalk  <- c("Abay" = "Abai", "Almaty (city)" = "Almaty City", "Astana (city)" = "Astana",
                "Karagandy" = "Karaganda", "Shymkent (city)" = "Shymkent", "Turkestan" = "Turkistan")

kaz <- kaz %>%
  mutate(region = if_else(is_almaty | is_zhetisu, "Almaty/Zhetisu", ADM1_EN)) %>%
  group_by(region) %>% summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf() %>% st_make_valid() %>%
  mutate(region = if_else(region %in% names(crosswalk), unname(crosswalk[region]), region))

old_s2 <- sf_use_s2(); sf_use_s2(FALSE)
centroids <- kaz %>% st_set_agr("constant") %>% st_point_on_surface() %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>% st_drop_geometry()
sf_use_s2(old_s2)

map_plot <- function(data, value, legend, title, subtitle, labels = waiver()) {
  m <- kaz %>% left_join(data, by = c("region" = "refer_region"))
  unmatched <- setdiff(data$refer_region, kaz$region)
  if (length(unmatched) > 0) warning("No map polygon for: ", paste(unmatched, collapse = ", "))
  ggplot(m) +
    geom_sf(aes(fill = .data[[value]]), colour = "black", linewidth = 0.2) +
    geom_text(data = centroids, aes(lon, lat, label = label_facility_region(region)), size = 2.5) +
    scale_fill_gradient(low = "white", high = "grey20", na.value = "grey85", name = legend, labels = labels) +
    theme_void(base_size = 11) +
    labs(title = title, subtitle = subtitle)
}

fig6 <- map_plot(regional_perf %>% filter(model == CFG$analysis_model), "balanced_accuracy",
                 "Balanced\naccuracy", "Regional variation in model performance",
                 paste0(CFG$analysis_model, ", balanced accuracy by referring region, external validation (2023)"),
                 labels = scales::percent)
save_fig(fig6, "figure6_regional_model_performance.png", width = 9, height = 6.5)

fig_hhi <- map_plot(regional_hhi, "hhi", "Referral\nconcentration\n(HHI)",
                    "Regional variation in referral concentration",
                    "Herfindahl-Hirschman index of referral destinations by referring region, 2022-2023")
save_fig(fig_hhi, "figure_regional_hhi.png", width = 9, height = 6.5)

rm(coh); invisible(gc())
cat("Step 13 complete.\n")
