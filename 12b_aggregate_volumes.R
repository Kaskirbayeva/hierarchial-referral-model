############################################################
# 12b_aggregate_volumes.R - PREDICTED VS OBSERVED REFERRAL VOLUMES
# (evaluation for planners: predicted vs observed volumes)
#
# For the 2023 external validation cohort, the expected number of
# referrals to each destination is the SUM of the (recalibrated)
# predicted probabilities, nationally and by referring region, and
# is compared with the observed number. Classify-and-count (number
# of top-ranked predictions) and the uncalibrated sums are shown for
# comparison.
#
# Outputs (output/tables, output/figures):
#   table6_volumes_national.csv          national, by destination and model
#   table6_volumes_by_region.csv         referring region x destination x model
#   table6_volumes_region_summary.csv    error across regions, by destination and model
#   figure_volumes_by_region.png
############################################################

if (!exists("CFG")) source("00_config.R")

stub <- c("Flat RF" = "flat_rf", "Weighted flat RF" = "flat_rf_weighted",
          "Weighted flat CatBoost" = "flat_cb_weighted", "Flat CatBoost" = "flat_cb",
          "Hierarchical RF" = "hier_rf", "Hierarchical CatBoost" = "hier_cb")
files <- paste0("pred_", stub, "_recalibrated.rds"); names(files) <- names(stub)
files <- files[file.exists(out_path("preds", files))]
if (!length(files)) stop("Run 16b_recalibration.R first.")
ext_region <- read_rds_out("data", "cohorts.rds")$df_extval_2023$refer_region

long <- imap_dfr(files, function(f, model) {
  d <- readRDS(out_path("preds", f))
  stopifnot(nrow(d) == length(ext_region))
  map_dfr(class_order, function(cl) tibble(
    model = model, refer_region = ext_region, destination = cl,
    observed = as.integer(d$actual == cl),
    expected_recalibrated = d[[paste0(cl, "_recal")]],
    expected_uncalibrated = d[[cl]],
    classify_count = as.integer(d$pred == cl)))
})

summ <- function(x) x %>% summarise(observed = sum(observed),
                                    expected_recalibrated = sum(expected_recalibrated),
                                    expected_uncalibrated = sum(expected_uncalibrated),
                                    classify_count = sum(classify_count), .groups = "drop") %>%
  mutate(pct_diff_recalibrated = 100 * (expected_recalibrated - observed) / observed,
         pct_diff_uncalibrated = 100 * (expected_uncalibrated - observed) / observed,
         pct_diff_classify_count = 100 * (classify_count - observed) / observed)

national <- long %>% group_by(model, destination) %>% summ() %>%
  mutate(destination = factor(destination, levels = class_order)) %>% arrange(model, destination)
by_region <- long %>% group_by(model, refer_region, destination) %>% summ()
region_summary <- by_region %>% filter(observed >= 30) %>%
  group_by(model, destination) %>%
  summarise(n_regions = n(),
            median_abs_pct_diff = median(abs(pct_diff_recalibrated)),
            min_pct_diff = min(pct_diff_recalibrated), max_pct_diff = max(pct_diff_recalibrated),
            weighted_abs_pct_error = 100 * sum(abs(expected_recalibrated - observed)) / sum(observed),
            .groups = "drop") %>%
  mutate(destination = factor(destination, levels = class_order)) %>% arrange(model, destination)

print(national, n = Inf, width = Inf); print(region_summary, n = Inf, width = Inf)
save_table(national, "table6_volumes_national.csv")
save_table(by_region, "table6_volumes_by_region.csv")
save_table(region_summary, "table6_volumes_region_summary.csv")

fig <- by_region %>% filter(model %in% c("Hierarchical RF", "Hierarchical CatBoost")) %>%
  ggplot(aes(observed, expected_recalibrated, colour = model)) +
  geom_abline(linetype = "dashed", colour = "grey55") + geom_point(alpha = 0.8) +
  facet_wrap(~ factor(destination, levels = class_order), scales = "free") +
  scale_colour_manual(values = model_colours) + theme_ms(11) +
  labs(title = "Observed and predicted referral volumes by referring region, 2023",
       subtitle = "Predicted = sum of recalibrated probabilities (recalibration fitted on Oct-Dec 2022)",
       x = "Observed referrals", y = "Predicted referrals", colour = NULL)
save_fig(fig, "figure_volumes_by_region.png", width = 12, height = 8)

rm(long); invisible(gc())
cat("Step 12b complete.\n")
