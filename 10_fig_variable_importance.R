############################################################
# 10_fig_variable_importance.R - VARIABLE IMPORTANCE FIGURES
#
#   Stage 1 (DRG vs non-DRG) and Stage 3 (non-DRG destinations),
#   hierarchical RF (permutation importance) vs hierarchical
#   CatBoost (PredictionValuesChange). Each model's importance
#   is scaled to max = 1 so the two can be compared.
#
# Outputs: output/figures/figure_importance_stage1.png,
#          figure_importance_stage3.png,
#          output/tables/variable_importance_comparison.csv
############################################################

if (!exists("CFG")) source("00_config.R")

imp <- bind_rows(
  read_csv(out_path("tables", "importance_hier_rf.csv"), show_col_types = FALSE),
  read_csv(out_path("tables", "importance_hier_cb.csv"), show_col_types = FALSE)
) %>%
  group_by(model, stage) %>%
  mutate(importance_scaled = pmax(importance, 0) / max(importance)) %>%
  ungroup()

save_table(imp %>% arrange(stage, model, desc(importance_scaled)), "variable_importance_comparison.csv")

plot_importance <- function(stage_name, title, top_n = 15) {
  d <- imp %>% filter(stage == stage_name)
  top_vars <- d %>% group_by(variable) %>% summarise(m = mean(importance_scaled)) %>%
    slice_max(m, n = top_n) %>% arrange(m) %>% pull(variable)
  d %>%
    filter(variable %in% top_vars) %>%
    mutate(label = factor(label_var(variable), levels = label_var(top_vars)),
           model = factor(model, levels = c("Random Forest", "CatBoost"))) %>%
    ggplot(aes(x = importance_scaled, y = label, fill = model)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    scale_fill_manual(values = model_colours) +
    scale_x_continuous(limits = c(0, 1.02), expand = c(0, 0)) +
    theme_ms() +
    labs(title = title, x = "Relative importance (maximum = 1)", y = NULL, fill = NULL)
}

p1 <- plot_importance("Stage 1", "Variable importance: Stage 1 (DRG vs non-DRG funded destination)")
p3 <- plot_importance("Stage 3", "Variable importance: Stage 3 (Republican/National, Rural/District, Specialised)")
save_fig(p1, "figure_importance_stage1.png", width = 9, height = 6.5)
save_fig(p3, "figure_importance_stage3.png", width = 9, height = 6.5)

cat("Step 10 complete.\n")
