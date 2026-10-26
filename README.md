# Predicting hospital referral destinations in Kazakhstan with hierarchical machine learning

Code for the study on predicting the destination of inter-hospital referrals in Kazakhstan's
national Hospital Episode Statistics (HES), 2022–2023. Starting from analysis-ready cohorts, the
repository contains the full modelling pipeline (four prediction models, all manuscript tables and
figures) and an interactive Shiny decision-support prototype.

> **Data note.** The HES data are confidential and are **not** included. The repository ships with
> **synthetic data** that have the same structure as the analytical data sets, so every script and
> the Shiny app can be run end to end. Results obtained on the synthetic data are for demonstration
> only and say nothing about the real study (see [Synthetic data](#synthetic-data)).

## Study design in brief

- **Unit of analysis:** hospital referral episode (referring facility ≠ accepting facility), public facilities only.
- **Outcome:** level of the accepting facility – City, Regional, Republican/National, Rural/District, Specialised.
- **Predictors:** information available to the referring clinician before the referral decision –
  patient characteristics, diagnosis at referral (ICD-10), admission day and month, surgery type,
  referring facility level and region, and regional health-system capacity and economic indicators.
  Characteristics of the accepting facility are excluded.
- **Validation:** temporal. 2022 January–September = training; 2022 October–December = internal
  validation; 2023 = external validation.
- **Models**
  1. *Naive baseline* – the most common historical destination for each referring facility level.
  2. *Flat random forest* – direct five-class classification.
  3. *Hierarchical random forest* and 4. *hierarchical CatBoost*, following the referral system:

```
                     Referral destination
                              |
        Stage 1: DRG-funded vs non-DRG-funded (class-weighted)
                 |                              |
   Stage 2: City vs Regional      Stage 3: Republican/National vs
                                  Rural/District vs Specialised
```

  Final probabilities are P(stage-1 branch) × P(class | branch), computed for every episode from
  the predicted stage-1 probabilities, so stage-1 errors propagate as they would in practice.
- **Tuning:** grid search on a training subsample selected by balanced accuracy (RF: `mtry`,
  minimum node size, sampling fraction, out-of-bag; CatBoost: iterations, depth, learning rate,
  held-out split). RF uses 1,000 trees. The same hyperparameters are used in all three stages.
- **Evaluation:** accuracy, balanced accuracy, macro precision/recall/F1 with 95% bootstrap CIs,
  per-class recall, confusion matrices, stage-1 AUC, calibration, decision curve analysis,
  regional variation and an outcome-concordance analysis (length of stay, complications).

## Repository structure

```
00_main.R                       runs the whole pipeline (switch steps on/off)
00_config.R                     settings: data files, seed, tuning grids, model for maps/concordance
01_functions.R                  metrics, bootstrap CIs, hierarchical combination, calibration, DCA, labels
02_cohorts_split.R              outcome, temporal split, DRG / non-DRG target
03_naive_and_flat_rf.R          models 1–2 and random forest tuning
04_hierarchical_rf.R            model 3
05_catboost_tuning.R            CatBoost tuning
06_hierarchical_catboost.R      model 4
07_table1_characteristics.R     Table 1   cohort characteristics
08_table2_destinations.R        Table 2   destinations by cohort
09_tables_performance.R         Tables 3, 4, S2; confusion matrices; stage-1 ROC
10_fig_variable_importance.R    variable importance, stages 1 and 3
11_fig_calibration.R            calibration (stage 1, rural/district), stage-1 probability for rural/district referrals
12_dca.R                        decision curve analysis
13_regional_maps.R              regional model performance and referral concentration maps
14_concordance.R                outcome-concordance analysis
15_shiny_inputs.R               inputs for the Shiny app
app.R                           Shiny decision-support prototype
install_packages.R              installs all required R packages
data/synthetic/                 synthetic analysis-ready cohorts (.rds for the pipeline, .csv.gz for inspection)
data-raw/                       script (and public regional indicators) that generated the synthetic cohorts
docs/data_dictionary.md         variables in the analysis-ready data sets
```

All results are written to `output/` (`tables/`, `figures/`, `models/`, `predictions/`), which is not tracked by git.

## Quick start

1. Install R (developed with R 4.6.1) and the packages:
   ```r
   source("install_packages.R")
   ```
   `catboost` is not on CRAN; the script installs the binary from the CatBoost GitHub releases.
2. Open `referral-pathway-ml.Rproj` in RStudio (or `setwd()` to the repository folder) and run:
   ```r
   source("00_main.R")
   ```
   With the default synthetic data the whole pipeline takes about 5 minutes on a laptop (the real data take about 1.5 hours).
3. Launch the decision-support prototype (after step 15 has run):
   ```r
   shiny::runApp()
   ```

Each step saves its results, so steps can be switched off in `run_steps` in `00_main.R` and figures
or tables can be redrawn without refitting the models.

## Data availability

The study used de-identified national HES data for 2022 and 2023. These data are confidential
and are not shared; access requires approval from the data holder
([DATA HOLDER / ACCESS PROCEDURE – to be completed]). The pipeline starts from analysis-ready
cohorts (one row per referral episode; see `docs/data_dictionary.md`). Users with approved access
to data of the same structure can point `data_files` in `00_config.R` to their own files. Never
commit such files: `.gitignore` excludes all data except `data/synthetic/`.

## Synthetic data

`data/synthetic/` contains 24,000 (2022) and 20,000 (2023) artificial referral episodes created by
`data-raw/make_synthetic_data.R`. No real records were used: category frequencies follow the
published aggregate percentages in Table 1 of the paper, regional indicators are public statistics,
and referral destinations are drawn from an invented model tuned only to reproduce the published
destination shares. The synthetic data reproduce the structure of the real data (column names,
types and category labels), not the relationships in it.

## Outputs and manuscript items

| Manuscript item | File in `output/` |
|---|---|
| Table 1 | `tables/table1_characteristics.csv` |
| Table 2 | `tables/table2_destinations.csv` |
| Table 3 | `tables/table3_per_category_recall.csv` |
| Table 4 | `tables/table4_overall_performance_ci.csv` |
| Table S2, Figure S4 | `tables/tableS2_stage1_performance_auc.csv`, `figures/figureS4_stage1_roc.png` |
| Variable importance | `figures/figure_importance_stage1.png`, `figures/figure_importance_stage3.png` |
| Calibration | `figures/figure_calibration_stage1.png`, `figures/figure5_calibration_rural_district.png` |
| Stage-1 probability, rural/district referrals | `figures/figure_stage1_probability_rural_district.png` |
| Figure 6 | `figures/figure6_regional_model_performance.png` |
| Figure 8 | `figures/figure8_dca_non_drg.png` |
| Concordance analysis | `tables/concordance_*.csv`, `figures/figure_concordance_outcomes.png` |

## Shiny prototype

`app.R` is a proof-of-concept showing how the hierarchical model could support referral decisions.
It asks only for information available at referral; regional indicators are filled in from the
selected region. Predictions with a top probability of at least 70% are shown as a recommendation,
lower ones are flagged for manual review (an illustrative, not optimised, threshold).
**It is not a validated clinical decision-support tool.**

## Software

R 4.6.1; random forests with `ranger`, gradient boosting with `catboost`, ROC analysis with `pROC`,
data management and graphics with the `tidyverse`; maps with `sf` and `geokz`; app with `shiny`.
Random seed: 20260818.

## Citation

If you use this code, please cite the accompanying article (citation to be added on publication).

## Licence

MIT – see [LICENSE](LICENSE).
