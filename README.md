# A Hierarchical Machine Learning Framework for Predicting Hospital Referral Destinations in Kazakhstan

This repository contains the analytical code accompanying a study that developed and temporally validated machine learning models to predict the destination of inter-hospital referrals in Kazakhstan, using national Hospital Episode Statistics (HES) for 2022 and 2023. Starting from analysis-ready cohorts, it provides the complete modelling pipeline – four prediction models, together with the code that produces all tables and figures reported in the manuscript – and an interactive Shiny prototype illustrating how model predictions could be integrated into referral decision-making.

> **Data availability.** The HES data are confidential and are not distributed with this repository. To allow the code to be executed end to end, the repository includes **synthetic data** that reproduce the structure of the analytical data sets (variable names, types and category labels) but not the associations within them. Results obtained from the synthetic data are illustrative only and should not be interpreted as findings of the study (see [Synthetic data](#synthetic-data)).

## Study design

**Setting and population.** The HES database covers all inpatient admissions to hospitals nationwide. A referral was defined as an admission in which the patient was referred from one healthcare facility to a different facility; records in which the referring and receiving facilities were the same were excluded. Admissions to private facilities were also excluded, as private providers operate under referral pathways distinct from those of public facilities and are not subject to the same regional planning arrangements. The unit of analysis was the hospital referral episode. Data from 2022 and 2023 were used to reflect referral behaviour under the current financing arrangements following the introduction of Mandatory Social Health Insurance (MSHI) in 2020 and the disruption associated with the COVID-19 pandemic.

**Outcome.** The outcome was the level of the accepting facility, grouped into five categories: city, regional, republican/national, rural/district and specialised.

**Predictors.** Predictors were restricted to information available to the referring clinician before the referral decision:

- **Patient characteristics:** age, sex, area of residence, employment status and MSHI benefit category.
- **Admission characteristics:** surgery type, and day of week and month of admission.
- **Clinical diagnosis** recorded at referral (ICD-10).
- **Referring facility:** level and region.

Characteristics of potential accepting facilities were not included, as they are related to the referral outcome and could introduce information available only after, or as part of, the referral decision. Instead, regional indicators of health-system capacity (doctors and hospital beds per 10,000 population) and of economic context (average wage, unemployment rate, population and gross regional product per capita) were included.

**Temporal validation.** Rather than splitting the data randomly, the 2022 cohort was divided chronologically:

- admissions from January to September formed the **training** set;
- admissions from October to December were used for **internal temporal validation**;
- the 2023 cohort served as an **external temporal validation** set.

This approach is consistent with TRIPOD recommendations for clinical prediction models.

## Models

Four models were compared:

1. **Naive baseline** – a simple approximation of current referral practice that predicts, for each referring facility level, the most common historical destination in the training data, without accounting for patient or clinical characteristics.
2. **Flat random forest (RF)** – direct classification of referrals into the five destination categories.
3. **Hierarchical RF** and 4. **Hierarchical CatBoost** – two algorithms applied within a three-stage framework reflecting the structure of the referral system, which distinguishes destinations reimbursed through diagnosis-related groups (DRG) from those reimbursed through other payment methods:

```
                         Referral destination
                                  |
       Stage 1: DRG-funded vs non-DRG-funded (inverse-frequency class weights)
                 |                                      |
   Stage 2: City vs Regional              Stage 3: Republican/National vs
                                          Rural/District vs Specialised
```

For each observation, the probability of each final destination was obtained by multiplying the stage 1 probability by the corresponding stage 2 or stage 3 conditional probability. These probabilities were computed for every observation irrespective of its actual destination, so that errors at the first stage propagate to the final prediction, as they would in practice; the actual destination was never used to select the second- or third-stage model.

RF and CatBoost were chosen because both are tree-based ensemble methods that capture non-linear relationships and interactions without prespecified functional forms and are more interpretable than deep learning approaches. They represent two distinct ensemble strategies (bagging and gradient boosting), which allowed us to assess whether the benefits of the hierarchical approach were consistent across algorithms. CatBoost also handles categorical predictors natively, which is advantageous given the high cardinality of variables such as diagnosis and referring region.

**Hyperparameter tuning.** Hyperparameters were selected by grid search on a subsample of the training data, using balanced accuracy as the selection criterion:

- **RF:** number of variables considered at each split, minimum terminal node size and sampling fraction, evaluated on out-of-bag predictions.
- **CatBoost:** number of iterations, tree depth and learning rate, evaluated on a held-out validation set within the training subsample.

The best-performing configuration was refitted to the full training data; RF models used 1,000 trees. The same hyperparameters were applied at all three stages, so that differences in performance between stages reflect the underlying classification task.

## Evaluation

- **Discrimination:** accuracy, balanced accuracy and macro-averaged precision, recall and F1-score, with 95% bootstrap confidence intervals; per-category recall and confusion matrices; and, for the stage 1 classifier, the area under the receiver operating characteristic curve.
- **Calibration:** calibration of the stage 1 probability and of the combined hierarchical probability for rural/district referrals.
- **Clinical utility:** decision curve analysis on the external validation cohort, assessing net benefit for referral to a non-DRG-funded facility and for identification of rural/district referrals, compared with referring all patients, referring none and the naive baseline.
- **Regional variation:** model performance by referring region.
- **Outcome concordance:** length of stay and in-hospital complication rates compared between referrals whose actual destination matched the model's top-ranked prediction and those that did not, overall, for high-confidence predictions and stratified by ICD-10 chapter. This retrospective analysis is hypothesis-generating and does not imply a causal effect of model-concordant referral on patient outcomes.

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
and are not shared; access requires approval from the data holder. The pipeline starts from analysis-ready
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
