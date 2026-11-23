# A Hierarchical Machine Learning Framework for Predicting Hospital Referral Destinations in Kazakhstan

This repository contains the analytical code accompanying a study that developed and temporally validated machine-learning models to predict the destinations of interhospital referrals in Kazakhstan using national Hospital Episode Statistics (HES) from 2022 and 2023. Starting with analysis-ready cohorts, the repository provides the complete modelling pipeline for six prediction models, the code used to produce all tables and figures reported in the manuscript, and an interactive Shiny prototype illustrating how model predictions could be integrated into referral decision-making.

> **Data availability.** The HES data are confidential and are not distributed with this repository. To allow the code to be executed end to end, the repository includes **synthetic data** that reproduce the structure of the analytical datasets (variable names, types and category labels) but not the associations within them. Results obtained from the synthetic data are illustrative only and should not be interpreted as findings of the study (see [Synthetic data](#synthetic-data)).

## Study design

**Setting and population.** The HES database covers all inpatient admissions to hospitals nationwide. A referral was defined as an admission in which the patient was referred from one healthcare facility to a different facility. Records in which the referring and receiving facilities were the same were excluded. Admissions to private facilities were also excluded, as private providers operate under referral pathways distinct from those of public facilities and are not subject to the same regional planning arrangements. The unit of analysis was the hospital referral episode. Data from 2022 and 2023 were used to reflect referral behaviour under the current financing arrangements following the introduction of Mandatory Social Health Insurance (MSHI) in 2020 and the disruption associated with the COVID-19 pandemic.

**Outcome.** The outcome was the level of the accepting facility, grouped into five categories: city, regional, republican/national, rural/district and specialised.

**Predictors.** Predictors were restricted to information available to the referring clinician before the referral decision:

- **Patient characteristics:** age, sex, area of residence, employment status and MSHI benefit category.
- **Admission characteristics:** surgery type (the main operation and any additional operations proposed by the referring facility) and day of the week and month of admission.
- **Clinical diagnosis** recorded at referral (ICD-10), entered by the referring facility; a referral diagnosis recorded as “Not specified” was retained as a separate category as its own category, "Not specified".
- **Referring facility:** level and region.

Characteristics of potential accepting facilities were not included, as they are related to the referral outcome and could introduce information available only after, or as part of, the referral decision. Instead, regional indicators of health-system capacity (doctors and hospital beds per 10,000 population) and of economic context (average wage, unemployment rate, population and gross regional product per capita) were included. Each indicator is a single 2022 value per region, also applied to 2023.

**Temporal validation.** Rather than splitting the data randomly, the 2022 cohort was divided chronologically:

- admissions from January to September formed the **training** set;
- admissions from October to December were used for **internal temporal validation**;
- the 2023 cohort served as an **external temporal validation** set.

This approach is consistent with TRIPOD recommendations for clinical prediction models.

## Models

Six models were compared:

1. **Naive baseline** – a simple approximation of current referral practice that predicts, for each referring facility level, the most common historical destination in the training data, without accounting for patient or clinical characteristics.
2. **Flat random forest (RF)** – direct classification of referrals into the five destination categories, without class weights.
3. **Weighted flat RF** and 4. **Weighted flat CatBoost** – flat five-class models with the same inverse-frequency class weights and tuned hyperparameters as the hierarchical models. These models help distinguish the contribution of class weighting from that of the hierarchical structure to minority-class recall.
5. **Hierarchical RF** and 6. **Hierarchical CatBoost** – two algorithms applied within a three-stage framework reflecting the structure of the referral system, which distinguishes destinations reimbursed through diagnosis-related groups (DRG) from those reimbursed through other payment methods:

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

The best-performing configuration was refitted to the full training data; RF models used 1,000 trees. The same hyperparameters were applied at all three stages to reduce the extent to which performance differences between stages were attributable to differences in tuning.

## Evaluation

- **Discrimination:** accuracy, balanced accuracy and macro-averaged precision, recall and F1-score, with 95% bootstrap confidence intervals; per-category recall and precision, confusion matrices, and class-specific AUCs and ROC curves; class-specific AUCs and ROC curves.
- **Precision–recall trade-off:** one-vs-rest precision–recall curves, average precision, and balanced accuracy compared at matched macro-precision (class probabilities reweighted to trace each model's operating points).
- **Calibration and recalibration:** calibration of the stage 1 probability and of the combined hierarchical probability for rural/district referrals. Probabilities were recalibrated on the internal validation period (logistic recalibration for stage 1, multinomial logistic recalibration for the five destinations) and applied unchanged to 2023; decision curves, the concordance analysis, the volume analysis and the prototype use the recalibrated probabilities.
- **Aggregate volumes (planning use):** predicted 2023 referral volumes (sum of recalibrated probabilities) compared with observed volumes, nationally and by referring region.
- **Clinical utility:** decision curve analysis on the external validation cohort, assessing net benefit for referral to a non-DRG-funded facility and for identification of rural/district referrals, compared with referring all patients, referring none and the naive baseline.
- **Regional variation:** model performance and referral concentration by referring region; proportion of referral diagnoses recorded as "Not specified" referral diagnoses by region and referring level.
- **Sensitivity analyses:** hierarchical models refitted without surgery type; performance among 2023 patients not present in the training data; obstetric versus non-obstetric referrals (ICD-10 chapter O of the referral diagnosis).
- **Outcome concordance:** length of stay and in-hospital complication rates compared between referrals whose actual destination matched the model's top-ranked prediction and those that did not, overall, for high-confidence predictions and stratified by ICD-10 chapter. This retrospective analysis is hypothesis-generating and does not imply a causal effect of model-concordant referral on patient outcomes.

## Repository structure

```
00_main.R                          runs the whole pipeline (switches individual steps on or off)
00_config.R                        settings: data files, seed, tuning grids, model for maps/concordance
01_functions.R                     metrics, bootstrap CIs, hierarchical combination, calibration,
                                   recalibration, precision-recall, DCA, labels
02_cohorts_split.R                 outcome, temporal split, DRG / non-DRG target
03_naive_and_flat_rf.R             naive baseline, flat RF and random forest tuning
04_hierarchical_rf.R               hierarchical RF
05_catboost_tuning.R               CatBoost tuning
06_hierarchical_catboost.R         hierarchical CatBoost
06b_weighted_flat_models.R         weighted flat RF and weighted flat CatBoost
07_table1_characteristics.R        Table 1   cohort characteristics
07b_referral_diagnosis.R           referral diagnosis "Not specified" (Table S9)
08_table2_destinations.R           Table 2   destinations by cohort
09_tables_performance.R            Tables 3, 4; confusion matrices; stage-1 ROC
09b_precision_recall.R             per-class precision, PR curves, matched macro-precision
10_fig_variable_importance.R       variable importance, stages 1 and 3
11_fig_calibration.R               calibration (stage 1, rural/district)
11b_recalibration.R                recalibration fitted on Oct-Dec 2022, applied to 2023
12_dca.R                           decision curve analysis (recalibrated probabilities)
12b_aggregate_volumes.R            predicted vs observed referral volumes
13_regional_maps.R                 regional model performance and referral concentration maps
14_concordance.R                   outcome-concordance analysis (recalibrated probabilities)
15_shiny_inputs.R                  inputs for the Shiny app
16_sensitivity_no_surgery_type.R   hierarchical models refitted without surgery type
17_sensitivity_patient_overlap.R   2023 patients not present in the training data
18_sensitivity_obstetric.R         obstetric vs non-obstetric referrals
19_tableS2_figureS1.R              class-specific metrics and ROC curves (Table S2, Figure S1)
app.R                              Shiny prototype (recalibrated hierarchical RF)
install_packages.R                 installs all required R packages
data/synthetic/                    synthetic analysis-ready cohorts (.rds for the pipeline, .csv.gz for inspection)
data-raw/                          script (and public regional indicators) that generated the synthetic cohorts
docs/data_dictionary.md            variables in the analysis-ready datasets
```

All results are written to `output/` (`tables/`, `figures/`, `models/`, `predictions/`).

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
   With the default synthetic data the whole pipeline takes about 10 minutes on a laptop (the real data take several hours).
3. Launch the prototype (after step 15 has been completed):
   ```r
   shiny::runApp()
   ```

Each step saves its results, so steps can be switched off in `run_steps` in `00_main.R` and figures
or tables can be redrawn without refitting the models.

## Data availability

The study used de-identified national HES data for 2022 and 2023. These data are confidential
and are not shared; access requires approval from the data holder. The pipeline starts from analysis-ready
cohorts (one row per referral episode; see `docs/data_dictionary.md`). Users with approved access
to data of the same structure can point `data_files` in `00_config.R` to their own files.
Facility regions in the analysis-ready data were derived from facility names; the coding was
corrected so that Almaty City facilities are not matched by the text pattern for Almaty region
(Almaty region and Zhetisu remain combined, as Zhetisu was separated from Almaty region in June 2022). Never
commit such files: `.gitignore` excludes all data except `data/synthetic/`.

## Synthetic data

`data/synthetic/` contains 24,000 (2022) and 20,000 (2023) artificial referral episodes created by
`data-raw/make_synthetic_data.R`. No real records were used: category frequencies follow the
published aggregate percentages in Table 1 of the paper (with the corrected Almaty coding), regional
indicators are public statistics, patient identifiers are random codes (about 57% of 2023 episodes
belong to patients also seen in 2022),
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
| Table 5 | `tables/concordance_outcomes_overall_and_high_confidence.csv` |
| Table 6 | `tables/table6_volumes_national.csv` |
| Figures 2–3 | `figures/figure_importance_stage1.png`, `figures/figure_importance_stage3.png` |
| Figures 4–5 | `figures/figure_calibration_stage1.png`, `figures/figure5_calibration_rural_district.png` |
| Figure 6 | `figures/figure6_regional_model_performance.png` |
| Figure 7 | `figures/figure_regional_hhi.png` |
| Figures 8–9 | `figures/figure8_dca_non_drg.png`, `figures/figure_dca_rural_district.png` |
| Table S2, Figure S1 | `tables/tableS2_perclass_external.csv`, `figures/figureS1_roc_external.png` |
| Table S4 | `tables/per_class_precision_recall.csv` |
| Table S5 | `tables/concordance_outcomes_by_icd10_chapter.csv` |
| Table S6 | `tables/regional_balanced_accuracy_external.csv` |
| Table S7, Figure S2 | `tables/pr_average_precision.csv`, `tables/matched_macro_precision_comparison.csv`, `figures/figure_pr_curves_external.png` |
| Table S8 | `tables/recalibration_stage1_stats.csv`, `tables/recalibration_by_class_stats.csv` |
| Table S9 | `tables/referral_dx_not_specified_by_region.csv`, `tables/referral_dx_not_specified_by_level.csv` |
| Table S10 | `tables/table6_volumes_by_region.csv` |
| Sensitivity analyses | `tables/sens_no_surgery_type_performance.csv`, `tables/sens_patient_overlap.csv`, `tables/sens_obstetric_performance.csv` |

Figure 1 (study flow) and Tables S1 and S3 (candidate predictors, data preparation) describe the
study design and data preparation, which precede the analysis-ready data, and are not produced by the code.

## Shiny prototype

`app.R` is a proof-of-concept showing how the hierarchical model could support referral decisions.
It uses the hierarchical RF with recalibrated probabilities and asks only for information available
at referral, including the proposed surgery type; regional indicators are filled in from the selected region. Predictions with a top probability of at least 70% are shown as a recommendation,
lower ones are flagged for manual review (an illustrative, not optimised, threshold).
**It is not a validated clinical decision-support tool.**

## Software

R 4.6.1; random forests with `ranger`, gradient boosting with `catboost`, ROC analysis with `pROC`,
recalibration with `nnet`,
data management and graphics with the `tidyverse`; maps with `sf` and `geokz`; application with `shiny`.
Random seed: 20260818.

## Citation

If you use this code, please cite the accompanying article (citation to be added on publication).

## Licence

MIT – see [LICENSE](LICENSE).
