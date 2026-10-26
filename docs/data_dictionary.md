# Data dictionary – analysis-ready data sets (2022, 2023)

One row per hospital referral episode (referring facility different from the accepting facility,
public facilities only). The repository supplies synthetic versions in `data/synthetic/`.

## Outcome

| Variable | Type | Description |
|---|---|---|
| `accept_level` | character | Level of the accepting facility: City, Regional, Republican/National, Rural/District, Specialised (Polyclinic is excluded in `02_cohorts_split.R`). Renamed `referral_destination` for modelling. |

Derived in `02_cohorts_split.R`: `referral_destination` (factor, 5 levels) and `destination_broad`
(DRG = City or Regional; NON_DRG = Republican/National, Rural/District, Specialised).

## Predictors (`predictor_vars` in `00_config.R`)

| Variable | Type | Description |
|---|---|---|
| `age` | numeric | Age in years |
| `female` | 0/1 | Female sex |
| `residence` | character | Region of patient residence |
| `employment_group` | character | Employed, Pensioner, Homemaker, Student/Child, Unemployed, Disabled, Military, Prisoner, Other |
| `benefit_group` | character | MSHI benefit category: Benefit / No benefit |
| `refer_level` | character | Referring facility level (City, Polyclinic, Regional, Republican/National, Rural/District, Specialised) |
| `refer_region` | character | Region of the referring facility |
| `icd10_ref` | character | ICD-10 diagnosis at referral; "Not specified" when missing |
| `sur_type` | character | Surgery type: Primary_operation / Additional_operation |
| `admission_day_of_week` | ordered factor | Day of week of admission (English day names) |
| `admission_month` | ordered factor | Month of admission (English month names) |
| `doctors_per10k` | numeric | Doctors per 10,000 population, region (2022) |
| `beds_per10k` | numeric | Hospital beds per 10,000 population, region (2022) |
| `wage` | numeric | Average monthly wage, tenge, region (2022) |
| `employment_region` | numeric | Employed population, region (2022) |
| `unemployment_rate` | numeric | Unemployment rate, %, region (2022) |
| `log_population` | numeric | log population, region (2022) |
| `log_grp_pc` | numeric | log gross regional product per capita (2022) |
| `log_investment_pc` | numeric | log investment per capita, region (2022) |

## Other variables (descriptive / secondary analyses, not predictors)

| Variable | Type | Description |
|---|---|---|
| `gender`, `age_cat` | character | Sex (F/M) and age group, for Table 1 |
| `region` | character | Region used to attach the regional indicators |
| `icd3` | character | Principal (final) diagnosis, 3-character ICD-10 – Table 1 only (not a predictor: known only after admission) |
| `accept_region` | character | Region of the accepting facility – descriptive only (post-decision) |
| `los` | numeric | Length of stay, days – concordance analysis |
| `complications` | 0/1 | In-hospital complication – concordance analysis |

Region labels follow the analysis convention: "Almaty/Zhetisu" combines Almaty region and Zhetisu
region for facility regions; patient residence keeps "Zhetisu" separately.
