# R analysis code

## Scope and analysis inputs

The R scripts document the models, sensitivity analyses, summary tables and figures used in the manuscript. Statistical analyses were conducted on locally prepared study datasets derived from MIMIC-IV and eICU-CRD, rather than directly on the raw database tables within R.

The original analysis used one harmonized cohort workbook per database and separate serial-creatinine, RRT and nonrenal SOFA component extracts for the revision analyses. Those workbooks and other patient-level files are not part of the public code. The scripts therefore require the user to prepare input tables with the corresponding variables; they do not describe every step of the authors' local Excel consolidation process.

`01_import_harmonize_qc.R` is a data interface and quality-check helper. In the current code it can join individual SQL module exports (`modules`, the default) or read locally merged cohort tables (`merged`). The downstream statistical scripts use the standardized RDS objects written by R01. Neither input mode removes the need to verify column definitions and cohort membership against the SQL and Supplementary Methods.

A valid analysis dataset must retain the index stay identifier, AKI trajectory and eligibility flags, outcome and follow-up information, age, sex, the five chronic comorbidities, first-day nonrenal SOFA, and eICU hospital identifier for clustered inference. The relevant sensitivity analyses additionally require reference-creatinine and timing fields, peak AKI stage, AECOPD status, IMV and vasopressor indicators, longitudinal creatinine measurements, RRT evidence, and SOFA component QC extracts. Exact column names are defined in the scripts. Patient-level datasets should remain in restricted local storage.

## Main R scripts

| Script | Analysis / reported result |
| --- | --- |
| `01_import_harmonize_qc.R` | Import, harmonization and checks; prepares local analysis datasets |
| `02_baseline_tables.R` | Descriptive characteristics and standardized mean differences; Tables 1–2 |
| `03_primary_models_and_sensitivities.R` | Main logistic models, four prespecified sensitivities, within-AKI analysis; S1–S3 |
| `04_eicu_trajectory_classifiability.R` | Classifiable versus unclassifiable eICU trajectories; S5 |
| `05_model_diagnostics_collinearity.R` | Splines, influence diagnostics, multicollinearity; S12–S13 |
| `06_scr_monitoring_rrt_sofa_sensitivity.R` | Monitoring, threshold, RRT and SOFA component checks; S6, S8, S9 and S11 |
| `07_temporal_pair_reclassification.R` | Strictly ordered creatinine-pair reclassification; S7 Panels A–B |
| `08_temporal_pair_same_sample_models.R` | Comparison of classification methods in the same patients; S7 Panel C |
| `09_alive_discharge_30d.R` | Discharge alive within 30 days after the landmark; S4 |
| `10_figure2_primary_forest.R` | Primary adjusted-estimate forest plot; Figure 2 |
| `11_figure3_sensitivity_forest.R` | Primary and sensitivity-analysis forest plot; Figure 3 |

The study-flow figure uses counts derived from the SQL cohort queries; this collection has no separate script to redraw Figure 1. Supplementary Table S10 is a variable-definition table, not an R analysis output.

## Statistical specification

The primary outcome is subsequent in-hospital mortality after the ICU 60-hour landmark. The main multivariable logistic regression adjusts for age, female sex, chronic kidney disease, heart failure, diabetes mellitus, chronic liver disease, malignancy and first-day nonrenal SOFA. The no-early-AKI group is the reference. MIMIC-IV uses model-based standard errors; eICU-CRD uses hospital-clustered HC1 robust standard errors.

The four prespecified sensitivities address the narrower 42–54-hour assessment window, restriction to a measured pre-ICU reference creatinine, additional adjustment for invasive ventilation and vasopressor use, and restriction to AECOPD. Within the early-AKI subgroup, ongoing and resolved trajectories are compared with additional adjustment for peak creatinine-defined AKI stage.

Further analyses examine creatinine monitoring, strictly chronological creatinine pairs, classification near the resolution threshold, RRT-related evidence, model diagnostics and discharge alive within 30 days. The strict temporal-pair sensitivity preserves an *indeterminate* category when the data cannot support classification. The discharge-alive endpoint is not a measure of kidney recovery.

The two scripts under `exploratory/` contain additional IPW/RRT and time-to-event/length-of-stay analyses that were not reported as the final manuscript analyses. They are optional and are not required to reproduce the reported tables and figures.

## Running the analyses

The manuscript reports R **4.5.3**. The scripts use packages including `dplyr`, `readr`, `readxl`, `stringr`, `tibble`, `tidyr`, `sandwich`, `car`, `ggplot2`, `patchwork` and `openxlsx`. Install the packages needed for the selected scripts; package versions have not yet been frozen.

Run from the repository root. The import script expects local files in the paths defined near the top of R01, typically under `data/sql_exports/` for SQL-module output or `data/merged/` for the pre-merged input option. The following illustrates the main dependency chain:

```sh
Rscript R/01_import_harmonize_qc.R
Rscript R/03_primary_models_and_sensitivities.R
Rscript R/10_figure2_primary_forest.R
Rscript R/11_figure3_sensitivity_forest.R
```

Other scripts run after R01; R08 additionally requires the local classification output from R07. Scripts R06 and R07 require the separate time-stamped SQL extracts. The detailed input names, output filenames and quality checks are specified in each R script. Output is written locally under `output/R_analysis_output/`; individual-level RDS and CSV files must not be added to the public repository.

## Status and interpretation

The reported primary analytic cohorts comprised 7,627 MIMIC-IV and 7,738 eICU-CRD patients. The corresponding numbers of subsequent in-hospital deaths were 832 and 709. The scripts use manuscript values for consistency checks; those values do not replace calculated estimates.

The reorganized scripts have not yet been run as a complete SQL-to-R pipeline against the original authorized extracts. The code documents the analysis methods and intended interfaces, but a reader should not treat the repository as a verified one-command reproduction package. Any differences observed in an independent run should be investigated and reported, not forced to match the manuscript.
