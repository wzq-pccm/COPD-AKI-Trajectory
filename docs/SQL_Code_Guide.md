# SQL code

## Source databases

SQL scripts use the PostgreSQL dialect and were prepared for MIMIC-IV v3.1 and eICU-CRD v2.0. They were adapted from the investigators' local SQL workflow. They are not BigQuery scripts.

The MIMIC scripts use raw `mimiciv_hosp` and `mimiciv_icu` tables and, where indicated, the `mimiciv_derived` concepts, including age, Charlson comorbidity, first-day SOFA, ventilation, vasoactive medication and RRT concepts. These derived relations must be available in the local database; installing only the raw MIMIC tables is not sufficient for all scripts.

The eICU scripts use `eicu_crd` source tables. COPD and acute COPD exacerbation are identified from `diagnosis.icd9code` using ICD-9-CM 491.20/491.21 and ICD-10-CM J44.9/J44.1, as listed in Supplementary Table S10. The public scripts do not require the locally defined `dictionary.d_icd_diagnosis` table.

## MIMIC-IV scripts

| File | Main purpose |
| --- | --- |
| `01_cohort_landmark.sql` | COPD cohort and 60-hour landmark eligibility; flow counts |
| `02_aki_trajectory.sql` | Reference serum creatinine, early AKI, trajectory classification and outcome |
| `03_demographics_comorbidities.sql` | Demographics, comorbidities, COPD phenotype and Charlson index |
| `04_nonrenal_sofa_and_qc.sql` | First-day nonrenal SOFA and component checks |
| `05_first24h_labs.sql` | Hemoglobin, BUN and other original first-day laboratory extracts |
| `06_imv_vasopressor.sql` | Invasive mechanical ventilation and vasopressor use during ICU hours 0–24 |
| `07_serial_creatinine.sql` | Time-stamped creatinine measurements for revision analyses |
| `08_rrt_events.sql` | RRT-related records and timing/evidence fields |

## eICU-CRD scripts

| File | Main purpose |
| --- | --- |
| `01_cohort_landmark.sql` | COPD cohort and 60-hour landmark eligibility; flow counts |
| `02_aki_trajectory.sql` | Reference serum creatinine, early AKI and trajectory classification |
| `03_demographics_comorbidities.sql` | Demographics, comorbidities and COPD phenotype |
| `04_charlson_comorbidity_index.sql` | Charlson index based on eICU source information |
| `05_nonrenal_sofa_and_qc.sql` | First-day nonrenal SOFA and component checks |
| `06_first24h_labs.sql` | First-day hemoglobin and BUN |
| `07_imv_vasopressor.sql` | Invasive mechanical ventilation and vasopressor use during ICU hours 0–24 |
| `08_serial_creatinine.sql` | Time-stamped creatinine measurements for revision analyses |
| `09_rrt_events.sql` | RRT-related records and timing/evidence fields |

## How the SQL outputs are used

The cohort and AKI scripts establish the analysis population and exposure classification. The demographic, SOFA, laboratory and organ-support scripts provide covariates and descriptive variables. Serial-creatinine and RRT scripts produce event-level records used in the additional sensitivity analyses; these are not one-row-per-patient tables.

Most scripts reconstruct the applicable landmark cohort independently and create temporary tables. Run each script in full within one PostgreSQL session, then export its final query result. The `01_cohort_landmark.sql` outputs document cohort flow and are not necessarily inputs to the R data merge. The other patient-level extracts are joined by the database-specific index stay key. Keep time-stamped creatinine and RRT extracts separate until the relevant R analysis.

The primary AKI trajectory uses a fixed creatinine reference: the most recent measured value within seven days before ICU admission during the index hospitalization, or, if unavailable, the minimum value during ICU hours 0–24 as a surrogate. Early AKI is identified during hours 0–48 using the specified magnitude criteria. Trajectory is assessed in the 36–60-hour window. The primary classification does not enforce the KDIGO temporal-pair intervals; strictly ordered pairs are examined separately in a sensitivity analysis.

RRT documentation is used as evidence for a sensitivity exclusion. A record timestamp should not be interpreted as a verified time of treatment initiation.

## Checks before using the extracts

Published landmark counts were 7,794 for MIMIC-IV and 8,495 for eICU-CRD. After excluding unclassifiable trajectories, the reported primary analytic cohorts were 7,627 and 7,738, respectively. These are manuscript reference values, not independently verified outputs from the reorganized scripts.

Check the stay-ID set, not only the row count. Verify that patient-level extracts have one row per index stay and that the event extracts retain all relevant measurements or events. The original study used locally optimized SQL anchored to a previously established cohort; the public serial-creatinine and RRT scripts regenerate that cohort instead of embedding an ID list. Equivalence of all extracts still requires local execution and comparison.
