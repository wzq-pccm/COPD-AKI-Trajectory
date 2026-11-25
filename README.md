# Code for early creatinine-defined AKI trajectories in critically ill patients with COPD

Code accompanying the manuscript *Association of Early Creatinine-Defined Acute Kidney Injury Trajectory with Subsequent In-Hospital Mortality in Critically Ill Patients with COPD: A Two-Cohort Retrospective Study*.

The study used MIMIC-IV v3.1 and eICU-CRD v2.0. Early creatinine-defined AKI was assessed during ICU hours 0–48, with trajectory classification based on a serum creatinine measurement around ICU hour 48. The outcome was in-hospital mortality after the 60-hour landmark. The manuscript and Supplementary Methods provide the full study definitions.

## Repository contents

| Location | Contents |
| --- | --- |
| `sql/mimiciv/` | Eight PostgreSQL scripts for cohort construction, variable extraction and revision analyses |
| `sql/eicu/` | Nine PostgreSQL scripts for the corresponding eICU analyses |
| `R/` | Eleven R scripts for the reported statistical analyses, tables and figures |
| `exploratory/` | Two optional R scripts for additional analyses not reported in the final manuscript |
| `docs/` | Notes on SQL, R analysis and their inputs |

The SQL code records how cohorts and study variables were derived from the source databases. The R code documents the statistical analyses conducted on locally prepared, harmonized study datasets. The R scripts are not intended as a general-purpose data-cleaning system, and the original local workbooks are not distributed.

## Documentation

- [SQL code and database requirements](docs/SQL_Code_Guide.md)
- [R analyses and input requirements](docs/R_Analysis_Guide.md)

## Data access and code status

MIMIC-IV and eICU-CRD are controlled-access datasets available to eligible users through PhysioNet. This repository does not include patient-level records, stay-ID lists, analysis workbooks, or individual-level R output. Users must obtain their own database access and prepare analysis inputs with the variables required by the scripts.

The code has been reorganized for public sharing. The reorganized SQL-to-R workflow has **not yet undergone complete end-to-end execution and verification** against the authorized study extracts. The cohort counts and published estimates described in the documentation are checks against the manuscript, not outputs newly generated for this release. This repository should be treated as a documentation draft until that validation is complete.
