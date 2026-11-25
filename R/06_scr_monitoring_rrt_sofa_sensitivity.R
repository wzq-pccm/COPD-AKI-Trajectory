# ==============================================================================
# COPD early creatinine-defined AKI trajectory study
# R/06_scr_monitoring_rrt_sofa_sensitivity.R
# MIMIC-IV v3.1 and eICU-CRD v2.0 | FINAL Supplementary S6, S8, S9, S11
#
# Adapted from the author's source:
#   07_Reviewer1_SCr_RRT_SOFA_Sensitivity_Analysis_v2.R
#
# PRESERVED SCIENTIFIC CONVENTIONS:
#   Primary fixed-reference early-AKI exposure and 60-h landmark are imported,
#   never recomputed. sCr monitoring is counted from serial laboratory events.
#   The monitoring-restricted sensitivity requires 36-60-h sCr in every group,
#   plus an assessment strictly later than the first AKI-qualifying measurement
#   for patients with early AKI. Threshold-proximity exclusion uses +/-0.10
#   mg/dL around 0.30 or +/-0.10 around 1.50x; either is sufficient.
#   RRT evidence documented during ICU 0-60 h is used for S9 exclusion;
#   documentation time DOES NOT prove RRT initiation time.
#   MIMIC model-based SE; eICU hospital-clustered HC1 robust SE.
#   The 5 source SOFA components retain missingness in QC; a score of zero
#   imputed for an unavailable component does NOT mean observed normal function.
#
# INPUTS from R/01_import_harmonize_qc.R (run R01 first):
#   output/R_analysis_output/mimic_full_clean.rds
#   output/R_analysis_output/eicu_full_clean.rds
#
# INDEPENDENT SQL EVENT/QC EXPORTS (CSV UTF-8 or matching .xlsx/.xls):
#   data/sql_exports/mimiciv/07_serial_creatinine.csv
#   data/sql_exports/mimiciv/08_rrt_events.csv
#   data/sql_exports/mimiciv/04_nonrenal_sofa_and_qc.csv
#   data/sql_exports/eicu/08_serial_creatinine.csv
#   data/sql_exports/eicu/09_rrt_events.csv
#   data/sql_exports/eicu/05_nonrenal_sofa_and_qc.csv
#
# IMPORTANT: only export the FINAL SELECT of each SQL script. Do NOT export
# intermediate tables or a patient-ID anchor; SQL cohort generation is already
# handled by the public SQL. Each long table must include all landmark stays.
#
# OUTPUTS (output/R_analysis_output/06_scr_monitoring_rrt_sofa_sensitivity/):
#   TableS6_PanelA_monitoring.csv, TableS6_PanelB_timing.csv,
#   TableS6_PanelC_monitoring_restricted.csv
#   TableS8_PanelA_threshold_composition.csv,
#   TableS8_PanelB_threshold_adjusted.csv, TableS8_PanelC_AKI_only.csv
#   TableS9_PanelA_rrt_evidence.csv, TableS9_PanelB_rrt_exclusion.csv
#   TableS11_nonrenal_sofa_components.csv
#   cohort and event source QC, model estimates, manuscript QC, run notes.
#
# A small number of local patient-level JOINED .rds files are intentionally
# produced to support the subsequent strict temporal sensitivity scripts.
# NEVER commit data/, output/, individual-level RDS/CSV/XLSX, or private IDs.
# Run from project root: Rscript R/06_scr_monitoring_rrt_sofa_sensitivity.R
# Or set COPD_AKI_PROJECT_ROOT to the project root directory.
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)

# 1. SETTINGS ---------------------------------------------------------------
root_override <- Sys.getenv("COPD_AKI_PROJECT_ROOT", unset = "")
PROJECT_ROOT <- normalizePath(
  if (nzchar(root_override)) root_override else ".",
  winslash = "/", mustWork = TRUE
)
DATA_DIR <- file.path(PROJECT_ROOT, "data", "sql_exports")
RDS_DIR <- file.path(PROJECT_ROOT, "output", "R_analysis_output")
OUTPUT_DIR <- file.path(RDS_DIR, "06_scr_monitoring_rrt_sofa_sensitivity")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

MIMIC_RDS <- file.path(RDS_DIR, "mimic_full_clean.rds")
EICU_RDS <- file.path(RDS_DIR, "eicu_full_clean.rds")
STOP_ON_FROZEN_COHORT_MISMATCH <- TRUE
SAVE_JOINED_LOCAL_RDS <- TRUE  # Required for optional R07/R08 reuse; private.
GRAY_ZONE_DELTA_MGDL <- 0.10
GRAY_ZONE_RATIO <- 0.10
TIME_TOL_HOURS <- 1e-6

resolve_export <- function(database_dir, filename) {
  directory <- file.path(DATA_DIR, database_dir)
  stem <- tools::file_path_sans_ext(filename)
  candidates <- file.path(directory, paste0(stem, c(".csv", ".xlsx", ".xls")))
  hits <- candidates[file.exists(candidates)]
  if (length(hits) != 1L) {
    stop("Expected exactly one SQL export for ", stem,
         " under ", directory, "; found ", length(hits),
         ". Export the FINAL SELECT as .csv (preferred) or .xlsx/.xls.")
  }
  normalizePath(hits, winslash = "/", mustWork = TRUE)
}

INPUT <- c(
  mimic_scr = resolve_export("mimiciv", "07_serial_creatinine.csv"),
  eicu_scr = resolve_export("eicu", "08_serial_creatinine.csv"),
  mimic_rrt = resolve_export("mimiciv", "08_rrt_events.csv"),
  eicu_rrt = resolve_export("eicu", "09_rrt_events.csv"),
  mimic_sofa = resolve_export("mimiciv", "04_nonrenal_sofa_and_qc.csv"),
  eicu_sofa = resolve_export("eicu", "05_nonrenal_sofa_and_qc.csv")
)
missing_rds <- c(MIMIC_RDS, EICU_RDS)[
  !file.exists(c(MIMIC_RDS, EICU_RDS))
]
if (length(missing_rds)) stop("Missing R01 RDS files: ", paste(missing_rds, collapse = " ; "))

required_packages <- c("dplyr", "readr", "sandwich")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install R packages before running: ", paste(missing_packages, collapse = ", "))
}
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sandwich)
})
# 'readxl' is required only when an input is exported as an Excel workbook.
if (any(tolower(tools::file_ext(INPUT)) %in% c("xlsx", "xls")) &&
    !requireNamespace("readxl", quietly = TRUE)) {
  stop("Install 'readxl' or export all six SQL outputs to CSV.")
}
# 'openxlsx' is optional: CSV outputs are sufficient for full reproducibility.

# 2. HELPERS ---------------------------------------------------------------
log_lines <- character(0)
log_msg <- function(...) {
  x <- paste0(...)
  message(x)
  log_lines <<- c(log_lines, x)
  invisible(x)
}
require_cols <- function(d, cols, label) {
  miss <- setdiff(cols, names(d))
  if (length(miss)) stop(label, " missing columns: ", paste(miss, collapse = ", "))
}
read_all_text <- function(path) {
  ext <- tolower(tools::file_ext(path))
  na_tokens <- c("", "NA", "N/A", "NULL", "null", "NaN")
  if (ext == "csv") {
    d <- readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character()),
      na = na_tokens, name_repair = "minimal", show_col_types = FALSE,
      progress = FALSE, locale = readr::locale(encoding = "UTF-8")
    )
  } else if (ext %in% c("xlsx", "xls")) {
    d <- readxl::read_excel(
      path, sheet = 1, col_types = "text", na = na_tokens,
      .name_repair = "minimal"
    )
  } else stop("Unsupported SQL export type: ", path)
  d <- as.data.frame(d, check.names = FALSE)
  names(d) <- trimws(names(d))
  if (anyDuplicated(names(d))) stop("Duplicate column name in ", path)
  d
}
as_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "NULL", "null", "NaN")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", "", x, fixed = TRUE)))
}
as_id <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "NULL", "null")] <- NA_character_
  ok <- !is.na(x) & grepl("^[0-9]+[.]0+$", x)
  x[ok] <- sub("[.]0+$", "", x[ok])
  x
}
num_cols <- function(d, cols) {
  for (n in intersect(cols, names(d))) d[[n]] <- as_num(d[[n]])
  d
}
safe_min <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) min(x) else NA_real_
}
safe_median <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) stats::median(x) else NA_real_
}
safe_q <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x)) as.numeric(stats::quantile(x, p, names = FALSE)) else NA_real_
}
safe_rate <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) mean(x) else NA_real_
}
check_unique <- function(d, col, label) {
  if (any(is.na(d[[col]]) | d[[col]] == "")) stop(label, ": missing ID")
  if (anyDuplicated(d[[col]])) stop(label, ": duplicated ID")
}
check_id_set <- function(parent_ids, child_ids, label) {
  extra <- setdiff(unique(child_ids), unique(parent_ids))
  missing <- setdiff(unique(parent_ids), unique(child_ids))
  if (length(extra) || length(missing)) {
    stop(label, ": IDs do not match frozen original cohort (extra=",
         length(extra), ", missing=", length(missing), ").")
  }
}
flag_yes <- function(x) !is.na(x) & x == 1
approximately_equal <- function(a, b, tol = 1e-8) {
  both_missing <- is.na(a) & is.na(b)
  both_finite <- is.finite(a) & is.finite(b)
  both_missing | (both_finite & abs(a - b) <= tol)
}
exposure_labels <- c("No early AKI", "Resolved early AKI", "Ongoing early AKI")
base_numeric <- c(
  "eligible_at_60h_flag", "primary_analysis_flag", "primary_exposure_code",
  "sensitivity_narrow_window_flag", "sensitivity_narrow_exposure_code",
  "sensitivity_pre_icu_reference_flag", "sensitivity_pre_icu_reference_exposure_code",
  "hospital_death_after_60h", "age", "female_flag", "ckd_flag",
  "heart_failure_flag", "diabetes_flag", "chronic_liver_disease_flag",
  "malignancy_flag", "standard_nonrenal_sofa_24h", "peak_aki_stage_0_48h",
  "imv_any_0_24h", "vasopressor_any_0_24h", "aecopd_any_flag", "aecopd_flag",
  "reference_scr", "assessment_scr_36_60h", "assessment_offset_from_48h_36_60h",
  "aki_onset_hours_from_icu", "assessment_minus_reference_36_60h",
  "assessment_to_reference_ratio_36_60h", "reference_to_aki_onset_hours",
  "aki_onset_before_reference_time_flag", "relative_criterion_flag",
  "absolute_criterion_flag", "scr_measurement_n_0_48h",
  "scr_measurement_n_36_60h", "scr_measurement_n_42_54h",
  "pre_icu_reference_available_flag", "surrogate_reference_0_24h_flag"
)
model_covariates <- c(
  "age", "female_flag", "ckd_flag", "heart_failure_flag", "diabetes_flag",
  "chronic_liver_disease_flag", "malignancy_flag", "standard_nonrenal_sofa_24h"
)

# 3. ORIGINAL FROZEN LANDMARK COHORTS (imported from R01) -------------------
# R01 already merges one-row-per-stay SQL exports and checks the cohort.
# This module MUST NOT recalculate/reassign any primary AKI classification.
log_msg("Reading original frozen landmark cohort datasets from R01 RDS...")
mimic <- readRDS(MIMIC_RDS)
eicu <- readRDS(EICU_RDS)
if (!is.data.frame(mimic) || !is.data.frame(eicu)) {
  stop("R01 RDS inputs must contain data.frames; rerun R/01.")
}
mimic <- num_cols(mimic, base_numeric)
eicu <- num_cols(eicu, base_numeric)
require_cols(mimic, c("stay_id", "hadm_id", "primary_analysis_flag",
  "primary_exposure_code", "hospital_death_after_60h", "reference_scr_source",
  "assessment_offset_from_48h_36_60h", "aki_onset_hours_from_icu",
  model_covariates), "MIMIC R01 full")
require_cols(eicu, c("patientunitstayid", "hospitalid", "primary_analysis_flag",
  "primary_exposure_code", "hospital_death_after_60h", "reference_scr_source",
  "assessment_offset_from_48h_36_60h", "aki_onset_hours_from_icu",
  model_covariates), "eICU R01 full")
mimic$icu_id <- as_id(mimic$stay_id)
eicu$icu_id <- as_id(eicu$patientunitstayid)
mimic$database <- "MIMIC-IV"
eicu$database <- "eICU-CRD"
mimic$hospital_id <- "MIMIC"
eicu$hospital_id <- as_id(eicu$hospitalid)
check_unique(mimic, "icu_id", "MIMIC R01")
check_unique(eicu, "icu_id", "eICU R01")

frozen_expected <- data.frame(
  Database = c("MIMIC-IV", "eICU-CRD"),
  Full_N = c(7794, 8495),
  Primary_N = c(7627, 7738),
  Primary_deaths = c(832, 709),
  No_AKI = c(5796, 6644),
  Resolved = c(898, 465),
  Ongoing = c(933, 629)
)
cohort_counter <- function(d) {
  p <- d[flag_yes(d$primary_analysis_flag) & d$primary_exposure_code %in% 0:2, ]
  c(Full_N = nrow(d), Primary_N = nrow(p),
    Primary_deaths = sum(p$hospital_death_after_60h == 1, na.rm = TRUE),
    No_AKI = sum(p$primary_exposure_code == 0),
    Resolved = sum(p$primary_exposure_code == 1),
    Ongoing = sum(p$primary_exposure_code == 2))
}
actual_counts <- rbind(cohort_counter(mimic), cohort_counter(eicu))
cohort_qc <- data.frame(Database = frozen_expected$Database, actual_counts,
  check.names = FALSE)
cohort_qc$pass <- apply(cohort_qc[, colnames(actual_counts), drop = FALSE] ==
  frozen_expected[, colnames(actual_counts), drop = FALSE], 1, all)
if (any(!cohort_qc$pass)) {
  msg <- "R01 frozen landmark cohort counts differ from the original study. Do not interpret models."
  if (STOP_ON_FROZEN_COHORT_MISMATCH) stop(msg) else warning(msg)
}
# Unlike glm's default na.omit, the baseline model must not silently change N.
for (nm in c("MIMIC-IV", "eICU-CRD")) {
  z <- if (nm == "MIMIC-IV") mimic else eicu
  z <- z[flag_yes(z$primary_analysis_flag) & z$primary_exposure_code %in% 0:2,
         , drop = FALSE]
  model_columns <- c("hospital_death_after_60h", model_covariates)
  absent <- vapply(z[, model_columns, drop = FALSE],
                   function(v) sum(is.na(v)), integer(1))
  if (any(absent > 0)) {
    stop(nm, ": missing original primary model covariates: ",
         paste(paste(names(absent)[absent > 0], absent[absent > 0], sep = "="),
               collapse = ", "))
  }
}
log_msg("R01 cohorts verified: MIMIC 7794/7627; eICU 8495/7738.")

# 4. LONGITUDINAL sCr -----------------------------------------------------
read_scr_long <- function(path, database, original_ids) {
  d <- read_all_text(path)
  idname <- if (database == "MIMIC-IV") "stay_id" else "patientunitstayid"
  eventid <- if (database == "MIMIC-IV") "labevent_id" else "labid"
  hname <- if (database == "MIMIC-IV") "scr_hours_from_icu" else
    "labresultoffset_minutes_from_index_icu"
  require_cols(d, c(idname, eventid, hname, "has_scr_event", "scr_numeric_as_extracted"),
               paste0(database, " sCr long"))
  d$icu_id <- as_id(d[[idname]])
  check_id_set(original_ids, d$icu_id, paste0(database, " sCr"))
  d$has_event <- as_num(d$has_scr_event)
  d$scr <- as_num(d$scr_numeric_as_extracted)
  d$hour <- as_num(d[[hname]])
  if (database != "MIMIC-IV") d$hour <- d$hour / 60
  d$event_id <- as_id(d[[eventid]])
  e <- d[!is.na(d$has_event) & d$has_event == 1 & is.finite(d$scr) & d$scr > 0 &
           is.finite(d$hour), c("icu_id", "event_id", "scr", "hour")]
  # The official laboratory primary keys must be populated on real events.
  if (any(is.na(e$event_id))) stop(database, ": laboratory event without ID")
  duplicates <- sum(duplicated(e[c("icu_id", "event_id")]))
  if (duplicates) {
    warning(database, ": ", duplicates, " duplicate lab event keys; deduplicating.")
    e <- dplyr::distinct(e, icu_id, event_id, .keep_all = TRUE)
  }
  summary <- e %>% group_by(icu_id) %>% summarise(
    long_scr_0_24_n = sum(hour >= 0 & hour <= 24),
    long_scr_0_48_n = sum(hour >= 0 & hour <= 48),
    long_scr_36_60_n = sum(hour >= 36 & hour <= 60),
    long_scr_42_54_n = sum(hour >= 42 & hour <= 54),
    long_scr_48_60_n = sum(hour > 48 & hour <= 60),
    long_scr_preicu_7d_n = sum(hour >= -168 & hour < 0),
    long_scr_min_hour_36_60 = safe_min(hour[hour >= 36 & hour <= 60]),
    .groups = "drop"
  ) %>% as.data.frame()
  attr(summary, "n_valid_event_rows") <- nrow(e)
  attr(summary, "n_duplicate_events") <- duplicates
  summary
}
log_msg("Reading the 01-02 sCr long tables...")
scr_m <- read_scr_long(INPUT[["mimic_scr"]], "MIMIC-IV", mimic$icu_id)
scr_e <- read_scr_long(INPUT[["eicu_scr"]], "eICU-CRD", eicu$icu_id)

# 5. RRT EVIDENCE (NOT CONFIRMED TREATMENT INITIATION) -----------------------
read_rrt_long <- function(path, database, original_ids) {
  d <- read_all_text(path)
  idname <- if (database == "MIMIC-IV") "index_stay_id" else "patientunitstayid"
  hname <- if (database == "MIMIC-IV") "hours_from_index_icu" else
    "event_hours_from_index_icu"
  required <- if (database == "MIMIC-IV") c(
    idname, hname, "has_rrt_evidence_record", "direct_treatment_procedure_flag",
    "derived_active_dialysis_flag") else c(
    idname, hname, "has_rrt_candidate_record", "rrt_treatment_documented_flag",
    "rrt_dialysis_flow_flag", "rrt_cumulative_balance_change_flag",
    "rrt_therapy_or_dialysis_flow_evidence_flag")
  require_cols(d, required, paste0(database, " RRT long"))
  d$icu_id <- as_id(d[[idname]])
  check_id_set(original_ids, d$icu_id, paste0(database, " RRT"))
  d$hour <- as_num(d[[hname]])
  if (database == "MIMIC-IV") {
    d$candidate <- as_num(d$has_rrt_evidence_record) == 1
    d$direct <- as_num(d$direct_treatment_procedure_flag) == 1
    d$derived <- as_num(d$derived_active_dialysis_flag) == 1
    d$strong <- d$direct | d$derived
    d$flow <- FALSE
    d$cumulative <- FALSE
  } else {
    d$candidate <- as_num(d$has_rrt_candidate_record) == 1
    d$direct <- as_num(d$rrt_treatment_documented_flag) == 1
    d$derived <- FALSE
    d$flow <- as_num(d$rrt_dialysis_flow_flag) == 1
    d$cumulative <- as_num(d$rrt_cumulative_balance_change_flag) == 1
    d$strong <- as_num(d$rrt_therapy_or_dialysis_flow_evidence_flag) == 1
  }
  # All Boolean NA values are treated as FALSE for candidate-evidence flags.
  for (v in c("candidate", "direct", "derived", "flow", "cumulative", "strong")) {
    d[[v]][is.na(d[[v]])] <- FALSE
  }
  d$in_0_60 <- is.finite(d$hour) & d$hour >= 0 & d$hour <= 60
  d$up_to_60 <- is.finite(d$hour) & d$hour <= 60
  d$in_0_48 <- is.finite(d$hour) & d$hour >= 0 & d$hour <= 48
  summary <- d %>% group_by(icu_id) %>% summarise(
    rrt_candidate_record_n_0_60 = sum(candidate & in_0_60),
    rrt_strong_record_n_0_60 = sum(strong & in_0_60),
    rrt_candidate_any_0_60 = as.integer(any(candidate & in_0_60)),
    rrt_strong_any_0_60 = as.integer(any(strong & in_0_60)),
    rrt_strong_any_0_48 = as.integer(any(strong & in_0_48)),
    rrt_direct_any_0_60 = as.integer(any(direct & in_0_60)),
    rrt_derived_any_0_60 = as.integer(any(derived & in_0_60)),
    rrt_flow_any_0_60 = as.integer(any(flow & in_0_60)),
    rrt_cumulative_any_0_60 = as.integer(any(cumulative & in_0_60)),
    rrt_first_strong_0_60_hour = safe_min(hour[strong & in_0_60]),
    rrt_first_strong_any_prior60_hour = safe_min(hour[strong & up_to_60]),
    .groups = "drop"
  ) %>% as.data.frame()
  summary
}
log_msg("Reading the 03-04 RRT evidence long tables...")
rrt_m <- read_rrt_long(INPUT[["mimic_rrt"]], "MIMIC-IV", mimic$icu_id)
rrt_e <- read_rrt_long(INPUT[["eicu_rrt"]], "eICU-CRD", eicu$icu_id)

# 6. NONRENAL SOFA ---------------------------------------------------------
read_sofa_qc <- function(path, database, original_ids) {
  d <- read_all_text(path)
  idname <- if (database == "MIMIC-IV") "stay_id" else "patientunitstayid"
  components <- c("respiratory", "coagulation", "liver", "cardiovascular", "cns")
  raw_names <- paste0(components, "_sofa_24h_raw")
  require_cols(d, c(idname, "standard_nonrenal_sofa_24h", raw_names),
               paste0(database, " SOFA QC"))
  d$icu_id <- as_id(d[[idname]])
  check_unique(d, "icu_id", paste0(database, " SOFA QC"))
  check_id_set(original_ids, d$icu_id, paste0(database, " SOFA"))
  v <- data.frame(icu_id = d$icu_id,
                  sofa_from_05_06 = as_num(d$standard_nonrenal_sofa_24h))
  comp_matrix <- sapply(raw_names, function(n) as_num(d[[n]]))
  if (is.null(dim(comp_matrix))) comp_matrix <- matrix(comp_matrix, ncol = 5)
  v$sofa_raw_observed_n <- rowSums(!is.na(comp_matrix))
  v$sofa_raw_missing_n <- 5L - v$sofa_raw_observed_n
  v$sofa_score_from_raw_sum5 <- rowSums(comp_matrix, na.rm = TRUE)
  for (j in seq_along(components)) {
    v[[paste0("sofa_missing_", components[j])]] <- as.integer(is.na(comp_matrix[, j]))
  }
  if (database == "eICU-CRD") {
    # New public SQL 05 omits the original workbook-only MD5 snapshots.
    # Check the independently calculated per-stay raw score and sum below.
    qc_cols <- intersect(c(
      "qc_raw_sum5_mismatch_flag", "qc_scored_sum5_mismatch_flag",
      "qc_zero_imputation_mismatch_flag", "qc_availability_flags_mismatch_flag",
      "qc_vaso_unresolved_flag_mismatch_flag",
      "qc_sofa_score_out_of_range_flag"
    ), names(d))
    for (qc_col in qc_cols) {
      if (any(as_num(d[[qc_col]]) != 0, na.rm = TRUE)) {
        stop("eICU SOFA public SQL QC mismatch in: ", qc_col)
      }
    }
    if ("sofa_vaso_rate_unresolved_flag" %in% names(d)) {
      v$sofa_vaso_unresolved_flag <- as_num(d$sofa_vaso_rate_unresolved_flag)
    }
  }
  # Original SOFA is NOT overwritten. This table is for independent QC.
  v
}
log_msg("Reading the 05-06 SOFA component QC tables...")
sofa_m <- read_sofa_qc(INPUT[["mimic_sofa"]], "MIMIC-IV", mimic$icu_id)
sofa_e <- read_sofa_qc(INPUT[["eicu_sofa"]], "eICU-CRD", eicu$icu_id)

# 7. JOIN BY INDEX ICU STAY ONLY -------------------------------------------
combine_patient <- function(d, scr, rrt, sofa) {
  out <- d %>%
    left_join(scr, by = "icu_id") %>%
    left_join(rrt, by = "icu_id") %>%
    left_join(sofa, by = "icu_id")
  if (nrow(out) != nrow(d)) stop(unique(d$database), ": joining multiplied patient rows")
  if (anyDuplicated(out$icu_id)) stop("One-to-many join detected")
  numeric_zero <- c("long_scr_0_24_n", "long_scr_0_48_n", "long_scr_36_60_n",
    "long_scr_42_54_n", "long_scr_48_60_n", "long_scr_preicu_7d_n",
    "rrt_candidate_record_n_0_60", "rrt_strong_record_n_0_60",
    "rrt_candidate_any_0_60", "rrt_strong_any_0_60", "rrt_strong_any_0_48",
    "rrt_direct_any_0_60", "rrt_derived_any_0_60", "rrt_flow_any_0_60",
    "rrt_cumulative_any_0_60")
  for (v in numeric_zero) out[[v]][is.na(out[[v]])] <- 0
  # The original AKI algorithm assesses early AKI using sCr from 0 to 48h,
  # then chooses the nearest valid post-onset sCr in the 36-60h window.
  out$assessment_hour_36_60 <- ifelse(out$primary_exposure_code %in% c(1, 2),
    48 + out$assessment_offset_from_48h_36_60h, NA_real_)
  out$onset_assessment_gap_hours <- out$assessment_hour_36_60 -
    out$aki_onset_hours_from_icu
  out$assessment_strictly_after_onset_flag <- as.integer(
    is.finite(out$onset_assessment_gap_hours) &
      out$onset_assessment_gap_hours > TIME_TOL_HOURS)
  # Before assessment: only for early AKI; include earlier ICU/hospital RRT
  # evidence if recorded for this hospitalization (not proof of initiation).
  out$rrt_strong_evidence_before_assessment <- as.integer(
    out$primary_exposure_code %in% c(1, 2) &
      is.finite(out$assessment_hour_36_60) &
      is.finite(out$rrt_first_strong_any_prior60_hour) &
      out$rrt_first_strong_any_prior60_hour <= out$assessment_hour_36_60)
  out$rrt_0_60_or_preassessment <- as.integer(ifelse(
    out$primary_exposure_code == 0,
    out$rrt_strong_any_0_60 == 1,
    out$rrt_strong_evidence_before_assessment == 1))
  out$borderline_recovery_flag <- as.integer(
    out$primary_exposure_code %in% c(1, 2) &
      ((is.finite(out$assessment_minus_reference_36_60h) &
        abs(out$assessment_minus_reference_36_60h - 0.3) <= GRAY_ZONE_DELTA_MGDL) |
       (is.finite(out$assessment_to_reference_ratio_36_60h) &
        abs(out$assessment_to_reference_ratio_36_60h - 1.5) <= GRAY_ZONE_RATIO)))
  out$scr_count_discrepancy_0_48 <- as.integer(
    !is.na(out$scr_measurement_n_0_48h) &
      out$scr_measurement_n_0_48h != out$long_scr_0_48_n)
  out$scr_count_discrepancy_36_60 <- as.integer(
    !is.na(out$scr_measurement_n_36_60h) &
      out$scr_measurement_n_36_60h != out$long_scr_36_60_n)
  out$sofa_source_discrepancy <- as.integer(
    !approximately_equal(out$standard_nonrenal_sofa_24h, out$sofa_from_05_06))
  out$sofa_sum5_discrepancy <- as.integer(
    !is.na(out$sofa_from_05_06) &
      abs(out$sofa_from_05_06 - out$sofa_score_from_raw_sum5) > 1e-8)
  if (sum(out$sofa_source_discrepancy) > 0 || sum(out$sofa_sum5_discrepancy) > 0) {
    stop(unique(d$database), ": SOFA variable changed after original model. Review 05/06.")
  }
  out
}
mimic <- combine_patient(mimic, scr_m, rrt_m, sofa_m)
eicu <- combine_patient(eicu, scr_e, rrt_e, sofa_e)

# 8. QC TABLES -------------------------------------------------------------
all_patients <- bind_rows(mimic, eicu)
primary <- all_patients %>% filter(primary_analysis_flag == 1,
  primary_exposure_code %in% 0:2) %>% mutate(
    Group = factor(primary_exposure_code, levels = 0:2, labels = exposure_labels))

input_qc <- all_patients %>% group_by(database) %>% summarise(
  original_N = n(), original_unique_stays = n_distinct(icu_id),
  primary_N = sum(primary_analysis_flag == 1 & primary_exposure_code %in% 0:2),
  scr_count_mismatch_0_48_n = sum(scr_count_discrepancy_0_48),
  scr_count_mismatch_36_60_n = sum(scr_count_discrepancy_36_60),
  sofa_score_mismatch_n = sum(sofa_source_discrepancy),
  sofa_five_sum_mismatch_n = sum(sofa_sum5_discrepancy),
  rrt_stronger_evidence_any_0_60_n = sum(rrt_strong_any_0_60 == 1),
  .groups = "drop"
)
if (any(input_qc$scr_count_mismatch_0_48_n > 0 |
        input_qc$scr_count_mismatch_36_60_n > 0)) {
  stop("Original sCr frequency counts differ from 01/02 long exports. Halt to investigate.")
}

scr_frequency <- primary %>% group_by(database, Group) %>% summarise(
  N = n(), deaths = sum(hospital_death_after_60h == 1),
  early_scr_median = safe_median(long_scr_0_48_n),
  early_scr_Q1 = safe_q(long_scr_0_48_n, .25),
  early_scr_Q3 = safe_q(long_scr_0_48_n, .75),
  late_scr_median = safe_median(long_scr_36_60_n),
  late_scr_Q1 = safe_q(long_scr_36_60_n, .25),
  late_scr_Q3 = safe_q(long_scr_36_60_n, .75),
  n_early_scr_1 = sum(long_scr_0_48_n == 1),
  n_early_scr_ge2 = sum(long_scr_0_48_n >= 2),
  n_late_scr_0 = sum(long_scr_36_60_n == 0),
  n_late_scr_ge1 = sum(long_scr_36_60_n >= 1),
  n_early_aki_same_onset_assessment = sum(primary_exposure_code %in% 1:2 &
    is.finite(onset_assessment_gap_hours) &
    abs(onset_assessment_gap_hours) <= TIME_TOL_HOURS),
  n_early_aki_assessment_ge12h = sum(primary_exposure_code %in% 1:2 &
    is.finite(onset_assessment_gap_hours) & onset_assessment_gap_hours >= 12),
  .groups = "drop"
)

baseline_source <- primary %>% group_by(database, Group, reference_scr_source) %>% summarise(
  N = n(), deaths = sum(hospital_death_after_60h == 1),
  mortality_pct = 100 * deaths / N,
  future_reference_to_onset_n = sum(aki_onset_before_reference_time_flag == 1,
    na.rm = TRUE),
  absolute_only_outside_48h_n = sum(absolute_criterion_flag == 1 &
    relative_criterion_flag == 0 & reference_to_aki_onset_hours > 48,
    na.rm = TRUE),
  .groups = "drop"
)

rrt_by_group <- primary %>% group_by(database, Group) %>% summarise(
  N = n(), deaths = sum(hospital_death_after_60h == 1),
  any_rrt_candidate_0_60_n = sum(rrt_candidate_any_0_60 == 1),
  stronger_rrt_evidence_0_60_n = sum(rrt_strong_any_0_60 == 1),
  stronger_rrt_evidence_0_48_n = sum(rrt_strong_any_0_48 == 1),
  stronger_rrt_evidence_before_assessment_n =
    sum(rrt_strong_evidence_before_assessment == 1),
  strongest_documented_or_procedure_0_60_n = sum(rrt_direct_any_0_60 == 1),
  derived_active_only_flag_0_60_n = sum(rrt_derived_any_0_60 == 1),
  eicu_flow_evidence_0_60_n = sum(rrt_flow_any_0_60 == 1),
  eicu_cumulative_balance_change_0_60_n = sum(rrt_cumulative_any_0_60 == 1),
  RRT_candidate_RECORDS_0_60 = sum(rrt_candidate_record_n_0_60),
  RRT_strong_RECORDS_0_60 = sum(rrt_strong_record_n_0_60),
  .groups = "drop"
)

sofa_missing <- all_patients %>% group_by(database) %>% summarise(
  N = n(),
  five_raw_components_available_n = sum(sofa_raw_observed_n == 5),
  respiratory_raw_missing_n = sum(sofa_missing_respiratory == 1),
  coagulation_raw_missing_n = sum(sofa_missing_coagulation == 1),
  liver_raw_missing_n = sum(sofa_missing_liver == 1),
  cardiovascular_raw_missing_n = sum(sofa_missing_cardiovascular == 1),
  cns_raw_missing_n = sum(sofa_missing_cns == 1),
  original_sofa_missing_n = sum(is.na(standard_nonrenal_sofa_24h)),
  .groups = "drop"
)

# 9. SCENARIOS -------------------------------------------------------------
# Primary adjustment matches the original revised fixed-reference paper.
# eICU uses hospital-clustered HC1 variance; MIMIC uses ordinary model variance.
# The manuscript reports three new adjusted sensitivity models here.
# Existing prespecified narrow-window / pre-ICU models are handled by R03.
# Other reviewer-exploration scenarios remain archived with the source script.
scenarios <- c(
  "Primary_original", "Measurement_late_required",
  "Recovery_borderline_excluded", "RRT_exclude_strong_0_60"
)
make_scenario <- function(d, scenario) {
  if (scenario == "Narrow_42_54h_original") {
    x <- d[d$sensitivity_narrow_window_flag == 1 &
      d$sensitivity_narrow_exposure_code %in% 0:2, , drop = FALSE]
    x$analysis_code <- x$sensitivity_narrow_exposure_code
  } else if (scenario == "PreICU_reference_original") {
    x <- d[d$sensitivity_pre_icu_reference_flag == 1 &
      d$sensitivity_pre_icu_reference_exposure_code %in% 0:2, , drop = FALSE]
    x$analysis_code <- x$sensitivity_pre_icu_reference_exposure_code
  } else {
    x <- d[d$primary_analysis_flag == 1 & d$primary_exposure_code %in% 0:2,
           , drop = FALSE]
    x$analysis_code <- x$primary_exposure_code
    if (scenario == "Measurement_late_required") {
      x <- x[x$long_scr_36_60_n >= 1 & (x$analysis_code == 0 |
        x$assessment_strictly_after_onset_flag == 1), , drop = FALSE]
    } else if (scenario == "Measurement_2early_and_late") {
      x <- x[x$long_scr_0_48_n >= 2 & x$long_scr_36_60_n >= 1 &
        (x$analysis_code == 0 | x$assessment_strictly_after_onset_flag == 1),
        , drop = FALSE]
    } else if (scenario == "Onset_assessment_ge12h") {
      x <- x[x$long_scr_36_60_n >= 1 & (x$analysis_code == 0 |
        (is.finite(x$onset_assessment_gap_hours) &
        x$onset_assessment_gap_hours >= 12)), , drop = FALSE]
    } else if (scenario == "RRT_exclude_strong_0_60") {
      x <- x[x$rrt_strong_any_0_60 == 0, , drop = FALSE]
    } else if (scenario == "RRT_exclude_before_assessment_or60") {
      x <- x[x$rrt_0_60_or_preassessment == 0, , drop = FALSE]
    } else if (scenario == "RRT_documented_informed_reclass") {
      x$analysis_code[x$analysis_code == 1 &
        x$rrt_strong_evidence_before_assessment == 1] <- 2
    } else if (scenario == "Recovery_borderline_excluded") {
      x <- x[x$borderline_recovery_flag == 0, , drop = FALSE]
    } else if (scenario != "Primary_original") {
      stop("Unknown scenario: ", scenario)
    }
  }
  x$analysis_exposure <- factor(x$analysis_code, levels = 0:2,
                                labels = exposure_labels)
  x
}

# 10. REGRESSION -----------------------------------------------------------
adjusted_formula <- stats::as.formula(paste(
  "hospital_death_after_60h ~ analysis_exposure +",
  paste(model_covariates, collapse = " + ")
))
unadjusted_formula <- hospital_death_after_60h ~ analysis_exposure
aki_adjusted_formula <- stats::as.formula(paste(
  "hospital_death_after_60h ~ recovery_status + peak_stage_high +",
  paste(model_covariates, collapse = " + ")
))

model_notes <- character(0)
fit_one <- function(df, database, scenario, kind, formula, term_pattern) {
  needed <- all.vars(formula)
  if (database == "eICU-CRD") needed <- union(needed, "hospital_id")
  complete <- stats::complete.cases(df[, needed, drop = FALSE])
  x <- df[complete, , drop = FALSE]
  if (nrow(x) < 100 || length(unique(x$hospital_death_after_60h)) != 2) {
    model_notes <<- c(model_notes, paste(database, scenario, kind,
      "INSUFFICIENT complete data or outcomes"))
    return(data.frame())
  }
  captured <- character(0)
  fit <- tryCatch(withCallingHandlers(
    stats::glm(formula, data = x, family = stats::binomial(),
      na.action = stats::na.fail, model = TRUE),
    warning = function(w) {
      captured <<- c(captured, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  ), error = function(e) e)
  if (inherits(fit, "error")) {
    model_notes <<- c(model_notes, paste(database, scenario, kind,
      "MODEL ERROR:", conditionMessage(fit)))
    return(data.frame())
  }
  if (!isTRUE(fit$converged)) {
    model_notes <<- c(model_notes, paste(database, scenario, kind,
      "NOT CONVERGED; no OR reported"))
    return(data.frame())
  }
  if (length(captured)) model_notes <<- c(model_notes,
    paste(database, scenario, kind, "WARN:", paste(unique(captured), collapse = " | ")))
  vc <- tryCatch(
    if (database == "eICU-CRD") {
      if (length(unique(x$hospital_id)) < 2) stop("<2 hospital clusters")
      sandwich::vcovCL(fit, cluster = x$hospital_id, type = "HC1")
    } else stats::vcov(fit),
    error = function(e) e
  )
  if (inherits(vc, "error")) {
    model_notes <<- c(model_notes, paste(database, scenario, kind,
      "VARIANCE ERROR:", conditionMessage(vc)))
    return(data.frame())
  }
  betas <- stats::coef(fit)
  terms <- grep(term_pattern, names(betas), value = TRUE)
  if (!length(terms)) {
    model_notes <<- c(model_notes, paste(database, scenario, kind,
      "Exposure term absent"))
    return(data.frame())
  }
  results <- lapply(terms, function(term) {
    b <- unname(betas[term])
    se <- sqrt(unname(vc[term, term]))
    if (!is.finite(b) || !is.finite(se) || se <= 0) return(NULL)
    data.frame(Database = database, Scenario = scenario,
      Model = kind, Comparison = gsub(term_pattern, "", term),
      N_complete_cases = nrow(x), Deaths_complete_cases = sum(x$hospital_death_after_60h == 1),
      Hospitals = length(unique(x$hospital_id)),
      SE_type = ifelse(database == "eICU-CRD", "Hospital-clustered HC1", "Model-based"),
      OR = exp(b), CI95_lower = exp(b - 1.96 * se),
      CI95_upper = exp(b + 1.96 * se),
      P_value = 2 * stats::pnorm(abs(b / se), lower.tail = FALSE),
      stringsAsFactors = FALSE)
  })
  ans <- dplyr::bind_rows(results)
  if (!nrow(ans)) {
    model_notes <<- c(model_notes, paste(database, scenario, kind,
      "No estimable exposure coefficients"))
  }
  ans
}

model_list <- list()
scenario_list <- list()
aki_list <- list()
for (database in c("MIMIC-IV", "eICU-CRD")) {
  d <- if (database == "MIMIC-IV") mimic else eicu
  log_msg("Running sensitivity analyses: ", database)
  for (scenario in scenarios) {
    x <- make_scenario(d, scenario)
    cnt <- table(factor(x$analysis_code, levels = 0:2))
    scenario_list[[length(scenario_list) + 1L]] <- data.frame(
      Database = database, Scenario = scenario, N = nrow(x),
      Deaths = sum(x$hospital_death_after_60h == 1, na.rm = TRUE),
      No_AKI = unname(cnt[1]), Resolved = unname(cnt[2]), Ongoing = unname(cnt[3]),
      No_AKI_deaths = sum(x$analysis_code == 0 & x$hospital_death_after_60h == 1,
                          na.rm = TRUE),
      Resolved_deaths = sum(x$analysis_code == 1 & x$hospital_death_after_60h == 1,
                            na.rm = TRUE),
      Ongoing_deaths = sum(x$analysis_code == 2 & x$hospital_death_after_60h == 1,
                           na.rm = TRUE),
      N_complete_adjusted = sum(stats::complete.cases(
        x[, c("hospital_death_after_60h", "analysis_exposure", model_covariates),
          drop = FALSE])), stringsAsFactors = FALSE)
    if (all(cnt > 0)) {
      model_list[[length(model_list) + 1L]] <- fit_one(x, database, scenario,
        "Unadjusted", unadjusted_formula, "^analysis_exposure")
      model_list[[length(model_list) + 1L]] <- fit_one(x, database, scenario,
        "Original covariate adjusted", adjusted_formula, "^analysis_exposure")
    } else {
      model_notes <- c(model_notes, paste(database, scenario,
        "SKIPPED: an exposure group has zero patients"))
    }
    # In early-AKI cases only, adjust for peak early-AKI severity as in the
    # original supplementary AKI-only model.
    a <- x[x$analysis_code %in% 1:2, , drop = FALSE]
    if (nrow(a) > 0 && all(table(factor(a$analysis_code, levels = 1:2)) > 0)) {
      a$recovery_status <- factor(a$analysis_code, levels = 1:2,
        labels = c("Resolved", "Ongoing"))
      a$peak_stage_high <- as.integer(a$peak_aki_stage_0_48h >= 2)
      a$peak_stage_high[is.na(a$peak_aki_stage_0_48h)] <- NA_integer_
      aki_list[[length(aki_list) + 1L]] <- fit_one(a, database, scenario,
        "AKI-only + peak stage adjusted", aki_adjusted_formula, "^recovery_status")
    }
  }
}
scenario_counts <- bind_rows(scenario_list)
model_results <- bind_rows(c(model_list, aki_list))
if (!nrow(model_results)) warning("No estimable models; inspect Model_warnings.txt")

# 11. FINAL MANUSCRIPT TABLES AND QC ---------------------------------------
# The author's original patient-level computations / regressions above are
# unchanged except for loading R01 RDS and the reduced published scenarios.
# Only aggregate results are exported to CSV/XLSX; optional joined RDS stays local.
write_aggregate <- function(data, name) {
  readr::write_excel_csv(data, file.path(OUTPUT_DIR, name), na = "")
  log_msg("Saved: ", name)
  invisible(data)
}
fmt_or <- function(or, lower, upper) {
  ifelse(is.finite(or) & is.finite(lower) & is.finite(upper),
         sprintf("%.2f (%.2f-%.2f)", or, lower, upper), NA_character_)
}
extract_model <- function(database, scenario, model_name, comparison) {
  z <- model_results[model_results$Database == database &
    model_results$Scenario == scenario & model_results$Model == model_name &
    model_results$Comparison == comparison, , drop = FALSE]
  if (nrow(z) != 1L) {
    warning(database, " ", scenario, ": expected one coefficient for ", comparison,
            "; observed ", nrow(z), ".")
    return(data.frame(OR = NA_real_, CI95_lower = NA_real_,
                      CI95_upper = NA_real_, OR_95CI = NA_character_))
  }
  data.frame(OR = z$OR, CI95_lower = z$CI95_lower,
    CI95_upper = z$CI95_upper,
    OR_95CI = fmt_or(z$OR, z$CI95_lower, z$CI95_upper))
}
model_row <- function(database, scenario, model_name, comparison, context = NULL) {
  data.frame(Database = database, Scenario = scenario,
             Model = model_name, Comparison = comparison,
             context, extract_model(database, scenario, model_name, comparison),
             stringsAsFactors = FALSE, check.names = FALSE)
}
find_count <- function(database, scenario) {
  z <- scenario_counts[scenario_counts$Database == database &
    scenario_counts$Scenario == scenario, , drop = FALSE]
  if (nrow(z) != 1L) stop("Scenario summary unavailable: ", database, " / ", scenario)
  z
}

# S6 Panel A: sCr monitoring frequency in the frozen primary cohort.
table_s6a <- scr_frequency %>% transmute(
  Database = database, Trajectory = as.character(Group), N, Deaths = deaths,
  Scr_0_48_median = early_scr_median, Scr_0_48_Q1 = early_scr_Q1,
  Scr_0_48_Q3 = early_scr_Q3,
  Scr_36_60_median = late_scr_median, Scr_36_60_Q1 = late_scr_Q1,
  Scr_36_60_Q3 = late_scr_Q3, No_Scr_36_60_n = n_late_scr_0,
  First_qualifying_equals_assessment_n = n_early_aki_same_onset_assessment
)
# For no-early-AKI there is no first qualifying sCr; use not-applicable.
table_s6a$First_qualifying_equals_assessment_n[
  table_s6a$Trajectory == "No early AKI"] <- NA_integer_
table_s6a$No_Scr_36_60_pct <- round(
  100 * table_s6a$No_Scr_36_60_n / table_s6a$N, 1
)
table_s6a$First_qualifying_equals_assessment_pct <- round(
  100 * table_s6a$First_qualifying_equals_assessment_n / table_s6a$N, 1
)

# S6 Panel B: first qualifying sCr, selected assessment and their interval.
table_s6b <- primary %>%
  filter(primary_exposure_code %in% 1:2) %>%
  group_by(database, Group) %>%
  summarise(
    N = n(),
    First_qualifying_scr_hour_median = safe_median(aki_onset_hours_from_icu),
    First_qualifying_scr_hour_Q1 = safe_q(aki_onset_hours_from_icu, .25),
    First_qualifying_scr_hour_Q3 = safe_q(aki_onset_hours_from_icu, .75),
    Assessment_hour_median = safe_median(assessment_hour_36_60),
    Assessment_hour_Q1 = safe_q(assessment_hour_36_60, .25),
    Assessment_hour_Q3 = safe_q(assessment_hour_36_60, .75),
    Onset_assessment_gap_hour_median = safe_median(onset_assessment_gap_hours),
    Onset_assessment_gap_hour_Q1 = safe_q(onset_assessment_gap_hours, .25),
    Onset_assessment_gap_hour_Q3 = safe_q(onset_assessment_gap_hours, .75),
    .groups = "drop"
  ) %>% rename(Database = database, Trajectory = Group)

# S6 Panel C: same filtering as the original published sensitivity.
table_s6c <- dplyr::bind_rows(lapply(c("MIMIC-IV", "eICU-CRD"), function(database) {
  cnt <- find_count(database, "Measurement_late_required")
  data.frame(Database = database, Included_N = cnt$N, Included_Deaths = cnt$Deaths,
    No_AKI_N = cnt$No_AKI, No_AKI_Deaths = cnt$No_AKI_deaths,
    Resolved_N = cnt$Resolved, Resolved_Deaths = cnt$Resolved_deaths,
    Ongoing_N = cnt$Ongoing, Ongoing_Deaths = cnt$Ongoing_deaths,
    extract_model(database, "Measurement_late_required",
                  "Original covariate adjusted", "Ongoing early AKI"))
}))

# S8 Panel A: count patients excluded near the resolution boundary; never
# reclassify the remaining primary AKI groups.
table_s8a <- dplyr::bind_rows(lapply(c("MIMIC-IV", "eICU-CRD"), function(database) {
  d <- primary[primary$database == database, , drop = FALSE]
  retained <- d[d$borderline_recovery_flag == 0, , drop = FALSE]
  out <- lapply(0:2, function(code) {
    g <- d[d$primary_exposure_code == code, , drop = FALSE]
    kept <- retained[retained$primary_exposure_code == code, , drop = FALSE]
    data.frame(Database = database, Trajectory = exposure_labels[code+1L],
      Original_N = nrow(g), Original_Deaths = sum(g$hospital_death_after_60h == 1),
      Excluded_N = nrow(g)-nrow(kept), Retained_N = nrow(kept),
      Retained_Deaths = sum(kept$hospital_death_after_60h == 1))
  })
  out[[4L]] <- data.frame(Database = database, Trajectory = "Overall",
    Original_N = nrow(d), Original_Deaths = sum(d$hospital_death_after_60h == 1),
    Excluded_N = nrow(d)-nrow(retained), Retained_N = nrow(retained),
    Retained_Deaths = sum(retained$hospital_death_after_60h == 1))
  dplyr::bind_rows(out)
}))
table_s8b <- dplyr::bind_rows(lapply(c("MIMIC-IV", "eICU-CRD"), function(database) {
  cnt <- find_count(database, "Recovery_borderline_excluded")
  data.frame(Database = database, N = cnt$N, Deaths = cnt$Deaths,
    Comparison = c("Resolved early AKI", "Ongoing early AKI")) %>%
    bind_cols(dplyr::bind_rows(lapply(c("Resolved early AKI", "Ongoing early AKI"),
      function(comp) extract_model(database, "Recovery_borderline_excluded",
       "Original covariate adjusted", comp))))
}))
table_s8c <- dplyr::bind_rows(lapply(c("MIMIC-IV", "eICU-CRD"), function(database) {
  cnt <- find_count(database, "Recovery_borderline_excluded")
  data.frame(Database = database, N = cnt$Resolved + cnt$Ongoing,
    Deaths = cnt$Resolved_deaths + cnt$Ongoing_deaths,
    Comparison = "Ongoing early AKI vs resolved early AKI",
    extract_model(database, "Recovery_borderline_excluded",
                  "AKI-only + peak stage adjusted", "Ongoing"))
}))

# S9 Panel A: time-stamped RRT-related evidence, not proven treatment initiation.
table_s9a <- rrt_by_group %>% transmute(
  Database = database, Trajectory = as.character(Group), N,
  RRT_evidence_0_60_n = stronger_rrt_evidence_0_60_n,
  Evidence_at_or_before_assessment_n = stronger_rrt_evidence_before_assessment_n
)
table_s9a$Evidence_at_or_before_assessment_n[
  table_s9a$Trajectory == "No early AKI"] <- NA_integer_
# S9 Panel B: symmetrically exclude evidence documented in ICU 0-60 h.
table_s9b <- dplyr::bind_rows(lapply(c("MIMIC-IV", "eICU-CRD"), function(database) {
  cnt <- find_count(database, "RRT_exclude_strong_0_60")
  data.frame(Database = database, Included_N = cnt$N, Included_Deaths = cnt$Deaths,
    No_AKI_N = cnt$No_AKI, No_AKI_Deaths = cnt$No_AKI_deaths,
    Resolved_N = cnt$Resolved, Resolved_Deaths = cnt$Resolved_deaths,
    Ongoing_N = cnt$Ongoing, Ongoing_Deaths = cnt$Ongoing_deaths,
    Resolved_vs_no = extract_model(database, "RRT_exclude_strong_0_60",
        "Original covariate adjusted", "Resolved early AKI")$OR_95CI,
    Ongoing_vs_no = extract_model(database, "RRT_exclude_strong_0_60",
        "Original covariate adjusted", "Ongoing early AKI")$OR_95CI)
}))

# S11: individual source availability checked before trajectory classifiability
# exclusions. Missing source data may contribute zero points in SOFA scoring,
# but are NOT treated as measurements of normal organ function.
table_s11 <- sofa_missing %>% transmute(
  Database = database, Landmark_N = N,
  All_5_components_observed_n = five_raw_components_available_n,
  Respiratory_missing_n = respiratory_raw_missing_n,
  Coagulation_missing_n = coagulation_raw_missing_n,
  Liver_missing_n = liver_raw_missing_n,
  Cardiovascular_missing_n = cardiovascular_raw_missing_n,
  CNS_missing_n = cns_raw_missing_n,
  Nonrenal_SOFA_missing_n = original_sofa_missing_n
) %>% left_join(
  input_qc %>% transmute(Database = database,
    Score_difference_n = sofa_score_mismatch_n,
    Five_component_sum_difference_n = sofa_five_sum_mismatch_n),
  by = "Database"
)
for (field in c("All_5_components_observed_n", "Respiratory_missing_n",
                "Coagulation_missing_n", "Liver_missing_n",
                "Cardiovascular_missing_n", "CNS_missing_n",
                "Nonrenal_SOFA_missing_n")) {
  table_s11[[sub("_n$", "_pct", field)]] <- round(
    100 * table_s11[[field]] / table_s11$Landmark_N, 1
  )
}

# 12. FROZEN MANUSCRIPT QC TARGETS (never used to fit a model or select data) --
expected_metrics <- data.frame(
  Database = rep(c("MIMIC-IV", "eICU-CRD"), each = 15L),
  Metric = rep(c(
    "landmark_n", "primary_n", "primary_deaths", "S6_monitoring_n",
    "S6_monitoring_deaths", "S8_threshold_retained_n",
    "S8_threshold_retained_deaths", "S9_rrt_retained_n",
    "S9_rrt_retained_deaths", "S11_all_five_observed_n",
    "S11_resp_missing_n", "S11_coag_missing_n", "S11_liver_missing_n",
    "S11_cardio_missing_n", "S11_cns_missing_n"
  ), times = 2L),
  Expected = c(
    7794, 7627, 832, 7006, 775, 7065, 730, 7511, 780,
    1920, 4156, 41, 3552, 10, 17,
    8495, 7738, 709, 5948, 599, 7400, 656, 7610, 686,
    1212, 5493, 1198, 5111, 86, 407
  ), stringsAsFactors = FALSE
)
get_expected_actual <- function(database, metric) {
  ix <- match(database, frozen_expected$Database)
  if (metric == "landmark_n") return(cohort_qc$Full_N[ix])
  if (metric == "primary_n") return(cohort_qc$Primary_N[ix])
  if (metric == "primary_deaths") return(cohort_qc$Primary_deaths[ix])
  if (metric == "S6_monitoring_n") return(find_count(database, "Measurement_late_required")$N)
  if (metric == "S6_monitoring_deaths") return(find_count(database, "Measurement_late_required")$Deaths)
  if (metric == "S8_threshold_retained_n") return(find_count(database, "Recovery_borderline_excluded")$N)
  if (metric == "S8_threshold_retained_deaths") return(find_count(database, "Recovery_borderline_excluded")$Deaths)
  if (metric == "S9_rrt_retained_n") return(find_count(database, "RRT_exclude_strong_0_60")$N)
  if (metric == "S9_rrt_retained_deaths") return(find_count(database, "RRT_exclude_strong_0_60")$Deaths)
  s <- table_s11[table_s11$Database == database, , drop = FALSE]
  columns <- c(
    S11_all_five_observed_n = "All_5_components_observed_n",
    S11_resp_missing_n = "Respiratory_missing_n",
    S11_coag_missing_n = "Coagulation_missing_n",
    S11_liver_missing_n = "Liver_missing_n",
    S11_cardio_missing_n = "Cardiovascular_missing_n",
    S11_cns_missing_n = "CNS_missing_n"
  )
  if (!metric %in% names(columns)) stop("Unknown QC metric: ", metric)
  s[[unname(columns[[metric]])]][1L]
}
expected_metrics$Observed <- mapply(get_expected_actual,
  expected_metrics$Database, expected_metrics$Metric,
  USE.NAMES = FALSE)
expected_metrics$Pass <- expected_metrics$Observed == expected_metrics$Expected

# Exact group-specific published counts from S6, S8 and S9.
expected_groups <- data.frame(
  Database = c(rep("MIMIC-IV", 3L), rep("eICU-CRD", 3L)),
  Trajectory = rep(exposure_labels, 2L),
  S6_no_scr_36_60_n = c(455, 0, 0, 1607, 0, 0),
  S8_excluded_n = c(0, 144, 418, 0, 71, 267),
  S9_rrt_0_60_n = c(20, 58, 38, 66, 29, 33),
  stringsAsFactors = FALSE
)
observed_groups <- expected_groups %>%
  left_join(table_s6a %>% select(Database, Trajectory, No_Scr_36_60_n),
            by = c("Database", "Trajectory")) %>%
  left_join(table_s8a %>% select(Database, Trajectory, Excluded_N),
            by = c("Database", "Trajectory")) %>%
  left_join(table_s9a %>% select(Database, Trajectory, RRT_evidence_0_60_n),
            by = c("Database", "Trajectory"))
observed_groups$S6_pass <- observed_groups$No_Scr_36_60_n ==
  observed_groups$S6_no_scr_36_60_n
observed_groups$S8_pass <- observed_groups$Excluded_N ==
  observed_groups$S8_excluded_n
observed_groups$S9_pass <- observed_groups$RRT_evidence_0_60_n ==
  observed_groups$S9_rrt_0_60_n

# The exact OR and confidence limit values in S6/S8/S9 are recorded as
# independent QC targets, not as inputs or overrides of the fitted models.
expected_or_targets <- data.frame(
  Database = c(
    "MIMIC-IV", "eICU-CRD",            # S6 Panel C
    "MIMIC-IV", "MIMIC-IV",            # S8 Panel B
    "eICU-CRD", "eICU-CRD",
    "MIMIC-IV", "eICU-CRD",            # S8 Panel C
    "MIMIC-IV", "MIMIC-IV",            # S9 Panel B
    "eICU-CRD", "eICU-CRD"
  ),
  Scenario = c(
    rep("Measurement_late_required", 2L),
    rep("Recovery_borderline_excluded", 6L),
    rep("RRT_exclude_strong_0_60", 4L)
  ),
  Model = c(
    rep("Original covariate adjusted", 6L),
    rep("AKI-only + peak stage adjusted", 2L),
    rep("Original covariate adjusted", 4L)
  ),
  Comparison = c(
    "Ongoing early AKI", "Ongoing early AKI",
    "Resolved early AKI", "Ongoing early AKI",
    "Resolved early AKI", "Ongoing early AKI",
    "Ongoing", "Ongoing",
    "Resolved early AKI", "Ongoing early AKI",
    "Resolved early AKI", "Ongoing early AKI"
  ),
  Expected_OR = c(2.81, 2.51, 1.22, 2.91, 1.49, 2.90,
                  2.42, 1.95, 1.10, 2.54, 1.48, 2.53),
  Expected_Lower = c(2.29, 1.92, 0.95, 2.31, 1.09, 2.13,
                     1.78, 1.24, 0.85, 2.08, 1.10, 1.98),
  Expected_Upper = c(3.44, 3.28, 1.56, 3.68, 2.05, 3.95,
                     3.29, 3.06, 1.40, 3.10, 2.00, 3.23)
)
observed_or_targets <- expected_or_targets %>%
  left_join(model_results %>%
    select(Database, Scenario, Model, Comparison, OR, CI95_lower, CI95_upper),
    by = c("Database", "Scenario", "Model", "Comparison"))
observed_or_targets$OR_pass <- !is.na(observed_or_targets$OR) &
  round(observed_or_targets$OR, 2L) == observed_or_targets$Expected_OR
observed_or_targets$Lower_pass <- !is.na(observed_or_targets$CI95_lower) &
  round(observed_or_targets$CI95_lower, 2L) == observed_or_targets$Expected_Lower
observed_or_targets$Upper_pass <- !is.na(observed_or_targets$CI95_upper) &
  round(observed_or_targets$CI95_upper, 2L) == observed_or_targets$Expected_Upper

# 13. SAVE AGGREGATE OUTPUTS AND LOCAL-ONLY JOINED INPUTS -------------------
write_aggregate(table_s6a, "TableS6_PanelA_monitoring.csv")
write_aggregate(table_s6b, "TableS6_PanelB_timing.csv")
write_aggregate(table_s6c, "TableS6_PanelC_monitoring_restricted.csv")
write_aggregate(table_s8a, "TableS8_PanelA_threshold_composition.csv")
write_aggregate(table_s8b, "TableS8_PanelB_threshold_adjusted.csv")
write_aggregate(table_s8c, "TableS8_PanelC_AKI_only.csv")
write_aggregate(table_s9a, "TableS9_PanelA_rrt_evidence.csv")
write_aggregate(table_s9b, "TableS9_PanelB_rrt_exclusion.csv")
write_aggregate(table_s11, "TableS11_nonrenal_sofa_components.csv")
write_aggregate(input_qc, "Source_merge_consistency_QC.csv")
write_aggregate(cohort_qc, "Frozen_cohort_QC.csv")
write_aggregate(baseline_source, "Reference_source_QC.csv")
write_aggregate(scenario_counts, "Scenario_sample_counts_QC.csv")
write_aggregate(model_results, "Sensitivity_models_full_coefficients.csv")
write_aggregate(expected_metrics, "Published_numeric_targets_QC.csv")
write_aggregate(observed_groups, "Published_group_counts_QC.csv")
write_aggregate(observed_or_targets, "Published_OR_estimates_QC.csv")

if (isTRUE(SAVE_JOINED_LOCAL_RDS)) {
  saveRDS(mimic, file.path(OUTPUT_DIR, "MIMIC_Reviewer1_joined_patient_data.rds"),
          compress = "xz")
  saveRDS(eicu, file.path(OUTPUT_DIR, "eICU_Reviewer1_joined_patient_data.rds"),
          compress = "xz")
  log_msg("Private, stay-level joined RDS saved under output/ (DO NOT PUBLISH).")
}
if (requireNamespace("openxlsx", quietly = TRUE)) {
  openxlsx::write.xlsx(list(
    S6_A = table_s6a, S6_B = table_s6b, S6_C = table_s6c,
    S8_A = table_s8a, S8_B = table_s8b, S8_C = table_s8c,
    S9_A = table_s9a, S9_B = table_s9b, S11 = table_s11,
    Manuscript_QC = expected_metrics, Group_QC = observed_groups,
    OR_QC = observed_or_targets
  ), file = file.path(OUTPUT_DIR, "Tables_S6_S8_S9_S11.xlsx"),
  overwrite = TRUE)
}
writeLines(unique(model_notes), file.path(OUTPUT_DIR, "Model_warnings.txt"),
           useBytes = TRUE)
writeLines(c(
  "eICU / MIMIC Reviewer-1 reproducibility notes (final supplement S6/S8/S9/S11)",
  "S6: require a 36-60h sCr for every trajectory group; for early AKI",
  "    the selected assessment must strictly follow first qualifying sCr.",
  "S8: exclude early AKI when |deltaScr-0.30| <= 0.10 mg/dL OR",
  "    |ScrRatio-1.50| <= 0.10. No-early-AKI unchanged; no reassignment.",
  "S8 Panel C: among retained early AKI, additionally adjust for peak stage.",
  "S9: evidence documentation at 0-60h; does not establish RRT start.",
  "S9: MIMIC stronger evidence = direct procedure or official dialysis_active.",
  "    eICU stronger evidence = treatment/flow/cumulative dialysis signal.",
  "S11: source missingness is distinct from imputed-zero scored components.",
  "The manuscript targets in QC do not affect model fitting or exclusions.",
  "Keep exported restricted data, private RDS and output directory out of Git.",
  paste0("Input paths: ", paste(INPUT, collapse = " ; "))
), file.path(OUTPUT_DIR, "00_Methods_and_execution_notes.txt"), useBytes = TRUE)
writeLines(c(log_lines, "", capture.output(utils::sessionInfo())),
           file.path(OUTPUT_DIR, "R06_run_log_and_session.txt"), useBytes = TRUE)
if (any(!expected_metrics$Pass, na.rm = TRUE) ||
    any(!observed_groups$S6_pass | !observed_groups$S8_pass |
        !observed_groups$S9_pass, na.rm = TRUE) ||
    any(!observed_or_targets$OR_pass | !observed_or_targets$Lower_pass |
        !observed_or_targets$Upper_pass)) {
  warning("One or more manuscript QC targets differ. Review the QC CSVs; do not treat the sensitivity estimates as validated.")
}
log_msg("R06 complete. Published-style summary tables saved: ", OUTPUT_DIR)
log_msg("Numerical publication agreement still requires local R/SQL execution.")
