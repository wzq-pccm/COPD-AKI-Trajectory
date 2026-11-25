# ==============================================================================
# COPD early creatinine-defined AKI trajectory study
# R/01_import_harmonize_qc.R
# MIMIC-IV v3.1 and eICU-CRD v2.0 | PostgreSQL SQL exports -> analysis datasets
#
# Public reproduction adaptation of the original revised fixed-reference R 01.
# The statistical exposure, covariate, outcome, and QC definitions below are
# preserved from the original script. This edition changes file interfaces only:
#   - no workstation-specific paths or automatic package installation;
#   - SQL MODULES (default): join one-row-per-stay exports from the 17 SQL scripts;
#   - MERGED: accept the author's legacy pre-merged landmark Excel/CSV exports;
#   - verify join key sets and values in every overlapping column;
#   - retain original RDS output names for downstream scripts 02-11.
#
# Run from the project root:
#   Rscript R/01_import_harmonize_qc.R
# Or set COPD_AKI_PROJECT_ROOT to the project root before running.
# Optional env: COPD_AKI_INPUT_MODE=modules (default) or merged.
#
# SQL MODULES INPUT (export the FINAL patient-level SELECT from each SQL file;
# one record per index stay, CSV UTF-8 preferred, .xlsx/.xls also supported):
#   data/sql_exports/mimiciv/02_aki_trajectory.csv
#   data/sql_exports/mimiciv/03_demographics_comorbidities.csv
#   data/sql_exports/mimiciv/04_nonrenal_sofa_and_qc.csv
#   data/sql_exports/mimiciv/05_first24h_labs.csv
#   data/sql_exports/mimiciv/06_imv_vasopressor.csv
#   data/sql_exports/eicu/02_aki_trajectory.csv
#   data/sql_exports/eicu/03_demographics_comorbidities.csv
#   data/sql_exports/eicu/04_charlson_comorbidity_index.csv
#   data/sql_exports/eicu/05_nonrenal_sofa_and_qc.csv
#   data/sql_exports/eicu/06_first24h_labs.csv
#   data/sql_exports/eicu/07_imv_vasopressor.csv
#
# SQL 01 in both databases includes flow information and is not joined here.
# MIMIC 07/08 and eICU 08/09 are LONG event datasets for later sensitivity
# scripts; do not merge them with the one-row-per-stay patient-level data here.
#
# MERGED INPUT (optional, for comparison with original analysis workbooks):
#   data/merged/mimic_full_merged.xlsx and eicu_full_merged.xlsx
#   or legacy MIMIC原始数据.xlsx and EICU原始数据.xlsx (also CSV accepted).
#
# IMPORTANT: outputs include individual-level restricted cohort records.
# Keep data/ and output/ LOCAL and excluded from Git. Publish only code and
# aggregate, disclosure-reviewed reports, not patient-level exports or RDS.
#
# Outputs (under output/R_analysis_output/):
#   mimic_full_clean.rds, eicu_full_clean.rds
#   mimic_primary_clean.rds, eicu_primary_clean.rds
#   primary CSV copies, QC_*.csv and analysis_manifest.csv
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)

# ------------------------------------------------------------------------------
# 0. User settings: portable project-relative paths
# ------------------------------------------------------------------------------
root_override <- Sys.getenv("COPD_AKI_PROJECT_ROOT", unset = "")
PROJECT_ROOT <- if (nzchar(root_override)) {
  normalizePath(root_override, winslash = "/", mustWork = TRUE)
} else {
  normalizePath(".", winslash = "/", mustWork = TRUE)
}

INPUT_MODE <- tolower(Sys.getenv("COPD_AKI_INPUT_MODE", unset = "modules"))
if (!INPUT_MODE %in% c("modules", "merged")) {
  stop("COPD_AKI_INPUT_MODE must be 'modules' or 'merged'.")
}

# The public SQL files may be executed independently; save the FINAL SELECT of
# each named module to the corresponding file in this folder.
SQL_EXPORT_DIR <- file.path(PROJECT_ROOT, "data", "sql_exports")
MERGED_INPUT_DIR <- file.path(PROJECT_ROOT, "data", "merged")
MIMIC_SHEET <- 1
EICU_SHEET <- 1

# Keep this location stable across scripts 01-11.
OUTPUT_DIR <- file.path(PROJECT_ROOT, "output", "R_analysis_output")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# This is the original study's frozen cohort; stop if a different extract is
# loaded, rather than silently re-computing a different analysis population.
STOP_ON_EXPECTED_COUNT_MISMATCH <- TRUE

# Used only in the optional merged-input mode.
MIMIC_MERGED_BASE_NAMES <- c("mimic_full_merged", "MIMIC原始数据")
EICU_MERGED_BASE_NAMES <- c("eicu_full_merged", "EICU原始数据")

# ------------------------------------------------------------------------------
# 1. Required packages
# ------------------------------------------------------------------------------
required_packages <- c("readxl", "readr", "dplyr", "stringr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    ". Install these before running (prefer renv::restore() if renv.lock is provided)."
  )
}
suppressPackageStartupMessages({
  library(readxl)
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
})

# ------------------------------------------------------------------------------
# 2. General helper functions
# ------------------------------------------------------------------------------
find_input_file <- function(directory, base_name) {
  candidates <- file.path(
    directory,
    paste0(base_name, c(".xlsx", ".xls", ".csv", ".CSV"))
  )
  existing <- candidates[file.exists(candidates)]

  if (length(existing) == 1L) {
    return(normalizePath(existing, winslash = "/"))
  }

  if (length(existing) > 1L) {
    stop(
      "Multiple files found for '", base_name, "':\n",
      paste(existing, collapse = "\n"),
      "\nKeep only one matching input file."
    )
  }

  all_files <- list.files(directory, full.names = TRUE)
  valid_ext <- tolower(tools::file_ext(all_files)) %in% c("xlsx", "xls", "csv")
  same_base <- tolower(tools::file_path_sans_ext(basename(all_files))) ==
    tolower(base_name)
  matches <- all_files[valid_ext & same_base]

  if (length(matches) == 0L) {
    stop(
      "Input file not found: ", base_name,
      "\nDirectory: ", directory,
      "\nAccepted extensions: .xlsx, .xls, .csv"
    )
  }

  if (length(matches) > 1L) {
    stop(
      "Multiple files found for '", base_name, "':\n",
      paste(matches, collapse = "\n"),
      "\nKeep only one matching input file."
    )
  }

  normalizePath(matches, winslash = "/")
}

read_source_file <- function(path, sheet = 1) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls")) {
    # Import all Excel fields as text. This prevents mixed-type inference from
    # silently changing IDs or columns that contain literal "NA" strings.
    out <- readxl::read_excel(
      path,
      sheet = sheet,
      col_types = "text",
      na = c("", "NA", "N/A", "NULL", "null", "None", "none", "NaN"),
      .name_repair = "minimal"
    )
  } else if (ext == "csv") {
    out <- readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character()),
      na = c("", "NA", "N/A", "NULL", "null", "None", "none", "NaN"),
      show_col_types = FALSE,
      progress = FALSE,
      locale = readr::locale(encoding = "UTF-8"),
      name_repair = "minimal"
    )
  } else {
    stop("Unsupported file extension: ", ext)
  }

  out <- as.data.frame(out, check.names = FALSE)
  names(out) <- trimws(names(out))

  if (anyDuplicated(names(out)) > 0L) {
    dup <- unique(names(out)[duplicated(names(out))])
    stop("Duplicated column names: ", paste(dup, collapse = ", "))
  }

  out
}

clean_character <- function(x) {
  y <- as.character(x)
  y <- stringr::str_replace_all(y, "\\u00A0|\\u3000", " ")
  y <- stringr::str_squish(y)
  y[y %in% c("", "NA", "N/A", "NULL", "null", "None", "none", "NaN")] <- NA_character_
  y
}

normalize_numeric_text <- function(x) {
  y <- clean_character(x)
  y <- stringr::str_replace_all(y, "−|–|—", "-")
  y <- stringr::str_replace_all(y, ",", "")

  accounting <- !is.na(y) & stringr::str_detect(y, "^\\(.*\\)$")
  y[accounting] <- paste0(
    "-",
    stringr::str_replace_all(y[accounting], "^\\(|\\)$", "")
  )
  y
}

numeric_regex <- "^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$"

is_strict_numeric_text <- function(x) {
  y <- normalize_numeric_text(x)
  is.na(y) | stringr::str_detect(y, numeric_regex)
}

conversion_issue_log <- tibble::tibble(
  database = character(),
  variable = character(),
  original_value = character(),
  n = integer()
)

convert_numeric_strict <- function(x, variable, database) {
  y <- normalize_numeric_text(x)
  valid <- is.na(y) | stringr::str_detect(y, numeric_regex)

  if (any(!valid)) {
    issue <- as.data.frame(table(y[!valid], useNA = "no"), stringsAsFactors = FALSE)
    names(issue) <- c("original_value", "n")
    issue$database <- database
    issue$variable <- variable
    issue <- issue[, c("database", "variable", "original_value", "n")]

    conversion_issue_log <<- dplyr::bind_rows(
      conversion_issue_log,
      tibble::as_tibble(issue)
    )
  }

  y[!valid] <- NA_character_
  suppressWarnings(as.numeric(y))
}

convert_integer_checked <- function(x, variable, database, tolerance = 1e-8) {
  z <- convert_numeric_strict(x, variable, database)
  non_integer <- !is.na(z) & abs(z - round(z)) > tolerance

  if (any(non_integer)) {
    warning(
      database, ": variable '", variable,
      "' contains non-integer values; values are retained as numeric."
    )
    return(z)
  }

  as.integer(round(z))
}

assert_required_columns <- function(data, columns, database) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(
      database, " is missing required columns: ",
      paste(missing, collapse = ", "),
      "\nThis usually indicates that an older SQL export was loaded."
    )
  }
}

check_allowed_values <- function(data, variable, allowed, database) {
  if (!variable %in% names(data)) return(invisible(NULL))
  observed <- sort(unique(data[[variable]][!is.na(data[[variable]])]))
  invalid <- setdiff(observed, allowed)

  if (length(invalid) > 0L) {
    stop(
      database, ": invalid values in '", variable, "': ",
      paste(invalid, collapse = ", "),
      ". Allowed values: ", paste(allowed, collapse = ", ")
    )
  }
}

check_unique_key <- function(data, key, database) {
  if (!key %in% names(data)) stop(database, ": key not found: ", key)

  missing_n <- sum(is.na(data[[key]]) | data[[key]] == "")
  duplicated_n <- sum(duplicated(data[[key]]) & !is.na(data[[key]]))

  if (missing_n > 0L || duplicated_n > 0L) {
    stop(
      database, ": key check failed for '", key,
      "'. Missing = ", missing_n,
      ", duplicated = ", duplicated_n
    )
  }

  invisible(TRUE)
}

compare_expected <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = 0))

  if (!ok) {
    msg <- paste0(label, ": expected ", expected, ", observed ", actual)
    if (STOP_ON_EXPECTED_COUNT_MISMATCH) stop(msg) else warning(msg)
  }

  ok
}

safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)


# ------------------------------------------------------------------------------
# 2A. Import the per-SQL result exports without changing cohort definitions
# ------------------------------------------------------------------------------
# Each subsequent SQL export must contain EXACTLY the same landmark stay IDs as
# its database's SQL 02. We compare not just row counts but the actual key sets.
# Duplicate fields from independent SQL scripts are compared and never silently
# overwritten. Only genuinely new columns are appended to the base table.
find_first_input <- function(directory, base_names) {
  if (!dir.exists(directory)) {
    stop("Merged input directory does not exist: ", directory)
  }
  for (base_name in base_names) {
    all_files <- list.files(directory, full.names = TRUE)
    present <- tolower(tools::file_path_sans_ext(basename(all_files))) ==
      tolower(base_name) &
      tolower(tools::file_ext(all_files)) %in% c("csv", "xlsx", "xls")
    if (any(present)) {
      # find_input_file() also catches ambiguous duplicate extensions.
      return(find_input_file(directory, base_name))
    }
  }
  stop(
    "No input file found in ", directory,
    ". Expected one of: ", paste(base_names, collapse = ", "),
    " (.csv, .xlsx, .xls)."
  )
}

# Legacy numeric columns sometimes differ only as text ("1" vs "1.0") after
# export. Tolerate equivalent numeric rendering, but NEVER tolerate discrepancies
# in identifying keys or differences in actual observed values.
overlap_equal <- function(x, y, column) {
  a <- clean_character(x)
  b <- clean_character(y)
  both_missing <- is.na(a) & is.na(b)
  both_observed <- !is.na(a) & !is.na(b)
  equal <- both_missing | (both_observed & a == b)
  equal[is.na(equal)] <- FALSE

  id_columns <- c(
    "subject_id", "hadm_id", "stay_id", "patient_key", "uniquepid",
    "patienthealthsystemstayid", "patientunitstayid", "hospitalid", "wardid"
  )
  if (!column %in% id_columns) {
    ix <- which(both_observed & !equal)
    if (length(ix) > 0L) {
      an <- normalize_numeric_text(a[ix])
      bn <- normalize_numeric_text(b[ix])
      valid_numeric <- is_strict_numeric_text(an) & is_strict_numeric_text(bn)
      if (any(valid_numeric)) {
        j <- ix[valid_numeric]
        aa <- suppressWarnings(as.numeric(an[valid_numeric]))
        bb <- suppressWarnings(as.numeric(bn[valid_numeric]))
        numeric_equal <- is.finite(aa) & is.finite(bb) &
          abs(aa - bb) <= 1e-10 * pmax(1, abs(aa), abs(bb))
        equal[j[numeric_equal]] <- TRUE
      }
    }
  }
  equal
}

SQL_MODULES <- list(
  mimiciv = c(
    "02_aki_trajectory", "03_demographics_comorbidities",
    "04_nonrenal_sofa_and_qc", "05_first24h_labs", "06_imv_vasopressor"
  ),
  eicu = c(
    "02_aki_trajectory", "03_demographics_comorbidities",
    "04_charlson_comorbidity_index", "05_nonrenal_sofa_and_qc",
    "06_first24h_labs", "07_imv_vasopressor"
  )
)

module_merge_log <- tibble::tibble(
  database = character(),
  module = character(),
  export_file = character(),
  n_rows = integer(),
  n_columns = integer(),
  overlapping_columns_checked = integer(),
  added_columns = integer()
)

import_sql_modules <- function(database, module_dir, module_names, stay_key, sheet) {
  if (!dir.exists(module_dir)) {
    stop(
      database, ": SQL export directory does not exist: ", module_dir,
      ". Export the SQL modules' final SELECT results to this directory."
    )
  }
  merged <- NULL
  for (base_name in module_names) {
    path <- find_input_file(module_dir, base_name)
    data <- read_source_file(path, sheet = sheet)
    assert_required_columns(data, stay_key, paste(database, base_name))
    data[[stay_key]] <- clean_character(data[[stay_key]])
    check_unique_key(data, stay_key, paste(database, base_name))

    n_overlap <- 0L
    n_added <- ncol(data)
    if (is.null(merged)) {
      merged <- data
    } else {
      if (nrow(data) != nrow(merged) ||
          !setequal(data[[stay_key]], merged[[stay_key]])) {
        stop(
          database, ": ", base_name,
          " has a different landmark stay-ID set from SQL 02. ",
          "Rows: ", nrow(data), " vs ", nrow(merged),
          ". Do not merge a changed or incomplete cohort."
        )
      }
      idx <- match(merged[[stay_key]], data[[stay_key]])
      if (anyNA(idx)) stop(database, ": internal key match failure.")
      aligned <- data[idx, , drop = FALSE]
      overlap <- setdiff(intersect(names(merged), names(aligned)), stay_key)
      for (column in overlap) {
        ok <- overlap_equal(merged[[column]], aligned[[column]], column)
        if (!all(ok)) {
          stop(
            database, ": conflicting shared column '", column,
            "' in SQL export ", base_name, "; differing stays: ",
            sum(!ok), ". Review the SQL exports before proceeding."
          )
        }
      }
      added <- setdiff(names(aligned), names(merged))
      merged <- cbind(merged, aligned[, added, drop = FALSE])
      n_overlap <- length(overlap)
      n_added <- length(added)
    }

    module_merge_log <<- dplyr::bind_rows(
      module_merge_log,
      tibble::tibble(
        database = database,
        module = base_name,
        export_file = file.path("data", "sql_exports",
                                if (database == "MIMIC-IV") "mimiciv" else "eicu",
                                basename(path)),
        n_rows = nrow(data),
        n_columns = ncol(data),
        overlapping_columns_checked = as.integer(n_overlap),
        added_columns = as.integer(n_added)
      )
    )
    message(database, " | ", base_name, ": ", nrow(data),
            " rows, +", n_added, " fields")
  }
  check_unique_key(merged, stay_key, database)
  merged
}

# ------------------------------------------------------------------------------
# 3. Variable definitions for the REVISED fixed-reference exports
# ------------------------------------------------------------------------------
# Identifiers remain character even if every value contains digits.
id_vars_mimic <- c("subject_id", "hadm_id", "stay_id")
id_vars_eicu <- c(
  "patient_key", "uniquepid", "patienthealthsystemstayid",
  "patientunitstayid", "hospitalid", "wardid"
)

# Variables used directly in the current analysis or in prespecified sensitivity
# analyses. Additional purely numeric QC columns can still be auto-converted below.
common_numeric_vars <- c(
  # Landmark / outcome
  "eligible_at_60h_flag", "hospital_death_after_60h", "followup_days_after_60h",

  # Fixed reference sCr ascertainment
  "pre_icu_7d_scr_n", "pre_icu_7d_latest_scr",
  "pre_icu_reference_hours_before_icu",
  "icu_0_12h_scr_n", "icu_0_12h_min_scr",
  "icu_0_24h_scr_n", "icu_0_24h_min_scr",
  "first_icu_scr_0_24h",
  "reference_scr", "reference_scr_hours_from_icu",
  "pre_icu_reference_available_flag", "surrogate_reference_0_24h_flag",
  "reference_scr_missing_flag",

  # Early AKI and measurement density
  "scr_measurement_n_0_48h", "scr_measurement_n_42_54h",
  "scr_measurement_n_36_60h", "scr_measurement_n_48_60h",
  "scr_record_n_0_48h", "evaluable_scr_n_0_48h",
  "peak_aki_stage_0_48h", "observed_peak_scr_0_48h",
  "peak_to_reference_ratio_0_48h", "peak_minus_reference_0_48h",
  "aki_onset_hours_from_icu", "aki_onset_scr", "aki_stage_at_onset",
  "relative_criterion_flag", "absolute_criterion_flag",
  "reference_to_aki_onset_hours", "aki_onset_before_reference_time_flag",
  "peak_aki_stage_scr", "peak_aki_stage",

  # Recovery assessment
  "assessment_scr_42_54h", "assessment_offset_from_48h_42_54h",
  "assessment_distance_from_48h_42_54h",
  "onset_to_assessment_hours_42_54h",
  "assessment_to_reference_ratio_42_54h",
  "assessment_minus_reference_42_54h",
  "assessment_scr_36_60h", "assessment_offset_from_48h_36_60h",
  "assessment_distance_from_48h_36_60h",
  "onset_to_assessment_hours_36_60h",
  "assessment_to_reference_ratio_36_60h",
  "assessment_minus_reference_36_60h",

  # Late-window QC
  "scr_record_n_48_60h", "evaluable_scr_n_48_60h",
  "peak_aki_stage_48_60h", "late_aki_onset_scr",
  "late_aki_stage_at_onset",

  # Primary and sensitivity exposure codes / flags
  "primary_exposure_code", "sensitivity_narrow_exposure_code",
  "sensitivity_pre_icu_reference_exposure_code",
  "sensitivity_strict_no_aki_exposure_code",
  "sensitivity_onset_ge12h_exposure_code",
  "primary_analysis_flag", "sensitivity_narrow_window_flag",
  "sensitivity_pre_icu_reference_flag", "sensitivity_onset_ge12h_flag",
  "sensitivity_strict_no_aki_flag", "aki_only_analysis_flag",
  "no_early_aki_flag", "resolved_early_aki_flag", "ongoing_early_aki_flag",
  "confirmed_no_aki_0_60h_flag", "new_aki_48_60h_flag",
  "indeterminate_aki_48_60h_flag", "onset_to_assessment_ge12h_flag",

  # Harmonized demographics / comorbidities / severity
  "age", "female_flag", "heart_failure_flag", "diabetes_flag", "ckd_flag",
  "chronic_liver_disease_flag", "malignancy_flag",
  "charlson_comorbidity_index", "standard_nonrenal_sofa_24h",

  # Additional-adjustment sensitivity covariates
  "imv_any_0_24h", "vasopressor_any_0_24h",
  # Descriptive-only labs; not in the primary adjustment set.
  "hemoglobin_first_24h", "bun_first_24h"
)

mimic_numeric_vars <- c(
  common_numeric_vars,
  "broad_copd_principal_flag", "aecopd_any_flag", "aecopd_principal_flag",
  "exacerbation_code_flag", "acute_infection_code_flag",
  "surgical_service_flag", "admission_category_code", "admission_urgency_code",
  "charlson_missing_flag", "principal_diagnosis_missing_flag",
  "first_service_missing_flag", "stage3a_primary_covariates_complete_flag",
  "total_sofa_24h", "renal_sofa_24h",
  "total_sofa_24h_missing_flag", "renal_sofa_24h_missing_flag",
  "standard_nonrenal_sofa_24h_missing_flag",
  "stage3b_primary_covariate_complete_flag"
)

eicu_numeric_vars <- c(
  common_numeric_vars,

  # Cohort / timing
  "hospitaldischargeyear", "hospitaladmitoffset", "hospitaldischargeoffset",
  "unitvisitnumber", "unitdischargeoffset", "icu_admission_offset",
  "reference_fallback_0_24h_end_offset", "recovery_window_36_60_start_offset",
  "recovery_window_42_54_start_offset", "target_48h_offset",
  "early_aki_0_48h_end_offset", "recovery_window_42_54_end_offset",
  "recovery_window_36_60_end_offset", "landmark_60h_offset",
  "scr_lookback_start_offset", "hospital_expire_flag",
  "left_icu_before_60h_but_still_hospitalized_flag",

  # eICU fixed-reference timestamp offsets
  "pre_icu_7d_latest_offset", "icu_0_12h_min_scr_offset",
  "icu_0_24h_min_scr_offset", "first_icu_scr_offset_0_24h",
  "reference_scr_offset", "aki_onset_offset", "peak_aki_stage_offset",
  "assessment_scr_offset_42_54h", "assessment_scr_offset_36_60h",
  "late_aki_onset_offset",

  # COPD / Stage 3A
  "broad_copd_flag", "copd_800070_flag", "aecopd_800108_flag", "aecopd_flag",
  "esrd_chronic_dialysis_flag",
  "diagnosis_record_n", "pasthistory_record_n", "apachepredvar_record_n",
  "diagnosis_data_available_flag", "pasthistory_data_available_flag",
  "apachepredvar_data_available_flag", "any_comorbidity_source_available_flag",
  "heart_failure_dx_flag", "heart_failure_ph_flag",
  "diabetes_dx_flag", "diabetes_ph_flag", "diabetes_apache_flag",
  "ckd_dx_flag", "ckd_ph_flag",
  "chronic_liver_disease_dx_flag", "chronic_liver_disease_ph_flag",
  "chronic_liver_disease_apache_flag",
  "malignancy_dx_flag", "malignancy_ph_flag", "malignancy_apache_flag",
  "elective_surgery_apache_flag", "surgical_admission_flag",
  "stage3a_primary_covariates_complete_flag",

  # eICU Stage 3B nonrenal SOFA source/QC variables
  "first_24h_end_offset", "apache_pao2", "apache_fio2_raw",
  "apache_fio2_fraction", "apache_pao2_fio2_ratio",
  "apache_intubated_at_worst_abg", "platelet_measurement_n_0_24h",
  "platelet_min_0_24h", "bilirubin_measurement_n_0_24h",
  "bilirubin_max_lab_0_24h", "apache_bilirubin", "bilirubin_max_0_24h",
  "invasive_map_n_0_24h", "invasive_map_min_0_24h",
  "noninvasive_map_n_0_24h", "noninvasive_map_min_0_24h",
  "raw_map_min_0_24h", "apache_meanbp", "map_min_0_24h",
  "sofa_vaso_record_n_0_24h", "sofa_vaso_active_record_n_0_24h",
  "sofa_vaso_rate_normalized_record_n_0_24h",
  "sofa_vaso_rate_unresolved_active_n_0_24h",
  "dopamine_any_0_24h", "dobutamine_any_0_24h",
  "epinephrine_any_0_24h", "norepinephrine_any_0_24h",
  "dopamine_max_mcg_kg_min_0_24h", "dobutamine_max_mcg_kg_min_0_24h",
  "epinephrine_max_mcg_kg_min_0_24h", "norepinephrine_max_mcg_kg_min_0_24h",
  "apache_gcs_eyes", "apache_gcs_motor", "apache_gcs_verbal",
  "apache_gcs_meds", "apache_gcs", "apache_gcs_unscorable_due_to_meds_flag",
  "apacheapsvar_available_flag",
  "respiratory_sofa_24h_raw", "coagulation_sofa_24h_raw",
  "liver_sofa_24h_raw", "cardiovascular_sofa_24h_raw", "cns_sofa_24h_raw",
  "respiratory_sofa_24h", "coagulation_sofa_24h", "liver_sofa_24h",
  "cardiovascular_sofa_24h", "cns_sofa_24h",
  "nonrenal_sofa_component_available_n",
  "nonrenal_sofa_all_components_available_flag",
  "respiratory_sofa_missing_flag", "coagulation_sofa_missing_flag",
  "liver_sofa_missing_flag", "cardiovascular_sofa_missing_flag",
  "cns_sofa_missing_flag", "stage3b_primary_covariate_complete_flag",
  "sofa_vaso_rate_unresolved_flag", "landmark_hour"
)

# Integer-looking study fields. Decimal hour-distance variables are intentionally
# NOT matched simply because their names contain the word "offset".
integer_name_regex <- paste0(
  "(_flag$|_code$|_stage$|_n$|_count$|_year$|_id$|_offset$|",
  "unitvisitnumber$|landmark_hour$)"
)

# Character variables that must never be auto-converted even if their content
# happens to look numeric.
never_auto_numeric <- c(
  id_vars_mimic, id_vars_eicu,
  "principal_icd_code", "principal_icd_version",
  "race", "ethnicity",
  "icu_intime", "icu_outtime", "recovery_window_36_60_start",
  "recovery_window_42_54_start", "target_48h_time",
  "recovery_window_42_54_end", "landmark_60h_time",
  "outcome_or_discharge_time", "pre_icu_7d_latest_time",
  "icu_0_12h_min_scr_time", "icu_0_24h_min_scr_time",
  "first_icu_scr_time_0_24h", "reference_scr_time", "aki_onset_time",
  "peak_aki_stage_time", "assessment_scr_time_42_54h",
  "assessment_scr_time_36_60h", "late_aki_onset_time", "first_service_time"
)

# Current shared analysis signature. These columns distinguish the revised
# fixed-reference exports from the previous rolling-KDIGO files.
required_common <- c(
  "eligible_at_60h_flag", "hospital_death_after_60h",
  "reference_scr", "reference_scr_source", "reference_scr_missing_flag",
  "primary_exposure_code", "primary_analysis_flag",
  "sensitivity_narrow_exposure_code", "sensitivity_narrow_window_flag",
  "sensitivity_pre_icu_reference_exposure_code",
  "sensitivity_pre_icu_reference_flag",
  "sensitivity_strict_no_aki_exposure_code", "sensitivity_strict_no_aki_flag",
  "sensitivity_onset_ge12h_exposure_code", "sensitivity_onset_ge12h_flag",
  "age", "female_flag", "ckd_flag", "heart_failure_flag", "diabetes_flag",
  "chronic_liver_disease_flag", "malignancy_flag",
  "standard_nonrenal_sofa_24h", "charlson_comorbidity_index",
  "imv_any_0_24h", "vasopressor_any_0_24h"
)

required_mimic <- c(required_common, id_vars_mimic, "aecopd_any_flag")
required_eicu  <- c(required_common, id_vars_eicu, "aecopd_flag")

# ------------------------------------------------------------------------------
# 4. Numeric normalization
# ------------------------------------------------------------------------------
normalize_dataset_types <- function(data, database, explicit_numeric, id_vars) {
  data[] <- lapply(data, function(x) {
    if (is.character(x)) clean_character(x) else x
  })

  for (v in intersect(id_vars, names(data))) {
    data[[v]] <- clean_character(data[[v]])
  }

  explicit_present <- intersect(unique(explicit_numeric), names(data))
  for (v in explicit_present) {
    if (stringr::str_detect(v, integer_name_regex) ||
        v %in% c("imv_any_0_24h", "vasopressor_any_0_24h")) {
      data[[v]] <- convert_integer_checked(data[[v]], v, database)
    } else {
      data[[v]] <- convert_numeric_strict(data[[v]], v, database)
    }
  }

  # Safety conversion for remaining numeric-looking text columns.
  character_vars <- names(data)[vapply(data, is.character, logical(1))]
  auto_candidates <- setdiff(character_vars, never_auto_numeric)

  for (v in auto_candidates) {
    values <- data[[v]]
    nonmissing <- !is.na(values)

    if (any(nonmissing) && all(is_strict_numeric_text(values[nonmissing]))) {
      if (stringr::str_detect(v, integer_name_regex) ||
          v %in% c("imv_any_0_24h", "vasopressor_any_0_24h")) {
        data[[v]] <- convert_integer_checked(values, v, database)
      } else {
        data[[v]] <- convert_numeric_strict(values, v, database)
      }
    }
  }

  data
}

# ------------------------------------------------------------------------------
# 5. Harmonization functions
# ------------------------------------------------------------------------------
recode_three_level_exposure <- function(code) {
  factor(
    code,
    levels = c(0, 1, 2),
    labels = c("No early AKI", "Resolved early AKI", "Ongoing early AKI")
  )
}

harmonize_mimic <- function(data) {
  assert_required_columns(data, required_mimic, "MIMIC")

  data %>%
    mutate(
      database = "MIMIC-IV",
      patient_id = as.character(subject_id),
      hospital_stay_id = as.character(hadm_id),
      icu_stay_id = as.character(stay_id),
      hospital_id = "MIMIC",
      aecopd_analysis_flag = as.integer(aecopd_any_flag),

      primary_exposure = recode_three_level_exposure(primary_exposure_code),
      sensitivity_narrow_exposure =
        recode_three_level_exposure(sensitivity_narrow_exposure_code),
      sensitivity_pre_icu_reference_exposure =
        recode_three_level_exposure(sensitivity_pre_icu_reference_exposure_code),
      sensitivity_strict_no_aki_exposure =
        recode_three_level_exposure(sensitivity_strict_no_aki_exposure_code),
      sensitivity_onset_ge12h_exposure =
        recode_three_level_exposure(sensitivity_onset_ge12h_exposure_code),

      reference_source = factor(
        reference_scr_source,
        levels = c("pre_icu_7d_latest", "icu_0_24h_min", "missing"),
        labels = c(
          "Measured pre-ICU reference",
          "ICU 0-24 h surrogate reference",
          "Missing reference"
        )
      ),

      sex = factor(
        female_flag,
        levels = c(0, 1),
        labels = c("Male", "Female")
      ),

      outcome = factor(
        hospital_death_after_60h,
        levels = c(0, 1),
        labels = c("Discharged alive", "In-hospital death after 60 h")
      )
    )
}

harmonize_eicu <- function(data) {
  assert_required_columns(data, required_eicu, "eICU")

  data %>%
    mutate(
      database = "eICU-CRD",
      patient_id = as.character(uniquepid),
      hospital_stay_id = as.character(patienthealthsystemstayid),
      icu_stay_id = as.character(patientunitstayid),
      hospital_id = as.character(hospitalid),
      aecopd_analysis_flag = as.integer(aecopd_flag),

      primary_exposure = recode_three_level_exposure(primary_exposure_code),
      sensitivity_narrow_exposure =
        recode_three_level_exposure(sensitivity_narrow_exposure_code),
      sensitivity_pre_icu_reference_exposure =
        recode_three_level_exposure(sensitivity_pre_icu_reference_exposure_code),
      sensitivity_strict_no_aki_exposure =
        recode_three_level_exposure(sensitivity_strict_no_aki_exposure_code),
      sensitivity_onset_ge12h_exposure =
        recode_three_level_exposure(sensitivity_onset_ge12h_exposure_code),

      reference_source = factor(
        reference_scr_source,
        levels = c("pre_icu_7d_latest", "icu_0_24h_min", "missing"),
        labels = c(
          "Measured pre-ICU reference",
          "ICU 0-24 h surrogate reference",
          "Missing reference"
        )
      ),

      sex = factor(
        female_flag,
        levels = c(0, 1),
        labels = c("Male", "Female")
      ),

      outcome = factor(
        hospital_death_after_60h,
        levels = c(0, 1),
        labels = c("Discharged alive", "In-hospital death after 60 h")
      )
    )
}

# ------------------------------------------------------------------------------
# 6. Locate and import the current source data
# ------------------------------------------------------------------------------
if (INPUT_MODE == "modules") {
  mimic_path <- file.path(SQL_EXPORT_DIR, "mimiciv")
  eicu_path  <- file.path(SQL_EXPORT_DIR, "eicu")

  mimic_raw <- import_sql_modules(
    database = "MIMIC-IV",
    module_dir = mimic_path,
    module_names = SQL_MODULES$mimiciv,
    stay_key = "stay_id",
    sheet = MIMIC_SHEET
  )
  eicu_raw <- import_sql_modules(
    database = "eICU-CRD",
    module_dir = eicu_path,
    module_names = SQL_MODULES$eicu,
    stay_key = "patientunitstayid",
    sheet = EICU_SHEET
  )
} else {
  mimic_path <- find_first_input(MERGED_INPUT_DIR, MIMIC_MERGED_BASE_NAMES)
  eicu_path  <- find_first_input(MERGED_INPUT_DIR, EICU_MERGED_BASE_NAMES)
  mimic_raw <- read_source_file(mimic_path, sheet = MIMIC_SHEET)
  eicu_raw  <- read_source_file(eicu_path, sheet = EICU_SHEET)
}

message("Input mode: ", INPUT_MODE)
message("MIMIC source: ", mimic_path)
message("eICU source : ", eicu_path)
message("Imported MIMIC rows: ", nrow(mimic_raw), "; columns: ", ncol(mimic_raw))
message("Imported eICU rows : ", nrow(eicu_raw), "; columns: ", ncol(eicu_raw))

# Check the fixed-reference signature BEFORE type conversion.
assert_required_columns(mimic_raw, required_mimic, "MIMIC")
assert_required_columns(eicu_raw, required_eicu, "eICU")

# ------------------------------------------------------------------------------
# 7. Normalize types and harmonize analysis variables
# ------------------------------------------------------------------------------
mimic_full <- normalize_dataset_types(
  mimic_raw,
  database = "MIMIC",
  explicit_numeric = mimic_numeric_vars,
  id_vars = id_vars_mimic
) %>%
  harmonize_mimic()

eicu_full <- normalize_dataset_types(
  eicu_raw,
  database = "eICU",
  explicit_numeric = eicu_numeric_vars,
  id_vars = id_vars_eicu
) %>%
  harmonize_eicu()

rm(mimic_raw, eicu_raw)
gc(verbose = FALSE)

# ------------------------------------------------------------------------------
# 8. Structural, value, and internal-consistency checks
# ------------------------------------------------------------------------------
check_unique_key(mimic_full, "stay_id", "MIMIC")
check_unique_key(eicu_full, "patientunitstayid", "eICU")

binary_vars <- c(
  "eligible_at_60h_flag", "hospital_death_after_60h",
  "primary_analysis_flag", "female_flag", "ckd_flag",
  "heart_failure_flag", "diabetes_flag", "chronic_liver_disease_flag",
  "malignancy_flag", "aecopd_analysis_flag",
  "pre_icu_reference_available_flag", "surrogate_reference_0_24h_flag",
  "reference_scr_missing_flag", "sensitivity_narrow_window_flag",
  "sensitivity_pre_icu_reference_flag", "sensitivity_onset_ge12h_flag",
  "sensitivity_strict_no_aki_flag", "imv_any_0_24h", "vasopressor_any_0_24h"
)

for (v in binary_vars) {
  check_allowed_values(mimic_full, v, c(0L, 1L), "MIMIC")
  check_allowed_values(eicu_full, v, c(0L, 1L), "eICU")
}

three_level_codes <- c(
  "primary_exposure_code",
  "sensitivity_narrow_exposure_code",
  "sensitivity_pre_icu_reference_exposure_code",
  "sensitivity_strict_no_aki_exposure_code",
  "sensitivity_onset_ge12h_exposure_code"
)

for (v in three_level_codes) {
  check_allowed_values(mimic_full, v, 0:2, "MIMIC")
  check_allowed_values(eicu_full, v, 0:2, "eICU")
}

allowed_reference_sources <- c("pre_icu_7d_latest", "icu_0_24h_min", "missing")
for (d in list(MIMIC = mimic_full, eICU = eicu_full)) {
  observed <- sort(unique(d$reference_scr_source[!is.na(d$reference_scr_source)]))
  invalid <- setdiff(observed, allowed_reference_sources)
  if (length(invalid) > 0L) {
    stop("Invalid reference_scr_source value(s): ", paste(invalid, collapse = ", "))
  }
}

# All rows in these source files should already satisfy the +60-h landmark.
if (any(mimic_full$eligible_at_60h_flag != 1L, na.rm = TRUE) ||
    any(is.na(mimic_full$eligible_at_60h_flag))) {
  stop("MIMIC: source file contains rows not eligible at the +60-h landmark.")
}
if (any(eicu_full$eligible_at_60h_flag != 1L, na.rm = TRUE) ||
    any(is.na(eicu_full$eligible_at_60h_flag))) {
  stop("eICU: source file contains rows not eligible at the +60-h landmark.")
}

# Fixed-reference source and missingness must agree.
check_reference_consistency <- function(data, database) {
  bad_missing <- data$reference_scr_source == "missing" &
    (!is.na(data$reference_scr) | data$reference_scr_missing_flag != 1L)

  bad_available <- data$reference_scr_source %in% c("pre_icu_7d_latest", "icu_0_24h_min") &
    (is.na(data$reference_scr) | data$reference_scr_missing_flag != 0L)

  if (any(bad_missing, na.rm = TRUE) || any(bad_available, na.rm = TRUE)) {
    stop(database, ": inconsistency between reference_scr_source, reference_scr, and missing flag.")
  }
}

check_reference_consistency(mimic_full, "MIMIC")
check_reference_consistency(eicu_full, "eICU")

# Primary-analysis flag should be equivalent to having one of the 3 primary
# exposure codes in the frozen Stage 2 export.
check_primary_flag_consistency <- function(data, database) {
  expected_flag <- as.integer(!is.na(data$primary_exposure_code) &
                                data$primary_exposure_code %in% 0:2)
  mismatch_n <- sum(data$primary_analysis_flag != expected_flag, na.rm = TRUE)

  if (mismatch_n > 0L) {
    stop(database, ": primary_analysis_flag mismatch in ", mismatch_n, " row(s).")
  }
}

check_primary_flag_consistency(mimic_full, "MIMIC")
check_primary_flag_consistency(eicu_full, "eICU")

if (any(!is.na(mimic_full$age) & (mimic_full$age < 18 | mimic_full$age > 120))) {
  stop("MIMIC: age outside 18-120 years.")
}
if (any(!is.na(eicu_full$age) & (eicu_full$age < 18 | eicu_full$age > 120))) {
  stop("eICU: age outside 18-120 years.")
}

for (nm in c("MIMIC", "eICU")) {
  d <- if (nm == "MIMIC") mimic_full else eicu_full
  bad_sofa <- !is.na(d$standard_nonrenal_sofa_24h) &
    (d$standard_nonrenal_sofa_24h < 0 | d$standard_nonrenal_sofa_24h > 20)
  if (any(bad_sofa)) {
    stop(nm, ": standard_nonrenal_sofa_24h outside the expected 0-20 range.")
  }
}

# ------------------------------------------------------------------------------
# 9. Create primary-analysis datasets
# ------------------------------------------------------------------------------
mimic_primary <- mimic_full %>%
  filter(primary_analysis_flag == 1L) %>%
  droplevels()

eicu_primary <- eicu_full %>%
  filter(primary_analysis_flag == 1L) %>%
  droplevels()

primary_model_variables <- c(
  "hospital_death_after_60h", "primary_exposure_code", "age",
  "female_flag", "ckd_flag", "heart_failure_flag", "diabetes_flag",
  "chronic_liver_disease_flag", "malignancy_flag",
  "standard_nonrenal_sofa_24h"
)

additional_adjustment_variables <- c(
  "imv_any_0_24h", "vasopressor_any_0_24h"
)

descriptive_common_variables <- c(
  "charlson_comorbidity_index", "aecopd_analysis_flag",
  "imv_any_0_24h", "vasopressor_any_0_24h"
)

model_missingness <- dplyr::bind_rows(
  tibble(
    database = "MIMIC-IV",
    variable = primary_model_variables,
    missing_n = vapply(
      mimic_primary[primary_model_variables],
      function(x) sum(is.na(x)), integer(1)
    ),
    missing_pct = 100 * missing_n / nrow(mimic_primary)
  ),
  tibble(
    database = "eICU-CRD",
    variable = primary_model_variables,
    missing_n = vapply(
      eicu_primary[primary_model_variables],
      function(x) sum(is.na(x)), integer(1)
    ),
    missing_pct = 100 * missing_n / nrow(eicu_primary)
  )
)

if (any(model_missingness$missing_n > 0L)) {
  warning(
    "Missing values exist in primary-model variables. Review QC_model_missingness.csv before modeling."
  )
}

descriptive_missingness <- dplyr::bind_rows(
  tibble(
    database = "MIMIC-IV",
    variable = descriptive_common_variables,
    missing_n = vapply(
      mimic_primary[descriptive_common_variables],
      function(x) sum(is.na(x)), integer(1)
    ),
    missing_pct = 100 * missing_n / nrow(mimic_primary)
  ),
  tibble(
    database = "eICU-CRD",
    variable = descriptive_common_variables,
    missing_n = vapply(
      eicu_primary[descriptive_common_variables],
      function(x) sum(is.na(x)), integer(1)
    ),
    missing_pct = 100 * missing_n / nrow(eicu_primary)
  )
)

# ------------------------------------------------------------------------------
# 10. Verify the frozen revised cohort counts
# ------------------------------------------------------------------------------
expected_checks <- tibble::tribble(
  ~database, ~measure, ~actual, ~expected,

  # MIMIC-IV revised fixed-reference cohort
  "MIMIC-IV", "full_n", nrow(mimic_full), 7794,
  "MIMIC-IV", "primary_n", nrow(mimic_primary), 7627,
  "MIMIC-IV", "primary_deaths",
    sum(mimic_primary$hospital_death_after_60h == 1L, na.rm = TRUE), 832,
  "MIMIC-IV", "no_early_aki_n",
    sum(mimic_primary$primary_exposure_code == 0L, na.rm = TRUE), 5796,
  "MIMIC-IV", "resolved_n",
    sum(mimic_primary$primary_exposure_code == 1L, na.rm = TRUE), 898,
  "MIMIC-IV", "ongoing_n",
    sum(mimic_primary$primary_exposure_code == 2L, na.rm = TRUE), 933,
  "MIMIC-IV", "reference_pre_icu_n",
    sum(mimic_full$reference_scr_source == "pre_icu_7d_latest", na.rm = TRUE), 2316,
  "MIMIC-IV", "reference_surrogate_0_24h_n",
    sum(mimic_full$reference_scr_source == "icu_0_24h_min", na.rm = TRUE), 5398,
  "MIMIC-IV", "reference_missing_n",
    sum(mimic_full$reference_scr_source == "missing", na.rm = TRUE), 80,
  "MIMIC-IV", "sensitivity_narrow_n",
    sum(mimic_full$sensitivity_narrow_window_flag == 1L, na.rm = TRUE), 7113,
  "MIMIC-IV", "sensitivity_pre_icu_reference_n",
    sum(mimic_full$sensitivity_pre_icu_reference_flag == 1L, na.rm = TRUE), 2288,

  # eICU-CRD revised fixed-reference cohort
  "eICU-CRD", "full_n", nrow(eicu_full), 8495,
  "eICU-CRD", "primary_n", nrow(eicu_primary), 7738,
  "eICU-CRD", "primary_deaths",
    sum(eicu_primary$hospital_death_after_60h == 1L, na.rm = TRUE), 709,
  "eICU-CRD", "no_early_aki_n",
    sum(eicu_primary$primary_exposure_code == 0L, na.rm = TRUE), 6644,
  "eICU-CRD", "resolved_n",
    sum(eicu_primary$primary_exposure_code == 1L, na.rm = TRUE), 465,
  "eICU-CRD", "ongoing_n",
    sum(eicu_primary$primary_exposure_code == 2L, na.rm = TRUE), 629,
  "eICU-CRD", "reference_pre_icu_n",
    sum(eicu_full$reference_scr_source == "pre_icu_7d_latest", na.rm = TRUE), 3508,
  "eICU-CRD", "reference_surrogate_0_24h_n",
    sum(eicu_full$reference_scr_source == "icu_0_24h_min", na.rm = TRUE), 4496,
  "eICU-CRD", "reference_missing_n",
    sum(eicu_full$reference_scr_source == "missing", na.rm = TRUE), 491,
  "eICU-CRD", "sensitivity_narrow_n",
    sum(eicu_full$sensitivity_narrow_window_flag == 1L, na.rm = TRUE), 7143,
  "eICU-CRD", "sensitivity_pre_icu_reference_n",
    sum(eicu_full$sensitivity_pre_icu_reference_flag == 1L, na.rm = TRUE), 3293
) %>%
  mutate(pass = actual == expected)


# Save the numerical audit *before* stopping on a mismatch.
readr::write_excel_csv(
  expected_checks,
  file.path(OUTPUT_DIR, "QC_expected_counts.csv"),
  na = ""
)

for (i in seq_len(nrow(expected_checks))) {
  compare_expected(
    actual = expected_checks$actual[i],
    expected = expected_checks$expected[i],
    label = paste(expected_checks$database[i], expected_checks$measure[i])
  )
}

# ------------------------------------------------------------------------------
# 11. QC summaries for later scripts
# ------------------------------------------------------------------------------
cohort_summary_one <- function(full, primary, database) {
  tibble(
    database = database,
    landmark_cohort_n = nrow(full),
    primary_n = nrow(primary),
    unclassifiable_n = nrow(full) - nrow(primary),
    primary_deaths_n = sum(primary$hospital_death_after_60h == 1L, na.rm = TRUE),
    primary_mortality_pct = 100 * mean(
      primary$hospital_death_after_60h == 1L,
      na.rm = TRUE
    ),
    no_early_aki_n = sum(primary$primary_exposure_code == 0L, na.rm = TRUE),
    resolved_n = sum(primary$primary_exposure_code == 1L, na.rm = TRUE),
    ongoing_n = sum(primary$primary_exposure_code == 2L, na.rm = TRUE),
    aecopd_primary_n = sum(primary$aecopd_analysis_flag == 1L, na.rm = TRUE),
    age_min = safe_min(primary$age),
    age_max = safe_max(primary$age),
    nonrenal_sofa_min = safe_min(primary$standard_nonrenal_sofa_24h),
    nonrenal_sofa_max = safe_max(primary$standard_nonrenal_sofa_24h),
    cci_missing_n = sum(is.na(primary$charlson_comorbidity_index))
  )
}

cohort_summary <- bind_rows(
  cohort_summary_one(mimic_full, mimic_primary, "MIMIC-IV"),
  cohort_summary_one(eicu_full, eicu_primary, "eICU-CRD")
)

exposure_outcome_summary <- bind_rows(
  mimic_primary %>%
    group_by(database, primary_exposure_code, primary_exposure) %>%
    summarise(
      n = n(),
      deaths = sum(hospital_death_after_60h == 1L, na.rm = TRUE),
      mortality_pct = 100 * deaths / n,
      .groups = "drop"
    ),
  eicu_primary %>%
    group_by(database, primary_exposure_code, primary_exposure) %>%
    summarise(
      n = n(),
      deaths = sum(hospital_death_after_60h == 1L, na.rm = TRUE),
      mortality_pct = 100 * deaths / n,
      .groups = "drop"
    )
)

reference_summary <- bind_rows(
  mimic_full %>%
    count(database, reference_scr_source, reference_source, name = "n") %>%
    mutate(pct = 100 * n / nrow(mimic_full)),
  eicu_full %>%
    count(database, reference_scr_source, reference_source, name = "n") %>%
    mutate(pct = 100 * n / nrow(eicu_full))
)

sensitivity_summary <- bind_rows(
  tibble(
    database = "MIMIC-IV",
    analysis = c(
      "Primary 36-60 h window",
      "Narrow 42-54 h window",
      "Measured pre-ICU reference only",
      "Onset-to-assessment >=12 h",
      "Strict no-AKI through 60 h"
    ),
    n = c(
      sum(mimic_full$primary_analysis_flag == 1L, na.rm = TRUE),
      sum(mimic_full$sensitivity_narrow_window_flag == 1L, na.rm = TRUE),
      sum(mimic_full$sensitivity_pre_icu_reference_flag == 1L, na.rm = TRUE),
      sum(mimic_full$sensitivity_onset_ge12h_flag == 1L, na.rm = TRUE),
      sum(mimic_full$sensitivity_strict_no_aki_flag == 1L, na.rm = TRUE)
    )
  ),
  tibble(
    database = "eICU-CRD",
    analysis = c(
      "Primary 36-60 h window",
      "Narrow 42-54 h window",
      "Measured pre-ICU reference only",
      "Onset-to-assessment >=12 h",
      "Strict no-AKI through 60 h"
    ),
    n = c(
      sum(eicu_full$primary_analysis_flag == 1L, na.rm = TRUE),
      sum(eicu_full$sensitivity_narrow_window_flag == 1L, na.rm = TRUE),
      sum(eicu_full$sensitivity_pre_icu_reference_flag == 1L, na.rm = TRUE),
      sum(eicu_full$sensitivity_onset_ge12h_flag == 1L, na.rm = TRUE),
      sum(eicu_full$sensitivity_strict_no_aki_flag == 1L, na.rm = TRUE)
    )
  )
)

organ_support_summary <- bind_rows(
  tibble(
    database = "MIMIC-IV",
    variable = c("imv_any_0_24h", "vasopressor_any_0_24h"),
    n = c(
      sum(mimic_full$imv_any_0_24h == 1L, na.rm = TRUE),
      sum(mimic_full$vasopressor_any_0_24h == 1L, na.rm = TRUE)
    ),
    denominator = nrow(mimic_full)
  ),
  tibble(
    database = "eICU-CRD",
    variable = c("imv_any_0_24h", "vasopressor_any_0_24h"),
    n = c(
      sum(eicu_full$imv_any_0_24h == 1L, na.rm = TRUE),
      sum(eicu_full$vasopressor_any_0_24h == 1L, na.rm = TRUE)
    ),
    denominator = nrow(eicu_full)
  )
) %>%
  mutate(pct = 100 * n / denominator)

variable_class_report <- bind_rows(
  tibble(
    database = "MIMIC-IV",
    variable = names(mimic_full),
    class = vapply(
      mimic_full,
      function(x) paste(class(x), collapse = "/"),
      character(1)
    ),
    missing_n = vapply(mimic_full, function(x) sum(is.na(x)), integer(1)),
    unique_n = vapply(
      mimic_full,
      function(x) length(unique(x[!is.na(x)])),
      integer(1)
    )
  ),
  tibble(
    database = "eICU-CRD",
    variable = names(eicu_full),
    class = vapply(
      eicu_full,
      function(x) paste(class(x), collapse = "/"),
      character(1)
    ),
    missing_n = vapply(eicu_full, function(x) sum(is.na(x)), integer(1)),
    unique_n = vapply(
      eicu_full,
      function(x) length(unique(x[!is.na(x)])),
      integer(1)
    )
  )
)

# ------------------------------------------------------------------------------
# 12. Save clean datasets and QC reports
# ------------------------------------------------------------------------------
saveRDS(mimic_full, file.path(OUTPUT_DIR, "mimic_full_clean.rds"), compress = "xz")
saveRDS(eicu_full, file.path(OUTPUT_DIR, "eicu_full_clean.rds"), compress = "xz")
saveRDS(mimic_primary, file.path(OUTPUT_DIR, "mimic_primary_clean.rds"), compress = "xz")
saveRDS(eicu_primary, file.path(OUTPUT_DIR, "eicu_primary_clean.rds"), compress = "xz")

# UTF-8 BOM CSVs open correctly in Chinese Windows Excel.
readr::write_excel_csv(
  mimic_primary,
  file.path(OUTPUT_DIR, "mimic_primary_clean.csv"),
  na = ""
)
readr::write_excel_csv(
  eicu_primary,
  file.path(OUTPUT_DIR, "eicu_primary_clean.csv"),
  na = ""
)

readr::write_excel_csv(cohort_summary, file.path(OUTPUT_DIR, "QC_cohort_summary.csv"), na = "")
readr::write_excel_csv(exposure_outcome_summary, file.path(OUTPUT_DIR, "QC_exposure_outcome_summary.csv"), na = "")
readr::write_excel_csv(reference_summary, file.path(OUTPUT_DIR, "QC_reference_summary.csv"), na = "")
readr::write_excel_csv(sensitivity_summary, file.path(OUTPUT_DIR, "QC_sensitivity_counts.csv"), na = "")
readr::write_excel_csv(organ_support_summary, file.path(OUTPUT_DIR, "QC_organ_support_summary.csv"), na = "")
readr::write_excel_csv(model_missingness, file.path(OUTPUT_DIR, "QC_model_missingness.csv"), na = "")
readr::write_excel_csv(descriptive_missingness, file.path(OUTPUT_DIR, "QC_descriptive_missingness.csv"), na = "")
readr::write_excel_csv(expected_checks, file.path(OUTPUT_DIR, "QC_expected_counts.csv"), na = "")
readr::write_excel_csv(variable_class_report, file.path(OUTPUT_DIR, "QC_variable_classes.csv"), na = "")
readr::write_excel_csv(conversion_issue_log, file.path(OUTPUT_DIR, "QC_numeric_conversion_issues.csv"), na = "")
readr::write_excel_csv(module_merge_log, file.path(OUTPUT_DIR, "QC_sql_module_merges.csv"), na = "")

manifest <- tibble(
  item = c(
    "analysis_timestamp",
    "analysis_version",
    "input_mode",
    "project_root",
    "mimic_input",
    "eicu_input",
    "mimic_rows_landmark_cohort",
    "eicu_rows_landmark_cohort",
    "mimic_rows_primary",
    "eicu_rows_primary",
    "numeric_conversion_issue_rows",
    "primary_adjustment_variables",
    "additional_adjustment_variables",
    "descriptive_common_variables"
  ),
  value = c(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "Revised fixed-reference creatinine-defined AKI trajectory; public SQL-module import",
    INPUT_MODE,
    PROJECT_ROOT,
    mimic_path,
    eicu_path,
    as.character(nrow(mimic_full)),
    as.character(nrow(eicu_full)),
    as.character(nrow(mimic_primary)),
    as.character(nrow(eicu_primary)),
    as.character(nrow(conversion_issue_log)),
    paste(
      c(
        "age", "female_flag", "ckd_flag", "heart_failure_flag",
        "diabetes_flag", "chronic_liver_disease_flag", "malignancy_flag",
        "standard_nonrenal_sofa_24h"
      ),
      collapse = ", "
    ),
    paste(additional_adjustment_variables, collapse = ", "),
    paste(descriptive_common_variables, collapse = ", ")
  )
)

readr::write_excel_csv(
  manifest,
  file.path(OUTPUT_DIR, "analysis_manifest.csv"),
  na = ""
)

# ------------------------------------------------------------------------------
# 13. Console report
# ------------------------------------------------------------------------------
cat("\n============================================================\n")
cat("Revised fixed-reference import, harmonization, and QC completed.\n")
cat("Input mode: ", INPUT_MODE, "\n", sep = "")
cat("Output directory:\n", normalizePath(OUTPUT_DIR, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("============================================================\n\n")

cat("Cohort summary:\n")
print(cohort_summary)

cat("\nPrimary exposure and post-60-h mortality summary:\n")
print(exposure_outcome_summary)

cat("\nReference sCr source summary:\n")
print(reference_summary)

cat("\nSensitivity-analysis sample sizes:\n")
print(sensitivity_summary)

cat("\nPrimary-model missingness:\n")
print(model_missingness)

cat("\nDescriptive/common-variable missingness:\n")
print(descriptive_missingness)

if (nrow(conversion_issue_log) == 0L) {
  cat("\nNumeric conversion: no malformed non-empty values detected.\n")
} else {
  cat(
    "\nNumeric conversion warning: ", nrow(conversion_issue_log),
    " distinct malformed value records were written to:\n",
    file.path(OUTPUT_DIR, "QC_numeric_conversion_issues.csv"), "\n",
    sep = ""
  )
}

if (all(expected_checks$pass)) {
  cat("Expected revised cohort-count checks: all passed.\n")
} else {
  cat("Expected cohort-count checks: one or more mismatches; review QC_expected_counts.csv.\n")
}

cat("\nFiles for subsequent analyses:\n")
cat("  - mimic_primary_clean.rds\n")
cat("  - eicu_primary_clean.rds\n")
cat("  - mimic_full_clean.rds\n")
cat("  - eicu_full_clean.rds\n")
cat("  - QC_expected_counts.csv\n")
cat("  - QC_model_missingness.csv\n")
cat("  - QC_reference_summary.csv\n")
cat("  - QC_sensitivity_counts.csv\n")
cat("  - QC_sql_module_merges.csv\n")
cat("Use the RDS files, rather than re-reading Excel, in later Table 1 and model scripts.\n")
