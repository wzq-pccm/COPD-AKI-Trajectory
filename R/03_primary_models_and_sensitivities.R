# ==============================================================================
# COPD early creatinine-defined AKI trajectory study
# R/03_primary_models_and_sensitivities.R
# MIMIC-IV v3.1 and eICU-CRD v2.0 | Main regressions and Supplementary S1-S3
#
# Reproducibility edition of the original author's:
#   03_models_and_sensitivity_revised_fixed_reference_v2.R
#
# Statistical algorithms retained from the author-validated script:
#   - primary fixed-reference 36-60-h early AKI trajectory classification;
#   - four prespecified sensitivity analyses (42-54-h window, measured pre-ICU
#     reference, AECOPD-only, additional IMV/vasopressor adjustment);
#   - within-early-AKI ongoing versus resolved model additionally adjusted for
#     peak creatinine-defined AKI stage (stage 2-3 versus stage 1);
#   - primary covariates: age, female sex, CKD, heart failure, diabetes,
#     chronic liver disease, malignancy, first-day nonrenal SOFA;
#   - MIMIC-IV: model-based SE; eICU-CRD: hospital-clustered robust HC1 SE;
#   - CCI is descriptive only; never a regression covariate.
#
# Inputs (from R/01_import_harmonize_qc.R):
#   output/R_analysis_output/mimic_full_clean.rds
#   output/R_analysis_output/eicu_full_clean.rds
#
# Run from the repository root:
#   Rscript R/03_primary_models_and_sensitivities.R
# Or set COPD_AKI_PROJECT_ROOT to the absolute project root.
#
# Outputs (aggregate CSV/XLSX, QC, and LOCAL model RDS):
#   output/R_analysis_output/03_primary_models_and_sensitivities/
#   TableS1_PanelA_sample_composition.csv
#   TableS1_PanelB_reference_source.csv
#   TableS2_adjusted_associations.csv
#   TableS3_PanelA_peak_stage_mortality.csv
#   TableS3_PanelB_AKI_only_adjusted.csv
#   published_S2_estimate_QC.csv
#   plus original author's full coefficient/model-QC/forest-data outputs.
#
# NOTE: model-object RDS files can embed individual-level model frames.
# Keep output/ and data/ local and excluded from Git. Publish this code only;
# share aggregate results only after privacy/disclosure review.
# No automatic package installation and no local workstation absolute paths.
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)

# ------------------------------------------------------------------------------
# 0. Portable user settings
# ------------------------------------------------------------------------------

root_override <- Sys.getenv("COPD_AKI_PROJECT_ROOT", unset = "")
PROJECT_ROOT <- normalizePath(
  if (nzchar(root_override)) root_override else ".",
  winslash = "/", mustWork = TRUE
)
INPUT_DIR <- file.path(PROJECT_ROOT, "output", "R_analysis_output")
OUTPUT_DIR <- file.path(INPUT_DIR, "03_primary_models_and_sensitivities")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

MIMIC_RDS <- file.path(INPUT_DIR, "mimic_full_clean.rds")
EICU_RDS  <- file.path(INPUT_DIR, "eicu_full_clean.rds")

# QC compares the independent study extract against the manuscript's frozen
# cohort counts; it does not alter / select the data to force a match.
STOP_ON_COUNT_MISMATCH <- TRUE
RUN_AECOPD_SENSITIVITY <- TRUE
RUN_AKI_ONLY_SUPPLEMENT <- TRUE

# ------------------------------------------------------------------------------
# 1. Required packages (no installation side effects)
# ------------------------------------------------------------------------------

required_packages <- c("dplyr", "readr", "tibble", "sandwich", "openxlsx")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required R packages: ", paste(missing_packages, collapse = ", "),
    ". Install them in your R environment or use renv::restore()."
  )
}
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(sandwich)
  library(openxlsx)
})

# ------------------------------------------------------------------------------
# 2. Analysis specifications and frozen QC counts
# ------------------------------------------------------------------------------

exposure_levels <- c(
  "No early AKI",
  "Resolved early AKI",
  "Ongoing early AKI"
)

core_analysis_specs <- tibble::tribble(
  ~Analysis_ID,                  ~Analysis_label,                                             ~Analysis_role,       ~Reference_definition,
  "Primary_36_60h",             "Primary analysis: recovery assessment during ICU hours 36-60", "Primary",          "No early AKI during ICU hours 0-48",
  "Narrow_42_54h",              "Sensitivity: recovery assessment during ICU hours 42-54",      "Core sensitivity", "No early AKI during ICU hours 0-48",
  "PreICU_reference_only",       "Sensitivity: measured pre-ICU reference sCr only",             "Core sensitivity", "No early AKI during ICU hours 0-48",
  "AECOPD_restricted",           "Sensitivity: AECOPD-restricted primary cohort",                 "Prespecified sensitivity", "No early AKI during ICU hours 0-48"
)

# Frozen counts from the current fixed-reference datasets.
expected_counts <- tibble::tribble(
  ~Database,  ~Analysis_ID,             ~N_expected, ~Deaths_expected, ~No_AKI_expected, ~Resolved_expected, ~Ongoing_expected,
  "MIMIC-IV", "Primary_36_60h",         7627L,       832L,             5796L,            898L,               933L,
  "MIMIC-IV", "Narrow_42_54h",          7113L,       762L,             5796L,            605L,               712L,
  "MIMIC-IV", "PreICU_reference_only",  2288L,       263L,             1779L,            189L,               320L,
  "MIMIC-IV", "AECOPD_restricted",      1492L,       188L,             1142L,            191L,               159L,
  "eICU-CRD", "Primary_36_60h",         7738L,       709L,             6644L,            465L,               629L,
  "eICU-CRD", "Narrow_42_54h",          7143L,       619L,             6644L,            212L,               287L,
  "eICU-CRD", "PreICU_reference_only",  3293L,       359L,             2765L,            166L,               362L,
  "eICU-CRD", "AECOPD_restricted",      2946L,       243L,             2568L,            157L,               221L
)

expected_aki_only <- tibble::tribble(
  ~Database,  ~N_expected, ~Deaths_expected, ~Resolved_expected, ~Ongoing_expected, ~Peak_stage_2_3_expected,
  "MIMIC-IV", 1831L,       349L,             898L,               933L,              351L,
  "eICU-CRD", 1094L,       195L,             465L,               629L,              221L
)

# ------------------------------------------------------------------------------
# 3. General helpers
# ------------------------------------------------------------------------------

stop_if_missing_file <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Required input file not found:\n",
      path,
      "\nRun revised Script 01 first."
    )
  }
}

assert_required_columns <- function(data, columns, database) {
  missing_columns <- setdiff(columns, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      database,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
}

check_binary <- function(x, variable, database) {
  observed <- sort(unique(x[!is.na(x)]))
  invalid <- setdiff(observed, c(0L, 1L))

  if (length(invalid) > 0L) {
    stop(
      database,
      ": invalid values in '",
      variable,
      "': ",
      paste(invalid, collapse = ", ")
    )
  }
}

check_three_level_code <- function(x, variable, database) {
  observed <- sort(unique(x[!is.na(x)]))
  invalid <- setdiff(observed, 0:2)

  if (length(invalid) > 0L) {
    stop(
      database,
      ": invalid values in '",
      variable,
      "': ",
      paste(invalid, collapse = ", ")
    )
  }
}

format_p <- function(p) {
  if (length(p) == 0L || is.na(p) || !is.finite(p)) return("")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

wilson_ci <- function(events, n, conf_level = 0.95) {
  if (is.na(n) || n <= 0L) return(c(NA_real_, NA_real_))

  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  p <- events / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt(
    (p * (1 - p) / n) +
      z^2 / (4 * n^2)
  ) / denom

  c(
    max(0, centre - half),
    min(1, centre + half)
  )
}

write_csv_bom <- function(x, filename) {
  readr::write_excel_csv(
    x,
    file.path(OUTPUT_DIR, filename),
    na = ""
  )
}

check_expected_value <- function(actual, expected, label) {
  pass <- isTRUE(
    as.integer(actual) == as.integer(expected)
  )

  if (!pass) {
    msg <- paste0(
      label,
      ": expected ",
      expected,
      ", observed ",
      actual
    )

    if (STOP_ON_COUNT_MISMATCH) {
      stop(msg)
    } else {
      warning(msg)
    }
  }

  pass
}

# ------------------------------------------------------------------------------
# 4. Prepare and validate full clean datasets
# ------------------------------------------------------------------------------

prepare_full_data <- function(data, database) {

  required <- c(
    "database",
    "patient_id",
    "hospital_stay_id",
    "icu_stay_id",
    "hospital_id",

    "primary_analysis_flag",
    "primary_exposure_code",

    "sensitivity_narrow_window_flag",
    "sensitivity_narrow_exposure_code",

    "sensitivity_pre_icu_reference_flag",
    "sensitivity_pre_icu_reference_exposure_code",

    "aki_only_analysis_flag",
    "peak_aki_stage_0_48h",

    "aecopd_analysis_flag",
    "hospital_death_after_60h",

    "age",
    "female_flag",
    "ckd_flag",
    "heart_failure_flag",
    "diabetes_flag",
    "chronic_liver_disease_flag",
    "malignancy_flag",
    "standard_nonrenal_sofa_24h",

    "imv_any_0_24h",
    "vasopressor_any_0_24h"
  )

  assert_required_columns(
    data,
    required,
    database
  )

  binary_vars <- c(
    "primary_analysis_flag",
    "sensitivity_narrow_window_flag",
    "sensitivity_pre_icu_reference_flag",
    "aki_only_analysis_flag",
    "aecopd_analysis_flag",
    "hospital_death_after_60h",
    "female_flag",
    "ckd_flag",
    "heart_failure_flag",
    "diabetes_flag",
    "chronic_liver_disease_flag",
    "malignancy_flag",
    "imv_any_0_24h",
    "vasopressor_any_0_24h"
  )

  for (v in binary_vars) {
    check_binary(
      data[[v]],
      v,
      database
    )
  }

  code_vars <- c(
    "primary_exposure_code",
    "sensitivity_narrow_exposure_code",
    "sensitivity_pre_icu_reference_exposure_code"
  )

  for (v in code_vars) {
    check_three_level_code(
      data[[v]],
      v,
      database
    )
  }

  if (anyDuplicated(as.character(data$icu_stay_id)) > 0L) {
    stop(
      database,
      ": duplicated icu_stay_id detected in full clean data."
    )
  }

  if (any(
    !is.na(data$standard_nonrenal_sofa_24h) &
      (
        data$standard_nonrenal_sofa_24h < 0 |
          data$standard_nonrenal_sofa_24h > 20
      )
  )) {
    stop(
      database,
      ": standard_nonrenal_sofa_24h outside 0-20."
    )
  }

  data %>%
    mutate(
      database = database,
      patient_id = as.character(patient_id),
      hospital_stay_id = as.character(hospital_stay_id),
      icu_stay_id = as.character(icu_stay_id),
      hospital_id = as.character(hospital_id)
    )
}

# ------------------------------------------------------------------------------
# 5. Construct analysis populations
# ------------------------------------------------------------------------------

make_core_analysis_data <- function(data, database, analysis_id) {

  if (analysis_id == "Primary_36_60h") {

    out <- data %>%
      filter(
        primary_analysis_flag == 1L,
        primary_exposure_code %in% 0:2
      ) %>%
      mutate(
        analysis_exposure_code = primary_exposure_code
      )

  } else if (analysis_id == "Narrow_42_54h") {

    out <- data %>%
      filter(
        sensitivity_narrow_window_flag == 1L,
        sensitivity_narrow_exposure_code %in% 0:2
      ) %>%
      mutate(
        analysis_exposure_code =
          sensitivity_narrow_exposure_code
      )

  } else if (analysis_id == "PreICU_reference_only") {

    out <- data %>%
      filter(
        sensitivity_pre_icu_reference_flag == 1L,
        sensitivity_pre_icu_reference_exposure_code %in% 0:2
      ) %>%
      mutate(
        analysis_exposure_code =
          sensitivity_pre_icu_reference_exposure_code
      )

  } else if (analysis_id == "AECOPD_restricted") {

    out <- data %>%
      filter(
        primary_analysis_flag == 1L,
        aecopd_analysis_flag == 1L,
        primary_exposure_code %in% 0:2
      ) %>%
      mutate(
        analysis_exposure_code =
          primary_exposure_code
      )

  } else {

    stop(
      "Unknown analysis ID: ",
      analysis_id
    )
  }

  spec <- core_analysis_specs %>%
    filter(
      Analysis_ID == analysis_id
    )

  if (nrow(spec) != 1L) {
    stop(
      "Analysis specification not found: ",
      analysis_id
    )
  }

  out <- out %>%
    mutate(
      Database = database,
      Analysis_ID = analysis_id,
      Analysis_label =
        spec$Analysis_label[[1]],
      Analysis_role =
        spec$Analysis_role[[1]],
      Reference_definition =
        spec$Reference_definition[[1]],
      analysis_exposure = factor(
        analysis_exposure_code,
        levels = c(0L, 1L, 2L),
        labels = exposure_levels
      )
    )

  if (any(is.na(out$analysis_exposure))) {
    stop(
      database,
      " | ",
      analysis_id,
      ": missing analysis exposure after filtering."
    )
  }

  if (nlevels(droplevels(out$analysis_exposure)) != 3L) {
    stop(
      database,
      " | ",
      analysis_id,
      ": all three trajectory groups are required."
    )
  }

  model_vars <- c(
    "hospital_death_after_60h",
    "analysis_exposure",
    "age",
    "female_flag",
    "ckd_flag",
    "heart_failure_flag",
    "diabetes_flag",
    "chronic_liver_disease_flag",
    "malignancy_flag",
    "standard_nonrenal_sofa_24h"
  )

  missing_n <- vapply(
    out[model_vars],
    function(x) sum(is.na(x)),
    integer(1)
  )

  if (any(missing_n > 0L)) {
    stop(
      database,
      " | ",
      analysis_id,
      ": missing primary-model values: ",
      paste(
        names(missing_n)[missing_n > 0L],
        missing_n[missing_n > 0L],
        sep = "=",
        collapse = ", "
      )
    )
  }

  out
}

make_aki_only_data <- function(data, database) {

  out <- data %>%
    filter(
      primary_analysis_flag == 1L,
      aki_only_analysis_flag == 1L,
      primary_exposure_code %in% c(1L, 2L),
      peak_aki_stage_0_48h %in% c(1L, 2L, 3L)
    ) %>%
    mutate(
      Database = database,
      Analysis_ID = "AKI_only_recovery",
      Analysis_label =
        "Supplementary: classified early-AKI patients only",

      recovery_status = factor(
        ifelse(
          primary_exposure_code == 1L,
          "Resolved early AKI",
          "Ongoing early AKI"
        ),
        levels = c(
          "Resolved early AKI",
          "Ongoing early AKI"
        )
      ),

      peak_stage_high = as.integer(
        peak_aki_stage_0_48h >= 2L
      ),

      peak_stage_category = factor(
        peak_stage_high,
        levels = c(0L, 1L),
        labels = c(
          "Peak stage 1",
          "Peak stage 2-3"
        )
      )
    )

  model_vars <- c(
    "hospital_death_after_60h",
    "recovery_status",
    "peak_stage_high",
    "age",
    "female_flag",
    "ckd_flag",
    "heart_failure_flag",
    "diabetes_flag",
    "chronic_liver_disease_flag",
    "malignancy_flag",
    "standard_nonrenal_sofa_24h"
  )

  missing_n <- vapply(
    out[model_vars],
    function(x) sum(is.na(x)),
    integer(1)
  )

  if (any(missing_n > 0L)) {
    stop(
      database,
      " | AKI-only: missing model values: ",
      paste(
        names(missing_n)[missing_n > 0L],
        missing_n[missing_n > 0L],
        sep = "=",
        collapse = ", "
      )
    )
  }

  if (nlevels(droplevels(out$recovery_status)) != 2L) {
    stop(
      database,
      ": both resolved and ongoing groups are required in AKI-only analysis."
    )
  }

  out
}

# ------------------------------------------------------------------------------
# 6. Regression formulas
# ------------------------------------------------------------------------------

core_unadjusted_formula <-
  hospital_death_after_60h ~
  analysis_exposure

core_adjusted_formula <-
  hospital_death_after_60h ~
  analysis_exposure +
  age +
  female_flag +
  ckd_flag +
  heart_failure_flag +
  diabetes_flag +
  chronic_liver_disease_flag +
  malignancy_flag +
  standard_nonrenal_sofa_24h

# Sensitivity only: IMV and vasopressor are not primary covariates.
core_organ_support_formula <-
  hospital_death_after_60h ~
  analysis_exposure +
  age +
  female_flag +
  ckd_flag +
  heart_failure_flag +
  diabetes_flag +
  chronic_liver_disease_flag +
  malignancy_flag +
  standard_nonrenal_sofa_24h +
  imv_any_0_24h +
  vasopressor_any_0_24h

aki_only_unadjusted_formula <-
  hospital_death_after_60h ~
  recovery_status

aki_only_adjusted_formula <-
  hospital_death_after_60h ~
  recovery_status +
  peak_stage_high +
  age +
  female_flag +
  ckd_flag +
  heart_failure_flag +
  diabetes_flag +
  chronic_liver_disease_flag +
  malignancy_flag +
  standard_nonrenal_sofa_24h

# ------------------------------------------------------------------------------
# 7. Logistic regression helpers
# ------------------------------------------------------------------------------

fit_logistic <- function(
  formula,
  data,
  database,
  analysis_label,
  model_name
) {

  captured_warnings <- character(0)

  fit <- withCallingHandlers(
    stats::glm(
      formula = formula,
      data = data,
      family = stats::binomial(
        link = "logit"
      ),
      na.action = stats::na.fail,
      model = TRUE,
      x = TRUE,
      y = TRUE
    ),
    warning = function(w) {
      captured_warnings <<- c(
        captured_warnings,
        conditionMessage(w)
      )
      invokeRestart(
        "muffleWarning"
      )
    }
  )

  attr(
    fit,
    "captured_warnings"
  ) <- unique(captured_warnings)

  if (!isTRUE(fit$converged)) {
    stop(
      database,
      " | ",
      analysis_label,
      " | ",
      model_name,
      ": model did not converge."
    )
  }

  fit
}

get_vcov <- function(
  fit,
  data,
  database,
  clustered = FALSE
) {

  if (!clustered) {
    return(
      stats::vcov(fit)
    )
  }

  cluster <- as.character(
    data$hospital_id
  )

  if (length(cluster) != stats::nobs(fit)) {
    stop(
      database,
      ": cluster vector length does not match model observations."
    )
  }

  if (any(is.na(cluster) | cluster == "")) {
    stop(
      database,
      ": missing hospital_id in clustered model."
    )
  }

  if (length(unique(cluster)) < 2L) {
    stop(
      database,
      ": fewer than two hospital clusters."
    )
  }

  sandwich::vcovCL(
    fit,
    cluster = cluster,
    type = "HC1"
  )
}

pretty_core_term <- function(
  term,
  reference_definition
) {

  if (term == "(Intercept)") {
    return("Intercept")
  }

  if (startsWith(
    term,
    "analysis_exposure"
  )) {
    level <- sub(
      "^analysis_exposure",
      "",
      term
    )

    return(
      paste0(
        level,
        " vs ",
        reference_definition
      )
    )
  }

  labels <- c(
    age =
      "Age, per 1 year",
    female_flag =
      "Female vs male",
    ckd_flag =
      "Chronic kidney disease: yes vs no",
    heart_failure_flag =
      "Heart failure: yes vs no",
    diabetes_flag =
      "Diabetes mellitus: yes vs no",
    chronic_liver_disease_flag =
      "Chronic liver disease: yes vs no",
    malignancy_flag =
      "Malignancy: yes vs no",
    standard_nonrenal_sofa_24h =
      "Nonrenal SOFA, per 1 point",
    imv_any_0_24h =
      "Invasive mechanical ventilation in ICU hours 0-24: yes vs no",
    vasopressor_any_0_24h =
      "Vasopressor use in ICU hours 0-24: yes vs no"
  )

  if (term %in% names(labels)) {
    return(
      unname(labels[term])
    )
  }

  term
}

pretty_aki_term <- function(term) {

  if (term == "(Intercept)") {
    return("Intercept")
  }

  if (term == "recovery_statusOngoing early AKI") {
    return(
      "Ongoing vs resolved early AKI"
    )
  }

  if (term == "peak_stage_high") {
    return(
      "Peak creatinine-defined AKI stage 2-3 vs stage 1"
    )
  }

  labels <- c(
    age =
      "Age, per 1 year",
    female_flag =
      "Female vs male",
    ckd_flag =
      "Chronic kidney disease: yes vs no",
    heart_failure_flag =
      "Heart failure: yes vs no",
    diabetes_flag =
      "Diabetes mellitus: yes vs no",
    chronic_liver_disease_flag =
      "Chronic liver disease: yes vs no",
    malignancy_flag =
      "Malignancy: yes vs no",
    standard_nonrenal_sofa_24h =
      "Nonrenal SOFA, per 1 point"
  )

  if (term %in% names(labels)) {
    return(
      unname(labels[term])
    )
  }

  term
}

tidy_logistic <- function(
  fit,
  vcov_matrix,
  database,
  analysis_id,
  analysis_label,
  analysis_role,
  reference_definition,
  model_name,
  se_method,
  analysis_type = c(
    "core",
    "aki_only"
  ),
  confidence = 0.95
) {

  analysis_type <- match.arg(
    analysis_type
  )

  beta <- stats::coef(fit)
  se <- sqrt(
    diag(vcov_matrix)
  )

  z_value <- beta / se
  p_value <- 2 *
    stats::pnorm(
      abs(z_value),
      lower.tail = FALSE
    )

  critical <- stats::qnorm(
    1 - (1 - confidence) / 2
  )

  if (analysis_type == "core") {

    term_labels <- vapply(
      names(beta),
      pretty_core_term,
      character(1),
      reference_definition =
        reference_definition
    )

    exposure_terms <- startsWith(
      names(beta),
      "analysis_exposure"
    )

  } else {

    term_labels <- vapply(
      names(beta),
      pretty_aki_term,
      character(1)
    )

    exposure_terms <-
      names(beta) ==
      "recovery_statusOngoing early AKI"
  }

  tibble(
    Database = database,
    Analysis_ID = analysis_id,
    Analysis_label = analysis_label,
    Analysis_role = analysis_role,
    Reference_definition =
      reference_definition,
    Model = model_name,
    SE_method = se_method,

    Term = names(beta),
    Term_label = term_labels,

    Term_category = case_when(
      names(beta) == "(Intercept)" ~
        "Intercept",
      exposure_terms ~
        "Exposure",
      TRUE ~
        "Covariate"
    ),

    Log_OR = as.numeric(beta),
    SE = as.numeric(se),
    Z_value = as.numeric(z_value),

    OR = exp(beta),
    CI_low =
      exp(beta - critical * se),
    CI_high =
      exp(beta + critical * se),

    OR_95CI = sprintf(
      "%.2f (%.2f-%.2f)",
      exp(beta),
      exp(beta - critical * se),
      exp(beta + critical * se)
    ),

    P_value_numeric =
      as.numeric(p_value),
    P_value =
      vapply(
        p_value,
        format_p,
        character(1)
      ),

    N = stats::nobs(fit),
    Events = sum(fit$y == 1L),
    Non_events = sum(fit$y == 0L)
  )
}

make_model_qc <- function(
  fit,
  data,
  database,
  analysis_id,
  analysis_label,
  model_name,
  clustered = FALSE
) {

  coefficients <-
    stats::coef(fit)

  conventional_se <-
    sqrt(
      diag(
        stats::vcov(fit)
      )
    )

  warnings_text <-
    attr(
      fit,
      "captured_warnings"
    )

  warnings_text <- if (
    length(warnings_text) == 0L
  ) {
    ""
  } else {
    paste(
      warnings_text,
      collapse = " | "
    )
  }

  cluster_sizes <- if (clustered) {
    table(data$hospital_id)
  } else {
    NULL
  }

  tibble(
    Database = database,
    Analysis_ID = analysis_id,
    Analysis_label = analysis_label,
    Model = model_name,

    Formula = paste(
      deparse(
        stats::formula(fit)
      ),
      collapse = " "
    ),

    N = stats::nobs(fit),
    Events = sum(fit$y == 1L),
    Non_events =
      sum(fit$y == 0L),

    Nonintercept_parameters =
      max(1L, fit$rank - 1L),

    Events_per_parameter =
      sum(fit$y == 1L) /
      max(1L, fit$rank - 1L),

    Converged =
      isTRUE(fit$converged),

    Iterations = fit$iter,
    AIC = stats::AIC(fit),

    Max_abs_coefficient =
      max(
        abs(coefficients),
        na.rm = TRUE
      ),

    Max_model_based_SE =
      max(
        conventional_se,
        na.rm = TRUE
      ),

    Potential_separation_flag =
      as.integer(
        any(
          !is.finite(coefficients)
        ) ||
          any(
            abs(coefficients) > 10,
            na.rm = TRUE
          ) ||
          any(
            conventional_se > 5,
            na.rm = TRUE
          )
      ),

    Captured_warnings =
      warnings_text,

    Hospital_clustered_SE =
      clustered,

    Hospital_clusters =
      if (clustered) {
        length(cluster_sizes)
      } else {
        1L
      },

    Minimum_cluster_size =
      if (clustered) {
        min(cluster_sizes)
      } else {
        NA_integer_
      },

    Median_cluster_size =
      if (clustered) {
        stats::median(
          as.numeric(
            cluster_sizes
          )
        )
      } else {
        NA_real_
      },

    Maximum_cluster_size =
      if (clustered) {
        max(cluster_sizes)
      } else {
        NA_integer_
      }
  )
}

# ------------------------------------------------------------------------------
# 8. Crude descriptive summaries
# ------------------------------------------------------------------------------

make_core_count_summary <- function(data) {

  data %>%
    group_by(
      Database,
      Analysis_ID,
      Analysis_label,
      Analysis_role,
      Reference_definition,
      analysis_exposure_code,
      analysis_exposure
    ) %>%
    summarise(
      N = n(),
      Deaths = sum(
        hospital_death_after_60h == 1L
      ),
      Live_discharges = sum(
        hospital_death_after_60h == 0L
      ),
      Mortality_percent =
        100 * Deaths / N,
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      Mortality_CI_low_percent =
        100 * wilson_ci(
          Deaths,
          N
        )[1],

      Mortality_CI_high_percent =
        100 * wilson_ci(
          Deaths,
          N
        )[2],

      Mortality_percent_95CI =
        sprintf(
          "%.2f%% (%.2f-%.2f)",
          Mortality_percent,
          Mortality_CI_low_percent,
          Mortality_CI_high_percent
        )
    ) %>%
    ungroup()
}

make_aki_only_count_summary <- function(data) {

  data %>%
    group_by(
      Database,
      recovery_status,
      peak_stage_category
    ) %>%
    summarise(
      N = n(),
      Deaths = sum(
        hospital_death_after_60h == 1L
      ),
      Mortality_percent =
        100 * Deaths / N,
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      Mortality_CI_low_percent =
        100 * wilson_ci(
          Deaths,
          N
        )[1],

      Mortality_CI_high_percent =
        100 * wilson_ci(
          Deaths,
          N
        )[2],

      Mortality_percent_95CI =
        sprintf(
          "%.2f%% (%.2f-%.2f)",
          Mortality_percent,
          Mortality_CI_low_percent,
          Mortality_CI_high_percent
        )
    ) %>%
    ungroup()
}

# ------------------------------------------------------------------------------
# 9. Load full datasets and create analysis populations
# ------------------------------------------------------------------------------

stop_if_missing_file(
  MIMIC_RDS
)

stop_if_missing_file(
  EICU_RDS
)

mimic_full <- prepare_full_data(
  readRDS(MIMIC_RDS),
  "MIMIC-IV"
)

eicu_full <- prepare_full_data(
  readRDS(EICU_RDS),
  "eICU-CRD"
)

database_data <- list(
  "MIMIC-IV" = mimic_full,
  "eICU-CRD" = eicu_full
)

active_analysis_ids <- c(
  "Primary_36_60h",
  "Narrow_42_54h",
  "PreICU_reference_only"
)

if (RUN_AECOPD_SENSITIVITY) {
  active_analysis_ids <- c(
    active_analysis_ids,
    "AECOPD_restricted"
  )
}

core_datasets <- list()

for (database in names(database_data)) {

  for (
    analysis_id in active_analysis_ids
  ) {

    key <- paste(
      database,
      analysis_id,
      sep = "__"
    )

    core_datasets[[key]] <-
      make_core_analysis_data(
        database_data[[database]],
        database,
        analysis_id
      )
  }
}

aki_only_datasets <- list()

if (RUN_AKI_ONLY_SUPPLEMENT) {

  aki_only_datasets <- list(
    "MIMIC-IV" =
      make_aki_only_data(
        mimic_full,
        "MIMIC-IV"
      ),
    "eICU-CRD" =
      make_aki_only_data(
        eicu_full,
        "eICU-CRD"
      )
  )
}

rm(
  mimic_full,
  eicu_full,
  database_data
)

gc(
  verbose = FALSE
)

# ------------------------------------------------------------------------------
# 10. Frozen count checks
# ------------------------------------------------------------------------------

observed_counts <- bind_rows(
  lapply(
    core_datasets,
    function(data) {
      tibble(
        Database =
          data$Database[[1]],
        Analysis_ID =
          data$Analysis_ID[[1]],

        N_observed =
          nrow(data),

        Deaths_observed =
          sum(
            data$hospital_death_after_60h == 1L
          ),

        No_AKI_observed =
          sum(
            data$analysis_exposure_code == 0L
          ),

        Resolved_observed =
          sum(
            data$analysis_exposure_code == 1L
          ),

        Ongoing_observed =
          sum(
            data$analysis_exposure_code == 2L
          )
      )
    }
  )
)

expected_active <- expected_counts %>%
  filter(
    Analysis_ID %in%
      active_analysis_ids
  )

count_checks <- expected_active %>%
  left_join(
    observed_counts,
    by = c(
      "Database",
      "Analysis_ID"
    )
  ) %>%
  mutate(
    N_pass =
      N_observed == N_expected,
    Deaths_pass =
      Deaths_observed == Deaths_expected,
    No_AKI_pass =
      No_AKI_observed == No_AKI_expected,
    Resolved_pass =
      Resolved_observed == Resolved_expected,
    Ongoing_pass =
      Ongoing_observed == Ongoing_expected,

    All_pass =
      N_pass &
      Deaths_pass &
      No_AKI_pass &
      Resolved_pass &
      Ongoing_pass
  )

for (i in seq_len(
  nrow(count_checks)
)) {

  prefix <- paste(
    count_checks$Database[i],
    count_checks$Analysis_ID[i]
  )

  check_expected_value(
    count_checks$N_observed[i],
    count_checks$N_expected[i],
    paste(prefix, "N")
  )

  check_expected_value(
    count_checks$Deaths_observed[i],
    count_checks$Deaths_expected[i],
    paste(prefix, "deaths")
  )

  check_expected_value(
    count_checks$No_AKI_observed[i],
    count_checks$No_AKI_expected[i],
    paste(prefix, "no early AKI")
  )

  check_expected_value(
    count_checks$Resolved_observed[i],
    count_checks$Resolved_expected[i],
    paste(prefix, "resolved")
  )

  check_expected_value(
    count_checks$Ongoing_observed[i],
    count_checks$Ongoing_expected[i],
    paste(prefix, "ongoing")
  )
}

aki_only_count_checks <- tibble()

if (RUN_AKI_ONLY_SUPPLEMENT) {

  aki_only_observed <- bind_rows(
    lapply(
      aki_only_datasets,
      function(data) {
        tibble(
          Database =
            data$Database[[1]],

          N_observed =
            nrow(data),

          Deaths_observed =
            sum(
              data$hospital_death_after_60h == 1L
            ),

          Resolved_observed =
            sum(
              data$recovery_status ==
                "Resolved early AKI"
            ),

          Ongoing_observed =
            sum(
              data$recovery_status ==
                "Ongoing early AKI"
            ),

          Peak_stage_2_3_observed =
            sum(
              data$peak_stage_high == 1L
            )
        )
      }
    )
  )

  aki_only_count_checks <-
    expected_aki_only %>%
    left_join(
      aki_only_observed,
      by = "Database"
    ) %>%
    mutate(
      N_pass =
        N_observed == N_expected,
      Deaths_pass =
        Deaths_observed == Deaths_expected,
      Resolved_pass =
        Resolved_observed == Resolved_expected,
      Ongoing_pass =
        Ongoing_observed == Ongoing_expected,
      Peak_stage_2_3_pass =
        Peak_stage_2_3_observed ==
        Peak_stage_2_3_expected,

      All_pass =
        N_pass &
        Deaths_pass &
        Resolved_pass &
        Ongoing_pass &
        Peak_stage_2_3_pass
    )

  for (i in seq_len(
    nrow(
      aki_only_count_checks
    )
  )) {

    prefix <- paste(
      aki_only_count_checks$Database[i],
      "AKI-only"
    )

    check_expected_value(
      aki_only_count_checks$N_observed[i],
      aki_only_count_checks$N_expected[i],
      paste(prefix, "N")
    )

    check_expected_value(
      aki_only_count_checks$Deaths_observed[i],
      aki_only_count_checks$Deaths_expected[i],
      paste(prefix, "deaths")
    )

    check_expected_value(
      aki_only_count_checks$Resolved_observed[i],
      aki_only_count_checks$Resolved_expected[i],
      paste(prefix, "resolved")
    )

    check_expected_value(
      aki_only_count_checks$Ongoing_observed[i],
      aki_only_count_checks$Ongoing_expected[i],
      paste(prefix, "ongoing")
    )

    check_expected_value(
      aki_only_count_checks$Peak_stage_2_3_observed[i],
      aki_only_count_checks$Peak_stage_2_3_expected[i],
      paste(prefix, "peak stage 2-3")
    )
  }
}

# ------------------------------------------------------------------------------
# 11. Crude mortality summaries
# ------------------------------------------------------------------------------

core_crude_mortality <- bind_rows(
  lapply(
    core_datasets,
    make_core_count_summary
  )
) %>%
  arrange(
    Database,
    match(
      Analysis_ID,
      core_analysis_specs$Analysis_ID
    ),
    analysis_exposure_code
  )

aki_only_crude_mortality <- tibble()

if (RUN_AKI_ONLY_SUPPLEMENT) {

  aki_only_crude_mortality <-
    bind_rows(
      lapply(
        aki_only_datasets,
        make_aki_only_count_summary
      )
    ) %>%
    arrange(
      Database,
      recovery_status,
      peak_stage_category
    )
}

# ------------------------------------------------------------------------------
# 12. Fit primary and alternative-cohort models
# ------------------------------------------------------------------------------

core_model_objects <- list()
core_coefficients_list <- list()
core_model_qc_list <- list()
eicu_conventional_se_list <- list()

for (key in names(
  core_datasets
)) {

  data <- core_datasets[[key]]

  database <-
    data$Database[[1]]

  analysis_id <-
    data$Analysis_ID[[1]]

  analysis_label <-
    data$Analysis_label[[1]]

  analysis_role <-
    data$Analysis_role[[1]]

  reference_definition <-
    data$Reference_definition[[1]]

  clustered <-
    identical(
      database,
      "eICU-CRD"
    )

  model_specs <- list(
    "Unadjusted" =
      core_unadjusted_formula,
    "Adjusted" =
      core_adjusted_formula
  )

  for (model_name in names(
    model_specs
  )) {

    fit <- fit_logistic(
      model_specs[[model_name]],
      data,
      database,
      analysis_label,
      model_name
    )

    fit_vcov <- get_vcov(
      fit,
      data,
      database,
      clustered = clustered
    )

    se_method <- if (clustered) {
      "Hospital-clustered robust HC1"
    } else {
      "Model-based"
    }

    object_key <- paste(
      database,
      analysis_id,
      model_name,
      sep = "__"
    )

    core_model_objects[[object_key]] <-
      fit

    core_coefficients_list[[object_key]] <-
      tidy_logistic(
        fit = fit,
        vcov_matrix = fit_vcov,
        database = database,
        analysis_id = analysis_id,
        analysis_label = analysis_label,
        analysis_role = analysis_role,
        reference_definition =
          reference_definition,
        model_name = model_name,
        se_method = se_method,
        analysis_type = "core"
      )

    core_model_qc_list[[object_key]] <-
      make_model_qc(
        fit = fit,
        data = data,
        database = database,
        analysis_id =
          analysis_id,
        analysis_label =
          analysis_label,
        model_name =
          model_name,
        clustered =
          clustered
      )

    # QC only: compare eICU robust and conventional SEs.
    if (clustered) {

      conventional_key <- paste0(
        object_key,
        "__Conventional"
      )

      eicu_conventional_se_list[[conventional_key]] <-
        tidy_logistic(
          fit = fit,
          vcov_matrix =
            stats::vcov(fit),
          database = database,
          analysis_id = analysis_id,
          analysis_label =
            analysis_label,
          analysis_role =
            analysis_role,
          reference_definition =
            reference_definition,
          model_name =
            model_name,
          se_method =
            "Model-based comparison only",
          analysis_type = "core"
        ) %>%
        filter(
          Term_category ==
            "Exposure"
        )
    }
  }
}

core_coefficients <- bind_rows(
  core_coefficients_list
)

core_model_qc <- bind_rows(
  core_model_qc_list
)

core_exposure_results <- core_coefficients %>%
  filter(
    Term_category == "Exposure"
  ) %>%
  select(
    Database,
    Analysis_ID,
    Analysis_label,
    Analysis_role,
    Reference_definition,
    Model,
    SE_method,
    Term_label,
    OR,
    CI_low,
    CI_high,
    OR_95CI,
    P_value_numeric,
    P_value,
    N,
    Events,
    Non_events
  ) %>%
  arrange(
    Database,
    match(
      Analysis_ID,
      core_analysis_specs$Analysis_ID
    ),
    factor(
      Model,
      levels = c(
        "Unadjusted",
        "Adjusted"
      )
    ),
    Term_label
  )

# Publication-facing adjusted estimates.
adjusted_core_results <-
  core_exposure_results %>%
  filter(
    Model == "Adjusted"
  ) %>%
  arrange(
    Database,
    match(
      Analysis_ID,
      core_analysis_specs$Analysis_ID
    ),
    Term_label
  )

primary_adjusted_results <-
  adjusted_core_results %>%
  filter(
    Analysis_ID ==
      "Primary_36_60h"
  )

sensitivity_adjusted_results <-
  adjusted_core_results %>%
  filter(
    Analysis_ID %in% c(
      "Narrow_42_54h",
      "PreICU_reference_only"
    )
  )

aecopd_adjusted_results <-
  adjusted_core_results %>%
  filter(
    Analysis_ID ==
      "AECOPD_restricted"
  )

# Full adjusted coefficients excluding the intercept.
adjusted_core_coefficients <-
  core_coefficients %>%
  filter(
    Model == "Adjusted",
    Term != "(Intercept)"
  ) %>%
  arrange(
    Database,
    match(
      Analysis_ID,
      core_analysis_specs$Analysis_ID
    ),
    factor(
      Term_category,
      levels = c(
        "Exposure",
        "Covariate"
      )
    ),
    Term_label
  )

# ------------------------------------------------------------------------------
# 13. Additional organ-support sensitivity model
# ------------------------------------------------------------------------------

organ_support_model_objects <- list()
organ_support_coefficients_list <- list()
organ_support_model_qc_list <- list()

for (database in c(
  "MIMIC-IV",
  "eICU-CRD"
)) {

  key <- paste(
    database,
    "Primary_36_60h",
    sep = "__"
  )

  data <- core_datasets[[key]]

  if (is.null(data)) {
    stop(
      "Primary dataset not found for ",
      database
    )
  }

  missing_support <- c(
    imv_any_0_24h =
      sum(
        is.na(
          data$imv_any_0_24h
        )
      ),
    vasopressor_any_0_24h =
      sum(
        is.na(
          data$vasopressor_any_0_24h
        )
      )
  )

  if (any(
    missing_support > 0L
  )) {
    stop(
      database,
      ": missing organ-support sensitivity covariates: ",
      paste(
        names(
          missing_support
        )[
          missing_support > 0L
        ],
        missing_support[
          missing_support > 0L
        ],
        sep = "=",
        collapse = ", "
      )
    )
  }

  clustered <-
    identical(
      database,
      "eICU-CRD"
    )

  fit <- fit_logistic(
    core_organ_support_formula,
    data,
    database,
    "Primary cohort + early organ-support adjustment",
    "Adjusted + IMV + vasopressor"
  )

  fit_vcov <- get_vcov(
    fit,
    data,
    database,
    clustered =
      clustered
  )

  se_method <- if (clustered) {
    "Hospital-clustered robust HC1"
  } else {
    "Model-based"
  }

  object_key <- paste(
    database,
    "Primary_36_60h",
    "Adjusted_support",
    sep = "__"
  )

  organ_support_model_objects[[object_key]] <- fit

  organ_support_coefficients_list[[object_key]] <-
    tidy_logistic(
      fit = fit,
      vcov_matrix = fit_vcov,
      database = database,
      analysis_id =
        "Primary_plus_IMV_vasopressor",
      analysis_label =
        "Sensitivity: primary model additionally adjusted for IMV and vasopressor use during ICU hours 0-24",
      analysis_role =
        "Core sensitivity",
      reference_definition =
        "No early AKI during ICU hours 0-48",
      model_name =
        "Adjusted + IMV + vasopressor",
      se_method =
        se_method,
      analysis_type =
        "core"
    )

  organ_support_model_qc_list[[object_key]] <-
    make_model_qc(
      fit = fit,
      data = data,
      database =
        database,
      analysis_id =
        "Primary_plus_IMV_vasopressor",
      analysis_label =
        "Sensitivity: primary model additionally adjusted for IMV and vasopressor use during ICU hours 0-24",
      model_name =
        "Adjusted + IMV + vasopressor",
      clustered =
        clustered
    )
}

organ_support_coefficients <-
  bind_rows(
    organ_support_coefficients_list
  )

organ_support_results <-
  organ_support_coefficients %>%
  filter(
    Term_category == "Exposure"
  ) %>%
  select(
    Database,
    Analysis_ID,
    Analysis_label,
    Analysis_role,
    Reference_definition,
    Model,
    SE_method,
    Term_label,
    OR,
    CI_low,
    CI_high,
    OR_95CI,
    P_value_numeric,
    P_value,
    N,
    Events,
    Non_events
  ) %>%
  arrange(
    Database,
    Term_label
  )

organ_support_full_coefficients <-
  organ_support_coefficients %>%
  filter(
    Term != "(Intercept)"
  ) %>%
  arrange(
    Database,
    factor(
      Term_category,
      levels = c(
        "Exposure",
        "Covariate"
      )
    ),
    Term_label
  )

organ_support_model_qc <-
  bind_rows(
    organ_support_model_qc_list
  )

# ------------------------------------------------------------------------------
# 14. Supplementary AKI-only models
# ------------------------------------------------------------------------------

aki_only_model_objects <- list()
aki_only_coefficients <- tibble()
aki_only_model_qc <- tibble()
aki_only_adjusted_results <- tibble()
aki_only_adjusted_coefficients <- tibble()

if (RUN_AKI_ONLY_SUPPLEMENT) {

  aki_coef_list <- list()
  aki_qc_list <- list()

  for (database in names(
    aki_only_datasets
  )) {

    data <-
      aki_only_datasets[[database]]

    clustered <-
      identical(
        database,
        "eICU-CRD"
      )

    model_specs <- list(
      "Unadjusted recovery model" =
        aki_only_unadjusted_formula,

      "Adjusted recovery model + peak AKI stage" =
        aki_only_adjusted_formula
    )

    for (model_name in names(
      model_specs
    )) {

      fit <- fit_logistic(
        model_specs[[model_name]],
        data,
        database,
        "Supplementary: early-AKI patients only",
        model_name
      )

      fit_vcov <- get_vcov(
        fit,
        data,
        database,
        clustered =
          clustered
      )

      se_method <- if (clustered) {
        "Hospital-clustered robust HC1"
      } else {
        "Model-based"
      }

      object_key <- paste(
        database,
        model_name,
        sep = "__"
      )

      aki_only_model_objects[[object_key]] <- fit

      aki_coef_list[[object_key]] <-
        tidy_logistic(
          fit = fit,
          vcov_matrix =
            fit_vcov,
          database =
            database,
          analysis_id =
            "AKI_only_recovery",
          analysis_label =
            "Supplementary: early-AKI patients only",
          analysis_role =
            "Supplementary",
          reference_definition =
            "Resolved early AKI",
          model_name =
            model_name,
          se_method =
            se_method,
          analysis_type =
            "aki_only"
        )

      aki_qc_list[[object_key]] <-
        make_model_qc(
          fit = fit,
          data = data,
          database =
            database,
          analysis_id =
            "AKI_only_recovery",
          analysis_label =
            "Supplementary: early-AKI patients only",
          model_name =
            model_name,
          clustered =
            clustered
        )
    }
  }

  aki_only_coefficients <-
    bind_rows(
      aki_coef_list
    )

  aki_only_model_qc <-
    bind_rows(
      aki_qc_list
    )

  aki_only_adjusted_results <-
    aki_only_coefficients %>%
    filter(
      Model ==
        "Adjusted recovery model + peak AKI stage",
      Term_category ==
        "Exposure"
    ) %>%
    select(
      Database,
      Analysis_ID,
      Analysis_label,
      Analysis_role,
      Reference_definition,
      Model,
      SE_method,
      Term_label,
      OR,
      CI_low,
      CI_high,
      OR_95CI,
      P_value_numeric,
      P_value,
      N,
      Events,
      Non_events
    ) %>%
    arrange(
      Database,
      Term_label
    )

  aki_only_adjusted_coefficients <-
    aki_only_coefficients %>%
    filter(
      Model ==
        "Adjusted recovery model + peak AKI stage",
      Term != "(Intercept)"
    ) %>%
    arrange(
      Database,
      factor(
        Term_category,
        levels = c(
          "Exposure",
          "Covariate"
        )
      ),
      Term_label
    )
}

# ------------------------------------------------------------------------------
# 15. Forest-plot-ready publication result set
# ------------------------------------------------------------------------------

forest_plot_data <- bind_rows(
  primary_adjusted_results %>%
    mutate(
      Display_analysis =
        "Primary: 36-60 h assessment"
    ),

  sensitivity_adjusted_results %>%
    mutate(
      Display_analysis =
        case_when(
          Analysis_ID ==
            "Narrow_42_54h" ~
            "Sensitivity: 42-54 h assessment",

          Analysis_ID ==
            "PreICU_reference_only" ~
            "Sensitivity: measured pre-ICU reference only",

          TRUE ~ Analysis_label
        )
    ),

  organ_support_results %>%
    mutate(
      Display_analysis =
        "Sensitivity: + IMV + vasopressor"
    )
) %>%
  mutate(
    Comparison = case_when(
      grepl(
        "^Resolved early AKI",
        Term_label
      ) ~
        "Resolved vs no early AKI",

      grepl(
        "^Ongoing early AKI",
        Term_label
      ) ~
        "Ongoing vs no early AKI",

      TRUE ~ Term_label
    ),

    Analysis_order = match(
      Display_analysis,
      c(
        "Primary: 36-60 h assessment",
        "Sensitivity: 42-54 h assessment",
        "Sensitivity: measured pre-ICU reference only",
        "Sensitivity: + IMV + vasopressor"
      )
    ),

    Comparison_order = match(
      Comparison,
      c(
        "Resolved vs no early AKI",
        "Ongoing vs no early AKI"
      )
    )
  ) %>%
  arrange(
    Database,
    Analysis_order,
    Comparison_order
  )

# ------------------------------------------------------------------------------
# 16. eICU robust-SE QC comparison
# ------------------------------------------------------------------------------

eicu_robust_exposure <-
  core_coefficients %>%
  filter(
    Database ==
      "eICU-CRD",
    Term_category ==
      "Exposure"
  )

eicu_conventional_exposure <-
  bind_rows(
    eicu_conventional_se_list
  )

eicu_se_comparison <-
  bind_rows(
    eicu_robust_exposure,
    eicu_conventional_exposure
  ) %>%
  select(
    Database,
    Analysis_ID,
    Analysis_label,
    Model,
    SE_method,
    Term_label,
    OR,
    CI_low,
    CI_high,
    OR_95CI,
    P_value_numeric,
    P_value,
    N,
    Events
  ) %>%
  arrange(
    match(
      Analysis_ID,
      core_analysis_specs$Analysis_ID
    ),
    Model,
    Term_label,
    SE_method
  )

# ------------------------------------------------------------------------------
# 16A. Manuscript Supplementary Table S1: sample composition and reference sCr
# ------------------------------------------------------------------------------
# These are formatted summaries of independent data-derived counts. None of the
# frozen values below is used in filtering or fitting a model.

s1_analysis_labels <- c(
  Primary_36_60h = "Primary analysis",
  Narrow_42_54h = "Narrow assessment window (42-54 h)",
  PreICU_reference_only = "Measured pre-ICU reference only",
  AECOPD_restricted = "AECOPD-restricted cohort"
)
s1_modification <- c(
  Primary_36_60h = "Fixed-reference AKI identified during ICU hours 0-48; trajectory assessment 36-60 h",
  Narrow_42_54h = "Trajectory assessment restricted to ICU hours 42-54",
  PreICU_reference_only = "Measured sCr within the prior 7 days; no ICU 0-24-h surrogate reference",
  AECOPD_restricted = "Restricted to acute COPD exacerbation"
)

s1_panel_a <- count_checks %>%
  mutate(
    Analysis = unname(s1_analysis_labels[Analysis_ID]),
    Modification = unname(s1_modification[Analysis_ID]),
    N_deaths = sprintf("%d/%d", N_observed, Deaths_observed)
  ) %>%
  select(
    Database, Analysis_ID, Analysis, Modification, N_deaths,
    No_early_AKI = No_AKI_observed,
    Resolved_early_AKI = Resolved_observed,
    Ongoing_early_AKI = Ongoing_observed
  )

format_stratum_mortality <- function(x) {
  if (nrow(x) == 0L) return("0/0 (NA)")
  n_stratum <- nrow(x)
  deaths <- sum(x$hospital_death_after_60h == 1L)
  sprintf("%d/%d (%.1f%%)", n_stratum, deaths, 100 * deaths / n_stratum)
}

source_levels <- c("pre_icu_7d_latest", "icu_0_24h_min")
source_labels <- c(
  pre_icu_7d_latest = "Measured pre-ICU",
  icu_0_24h_min = "ICU 0-24-h surrogate"
)

s1_panel_b <- bind_rows(lapply(c("MIMIC-IV", "eICU-CRD"), function(database) {
  cohort <- core_datasets[[paste(database, "Primary_36_60h", sep = "__")]]
  if (!"reference_scr_source" %in% names(cohort)) {
    stop(database, ": missing reference_scr_source required for Table S1 Panel B.")
  }
  if (any(is.na(cohort$reference_scr_source)) ||
      any(!as.character(cohort$reference_scr_source) %in% source_levels)) {
    stop(database, ": unrecognized reference source among primary patients.")
  }
  bind_rows(lapply(source_levels, function(source) {
    sub <- cohort[as.character(cohort$reference_scr_source) == source, , drop = FALSE]
    tibble(
      Database = database,
      Reference_sCr_source = unname(source_labels[source]),
      Total_N_deaths = sprintf("%d/%d", nrow(sub),
                               sum(sub$hospital_death_after_60h == 1L)),
      No_early_AKI = format_stratum_mortality(
        sub[sub$analysis_exposure_code == 0L, , drop = FALSE]),
      Resolved_early_AKI = format_stratum_mortality(
        sub[sub$analysis_exposure_code == 1L, , drop = FALSE]),
      Ongoing_early_AKI = format_stratum_mortality(
        sub[sub$analysis_exposure_code == 2L, , drop = FALSE])
    )
  }))
}))

# ------------------------------------------------------------------------------
# 16B. Manuscript Supplementary Table S2: all five adjusted specifications
# ------------------------------------------------------------------------------

s2_analysis_ids <- c(
  "Primary_36_60h", "Narrow_42_54h", "PreICU_reference_only",
  "AECOPD_restricted", "Primary_plus_IMV_vasopressor"
)
s2_analysis_labels <- c(
  Primary_36_60h = "Primary analysis",
  Narrow_42_54h = "Narrow assessment window (42-54 h)",
  PreICU_reference_only = "Measured pre-ICU reference sCr only",
  AECOPD_restricted = "AECOPD-restricted cohort",
  Primary_plus_IMV_vasopressor = "Additional adjustment for IMV and vasopressor use"
)

s2_all_results <- bind_rows(adjusted_core_results, organ_support_results)
get_s2_contrast <- function(data, prefix) {
  hit <- data[grepl(paste0("^", prefix), data$Term_label), , drop = FALSE]
  if (nrow(hit) != 1L) {
    stop("Unexpected number of S2 ", prefix, " contrasts: ", nrow(hit))
  }
  hit$OR_95CI[[1]]
}

s2_table <- bind_rows(lapply(s2_analysis_ids, function(analysis_id) {
  if (analysis_id == "AECOPD_restricted" && !RUN_AECOPD_SENSITIVITY) {
    return(NULL)
  }
  per_db <- lapply(c("MIMIC-IV", "eICU-CRD"), function(database) {
    d <- s2_all_results[
      s2_all_results$Database == database &
        s2_all_results$Analysis_ID == analysis_id, , drop = FALSE
    ]
    if (nrow(d) != 2L) {
      stop(database, " | ", analysis_id, ": S2 requires exactly two exposure contrasts.")
    }
    c(get_s2_contrast(d, "Resolved early AKI"),
      get_s2_contrast(d, "Ongoing early AKI"))
  })
  tibble(
    Analysis_ID = analysis_id,
    Sensitivity_analysis = unname(s2_analysis_labels[analysis_id]),
    MIMIC_Resolved_vs_No_AKI = per_db[[1]][[1]],
    MIMIC_Ongoing_vs_No_AKI = per_db[[1]][[2]],
    eICU_Resolved_vs_No_AKI = per_db[[2]][[1]],
    eICU_Ongoing_vs_No_AKI = per_db[[2]][[2]]
  )
}))

# QC compares (not replaces) model results against manuscript Table S2 values.
# Round-to-2dp equality is a review cue; small numerical differences from R
# versions / covariance implementations should be investigated, not hidden.
published_s2 <- tibble::tribble(
  ~Database, ~Analysis_ID, ~Contrast, ~Pub_OR, ~Pub_low, ~Pub_high,
  "MIMIC-IV", "Primary_36_60h", "Resolved", 1.20, 0.95, 1.52,
  "MIMIC-IV", "Primary_36_60h", "Ongoing",  2.61, 2.16, 3.17,
  "eICU-CRD", "Primary_36_60h", "Resolved", 1.50, 1.13, 1.99,
  "eICU-CRD", "Primary_36_60h", "Ongoing",  2.35, 1.84, 3.00,
  "MIMIC-IV", "Narrow_42_54h", "Resolved", 1.35, 1.04, 1.76,
  "MIMIC-IV", "Narrow_42_54h", "Ongoing",  2.78, 2.25, 3.44,
  "eICU-CRD", "Narrow_42_54h", "Resolved", 1.66, 1.20, 2.32,
  "eICU-CRD", "Narrow_42_54h", "Ongoing",  2.88, 2.04, 4.07,
  "MIMIC-IV", "PreICU_reference_only", "Resolved", 1.28, 0.80, 2.05,
  "MIMIC-IV", "PreICU_reference_only", "Ongoing",  3.14, 2.26, 4.36,
  "eICU-CRD", "PreICU_reference_only", "Resolved", 1.73, 1.14, 2.62,
  "eICU-CRD", "PreICU_reference_only", "Ongoing",  2.49, 1.83, 3.38,
  "MIMIC-IV", "AECOPD_restricted", "Resolved", 0.89, 0.53, 1.48,
  "MIMIC-IV", "AECOPD_restricted", "Ongoing",  2.77, 1.81, 4.26,
  "eICU-CRD", "AECOPD_restricted", "Resolved", 1.67, 0.93, 2.98,
  "eICU-CRD", "AECOPD_restricted", "Ongoing",  2.28, 1.57, 3.31,
  "MIMIC-IV", "Primary_plus_IMV_vasopressor", "Resolved", 1.23, 0.97, 1.55,
  "MIMIC-IV", "Primary_plus_IMV_vasopressor", "Ongoing",  2.54, 2.09, 3.09,
  "eICU-CRD", "Primary_plus_IMV_vasopressor", "Resolved", 1.48, 1.12, 1.96,
  "eICU-CRD", "Primary_plus_IMV_vasopressor", "Ongoing",  2.32, 1.81, 2.97
)

observed_s2 <- s2_all_results %>%
  mutate(Contrast = ifelse(grepl("^Resolved early AKI", Term_label),
                           "Resolved", "Ongoing")) %>%
  select(Database, Analysis_ID, Contrast, OR, CI_low, CI_high)

published_s2_qc <- published_s2 %>%
  filter(Analysis_ID %in% c(active_analysis_ids, "Primary_plus_IMV_vasopressor")) %>%
  left_join(observed_s2, by = c("Database", "Analysis_ID", "Contrast")) %>%
  mutate(
    OR_matches_2dp = !is.na(OR) & sprintf("%.2f", OR) == sprintf("%.2f", Pub_OR),
    Low_matches_2dp = !is.na(CI_low) & sprintf("%.2f", CI_low) == sprintf("%.2f", Pub_low),
    High_matches_2dp = !is.na(CI_high) & sprintf("%.2f", CI_high) == sprintf("%.2f", Pub_high),
    All_matches_2dp = OR_matches_2dp & Low_matches_2dp & High_matches_2dp
  )

# ------------------------------------------------------------------------------
# 16C. Manuscript Supplementary Table S3: early-AKI subgroup
# ------------------------------------------------------------------------------

s3_panel_a <- if (RUN_AKI_ONLY_SUPPLEMENT) {
  aki_only_crude_mortality %>%
    mutate(N_deaths = sprintf("%d/%d", N, Deaths),
           Mortality_percent = sprintf("%.1f", Mortality_percent)) %>%
    select(Database, Early_AKI_trajectory = recovery_status,
           Peak_AKI_stage = peak_stage_category, N_deaths, Mortality_percent)
} else { tibble() }

s3_panel_b <- if (RUN_AKI_ONLY_SUPPLEMENT) {
  aki_only_coefficients %>%
    filter(Model == "Adjusted recovery model + peak AKI stage",
           Term %in% c("recovery_statusOngoing early AKI", "peak_stage_high")) %>%
    mutate(N_deaths = sprintf("%d/%d", N, Events)) %>%
    select(Database, N_deaths, Comparison = Term_label,
           Adjusted_OR_95CI = OR_95CI)
} else { tibble() }

# ------------------------------------------------------------------------------
# 17. Save CSV/RDS outputs
# ------------------------------------------------------------------------------

write_csv_bom(s1_panel_a, "TableS1_PanelA_sample_composition.csv")
write_csv_bom(s1_panel_b, "TableS1_PanelB_reference_source.csv")
write_csv_bom(s2_table, "TableS2_adjusted_associations.csv")
write_csv_bom(published_s2_qc, "published_S2_estimate_QC.csv")
if (RUN_AKI_ONLY_SUPPLEMENT) {
  write_csv_bom(s3_panel_a, "TableS3_PanelA_peak_stage_mortality.csv")
  write_csv_bom(s3_panel_b, "TableS3_PanelB_AKI_only_adjusted.csv")
}

write_csv_bom(
  count_checks,
  "Model_count_checks.csv"
)

write_csv_bom(
  core_crude_mortality,
  "Crude_mortality_primary_and_sensitivity.csv"
)

write_csv_bom(
  primary_adjusted_results,
  "Primary_logistic_adjusted_exposure_results.csv"
)

write_csv_bom(
  sensitivity_adjusted_results,
  "Core_sensitivity_adjusted_exposure_results.csv"
)

if (RUN_AECOPD_SENSITIVITY) {
  write_csv_bom(
    aecopd_adjusted_results,
    "Optional_AECOPD_adjusted_exposure_results.csv"
  )
}

write_csv_bom(
  organ_support_results,
  "Sensitivity_IMV_vasopressor_adjusted_exposure_results.csv"
)

write_csv_bom(
  adjusted_core_coefficients,
  "Primary_and_alternative_cohort_adjusted_full_coefficients.csv"
)

write_csv_bom(
  organ_support_full_coefficients,
  "Sensitivity_IMV_vasopressor_full_coefficients.csv"
)

write_csv_bom(
  forest_plot_data,
  "Forest_plot_data_primary_and_core_sensitivity.csv"
)

write_csv_bom(
  core_model_qc,
  "Core_model_QC.csv"
)

write_csv_bom(
  organ_support_model_qc,
  "Organ_support_model_QC.csv"
)

write_csv_bom(
  eicu_se_comparison,
  "eICU_clustered_vs_conventional_SE_QC.csv"
)

if (RUN_AKI_ONLY_SUPPLEMENT) {

  write_csv_bom(
    aki_only_count_checks,
    "AKI_only_count_checks.csv"
  )

  write_csv_bom(
    aki_only_crude_mortality,
    "AKI_only_crude_mortality_by_recovery_and_peak_stage.csv"
  )

  write_csv_bom(
    aki_only_adjusted_results,
    "AKI_only_adjusted_recovery_results.csv"
  )

  write_csv_bom(
    aki_only_adjusted_coefficients,
    "AKI_only_adjusted_full_coefficients.csv"
  )

  write_csv_bom(
    aki_only_model_qc,
    "AKI_only_model_QC.csv"
  )
}

saveRDS(
  core_model_objects,
  file.path(
    OUTPUT_DIR,
    "core_model_objects.rds"
  ),
  compress = "xz"
)

saveRDS(
  organ_support_model_objects,
  file.path(
    OUTPUT_DIR,
    "organ_support_model_objects.rds"
  ),
  compress = "xz"
)

if (RUN_AKI_ONLY_SUPPLEMENT) {

  saveRDS(
    aki_only_model_objects,
    file.path(
      OUTPUT_DIR,
      "aki_only_model_objects.rds"
    ),
    compress = "xz"
  )
}

# ------------------------------------------------------------------------------
# 18. Publication-facing workbook
# ------------------------------------------------------------------------------

workbook_path <- file.path(
  OUTPUT_DIR,
  "Model_and_sensitivity_results.xlsx"
)

wb <- openxlsx::createWorkbook()

add_sheet <- function(
  wb,
  sheet_name,
  data
) {

  openxlsx::addWorksheet(
    wb,
    sheet_name
  )

  openxlsx::writeData(
    wb,
    sheet_name,
    data,
    withFilter = TRUE
  )

  openxlsx::freezePane(
    wb,
    sheet_name,
    firstActiveRow = 2L
  )

  header_style <-
    openxlsx::createStyle(
      fontColour = "#FFFFFF",
      fgFill = "#1F4E78",
      textDecoration = "bold",
      halign = "center",
      valign = "center"
    )

  openxlsx::addStyle(
    wb,
    sheet_name,
    header_style,
    rows = 1L,
    cols =
      seq_len(
        ncol(data)
      ),
    gridExpand = TRUE
  )

  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols =
      seq_len(
        ncol(data)
      ),
    widths = "auto"
  )
}

add_sheet(wb, "S1A_sample_composition", s1_panel_a)
add_sheet(wb, "S1B_reference_source", s1_panel_b)
add_sheet(wb, "S2_adjusted_associations", s2_table)
add_sheet(wb, "S2_published_QC", published_s2_qc)
if (RUN_AKI_ONLY_SUPPLEMENT) {
  add_sheet(wb, "S3A_stage_mortality", s3_panel_a)
  add_sheet(wb, "S3B_AKI_only_adjusted", s3_panel_b)
}

add_sheet(
  wb,
  "Primary_adjusted_OR",
  primary_adjusted_results
)

add_sheet(
  wb,
  "Core_sensitivity_OR",
  sensitivity_adjusted_results
)

add_sheet(
  wb,
  "IMV_vasopressor_OR",
  organ_support_results
)

if (RUN_AECOPD_SENSITIVITY) {
  add_sheet(
    wb,
    "AECOPD_optional_OR",
    aecopd_adjusted_results
  )
}

add_sheet(
  wb,
  "Forest_plot_data",
  forest_plot_data
)

add_sheet(
  wb,
  "Crude_mortality",
  core_crude_mortality
)

add_sheet(
  wb,
  "Count_checks",
  count_checks
)

add_sheet(
  wb,
  "Core_model_QC",
  core_model_qc
)

add_sheet(
  wb,
  "Organ_support_QC",
  organ_support_model_qc
)

add_sheet(
  wb,
  "eICU_SE_QC",
  eicu_se_comparison
)

if (RUN_AKI_ONLY_SUPPLEMENT) {

  add_sheet(
    wb,
    "AKI_only_adjusted_OR",
    aki_only_adjusted_results
  )

  add_sheet(
    wb,
    "AKI_only_counts",
    aki_only_count_checks
  )

  add_sheet(
    wb,
    "AKI_only_crude",
    aki_only_crude_mortality
  )

  add_sheet(
    wb,
    "AKI_only_QC",
    aki_only_model_qc
  )
}

openxlsx::saveWorkbook(
  wb,
  workbook_path,
  overwrite = TRUE
)

# ------------------------------------------------------------------------------
# 19. Manifest
# ------------------------------------------------------------------------------

manifest <- tibble(
  Item = c(
    "Analysis timestamp",
    "MIMIC input RDS",
    "eICU input RDS",
    "Output directory",
    "Primary analysis",
    "Core sensitivity 1",
    "Core sensitivity 2",
    "Core sensitivity 3",
    "Prespecified sensitivity",
    "Supplementary AKI-only analysis",
    "Analyses intentionally omitted from main script",
    "Primary adjustment variables",
    "MIMIC standard errors",
    "eICU standard errors",
    "CCI regression use",
    "Primary outcome"
  ),

  Value = c(
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    ),

    normalizePath(
      MIMIC_RDS,
      winslash = "/",
      mustWork = FALSE
    ),

    normalizePath(
      EICU_RDS,
      winslash = "/",
      mustWork = FALSE
    ),

    normalizePath(
      OUTPUT_DIR,
      winslash = "/",
      mustWork = FALSE
    ),

    "Fixed-reference early AKI trajectory; 36-60 h recovery-assessment window",

    "Narrow 42-54 h recovery-assessment window",

    "Measured pre-ICU reference sCr only",

    "Primary model additionally adjusted for IMV and vasopressor use during ICU hours 0-24",

    if (RUN_AECOPD_SENSITIVITY) {
      "AECOPD-restricted primary cohort (prespecified)"
    } else {
      "Not run"
    },

    if (RUN_AKI_ONLY_SUPPLEMENT) {
      "Ongoing versus resolved early AKI, additionally adjusted for peak creatinine-defined AKI stage"
    } else {
      "Not run"
    },

    "Onset-to-assessment >=12 h; strict no-AKI through 60 h",

    paste(
      "Age, female sex, CKD, heart failure, diabetes,",
      "chronic liver disease, malignancy, standard nonrenal SOFA"
    ),

    "Model-based",

    "Hospital-clustered robust HC1",

    "Descriptive only; excluded from regression models",

    "Post-60-h in-hospital mortality"
  )
)

write_csv_bom(
  manifest,
  "analysis_manifest_03.csv"
)

# ------------------------------------------------------------------------------
# 20. Session information
# ------------------------------------------------------------------------------

session_file <- file.path(
  OUTPUT_DIR,
  "sessionInfo_03.txt"
)

sink(
  session_file
)

cat(
  "COPD early-AKI trajectory study - revised Script 03\n"
)

cat(
  "Run time: ",
  format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  ),
  "\n\n",
  sep = ""
)

print(
  sessionInfo()
)

sink()

# ------------------------------------------------------------------------------
# 21. Console report
# ------------------------------------------------------------------------------

cat(
  "\n============================================================\n"
)

cat(
  "Primary models and sensitivity analyses (public version) completed.\n"
)

cat(
  "Output directory:\n"
)

cat(
  normalizePath(
    OUTPUT_DIR,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n"
)

cat(
  "============================================================\n\n"
)

cat(
  "Count checks:\n"
)

print(
  count_checks %>%
    select(
      Database,
      Analysis_ID,
      N_observed,
      Deaths_observed,
      No_AKI_observed,
      Resolved_observed,
      Ongoing_observed,
      All_pass
    )
)

cat(
  "\nPrimary adjusted exposure results:\n"
)

print(
  primary_adjusted_results %>%
    select(
      Database,
      Term_label,
      OR,
      CI_low,
      CI_high,
      P_value,
      N,
      Events
    )
)

cat(
  "\nCore sensitivity adjusted exposure results:\n"
)

print(
  sensitivity_adjusted_results %>%
    select(
      Database,
      Analysis_ID,
      Term_label,
      OR,
      CI_low,
      CI_high,
      P_value,
      N,
      Events
    )
)

cat(
  "\nAdditional IMV + vasopressor adjustment:\n"
)

print(
  organ_support_results %>%
    select(
      Database,
      Term_label,
      OR,
      CI_low,
      CI_high,
      P_value,
      N,
      Events
    )
)

if (RUN_AECOPD_SENSITIVITY) {

  cat(
    "\nOptional AECOPD-restricted adjusted results:\n"
  )

  print(
    aecopd_adjusted_results %>%
      select(
        Database,
        Term_label,
        OR,
        CI_low,
        CI_high,
        P_value,
        N,
        Events
      )
  )
}

if (RUN_AKI_ONLY_SUPPLEMENT) {

  cat(
    "\nSupplementary AKI-only adjusted recovery results:\n"
  )

  print(
    aki_only_adjusted_results %>%
      select(
        Database,
        Term_label,
        OR,
        CI_low,
        CI_high,
        P_value,
        N,
        Events
      )
  )
}

all_model_qc <- bind_rows(
  core_model_qc,
  organ_support_model_qc,
  aki_only_model_qc
)

cat(
  "\nModel QC flags:\n"
)

print(
  all_model_qc %>%
    select(
      Database,
      Analysis_ID,
      Model,
      N,
      Events,
      Converged,
      Events_per_parameter,
      Potential_separation_flag,
      Hospital_clusters,
      Captured_warnings
    )
)

cat(
  "\nKey files created:\n"
)

cat(
  "  - Primary_logistic_adjusted_exposure_results.csv\n"
)

cat(
  "  - Core_sensitivity_adjusted_exposure_results.csv\n"
)

cat(
  "  - Sensitivity_IMV_vasopressor_adjusted_exposure_results.csv\n"
)

if (RUN_AECOPD_SENSITIVITY) {
  cat(
    "  - Optional_AECOPD_adjusted_exposure_results.csv\n"
  )
}

if (RUN_AKI_ONLY_SUPPLEMENT) {
  cat(
    "  - AKI_only_adjusted_recovery_results.csv\n"
  )
}

cat(
  "  - Forest_plot_data_primary_and_core_sensitivity.csv\n"
)

cat(
  "  - Model_and_sensitivity_results.xlsx\n"
)

cat(
  "\nIf all count checks show All_pass = TRUE and model QC flags are 0,\n"
)

cat(
  "compare model estimates and QC with the manuscript before release.\n"
)
