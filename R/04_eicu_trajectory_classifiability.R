# ==============================================================================
# COPD early creatinine-defined AKI trajectory study
# R/04_eicu_trajectory_classifiability.R
# eICU-CRD v2.0 | Supplementary Table S5 (FINAL manuscript numbering)
#
# Source: the eICU classifiability section of the author's original
# 04_generate_S4_S5_S6_revised_fixed_reference_v3.R, where this analysis was
# called Supplementary Table S6. The eligibility logic, hierarchy of reasons,
# 11-table-variable specification, statistics and absolute SMD formulas below
# are retained; this standalone script removes unrelated S4/S5 model diagnostics.
#
# INPUT (from R/01_import_harmonize_qc.R):
#   output/R_analysis_output/eicu_full_clean.rds
#   All 8,495 stays eligible at the ICU +60-hour landmark, INCLUDING 757
#   whose early AKI trajectory cannot be classified. Do NOT use the primary-only
#   7,738-stay file here.
#
# OUTPUTS (aggregate only, in output/R_analysis_output/04_eicu_trajectory_classifiability/):
#   TableS5_eicu_classifiability.csv
#   TableS5_unclassifiable_reasons_QC.csv
#   TableS5_missingness_QC.csv
#   TableS5_published_values_QC.csv
#   TableS5_cohort_counts_QC.csv
#   TableS5_eicu_classifiability.xlsx (only when 'openxlsx' is installed)
#   analysis_manifest_S5.csv and sessionInfo_S5.txt
#
# Run from repository root:
#   Rscript R/04_eicu_trajectory_classifiability.R
# Or set COPD_AKI_PROJECT_ROOT to project root.
#
# S5 is DESCRIPTIVE ONLY. Continuous variables are reported as median (IQR),
# binary variables as n (%) of nonmissing observations; absolute SMD is a
# measure of imbalance, not a test of significance. No P values are computed.
# No individual identifiers, patient-level CSVs or RDS files are exported.
# Keep source data/ and output/ local and out of the public Git repository.
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)

# ------------------------------------------------------------------------------
# 0. Portable paths and frozen sample-count checks
# ------------------------------------------------------------------------------
root_override <- Sys.getenv("COPD_AKI_PROJECT_ROOT", unset = "")
PROJECT_ROOT <- normalizePath(
  if (nzchar(root_override)) root_override else ".",
  winslash = "/", mustWork = TRUE
)
INPUT_DIR <- file.path(PROJECT_ROOT, "output", "R_analysis_output")
OUTPUT_DIR <- file.path(INPUT_DIR, "04_eicu_trajectory_classifiability")
EICU_FULL_RDS <- file.path(INPUT_DIR, "eicu_full_clean.rds")
EICU_PRIMARY_RDS <- file.path(INPUT_DIR, "eicu_primary_clean.rds")

# Stop if the loaded data differ from the final manuscript's frozen cohort.
# These checks do NOT select patients or force observed results to match.
STOP_ON_COUNT_MISMATCH <- TRUE

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

EXPECTED_EICU_S5 <- list(
  landmark_n = 8495L,
  classifiable_n = 7738L,
  unclassifiable_n = 757L,
  missing_reference_n = 491L,
  no_scr_0_48_n = 149L,
  missing_fixed_stage_n = 0L,
  missing_assessment_n = 117L,
  other_n = 0L
)

# ------------------------------------------------------------------------------
# 1. Required packages (never install packages inside the analysis script)
# ------------------------------------------------------------------------------
required_packages <- c("dplyr", "tibble", "readr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    ". Install these in advance or use renv::restore()."
  )
}
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(readr)
})

# ------------------------------------------------------------------------------
# 2. Functions required by the author's original classifiability analysis
# ------------------------------------------------------------------------------
assert_required_columns <- function(data, columns, database) {
  absent <- setdiff(columns, names(data))
  if (length(absent) > 0L) {
    stop(database, " is missing required columns: ", paste(absent, collapse = ", "))
  }
}

check_expected_value <- function(actual, expected, label) {
  if (length(actual) != 1L || is.na(actual) || actual != expected) {
    msg <- paste0(label, ": observed ", actual, "; expected ", expected, ".")
    if (isTRUE(STOP_ON_COUNT_MISMATCH)) stop(msg) else warning(msg)
  }
  invisible(actual)
}

# Prevent a hidden identity/row-granularity mismatch before computing S5.
assert_valid_landmark_data <- function(data) {
  if (!is.data.frame(data)) stop("eicu_full_clean.rds must contain a data.frame.")
  assert_required_columns(
    data,
    c("patientunitstayid", "primary_analysis_flag", "primary_exposure_code",
      "eligible_at_60h_flag"),
    "eICU-CRD full landmark dataset"
  )
  if (anyNA(data$patientunitstayid) || anyDuplicated(data$patientunitstayid)) {
    stop("eICU: patientunitstayid must be present and unique (one row per ICU stay).")
  }
  for (nm in c("primary_analysis_flag", "eligible_at_60h_flag")) {
    values <- unique(data[[nm]][!is.na(data[[nm]])])
    if (anyNA(data[[nm]]) || any(!values %in% c(0L, 1L))) {
      stop("eICU: ", nm, " must contain nonmissing 0/1 values only.")
    }
  }
  exposure <- data$primary_exposure_code
  if (any(!is.na(exposure) & !exposure %in% c(0L, 1L, 2L))) {
    stop("eICU: primary_exposure_code contains a value outside 0/1/2/NA.")
  }
  expected_analysis <- as.integer(!is.na(exposure) & exposure %in% 0:2)
  if (!identical(as.integer(data$primary_analysis_flag), expected_analysis)) {
    stop("eICU: primary_analysis_flag disagrees with primary_exposure_code.")
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# 3. Original author's S5 [formerly S6] descriptive/SMD code
#    Keep the original formulas, the exclusion-reason hierarchy, and the row
#    specifications unchanged (except for manuscript S6 -> S5 numbering).
# ------------------------------------------------------------------------------
safe_numeric <- function(x) {
  suppressWarnings(
    as.numeric(
      as.character(x)
    )
  )
}

format_median_iqr <- function(
  x,
  digits = 0L
) {

  x <- safe_numeric(x)

  x <- x[
    is.finite(x)
  ]

  if (
    length(x) == 0L
  ) {
    return("")
  }

  q <- stats::quantile(
    x,
    probs = c(
      0.25,
      0.50,
      0.75
    ),
    names = FALSE,
    type = 7
  )

  fmt <- paste0(
    "%.",
    digits,
    "f"
  )

  sprintf(
    paste0(
      fmt,
      " (",
      fmt,
      "-",
      fmt,
      ")"
    ),
    q[2],
    q[1],
    q[3]
  )
}

format_binary <- function(
  x,
  digits = 1L
) {

  x <- safe_numeric(x)

  observed <- x[
    !is.na(x)
  ]

  if (
    length(observed) == 0L
  ) {
    return("")
  }

  numerator <- sum(
    observed == 1
  )

  denominator <-
    length(observed)

  sprintf(
    paste0(
      "%d (%.",
      digits,
      "f)"
    ),
    numerator,
    100 *
      numerator /
      denominator
  )
}

smd_continuous <- function(
  x1,
  x0
) {

  x1 <- safe_numeric(x1)
  x0 <- safe_numeric(x0)

  x1 <- x1[
    is.finite(x1)
  ]

  x0 <- x0[
    is.finite(x0)
  ]

  if (
    length(x1) < 2L ||
      length(x0) < 2L
  ) {
    return(
      NA_real_
    )
  }

  pooled_sd <- sqrt(
    (
      stats::var(x1) +
        stats::var(x0)
    ) /
      2
  )

  if (
    !is.finite(pooled_sd)
  ) {
    return(
      NA_real_
    )
  }

  if (
    pooled_sd == 0
  ) {
    return(
      ifelse(
        isTRUE(
          all.equal(
            mean(x1),
            mean(x0)
          )
        ),
        0,
        Inf
      )
    )
  }

  abs(
    mean(x1) -
      mean(x0)
  ) /
    pooled_sd
}

smd_binary <- function(
  x1,
  x0
) {

  x1 <- safe_numeric(x1)
  x0 <- safe_numeric(x0)

  x1 <- x1[
    !is.na(x1)
  ]

  x0 <- x0[
    !is.na(x0)
  ]

  if (
    length(x1) == 0L ||
      length(x0) == 0L
  ) {
    return(
      NA_real_
    )
  }

  p1 <- mean(
    x1 == 1
  )

  p0 <- mean(
    x0 == 1
  )

  pooled_sd <- sqrt(
    (
      p1 *
        (
          1 - p1
        ) +
        p0 *
        (
          1 - p0
        )
    ) /
      2
  )

  if (
    !is.finite(pooled_sd)
  ) {
    return(
      NA_real_
    )
  }

  if (
    pooled_sd == 0
  ) {
    return(
      ifelse(
        isTRUE(
          all.equal(
            p1,
            p0
          )
        ),
        0,
        Inf
      )
    )
  }

  abs(
    p1 -
      p0
  ) /
    pooled_sd
}

prepare_eicu_s5 <- function(
  eicu_full
) {

  required <- c(
    "eligible_at_60h_flag",
    "primary_analysis_flag",
    "primary_exposure_code",

    "reference_scr",
    "reference_scr_missing_flag",
    "scr_measurement_n_0_48h",
    "peak_aki_stage_0_48h",
    "assessment_scr_36_60h",

    "age",
    "female_flag",
    "ckd_flag",
    "heart_failure_flag",
    "diabetes_flag",
    "chronic_liver_disease_flag",
    "malignancy_flag",
    "standard_nonrenal_sofa_24h",
    "imv_any_0_24h",
    "vasopressor_any_0_24h",
    "hospital_death_after_60h"
  )

  assert_required_columns(
    eicu_full,
    required,
    "eICU-CRD full landmark cohort"
  )

  if (
    any(
      is.na(
        eicu_full$eligible_at_60h_flag
      )
    ) ||
      any(
        eicu_full$eligible_at_60h_flag != 1L
      )
  ) {
    stop(
      "eICU full clean RDS should contain only patients alive and still hospitalized at ICU +60 h."
    )
  }

  check_expected_value(
    nrow(eicu_full),
    EXPECTED_EICU_S5$landmark_n,
    "eICU S5 landmark cohort N"
  )

  eicu_full <- eicu_full %>%
    mutate(
      classifiable_flag =
        as.integer(
          primary_analysis_flag == 1L &
            primary_exposure_code %in%
            0:2
        ),

      classifiability_group =
        factor(
          ifelse(
            classifiable_flag == 1L,
            "Classifiable trajectory",
            "Unclassifiable trajectory"
          ),
          levels = c(
            "Classifiable trajectory",
            "Unclassifiable trajectory"
          )
        )
    )

  classifiable_n <-
    sum(
      eicu_full$classifiable_flag == 1L
    )

  unclassifiable_n <-
    sum(
      eicu_full$classifiable_flag == 0L
    )

  check_expected_value(
    classifiable_n,
    EXPECTED_EICU_S5$classifiable_n,
    "eICU S5 classifiable N"
  )

  check_expected_value(
    unclassifiable_n,
    EXPECTED_EICU_S5$unclassifiable_n,
    "eICU S5 unclassifiable N"
  )

  # Reconstruct the fixed-reference exclusion reason exactly in the same
  # hierarchy used by the revised Stage 2 SQL.
  eicu_full <- eicu_full %>%
    mutate(
      unclassifiable_reason =
        case_when(
          classifiable_flag == 1L ~
            NA_character_,

          is.na(reference_scr) |
            reference_scr_missing_flag == 1L ~
            "Missing reference sCr",

          scr_measurement_n_0_48h == 0 ~
            "Reference available but no sCr during ICU hours 0-48",

          is.na(
            peak_aki_stage_0_48h
          ) ~
            "Missing fixed-reference AKI stage",

          peak_aki_stage_0_48h >= 1 &
            is.na(
              assessment_scr_36_60h
            ) ~
            "Early AKI without valid 36-60 h assessment sCr",

          TRUE ~
            "Other"
        )
    )

  reason_counts <- eicu_full %>%
    filter(
      classifiable_flag == 0L
    ) %>%
    count(
      unclassifiable_reason,
      name = "N"
    ) %>%
    mutate(
      Percent =
        100 *
        N /
        sum(N)
    ) %>%
    arrange(
      desc(N)
    )

  reason_lookup <- function(reason) {
    z <- reason_counts$N[
      reason_counts$unclassifiable_reason ==
        reason
    ]

    if (
      length(z) == 0L
    ) {
      return(0L)
    }

    as.integer(
      z[[1]]
    )
  }

  check_expected_value(
    reason_lookup(
      "Missing reference sCr"
    ),
    EXPECTED_EICU_S5$missing_reference_n,
    "eICU S5 missing reference"
  )

  check_expected_value(
    reason_lookup(
      "Reference available but no sCr during ICU hours 0-48"
    ),
    EXPECTED_EICU_S5$no_scr_0_48_n,
    "eICU S5 no sCr 0-48 h"
  )

  check_expected_value(
    reason_lookup(
      "Missing fixed-reference AKI stage"
    ),
    EXPECTED_EICU_S5$missing_fixed_stage_n,
    "eICU S5 missing fixed-reference AKI stage"
  )

  check_expected_value(
    reason_lookup(
      "Early AKI without valid 36-60 h assessment sCr"
    ),
    EXPECTED_EICU_S5$missing_assessment_n,
    "eICU S5 missing 36-60 h assessment"
  )

  check_expected_value(
    reason_lookup(
      "Other"
    ),
    EXPECTED_EICU_S5$other_n,
    "eICU S5 other unclassifiable reason"
  )

  list(
    data = eicu_full,
    reasons = reason_counts
  )
}

build_s5_table <- function(
  eicu_full_prepared
) {

  data <-
    eicu_full_prepared$data

  classifiable <-
    data$classifiable_flag == 1L

  unclassifiable <-
    data$classifiable_flag == 0L

  table_spec <- list(
    list(
      variable =
        "Age, years",
      column =
        "age",
      type =
        "continuous",
      digits =
        0L
    ),

    list(
      variable =
        "Female sex, n (%)",
      column =
        "female_flag",
      type =
        "binary"
    ),

    list(
      variable =
        "Chronic kidney disease, n (%)",
      column =
        "ckd_flag",
      type =
        "binary"
    ),

    list(
      variable =
        "Heart failure, n (%)",
      column =
        "heart_failure_flag",
      type =
        "binary"
    ),

    list(
      variable =
        "Diabetes mellitus, n (%)",
      column =
        "diabetes_flag",
      type =
        "binary"
    ),

    list(
      variable =
        "Chronic liver disease, n (%)",
      column =
        "chronic_liver_disease_flag",
      type =
        "binary"
    ),

    list(
      variable =
        "Malignancy, n (%)",
      column =
        "malignancy_flag",
      type =
        "binary"
    ),

    list(
      variable =
        "Nonrenal SOFA score during the first 24 h",
      column =
        "standard_nonrenal_sofa_24h",
      type =
        "continuous",
      digits =
        0L
    ),

    list(
      variable =
        "IMV during ICU hours 0-24, n (%)",
      column =
        "imv_any_0_24h",
      type =
        "binary"
    ),

    list(
      variable =
        "Vasopressor use during ICU hours 0-24, n (%)",
      column =
        "vasopressor_any_0_24h",
      type =
        "binary"
    ),

    list(
      variable =
        "In-hospital mortality after the 60-hour landmark, n (%)",
      column =
        "hospital_death_after_60h",
      type =
        "binary"
    )
  )

  rows <- vector(
    "list",
    length(
      table_spec
    )
  )

  missingness_rows <- list()

  for (
    i in seq_along(
      table_spec
    )
  ) {

    item <-
      table_spec[[i]]

    x <-
      data[[item$column]]

    x1 <-
      x[classifiable]

    x0 <-
      x[unclassifiable]

    if (
      identical(
        item$type,
        "continuous"
      )
    ) {

      display_1 <-
        format_median_iqr(
          x1,
          digits =
            item$digits
        )

      display_0 <-
        format_median_iqr(
          x0,
          digits =
            item$digits
        )

      smd <-
        smd_continuous(
          x1,
          x0
        )

    } else {

      display_1 <-
        format_binary(
          x1,
          digits = 1L
        )

      display_0 <-
        format_binary(
          x0,
          digits = 1L
        )

      smd <-
        smd_binary(
          x1,
          x0
        )
    }

    rows[[i]] <- tibble(
      Variable =
        item$variable,

      `Classifiable trajectory (N = 7,738)` =
        display_1,

      `Unclassifiable trajectory (N = 757)` =
        display_0,

      `Absolute SMD` =
        round(
          smd,
          3
        )
    )

    missingness_rows[[length(
      missingness_rows
    ) + 1L]] <- tibble(
      Variable =
        item$variable,

      Group =
        "Classifiable trajectory",

      N =
        sum(
          classifiable
        ),

      Missing_n =
        sum(
          is.na(
            x1
          )
        ),

      Missing_percent =
        100 *
        mean(
          is.na(
            x1
          )
        )
    )

    missingness_rows[[length(
      missingness_rows
    ) + 1L]] <- tibble(
      Variable =
        item$variable,

      Group =
        "Unclassifiable trajectory",

      N =
        sum(
          unclassifiable
        ),

      Missing_n =
        sum(
          is.na(
            x0
          )
        ),

      Missing_percent =
        100 *
        mean(
          is.na(
            x0
          )
        )
    )
  }

  list(
    table =
      bind_rows(rows),

    missingness =
      bind_rows(
        missingness_rows
      ),

    reasons =
      eicu_full_prepared$reasons %>%
      filter(
        N > 0L
      )
  )
}

# ==============================================================================
# 4. Load the 60-hour eICU landmark cohort prepared by R Script 01
# ==============================================================================
if (!file.exists(EICU_FULL_RDS)) {
  stop("Missing input: ", EICU_FULL_RDS,
       "\nRun R/01_import_harmonize_qc.R first, using the complete landmark cohort.")
}
eicu_full <- readRDS(EICU_FULL_RDS)
assert_valid_landmark_data(eicu_full)

# Optional cross-check against the primary dataset produced by R01; this does
# not influence classifiability assignment or the subsequent summaries.
if (file.exists(EICU_PRIMARY_RDS)) {
  eicu_primary <- readRDS(EICU_PRIMARY_RDS)
  if (!"patientunitstayid" %in% names(eicu_primary)) {
    stop("Primary eICU RDS lacks patientunitstayid.")
  }
  flagged <- eicu_full$patientunitstayid[eicu_full$primary_analysis_flag == 1L]
  if (nrow(eicu_primary) != length(flagged) ||
      !setequal(as.character(eicu_primary$patientunitstayid), as.character(flagged))) {
    stop("Primary RDS stay ID set disagrees with classifiable stays in full RDS.")
  }
  rm(eicu_primary)
}

# ==============================================================================
# 5. Run final manuscript Supplementary Table S5
# ==============================================================================
eicu_s5_prepared <- prepare_eicu_s5(eicu_full)
s5 <- build_s5_table(eicu_s5_prepared)

# Outcome and key covariates are not modified; the groups are defined solely by
# the original primary_analysis_flag/primary_exposure_code combination.
classifiable_group <- eicu_s5_prepared$data$classifiable_flag == 1L
cohort_qc <- tibble::tibble(
  Metric = c(
    "Landmark stays", "Classifiable", "Unclassifiable",
    "Classifiable deaths", "Unclassifiable deaths",
    "Classifiable primary groups 0/1/2"
  ),
  Observed = c(
    nrow(eicu_full),
    sum(classifiable_group),
    sum(!classifiable_group),
    sum(eicu_full$hospital_death_after_60h[classifiable_group] == 1L, na.rm = TRUE),
    sum(eicu_full$hospital_death_after_60h[!classifiable_group] == 1L, na.rm = TRUE),
    paste(vapply(0:2, function(k) sum(eicu_full$primary_exposure_code[classifiable_group] == k), integer(1)), collapse = "/")
  ),
  Expected = c("8495", "7738", "757", "709", "63", "6644/465/629")
) %>%
  dplyr::mutate(Pass = as.character(Observed) == Expected)

if (any(!cohort_qc$Pass)) {
  msg <- paste("Manuscript S5 cohort/outcome QC mismatches:",
               paste(cohort_qc$Metric[!cohort_qc$Pass], collapse = ", "))
  if (isTRUE(STOP_ON_COUNT_MISMATCH)) stop(msg) else warning(msg)
}

# Published values are QC targets only; they are NEVER used to calculate rows.
# The table is independently computed from eicu_full_clean.rds above.
published_S5 <- tibble::tibble(
  Variable = c(
    "Age, years", "Female sex, n (%)", "Chronic kidney disease, n (%)",
    "Heart failure, n (%)", "Diabetes mellitus, n (%)",
    "Chronic liver disease, n (%)", "Malignancy, n (%)",
    "Nonrenal SOFA score during the first 24 h",
    "IMV during ICU hours 0-24, n (%)",
    "Vasopressor use during ICU hours 0-24, n (%)",
    "In-hospital mortality after the 60-hour landmark, n (%)"
  ),
  Classifiable_expected = c(
    "70 (61-78)", "3941 (50.9)", "1411 (18.2)", "2949 (38.1)",
    "2604 (33.7)", "215 (2.8)", "1364 (17.6)", "2 (1-4)",
    "1481 (19.1)", "779 (10.1)", "709 (9.2)"
  ),
  Unclassifiable_expected = c(
    "69 (61-78)", "384 (50.7)", "123 (16.2)", "289 (38.2)",
    "222 (29.3)", "13 (1.7)", "138 (18.2)", "1 (0-3)",
    "79 (10.4)", "16 (2.1)", "63 (8.3)"
  ),
  SMD_expected = c(
    .010, .004, .053, .001, .093, .072, .016, .539, .247, .337, .030
  )
)

# Normalization is for comparing typographic hyphen/en-dash in printed tables.
normalize_iqr_display <- function(x) {
  gsub("[\u2010\u2011\u2012\u2013\u2014\u2212]", "-", as.character(x), perl = TRUE)
}
class_col <- grep("^Classifiable trajectory \\(N", names(s5$table), value = TRUE)
unclass_col <- grep("^Unclassifiable trajectory \\(N", names(s5$table), value = TRUE)
if (length(class_col) != 1L || length(unclass_col) != 1L) {
  stop("Unexpected table column names: classifiable/unclassifiable display columns not found.")
}

published_qc <- published_S5 %>%
  dplyr::left_join(
    s5$table %>% dplyr::transmute(
      Variable,
      Classifiable_observed = .data[[class_col]],
      Unclassifiable_observed = .data[[unclass_col]],
      SMD_observed = .data[["Absolute SMD"]]
    ), by = "Variable"
  ) %>%
  dplyr::mutate(
    Group1_match = !is.na(Classifiable_observed) &
      normalize_iqr_display(Classifiable_expected) ==
      normalize_iqr_display(Classifiable_observed),
    Group2_match = !is.na(Unclassifiable_observed) &
      normalize_iqr_display(Unclassifiable_expected) ==
      normalize_iqr_display(Unclassifiable_observed),
    SMD_match = !is.na(SMD_observed) &
      abs(SMD_expected - SMD_observed) < 0.0000001,
    All_match = Group1_match & Group2_match & SMD_match
  )
if (!all(published_qc$All_match)) {
  warning(
    "One or more S5 rows differ from the manuscript. Inspect ",
    "TableS5_published_values_QC.csv after the script completes; ",
    "the script has NOT altered any results to force agreement."
  )
}

# ==============================================================================
# 6. Export aggregate results; no individual-level output
# ==============================================================================
write_csv_bom <- function(data, filename) {
  readr::write_excel_csv(data, file.path(OUTPUT_DIR, filename), na = "")
}

write_csv_bom(s5$table, "TableS5_eicu_classifiability.csv")
write_csv_bom(s5$reasons, "TableS5_unclassifiable_reasons_QC.csv")
write_csv_bom(s5$missingness, "TableS5_missingness_QC.csv")
write_csv_bom(published_qc, "TableS5_published_values_QC.csv")
write_csv_bom(cohort_qc, "TableS5_cohort_counts_QC.csv")

# The workbook is optional so the main CSVs can be reproduced with fewer
# dependencies; if openxlsx exists, bundle the same aggregate tables for Excel.
if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  sheets <- list(
    "TableS5" = s5$table,
    "Unclassifiable_reasons" = s5$reasons,
    "Missingness_QC" = s5$missingness,
    "Published_values_QC" = published_qc,
    "Cohort_counts_QC" = cohort_qc
  )
  for (sheet_name in names(sheets)) {
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet_name, sheets[[sheet_name]])
    openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(sheets[[sheet_name]])), widths = "auto")
  }
  openxlsx::saveWorkbook(
    wb, file.path(OUTPUT_DIR, "TableS5_eicu_classifiability.xlsx"),
    overwrite = TRUE
  )
} else {
  message("Optional 'openxlsx' not installed; S5 CSV files were generated; skipping XLSX.")
}

manifest <- tibble::tibble(
  Item = c(
    "script", "manuscript_table", "database", "input_rds", "landmark_n",
    "classifiable_n", "unclassifiable_n", "descriptive_variables_n",
    "continuous_summary", "binary_summary", "absolute_smd_method",
    "missingness_denominator", "p_values", "r_version"
  ),
  Value = c(
    "04_eicu_trajectory_classifiability.R",
    "Supplementary Table S5", "eICU-CRD v2.0", EICU_FULL_RDS,
    as.character(nrow(eicu_full)),
    as.character(sum(classifiable_group)),
    as.character(sum(!classifiable_group)),
    as.character(nrow(s5$table)),
    "median (Q1-Q3), R quantile type 7; continuous SMD uses means and variances",
    "n (%) among nonmissing observations",
    "original absolute standardized mean difference, continuous and binary",
    "within comparison group", "None", as.character(getRversion())
  )
)
write_csv_bom(manifest, "analysis_manifest_S5.csv")
writeLines(
  capture.output(sessionInfo()),
  con = file.path(OUTPUT_DIR, "sessionInfo_S5.txt"), useBytes = TRUE
)

cat("\n====================================================\n")
cat("Final Supplementary Table S5: eICU trajectory classifiability\n")
cat("Landmark N: ", nrow(eicu_full), "\n", sep = "")
cat("Classifiable: ", sum(classifiable_group), "\n", sep = "")
cat("Unclassifiable: ", sum(!classifiable_group), "\n", sep = "")
cat("Comparison variables: ", nrow(s5$table), "\n", sep = "")
cat("Manuscript comparison rows matching: ",
    sum(published_qc$All_match), "/", nrow(published_qc), "\n", sep = "")
cat("Output directory: ", OUTPUT_DIR, "\n", sep = "")
cat("====================================================\n\n")
print(s5$table, n = nrow(s5$table), width = Inf)
cat("\nUnclassifiable reasons:\n")
print(s5$reasons)
