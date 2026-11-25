# ==============================================================================
# COPD early creatinine-defined AKI trajectory study
# R/02_baseline_tables.R
# MIMIC-IV v3.1 and eICU-CRD v2.0 | Manuscript Tables 1 and 2
#
# Based on the author's original:
#   02_baseline_tables_revised_fixed_reference_v2.R
#
# The original descriptive estimators are retained:
#   - age: mean (SD)
#   - CCI, nonrenal SOFA, first-day hemoglobin and BUN: median (IQR)
#   - binary characteristics: n (%) of observed values
#   - continuous SMD: mean difference / equal-weight pooled SD
#   - binary SMD: difference in proportions / equal-weight pooled binomial SD
#   - Max SMD: maximum absolute pairwise SMD (0 vs 1, 0 vs 2, 1 vs 2)
#
# Completes the final manuscript Tables 1/2 compared with the original R02:
#   - five-category race/ethnicity rows and their Max SMD;
#   - descriptive hemoglobin and BUN rows;
#   - mortality after the +60-h landmark as the final row (no SMD);
#   - matches the final manuscript's ordering and displayed precision.
# These additions are DESCRIPTIVE ONLY; they do not alter model covariates.
#
# Input: files produced by R/01_import_harmonize_qc.R
#   output/R_analysis_output/mimic_primary_clean.rds
#   output/R_analysis_output/eicu_primary_clean.rds
# Run from repository root:
#   Rscript R/02_baseline_tables.R
# Or set COPD_AKI_PROJECT_ROOT=/path/to/COPD-AKI-Trajectory
#
# Outputs (aggregate only, under output/R_analysis_output/02_baseline_tables/):
#   Table1_MIMIC_baseline.csv
#   Table2_eICU_baseline.csv
#   baseline_summary_long.csv
#   baseline_pairwise_SMD.csv
#   baseline_missingness.csv
#   outcome_by_trajectory.csv
#   race_source_mapping.csv
#   published_reference_comparison.csv
#   baseline_table_manifest.csv
#
# Confidentiality: do not upload primary RDS files or patient-level SQL exports.
# This script writes only aggregate summaries. It does not install packages.
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)

# ------------------------------------------------------------------------------
# 0. Portable input/output paths
# ------------------------------------------------------------------------------
project_root_env <- Sys.getenv("COPD_AKI_PROJECT_ROOT", unset = "")
PROJECT_ROOT <- normalizePath(
  if (nzchar(project_root_env)) project_root_env else ".",
  winslash = "/", mustWork = TRUE
)
SCRIPT01_OUTPUT_DIR <- file.path(PROJECT_ROOT, "output", "R_analysis_output")
OUTPUT_DIR <- file.path(SCRIPT01_OUTPUT_DIR, "02_baseline_tables")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

MIMIC_RDS <- file.path(SCRIPT01_OUTPUT_DIR, "mimic_primary_clean.rds")
EICU_RDS <- file.path(SCRIPT01_OUTPUT_DIR, "eicu_primary_clean.rds")

# The published results are treated as external QC references, NOT as data.
STOP_ON_FROZEN_GROUP_MISMATCH <- TRUE
WARN_ON_PUBLISHED_REFERENCE_MISMATCH <- TRUE

EXPECTED_GROUP_N <- list(
  "MIMIC-IV" = c(`0` = 5796L, `1` = 898L, `2` = 933L),
  "eICU-CRD" = c(`0` = 6644L, `1` = 465L, `2` = 629L)
)
EXPECTED_GROUP_DEATHS <- list(
  "MIMIC-IV" = c(`0` = 483L, `1` = 115L, `2` = 234L),
  "eICU-CRD" = c(`0` = 514L, `1` = 63L, `2` = 132L)
)
EXPECTED_AECOPD_N <- c("MIMIC-IV" = 1492L, "eICU-CRD" = 2946L)
EXPECTED_RACE_MAX_SMD <- c("MIMIC-IV" = 0.130, "eICU-CRD" = 0.140)

# ------------------------------------------------------------------------------
# 1. Packages (no automatic installation)
# ------------------------------------------------------------------------------
required_packages <- c("dplyr", "readr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Required R package(s) not installed: ",
       paste(missing_packages, collapse = ", "),
       ". Install packages in your R environment or restore renv.lock.")
}
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

# ------------------------------------------------------------------------------
# 2. Specifications: EXACT manuscript row order
# ------------------------------------------------------------------------------
group_labels <- c(
  `0` = "No early AKI",
  `1` = "Resolved early AKI",
  `2` = "Ongoing early AKI"
)
group_scopes <- c("Overall", "0", "1", "2")
race_levels <- c("Asian", "Black", "Hispanic/Latino", "White", "Other/Unknown")

# Race is inserted between female sex and CCI as a 6-row categorical block.
# Mortality is inserted after vasopressor, with SMD shown as an em dash.
baseline_spec <- tibble::tribble(
  ~variable_name,               ~label,                                                   ~type,         ~display,       ~digits,
  "age",                        "Age, years",                                           "continuous", "mean_sd",      1L,
  "female_flag",                "Female sex, n (%)",                                    "binary",     "n_percent",    1L,
  "charlson_comorbidity_index", "Charlson Comorbidity Index",                           "continuous", "median_iqr",   1L,
  "standard_nonrenal_sofa_24h", "Nonrenal SOFA score during the first 24 h",            "continuous", "median_iqr",   1L,
  "aecopd_analysis_flag",       "AECOPD diagnosis, n (%)",                             "binary",     "n_percent",    1L,
  "ckd_flag",                   "Chronic kidney disease, n (%)",                        "binary",     "n_percent",    1L,
  "heart_failure_flag",         "Heart failure, n (%)",                                 "binary",     "n_percent",    1L,
  "diabetes_flag",              "Diabetes mellitus, n (%)",                             "binary",     "n_percent",    1L,
  "chronic_liver_disease_flag", "Chronic liver disease, n (%)",                         "binary",     "n_percent",    1L,
  "malignancy_flag",            "Malignancy, n (%)",                                     "binary",     "n_percent",    1L,
  "hemoglobin_first_24h",      "Hemoglobin within the first 24 h, g/dL",               "continuous", "median_iqr",   1L,
  "bun_first_24h",             "Blood urea nitrogen within the first 24 h, mg/dL",      "continuous", "median_iqr",   0L,
  "imv_any_0_24h",              "IMV during ICU hours 0\u201324, n (%)",                     "binary",     "n_percent",    1L,
  "vasopressor_any_0_24h",      "Vasopressor use during ICU hours 0\u201324, n (%)",         "binary",     "n_percent",    1L
)

primary_model_vars <- c(
  "hospital_death_after_60h", "primary_exposure_code", "age",
  "female_flag", "ckd_flag", "heart_failure_flag", "diabetes_flag",
  "chronic_liver_disease_flag", "malignancy_flag",
  "standard_nonrenal_sofa_24h"
)
required_common <- unique(c(
  "primary_analysis_flag", "primary_exposure_code", "hospital_death_after_60h",
  baseline_spec$variable_name
))
binary_vars <- c(
  baseline_spec$variable_name[baseline_spec$type == "binary"],
  "hospital_death_after_60h"
)
continuous_vars <- baseline_spec$variable_name[baseline_spec$type == "continuous"]
pair_matrix <- matrix(c(0, 1, 0, 2, 1, 2), ncol = 2, byrow = TRUE)

# ------------------------------------------------------------------------------
# 3. Original descriptive formatting and SMD functions
# ------------------------------------------------------------------------------
assert_file_exists <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path,
                               ". Run R/01_import_harmonize_qc.R first.")
}
assert_columns <- function(data, columns, database) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) stop(database, " is missing required variables: ",
                            paste(missing, collapse = ", "),
                            ". Check R01 and the required SQL exports.")
}
assert_binary <- function(x, variable, database) {
  observed <- sort(unique(x[!is.na(x)]))
  invalid <- setdiff(observed, c(0, 1))
  if (length(invalid)) stop(database, " invalid ", variable,
                            ": ", paste(invalid, collapse = ", "))
}
assert_primary_dataset <- function(data, database, stay_key) {
  assert_columns(data, c(required_common, stay_key), database)
  if (anyNA(data[[stay_key]]) || anyDuplicated(data[[stay_key]])) {
    stop(database, ": missing/duplicated stay identifiers in primary RDS.")
  }
  if (anyNA(data$primary_analysis_flag) ||
      any(data$primary_analysis_flag != 1L)) {
    stop(database, ": input must contain primary_analysis_flag = 1 only.")
  }
  if (anyNA(data$primary_exposure_code) ||
      any(!data$primary_exposure_code %in% 0:2)) {
    stop(database, ": exposure codes must be 0, 1, 2, with no missing values.")
  }
  for (v in continuous_vars) {
    if (!is.numeric(data[[v]])) {
      stop(database, ": variable ", v,
           " is not numeric. Check type conversion in R01.")
    }
  }
  for (v in binary_vars) assert_binary(data[[v]], v, database)

  observed <- vapply(0:2, function(g) sum(data$primary_exposure_code == g), integer(1))
  names(observed) <- as.character(0:2)
  expected <- EXPECTED_GROUP_N[[database]]
  if (!identical(unname(observed), unname(expected))) {
    msg <- paste0(database, " frozen group-count mismatch. Observed: ",
                  paste(observed, collapse = "/"), " expected: ",
                  paste(expected, collapse = "/"))
    if (STOP_ON_FROZEN_GROUP_MISMATCH) stop(msg) else warning(msg)
  }
  if (anyNA(data$hospital_death_after_60h)) {
    stop(database, ": hospital death outcome contains missing values.")
  }
  invisible(TRUE)
}

safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2L) return(NA_real_)
  stats::sd(x)
}
safe_quantile <- function(x, p) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  unname(stats::quantile(x, probs = p, type = 7))
}
format_mean_sd <- function(x, digits = 1L) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  paste0(formatC(mean(x), format = "f", digits = digits), " (",
         if (length(x) > 1L) formatC(stats::sd(x), format = "f", digits = digits)
         else "NA", ")")
}
format_median_iqr <- function(x, digits = 1L) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  q <- stats::quantile(x, probs = c(0.25, 0.50, 0.75),
                       names = FALSE, type = 7)
  paste0(formatC(q[2], format = "f", digits = digits), " (",
         formatC(q[1], format = "f", digits = digits), "\u2013",
         formatC(q[3], format = "f", digits = digits), ")")
}
format_n_percent <- function(x, digits = 1L) {
  observed <- x[!is.na(x)]
  if (!length(observed)) return(NA_character_)
  n_yes <- sum(observed == 1)
  paste0(n_yes, " (",
         formatC(100 * n_yes / length(observed), format = "f", digits = digits), ")")
}
continuous_smd <- function(x1, x2) {
  x1 <- x1[!is.na(x1)]
  x2 <- x2[!is.na(x2)]
  if (length(x1) < 2L || length(x2) < 2L) return(NA_real_)
  s1 <- stats::sd(x1)
  s2 <- stats::sd(x2)
  pooled <- sqrt((s1^2 + s2^2) / 2)
  if (is.na(pooled)) return(NA_real_)
  if (pooled == 0) return(if (isTRUE(all.equal(mean(x1), mean(x2)))) 0 else Inf)
  (mean(x1) - mean(x2)) / pooled
}
binary_smd <- function(x1, x2) {
  x1 <- x1[!is.na(x1)]
  x2 <- x2[!is.na(x2)]
  if (!length(x1) || !length(x2)) return(NA_real_)
  p1 <- mean(x1 == 1)
  p2 <- mean(x2 == 1)
  pooled <- sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2)
  if (pooled == 0) return(if (isTRUE(all.equal(p1, p2))) 0 else Inf)
  (p1 - p2) / pooled
}
safe_max_abs <- function(x) {
  if (!length(x) || all(is.na(x))) return(NA_real_)
  max(abs(x), na.rm = TRUE)
}
format_smd <- function(x) {
  if (length(x) != 1L || is.na(x)) return("")
  formatC(x, format = "f", digits = 3L)
}

scope_mask <- function(data, scope) {
  if (scope == "Overall") rep(TRUE, nrow(data))
  else data$primary_exposure_code == as.integer(scope)
}
scope_label <- function(scope) {
  if (scope == "Overall") "Overall" else unname(group_labels[scope])
}

# ------------------------------------------------------------------------------
# 4. Harmonize descriptive race/ethnicity from the two SQL sources
# ------------------------------------------------------------------------------
# MIMIC: admissions.race; eICU: patient.ethnicity.
# Five manuscript categories, with native/other/unknown/missing pooled into
# Other/Unknown. An exact aggregate source-to-category mapping is exported.
# These categories are NOT used as primary-model adjustment variables.
recode_race_five <- function(x) {
  raw <- as.character(x)
  value <- toupper(trimws(ifelse(is.na(raw), "", raw)))
  value <- gsub("[[:space:]]+", " ", value)
  out <- rep("Other/Unknown", length(value))

  # Order prevents a compound label such as "HISPANIC/LATINO - WHITE"
  # from being assigned to White instead of Hispanic/Latino.
  is_hispanic <- grepl("HISPANIC|LATIN", value)
  is_asian <- !is_hispanic & grepl("ASIAN", value)
  is_black <- !is_hispanic & !is_asian & grepl("BLACK|AFRICAN", value)
  is_white <- !is_hispanic & !is_asian & !is_black &
    grepl("WHITE|CAUCASIAN", value)

  out[is_hispanic] <- "Hispanic/Latino"
  out[is_asian] <- "Asian"
  out[is_black] <- "Black"
  out[is_white] <- "White"
  factor(out, levels = race_levels)
}

make_race_source_mapping <- function(data, database, field) {
  raw <- ifelse(is.na(data[[field]]) | trimws(as.character(data[[field]])) == "",
                "<missing>", as.character(data[[field]]))
  dplyr::count(
    tibble::tibble(database = database,
                   source_field = field,
                   source_value = raw,
                   manuscript_race_category = as.character(data$race_five)),
    database, source_field, source_value, manuscript_race_category,
    name = "n", sort = TRUE
  )
}

# ------------------------------------------------------------------------------
# 5. Load the two primary RDS files made by R01
# ------------------------------------------------------------------------------
assert_file_exists(MIMIC_RDS, "MIMIC primary RDS")
assert_file_exists(EICU_RDS, "eICU primary RDS")
mimic <- readRDS(MIMIC_RDS)
eicu <- readRDS(EICU_RDS)
assert_primary_dataset(mimic, "MIMIC-IV", "stay_id")
assert_primary_dataset(eicu, "eICU-CRD", "patientunitstayid")
assert_columns(mimic, "race", "MIMIC-IV")
assert_columns(eicu, "ethnicity", "eICU-CRD")

mimic$race_five <- recode_race_five(mimic$race)
eicu$race_five <- recode_race_five(eicu$ethnicity)
race_source_mapping <- dplyr::bind_rows(
  make_race_source_mapping(mimic, "MIMIC-IV", "race"),
  make_race_source_mapping(eicu, "eICU-CRD", "ethnicity")
)

for (database in c("MIMIC-IV", "eICU-CRD")) {
  dat <- if (database == "MIMIC-IV") mimic else eicu
  missing_in_model <- vapply(dat[primary_model_vars],
                             function(x) sum(is.na(x)), integer(1))
  if (any(missing_in_model > 0L)) {
    warning(database, ": missing primary-model variables: ",
            paste(names(missing_in_model)[missing_in_model > 0L],
                  missing_in_model[missing_in_model > 0L], sep = "=", collapse = ", "))
  }
}

# ------------------------------------------------------------------------------
# 6. Baseline summaries in original author's statistical specification
# ------------------------------------------------------------------------------
analyse_baseline <- function(data, database) {
  n_total <- nrow(data)
  n_by_group <- vapply(0:2,
                       function(g) sum(data$primary_exposure_code == g), integer(1))
  col_names <- c(
    paste0("Overall (N = ", n_total, ")"),
    paste0("No early AKI (N = ", n_by_group[1], ")"),
    paste0("Resolved early AKI (N = ", n_by_group[2], ")"),
    paste0("Ongoing early AKI (N = ", n_by_group[3], ")")
  )

  descriptive_rows <- list()
  pairwise_rows <- list()
  wide_rows <- list()

  for (j in seq_len(nrow(baseline_spec))) {
    var <- baseline_spec$variable_name[j]
    label <- baseline_spec$label[j]
    type <- baseline_spec$type[j]
    display_rule <- baseline_spec$display[j]
    digits <- baseline_spec$digits[j]
    position <- if (j <= 2L) as.numeric(j) else as.numeric(j + 1L)
    displays <- setNames(rep(NA_character_, 4L), group_scopes)

    for (scope in group_scopes) {
      x <- data[[var]][scope_mask(data, scope)]
      total <- length(x)
      observed <- sum(!is.na(x))
      n_yes <- if (type == "binary" && observed > 0L) sum(x == 1, na.rm = TRUE)
               else NA_integer_
      pct_yes <- if (type == "binary" && observed > 0L) 100 * n_yes / observed
                 else NA_real_
      displays[scope] <- if (display_rule == "mean_sd") {
        format_mean_sd(x, digits)
      } else if (display_rule == "median_iqr") {
        format_median_iqr(x, digits)
      } else {
        format_n_percent(x, digits)
      }
      descriptive_rows[[length(descriptive_rows) + 1L]] <- tibble::tibble(
        database = database,
        variable_order = position,
        variable_name = var,
        variable = label,
        category = NA_character_,
        variable_type = type,
        display_rule = display_rule,
        group_code = scope,
        group = scope_label(scope),
        N_total = total,
        N_observed = observed,
        N_missing = total - observed,
        N_yes = n_yes,
        percent_yes = pct_yes,
        mean = if (type == "continuous") safe_mean(x) else NA_real_,
        SD = if (type == "continuous") safe_sd(x) else NA_real_,
        median = if (type == "continuous") safe_quantile(x, 0.5) else NA_real_,
        Q1 = if (type == "continuous") safe_quantile(x, 0.25) else NA_real_,
        Q3 = if (type == "continuous") safe_quantile(x, 0.75) else NA_real_,
        display = displays[scope]
      )
    }

    smds <- numeric(nrow(pair_matrix))
    for (i in seq_len(nrow(pair_matrix))) {
      g1 <- pair_matrix[i, 1]
      g2 <- pair_matrix[i, 2]
      x1 <- data[[var]][data$primary_exposure_code == g1]
      x2 <- data[[var]][data$primary_exposure_code == g2]
      signed <- if (type == "continuous") continuous_smd(x1, x2)
                else binary_smd(x1, x2)
      smds[i] <- signed
      pairwise_rows[[length(pairwise_rows) + 1L]] <- tibble::tibble(
        database = database,
        variable_order = position,
        variable_name = var,
        variable = label,
        category = NA_character_,
        variable_type = type,
        group_1_code = g1,
        group_1 = unname(group_labels[as.character(g1)]),
        group_2_code = g2,
        group_2 = unname(group_labels[as.character(g2)]),
        signed_SMD = signed,
        absolute_SMD = abs(signed)
      )
    }
    wide_rows[[length(wide_rows) + 1L]] <- c(
      label, unname(displays), format_smd(safe_max_abs(smds))
    )
  }

  # Race rows: denominator = all patients, including Other/Unknown.
  race_wide_rows <- list()
  race_pairwise <- list()
  race_long <- list()
  race_smds <- numeric()
  for (k in seq_along(race_levels)) {
    level <- race_levels[k]
    displays <- setNames(rep(NA_character_, length(group_scopes)), group_scopes)
    for (scope in group_scopes) {
      x <- data$race_five[scope_mask(data, scope)]
      n <- length(x)
      n_yes <- sum(x == level)
      displays[scope] <- paste0(n_yes, " (",
                                formatC(100 * n_yes / n, format = "f", digits = 1L), ")")
      race_long[[length(race_long) + 1L]] <- tibble::tibble(
        database = database,
        variable_order = 3 + k / 100,
        variable_name = "race_five",
        variable = paste0("  ", level),
        category = level,
        variable_type = "categorical_level",
        display_rule = "n_percent",
        group_code = scope,
        group = scope_label(scope),
        N_total = n,
        N_observed = n,
        N_missing = 0L,
        N_yes = n_yes,
        percent_yes = 100 * n_yes / n,
        mean = NA_real_, SD = NA_real_, median = NA_real_,
        Q1 = NA_real_, Q3 = NA_real_, display = displays[scope]
      )
    }
    race_wide_rows[[k]] <- c(paste0("  ", level), unname(displays), "")

    # This is a MAXIMUM of the category-specific binary pairwise SMDs,
    # NOT the generalized (Mahalanobis) multi-category SMD. With the final
    # manuscript counts this reproduces Race Max SMD = 0.130 / 0.140.
    for (i in seq_len(nrow(pair_matrix))) {
      g1 <- pair_matrix[i, 1]
      g2 <- pair_matrix[i, 2]
      signed <- binary_smd(
        as.integer(data$race_five[data$primary_exposure_code == g1] == level),
        as.integer(data$race_five[data$primary_exposure_code == g2] == level)
      )
      race_smds <- c(race_smds, signed)
      race_pairwise[[length(race_pairwise) + 1L]] <- tibble::tibble(
        database = database,
        variable_order = 3,
        variable_name = "race_five",
        variable = "Race, n (%)",
        category = level,
        variable_type = "categorical_level",
        group_1_code = g1,
        group_1 = unname(group_labels[as.character(g1)]),
        group_2_code = g2,
        group_2 = unname(group_labels[as.character(g2)]),
        signed_SMD = signed,
        absolute_SMD = abs(signed)
      )
    }
  }
  race_max_smd <- safe_max_abs(race_smds)
  race_header <- c("Race, n (%)", rep("", 4L), format_smd(race_max_smd))

  # The last table row is the post-landmark mortality outcome, NOT a baseline
  # covariate and therefore has NO standardized mean difference.
  mortality_displays <- setNames(rep(NA_character_, length(group_scopes)), group_scopes)
  outcome_long <- list()
  for (scope in group_scopes) {
    x <- data$hospital_death_after_60h[scope_mask(data, scope)]
    mortality_displays[scope] <- format_n_percent(x, 1L)
    outcome_long[[length(outcome_long) + 1L]] <- tibble::tibble(
      database = database,
      variable_order = 18,
      variable_name = "hospital_death_after_60h",
      variable = "In-hospital mortality after the 60-h landmark, n (%)",
      category = NA_character_,
      variable_type = "outcome",
      display_rule = "n_percent",
      group_code = scope,
      group = scope_label(scope),
      N_total = length(x), N_observed = sum(!is.na(x)),
      N_missing = sum(is.na(x)),
      N_yes = sum(x == 1L, na.rm = TRUE),
      percent_yes = 100 * sum(x == 1L, na.rm = TRUE) / sum(!is.na(x)),
      mean = NA_real_, SD = NA_real_, median = NA_real_,
      Q1 = NA_real_, Q3 = NA_real_, display = mortality_displays[scope]
    )
  }
  mortality_row <- c(
    "In-hospital mortality after the 60-h landmark, n (%)",
    unname(mortality_displays), "\u2014"
  )

  # Assemble in exact Table 1 / Table 2 order.
  rows <- c(
    wide_rows[1:2],
    list(race_header), race_wide_rows,
    wide_rows[3:length(wide_rows)],
    list(mortality_row)
  )
  table_out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE,
                             check.names = FALSE)
  names(table_out) <- c("Variable", col_names, "Max SMD")
  rownames(table_out) <- NULL

  # Preserve machine-readable long and per-pair outputs as in original R02.
  all_long <- dplyr::bind_rows(descriptive_rows, race_long, outcome_long) %>%
    dplyr::arrange(variable_order, group_code)
  all_pairwise <- dplyr::bind_rows(pairwise_rows, race_pairwise) %>%
    dplyr::arrange(variable_order, category, group_1_code, group_2_code)
  missingness <- all_long %>%
    dplyr::filter(group_code == "Overall", variable_type != "categorical_level") %>%
    dplyr::transmute(
      database, variable_order, variable_name, variable,
      N_total, N_observed, N_missing,
      missing_pct = 100 * N_missing / N_total
    )
  # Race raw-source missingness is a separate QC concern, since the manuscript
  # explicitly pools it into Other/Unknown, not into missing analysis data.
  race_raw_field <- if (database == "MIMIC-IV") "race" else "ethnicity"
  raw_missing_n <- sum(is.na(data[[race_raw_field]]) |
                         trimws(as.character(data[[race_raw_field]])) == "")
  missingness <- dplyr::bind_rows(missingness, tibble::tibble(
    database = database,
    variable_order = 3,
    variable_name = race_raw_field,
    variable = "Race/ethnicity raw source (before Other/Unknown mapping)",
    N_total = n_total,
    N_observed = n_total - raw_missing_n,
    N_missing = raw_missing_n,
    missing_pct = 100 * raw_missing_n / n_total
  ))
  list(
    table = table_out,
    long = all_long,
    pairwise = all_pairwise,
    missingness = missingness,
    race_max_smd = race_max_smd
  )
}

# ------------------------------------------------------------------------------
# 7. Produce tables and concise outcome summary
# ------------------------------------------------------------------------------
mimic_result <- analyse_baseline(mimic, "MIMIC-IV")
eicu_result <- analyse_baseline(eicu, "eICU-CRD")
all_long <- dplyr::bind_rows(mimic_result$long, eicu_result$long)
all_pairwise <- dplyr::bind_rows(mimic_result$pairwise, eicu_result$pairwise)
all_missingness <- dplyr::bind_rows(mimic_result$missingness, eicu_result$missingness)

# Bind only the outcome fields. The full two-database RDS files have many
# database-specific columns and must not be coerced together for this summary.
outcome_by_trajectory <- dplyr::bind_rows(
  tibble::tibble(
    database = "MIMIC-IV",
    primary_exposure_code = mimic$primary_exposure_code,
    hospital_death_after_60h = mimic$hospital_death_after_60h
  ),
  tibble::tibble(
    database = "eICU-CRD",
    primary_exposure_code = eicu$primary_exposure_code,
    hospital_death_after_60h = eicu$hospital_death_after_60h
  )
) %>%
  dplyr::group_by(database, primary_exposure_code) %>%
  dplyr::summarise(
    n = dplyr::n(),
    deaths = sum(hospital_death_after_60h == 1L, na.rm = TRUE),
    mortality_pct = 100 * deaths / n,
    .groups = "drop"
  ) %>%
  dplyr::mutate(trajectory = unname(group_labels[as.character(primary_exposure_code)])) %>%
  dplyr::select(database, primary_exposure_code, trajectory, n, deaths, mortality_pct)

# ------------------------------------------------------------------------------
# 8. Compare observed aggregates to the final manuscript reference numbers
# ------------------------------------------------------------------------------
# All reference numbers below are frozen, public, aggregate results from the
# author's manuscript. They are NOT used to calculate rows or replace values.
reference_rows <- list()
for (database in c("MIMIC-IV", "eICU-CRD")) {
  dat <- if (database == "MIMIC-IV") mimic else eicu
  result <- if (database == "MIMIC-IV") mimic_result else eicu_result
  for (g in 0:2) {
    idx <- dat$primary_exposure_code == g
    reference_rows[[length(reference_rows) + 1L]] <- tibble::tibble(
      database = database, group_code = as.character(g),
      metric = "group_n", observed = sum(idx),
      published = as.numeric(EXPECTED_GROUP_N[[database]][as.character(g)])
    )
    reference_rows[[length(reference_rows) + 1L]] <- tibble::tibble(
      database = database, group_code = as.character(g),
      metric = "post_landmark_deaths",
      observed = sum(dat$hospital_death_after_60h[idx] == 1L),
      published = as.numeric(EXPECTED_GROUP_DEATHS[[database]][as.character(g)])
    )
  }
  reference_rows[[length(reference_rows) + 1L]] <- tibble::tibble(
    database = database, group_code = "Overall", metric = "aecopd_n",
    observed = sum(dat$aecopd_analysis_flag == 1L, na.rm = TRUE),
    published = as.numeric(EXPECTED_AECOPD_N[database])
  )
  reference_rows[[length(reference_rows) + 1L]] <- tibble::tibble(
    database = database, group_code = "Overall", metric = "race_max_SMD",
    observed = round(result$race_max_smd, 3L),
    published = as.numeric(EXPECTED_RACE_MAX_SMD[database])
  )
}

# The race-level reference counts are a direct, transparent QC of the mapping.
reference_race_counts <- list(
  "MIMIC-IV" = matrix(
    c(114, 543, 134, 5538, 1298,
      88, 402, 97, 4264, 945,
      12, 85, 17, 617, 167,
      14, 56, 20, 657, 186),
    nrow = 5L, ncol = 4L,
    dimnames = list(race_levels, group_scopes)
  ),
  "eICU-CRD" = matrix(
    c(53, 610, 279, 6528, 268,
      48, 501, 243, 5622, 230,
      2, 40, 14, 399, 10,
      3, 69, 22, 507, 28),
    nrow = 5L, ncol = 4L,
    dimnames = list(race_levels, group_scopes)
  )
)
for (database in c("MIMIC-IV", "eICU-CRD")) {
  dat <- if (database == "MIMIC-IV") mimic else eicu
  for (scope in group_scopes) {
    counts <- table(factor(dat$race_five[scope_mask(dat, scope)], levels = race_levels))
    for (category in race_levels) {
      reference_rows[[length(reference_rows) + 1L]] <- tibble::tibble(
        database = database, group_code = scope,
        metric = paste0("race_", category),
        observed = as.numeric(counts[category]),
        published = as.numeric(reference_race_counts[[database]][category, scope])
      )
    }
  }
}
published_reference_comparison <- dplyr::bind_rows(reference_rows) %>%
  dplyr::mutate(
    difference = observed - published,
    matches_published = !is.na(difference) & abs(difference) < 1e-9
  )
if (WARN_ON_PUBLISHED_REFERENCE_MISMATCH &&
    any(!published_reference_comparison$matches_published)) {
  bad <- published_reference_comparison %>% dplyr::filter(!matches_published)
  warning(
    "Some descriptive aggregates differ from manuscript reference values (",
    nrow(bad), " checks). See published_reference_comparison.csv. ",
    "Do not treat a matching cohort N alone as full table replication."
  )
}

# ------------------------------------------------------------------------------
# 9. Write aggregate outputs (no per-patient records)
# ------------------------------------------------------------------------------
write_aggregate <- function(data, name) {
  readr::write_excel_csv(data, file.path(OUTPUT_DIR, name), na = "")
}
write_aggregate(mimic_result$table, "Table1_MIMIC_baseline.csv")
write_aggregate(eicu_result$table, "Table2_eICU_baseline.csv")
write_aggregate(all_long, "baseline_summary_long.csv")
write_aggregate(all_pairwise, "baseline_pairwise_SMD.csv")
write_aggregate(all_missingness, "baseline_missingness.csv")
write_aggregate(outcome_by_trajectory, "outcome_by_trajectory.csv")
write_aggregate(race_source_mapping, "race_source_mapping.csv")
write_aggregate(published_reference_comparison, "published_reference_comparison.csv")

manifest <- tibble::tibble(
  item = c(
    "generated_at", "input_mimic_rds", "input_eicu_rds",
    "mimic_primary_n", "eicu_primary_n", "mimic_groups_0_1_2",
    "eicu_groups_0_1_2", "age_display", "CCI_SOFA_Hb_BUN_display",
    "categorical_display", "SMD_continuous", "SMD_binary",
    "SMD_race", "SMD_maximum", "race_mapping_MIMIC", "race_mapping_eICU",
    "mortality_row", "baseline_variables", "manuscript_tables"
  ),
  value = c(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    MIMIC_RDS, EICU_RDS,
    as.character(nrow(mimic)), as.character(nrow(eicu)),
    paste(EXPECTED_GROUP_N[["MIMIC-IV"]], collapse = "/"),
    paste(EXPECTED_GROUP_N[["eICU-CRD"]], collapse = "/"),
    "mean (SD)", "median (IQR), type-7 quantiles",
    "n (%) of nonmissing observations; unknown race pooled as Other/Unknown",
    "equal-weight pooled group SD", "equal-weight pooled binomial SD",
    "max of 15 binary category-specific pairwise SMD values",
    "maximum absolute pairwise SMD across three AKI trajectory groups",
    "admissions.race", "patient.ethnicity",
    "post-60h in-hospital deaths; SMD not calculated",
    paste(baseline_spec$variable_name, collapse = ", "),
    "MIMIC-IV Table 1; eICU-CRD Table 2"
  )
)
write_aggregate(manifest, "baseline_table_manifest.csv")

# ------------------------------------------------------------------------------
# 10. Console summary
# ------------------------------------------------------------------------------
cat("\n============================================================\n")
cat("Manuscript baseline Table 1 / Table 2 prepared from R01 datasets\n")
cat("============================================================\n\n")
cat("MIMIC-IV: ", nrow(mimic), " primary stays; ",
    sum(mimic$hospital_death_after_60h == 1L), " deaths\n", sep = "")
cat("eICU-CRD: ", nrow(eicu), " primary stays; ",
    sum(eicu$hospital_death_after_60h == 1L), " deaths\n", sep = "")
cat("Race Max SMD: MIMIC-IV = ", format_smd(mimic_result$race_max_smd),
    "; eICU = ", format_smd(eicu_result$race_max_smd), "\n", sep = "")
cat("Published-reference checks: ",
    sum(published_reference_comparison$matches_published), "/",
    nrow(published_reference_comparison), " matched\n", sep = "")
cat("\nTable 1 - MIMIC-IV:\n")
print(mimic_result$table, row.names = FALSE)
cat("\nTable 2 - eICU-CRD:\n")
print(eicu_result$table, row.names = FALSE)
cat("\nOutputs written to: ", OUTPUT_DIR, "\n", sep = "")
cat("Only aggregates are saved. No patient-level export is created by R02.\n")
