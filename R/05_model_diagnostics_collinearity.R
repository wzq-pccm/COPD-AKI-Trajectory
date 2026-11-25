# ==============================================================================
# COPD early creatinine-defined AKI trajectory study
# R/05_model_diagnostics_collinearity.R
# MIMIC-IV v3.1 and eICU-CRD v2.0 | FINAL Supplementary Tables S12 and S13
#
# Source: author's original
#   04_generate_S4_S5_S6_revised_fixed_reference_v3.R
#   * Original S4 -> FINAL Supplementary Table S12 (model diagnostics)
#   * Original S5 -> FINAL Supplementary Table S13 (multicollinearity)
#   * Original S6 -> FINAL S5; now handled by R/04_eicu_trajectory_classifiability.R
#
# This script retains the original author's primary model, spline basis,
# four-knot selection, joint Wald tests, delta AIC, influence flags and
# car::vif/GVIF calculation without replacing the original statistical method.
# It does NOT modify the original primary estimates (those are in R/03).
#
# INPUTS (run R/01_import_harmonize_qc.R first):
#   output/R_analysis_output/mimic_primary_clean.rds [N=7,627]
#   output/R_analysis_output/eicu_primary_clean.rds  [N=7,738]
# These are the primary-classifiable cohorts only; do NOT use full landmark RDS.
#
# OUTPUTS (aggregates, no patient-level identifiers):
#   output/R_analysis_output/05_model_diagnostics_collinearity/
#   TableS12_PanelA_linearity.csv
#   TableS12_PanelB_spline_stability.csv
#   TableS12_PanelC1_influence_flags.csv
#   TableS12_PanelC2_influence_maxima.csv
#   TableS12_spline_knots_QC.csv
#   TableS13_multicollinearity.csv
#   TableS12_published_values_QC.csv (summary)
#   TableS12_Panel[A/B/C1/C2]_published_QC.csv (detailed)
#   TableS13_published_values_QC.csv
#   TableS12_S13_model_counts_QC.csv
#   TableS12_S13_model_diagnostics.xlsx [optional: openxlsx]
#   analysis_manifest_S12_S13.csv and sessionInfo_S12_S13.txt
#
# Run from the repository root:
#   Rscript R/05_model_diagnostics_collinearity.R
# Or set COPD_AKI_PROJECT_ROOT to an absolute project-root path.
#
# Public-release safety: do not commit data/, output/, RDS, workbook exports,
# model objects or influential-case records to the Git repository. The original
# script exported top-20 individual influence observations with IDs; this
# public edition reports the aggregate S12 diagnostics only.
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)

# ------------------------------------------------------------------------------
# 0. Portable paths and fixed manuscript QC targets (not model inputs)
# ------------------------------------------------------------------------------
root_override <- Sys.getenv("COPD_AKI_PROJECT_ROOT", unset = "")
PROJECT_ROOT <- normalizePath(
  if (nzchar(root_override)) root_override else ".",
  winslash = "/", mustWork = TRUE
)
INPUT_DIR <- file.path(PROJECT_ROOT, "output", "R_analysis_output")
OUTPUT_DIR <- file.path(INPUT_DIR, "05_model_diagnostics_collinearity")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
MIMIC_PRIMARY_RDS <- file.path(INPUT_DIR, "mimic_primary_clean.rds")
EICU_PRIMARY_RDS <- file.path(INPUT_DIR, "eicu_primary_clean.rds")

STOP_ON_COUNT_MISMATCH <- TRUE
SPLINE_KNOTS <- 4L
COOK_THRESHOLD_MULTIPLIER <- 4
LEVERAGE_THRESHOLD_MULTIPLIER <- 2
STANDARDIZED_RESIDUAL_THRESHOLD <- 3
TOP_INFLUENTIAL_N <- 20L

EXPECTED_PRIMARY <- list(
  "MIMIC-IV" = list(n = 7627L, deaths = 832L),
  "eICU-CRD" = list(n = 7738L, deaths = 709L)
)

# ------------------------------------------------------------------------------
# 1. Packages: no automatic installation or machine-specific configuration
# ------------------------------------------------------------------------------
required_packages <- c("dplyr", "tibble", "tidyr", "readr", "sandwich", "car")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing required packages: ", paste(missing_packages, collapse = ", "),
       ". Install before running, or use renv::restore().")
}
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(readr)
  library(sandwich)
  library(car)
})

# ------------------------------------------------------------------------------
# 2. Original diagnostic and multicollinearity functions, reproduced below
#    The only alterations inside are final table numbering, display labels,
#    and private function names. Statistical expressions are unchanged.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 2. General helper functions
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
  missing_columns <- setdiff(
    columns,
    names(data)
  )

  if (length(missing_columns) > 0L) {
    stop(
      database,
      " is missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  }
}

assert_no_missing <- function(data, columns, database) {
  missing_n <- vapply(
    data[columns],
    function(x) sum(is.na(x)),
    integer(1)
  )

  if (any(missing_n > 0L)) {
    stop(
      database,
      " contains missing values in required model variables: ",
      paste(
        names(missing_n)[missing_n > 0L],
        missing_n[missing_n > 0L],
        sep = "=",
        collapse = ", "
      )
    )
  }
}

assert_binary <- function(x, variable, database) {
  observed <- sort(
    unique(
      x[!is.na(x)]
    )
  )

  invalid <- setdiff(
    observed,
    c(0, 1)
  )

  if (length(invalid) > 0L) {
    stop(
      database,
      ": invalid value(s) in ",
      variable,
      ": ",
      paste(
        invalid,
        collapse = ", "
      )
    )
  }
}

check_expected_value <- function(actual, expected, label) {
  pass <- isTRUE(
    as.integer(actual) ==
      as.integer(expected)
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

format_p <- function(p) {
  if (
    length(p) == 0L ||
      is.na(p) ||
      !is.finite(p)
  ) {
    return("")
  }

  if (p < 0.001) {
    return("<0.001")
  }

  sprintf(
    "%.3f",
    p
  )
}

format_or_ci <- function(or, low, high) {
  if (
    any(
      is.na(
        c(or, low, high)
      )
    )
  ) {
    return("")
  }

  sprintf(
    "%.2f (%.2f-%.2f)",
    or,
    low,
    high
  )
}

write_csv_bom <- function(data, filename) {
  readr::write_excel_csv(
    data,
    file.path(
      OUTPUT_DIR,
      filename
    ),
    na = ""
  )
}

# ------------------------------------------------------------------------------
# 3. Prepare primary-model data
# ------------------------------------------------------------------------------

prepare_primary_data <- function(data, database) {

  required <- c(
    "patient_id",
    "hospital_stay_id",
    "icu_stay_id",
    "hospital_id",
    "primary_analysis_flag",
    "primary_exposure_code",
    "hospital_death_after_60h",
    "age",
    "female_flag",
    "ckd_flag",
    "heart_failure_flag",
    "diabetes_flag",
    "chronic_liver_disease_flag",
    "malignancy_flag",
    "standard_nonrenal_sofa_24h"
  )

  assert_required_columns(
    data,
    required,
    database
  )

  # Primary clean RDS should contain only the primary analytic population.
  if (
    any(
      is.na(
        data$primary_analysis_flag
      )
    ) ||
      any(
        data$primary_analysis_flag != 1L
      )
  ) {
    stop(
      database,
      ": primary clean RDS contains rows with primary_analysis_flag != 1."
    )
  }

  for (
    v in c(
      "hospital_death_after_60h",
      "female_flag",
      "ckd_flag",
      "heart_failure_flag",
      "diabetes_flag",
      "chronic_liver_disease_flag",
      "malignancy_flag"
    )
  ) {
    assert_binary(
      data[[v]],
      v,
      database
    )
  }

  invalid_exposure <- setdiff(
    sort(
      unique(
        data$primary_exposure_code[
          !is.na(
            data$primary_exposure_code
          )
        ]
      )
    ),
    0:2
  )

  if (
    length(invalid_exposure) > 0L ||
      any(
        is.na(
          data$primary_exposure_code
        )
      )
  ) {
    stop(
      database,
      ": primary_exposure_code must be completely observed and limited to 0, 1, 2."
    )
  }

  model_variables <- c(
    "hospital_death_after_60h",
    "primary_exposure_code",
    "age",
    "female_flag",
    "ckd_flag",
    "heart_failure_flag",
    "diabetes_flag",
    "chronic_liver_disease_flag",
    "malignancy_flag",
    "standard_nonrenal_sofa_24h"
  )

  assert_no_missing(
    data,
    model_variables,
    database
  )

  data <- data %>%
    mutate(
      Database = database,

      primary_exposure = factor(
        primary_exposure_code,
        levels = c(
          0L,
          1L,
          2L
        ),
        labels = c(
          "No early AKI",
          "Resolved early AKI",
          "Ongoing early AKI"
        )
      ),

      patient_id = as.character(
        patient_id
      ),

      hospital_stay_id = as.character(
        hospital_stay_id
      ),

      icu_stay_id = as.character(
        icu_stay_id
      ),

      hospital_id = as.character(
        hospital_id
      )
    )

  if (
    nlevels(
      droplevels(
        data$primary_exposure
      )
    ) != 3L
  ) {
    stop(
      database,
      ": all three AKI trajectory groups are required."
    )
  }

  expected <- EXPECTED_PRIMARY[[database]]

  check_expected_value(
    nrow(data),
    expected$n,
    paste(
      database,
      "primary N"
    )
  )

  check_expected_value(
    sum(
      data$hospital_death_after_60h == 1L
    ),
    expected$deaths,
    paste(
      database,
      "primary deaths"
    )
  )

  if (
    database == "eICU-CRD" &&
      (
        any(
          is.na(
            data$hospital_id
          )
        ) ||
          any(
            data$hospital_id == ""
          ) ||
          length(
            unique(
              data$hospital_id
            )
          ) < 2L
      )
  ) {
    stop(
      "eICU-CRD: hospital_id is incomplete or insufficient for clustered HC1 inference."
    )
  }

  data
}

# ------------------------------------------------------------------------------
# 4. Primary logistic model and covariance
# ------------------------------------------------------------------------------

primary_formula <-
  hospital_death_after_60h ~
  primary_exposure +
  age +
  female_flag +
  ckd_flag +
  heart_failure_flag +
  diabetes_flag +
  chronic_liver_disease_flag +
  malignancy_flag +
  standard_nonrenal_sofa_24h

fit_logistic <- function(formula, data) {
  stats::glm(
    formula = formula,
    data = data,
    family = stats::binomial(
      link = "logit"
    ),
    na.action = stats::na.fail,
    control = stats::glm.control(
      maxit = 100
    ),
    model = TRUE,
    x = TRUE,
    y = TRUE
  )
}

get_inference_vcov <- function(
  fit,
  data,
  database
) {

  if (
    database == "MIMIC-IV"
  ) {
    return(
      stats::vcov(fit)
    )
  }

  sandwich::vcovCL(
    fit,
    cluster = data$hospital_id,
    type = "HC1"
  )
}

# ==============================================================================
# SUPPLEMENTARY TABLE S12
# ==============================================================================

# ------------------------------------------------------------------------------
# 5. Restricted cubic spline helper functions
# ------------------------------------------------------------------------------

choose_rcs_knots <- function(
  x,
  nk = 4L
) {

  if (nk != 4L) {
    stop(
      "This script currently supports four-knot restricted cubic splines only."
    )
  }

  x <- sort(
    x[
      is.finite(x) &
        !is.na(x)
    ]
  )

  unique_x <- unique(x)

  if (
    length(unique_x) < 6L
  ) {
    stop(
      "Too few unique values to construct a stable four-knot restricted cubic spline."
    )
  }

  candidate_probabilities <- list(
    c(
      0.05,
      0.35,
      0.65,
      0.95
    ),
    c(
      0.05,
      0.25,
      0.75,
      0.95
    ),
    c(
      0.10,
      0.35,
      0.65,
      0.90
    ),
    c(
      0.05,
      0.30,
      0.70,
      0.95
    )
  )

  for (
    probs in candidate_probabilities
  ) {

    knots <- as.numeric(
      stats::quantile(
        x,
        probs = probs,
        names = FALSE,
        type = 2,
        na.rm = TRUE
      )
    )

    if (
      length(
        unique(knots)
      ) == nk
    ) {
      return(
        sort(knots)
      )
    }
  }

  # Fallback for discrete variables such as SOFA.
  indices <- unique(
    round(
      seq(
        from = 1,
        to = length(unique_x),
        length.out = nk
      )
    )
  )

  knots <- unique_x[
    indices
  ]

  if (
    length(
      unique(knots)
    ) != nk
  ) {
    stop(
      "Unable to select four unique spline knots."
    )
  }

  sort(
    as.numeric(knots)
  )
}

positive_part_cubed <- function(z) {
  pmax(
    z,
    0
  )^3
}

make_rcs_nonlinear_basis <- function(
  x,
  knots,
  prefix
) {

  knots <- sort(
    as.numeric(knots)
  )

  k <- length(knots)

  if (k < 3L) {
    stop(
      "At least three knots are required."
    )
  }

  denominator <- (
    knots[k] -
      knots[1]
  )^2

  if (
    !is.finite(denominator) ||
      denominator <= 0
  ) {
    stop(
      "Invalid spline-knot range."
    )
  }

  output <- vector(
    "list",
    k - 2L
  )

  for (
    j in seq_len(
      k - 2L
    )
  ) {

    output[[j]] <- (
      positive_part_cubed(
        x - knots[j]
      ) -

        positive_part_cubed(
          x - knots[k - 1L]
        ) *
        (
          knots[k] -
            knots[j]
        ) /
        (
          knots[k] -
            knots[k - 1L]
        ) +

        positive_part_cubed(
          x - knots[k]
        ) *
        (
          knots[k - 1L] -
            knots[j]
        ) /
        (
          knots[k] -
            knots[k - 1L]
        )
    ) / denominator
  }

  output <- as.data.frame(
    output,
    check.names = FALSE
  )

  names(output) <- paste0(
    prefix,
    seq_len(
      ncol(output)
    )
  )

  output
}

add_spline_basis <- function(
  data,
  age_knots,
  sofa_knots
) {

  age_basis <-
    make_rcs_nonlinear_basis(
      data$age,
      age_knots,
      "age_rcs_nl"
    )

  sofa_basis <-
    make_rcs_nonlinear_basis(
      data$standard_nonrenal_sofa_24h,
      sofa_knots,
      "sofa_rcs_nl"
    )

  dplyr::bind_cols(
    data,
    age_basis,
    sofa_basis
  )
}

safe_inverse <- function(x) {
  tryCatch(
    solve(x),
    error = function(e) {
      qr.solve(
        x,
        diag(
          nrow(x)
        )
      )
    }
  )
}

joint_wald_test <- function(
  fit,
  vcov_matrix,
  coefficient_names
) {

  all_names <- names(
    stats::coef(fit)
  )

  selected <- which(
    all_names %in%
      coefficient_names
  )

  if (
    length(selected) == 0L
  ) {
    return(
      tibble(
        Wald_chisq = NA_real_,
        df = NA_integer_,
        P_value_numeric = NA_real_
      )
    )
  }

  beta <- stats::coef(fit)[
    selected
  ]

  variance <- vcov_matrix[
    selected,
    selected,
    drop = FALSE
  ]

  if (
    any(
      !is.finite(beta)
    ) ||
      any(
        !is.finite(variance)
      )
  ) {
    stop(
      "Non-finite coefficient or covariance value in spline Wald test."
    )
  }

  inverse_variance <-
    safe_inverse(
      variance
    )

  statistic <- as.numeric(
    t(beta) %*%
      inverse_variance %*%
      beta
  )

  degrees_freedom <-
    length(beta)

  p_value <- stats::pchisq(
    statistic,
    df = degrees_freedom,
    lower.tail = FALSE
  )

  tibble(
    Wald_chisq = statistic,
    df = as.integer(
      degrees_freedom
    ),
    P_value_numeric = p_value
  )
}

extract_exposure_effects <- function(
  fit,
  vcov_matrix,
  database,
  model_specification
) {

  beta <- stats::coef(fit)
  se <- sqrt(
    diag(
      vcov_matrix
    )
  )

  exposure_terms <- names(beta)[
    startsWith(
      names(beta),
      "primary_exposure"
    )
  ]

  if (
    length(exposure_terms) != 2L
  ) {
    stop(
      database,
      " | ",
      model_specification,
      ": expected exactly two AKI exposure coefficients."
    )
  }

  rows <- lapply(
    exposure_terms,
    function(term) {

      estimate <- beta[[term]]
      standard_error <- se[[term]]

      low <-
        estimate -
        1.96 *
        standard_error

      high <-
        estimate +
        1.96 *
        standard_error

      comparison <- if (
        grepl(
          "Resolved",
          term,
          fixed = TRUE
        )
      ) {
        "Resolved early AKI"
      } else if (
        grepl(
          "Ongoing",
          term,
          fixed = TRUE
        )
      ) {
        "Ongoing early AKI"
      } else {
        term
      }

      tibble(
        Cohort = database,
        Model_specification =
          model_specification,
        Comparison = comparison,
        OR = exp(estimate),
        CI_low = exp(low),
        CI_high = exp(high),
        OR_95CI = format_or_ci(
          exp(estimate),
          exp(low),
          exp(high)
        )
      )
    }
  )

  bind_rows(rows)
}

make_influence_results <- function(
  fit,
  data,
  database
) {

  n <- stats::nobs(fit)
  p <- length(
    stats::coef(fit)
  )

  cook_threshold <-
    COOK_THRESHOLD_MULTIPLIER /
    n

  leverage_threshold <-
    LEVERAGE_THRESHOLD_MULTIPLIER *
    p /
    n

  cook <-
    stats::cooks.distance(fit)

  leverage <-
    stats::hatvalues(fit)

  standardized_residual <-
    stats::rstandard(
      fit,
      type = "deviance"
    )

  observation_data <- tibble(
    Cohort = database,

    patient_id =
      as.character(
        data$patient_id
      ),

    hospital_stay_id =
      as.character(
        data$hospital_stay_id
      ),

    icu_stay_id =
      as.character(
        data$icu_stay_id
      ),

    hospital_id =
      as.character(
        data$hospital_id
      ),

    primary_exposure =
      as.character(
        data$primary_exposure
      ),

    outcome =
      as.integer(
        data$hospital_death_after_60h
      ),

    Cooks_distance =
      as.numeric(cook),

    Leverage =
      as.numeric(leverage),

    Standardized_deviance_residual =
      as.numeric(
        standardized_residual
      )
  ) %>%
    mutate(
      Cook_flag =
        as.integer(
          Cooks_distance >
            cook_threshold
        ),

      Leverage_flag =
        as.integer(
          Leverage >
            leverage_threshold
        ),

      Residual_flag =
        as.integer(
          abs(
            Standardized_deviance_residual
          ) >
            STANDARDIZED_RESIDUAL_THRESHOLD
        ),

      Any_influence_flag =
        as.integer(
          Cook_flag == 1L |
            Leverage_flag == 1L |
            Residual_flag == 1L
        )
    )

  summary <- tibble(
    Cohort = database,
    N = n,
    Parameters = p,

    Cook_threshold =
      cook_threshold,

    Cook_flag_n =
      sum(
        observation_data$Cook_flag
      ),

    Cook_flag_pct =
      100 *
      mean(
        observation_data$Cook_flag
      ),

    Leverage_threshold =
      leverage_threshold,

    Leverage_flag_n =
      sum(
        observation_data$Leverage_flag
      ),

    Leverage_flag_pct =
      100 *
      mean(
        observation_data$Leverage_flag
      ),

    Standardized_residual_threshold =
      STANDARDIZED_RESIDUAL_THRESHOLD,

    Residual_flag_n =
      sum(
        observation_data$Residual_flag
      ),

    Residual_flag_pct =
      100 *
      mean(
        observation_data$Residual_flag
      ),

    Any_influence_flag_n =
      sum(
        observation_data$Any_influence_flag
      ),

    Any_influence_flag_pct =
      100 *
      mean(
        observation_data$Any_influence_flag
      ),

    Maximum_Cooks_distance =
      max(
        observation_data$Cooks_distance,
        na.rm = TRUE
      ),

    Maximum_leverage =
      max(
        observation_data$Leverage,
        na.rm = TRUE
      ),

    Maximum_absolute_standardized_residual =
      max(
        abs(
          observation_data$Standardized_deviance_residual
        ),
        na.rm = TRUE
      )
  )

  top <- observation_data %>%
    arrange(
      desc(
        Cooks_distance
      )
    ) %>%
    slice_head(
      n = TOP_INFLUENTIAL_N
    ) %>%
    mutate(
      Cook_rank = row_number()
    ) %>%
    select(
      Cohort,
      Cook_rank,
      everything()
    )

  list(
    summary = summary,
    top = top,
    all = observation_data
  )
}

run_s12_for_database <- function(
  data,
  database
) {

  message(
    "Running S12 diagnostics for ",
    database,
    "..."
  )

  age_knots <-
    choose_rcs_knots(
      data$age,
      nk = SPLINE_KNOTS
    )

  sofa_knots <-
    choose_rcs_knots(
      data$standard_nonrenal_sofa_24h,
      nk = SPLINE_KNOTS
    )

  analysis_data <-
    add_spline_basis(
      data,
      age_knots,
      sofa_knots
    )

  age_spline_formula <-
    update(
      primary_formula,
      . ~ . +
        age_rcs_nl1 +
        age_rcs_nl2
    )

  sofa_spline_formula <-
    update(
      primary_formula,
      . ~ . +
        sofa_rcs_nl1 +
        sofa_rcs_nl2
    )

  full_spline_formula <-
    update(
      primary_formula,
      . ~ . +
        age_rcs_nl1 +
        age_rcs_nl2 +
        sofa_rcs_nl1 +
        sofa_rcs_nl2
    )

  linear_fit <-
    fit_logistic(
      primary_formula,
      analysis_data
    )

  age_fit <-
    fit_logistic(
      age_spline_formula,
      analysis_data
    )

  sofa_fit <-
    fit_logistic(
      sofa_spline_formula,
      analysis_data
    )

  full_fit <-
    fit_logistic(
      full_spline_formula,
      analysis_data
    )

  fits <- list(
    linear_fit,
    age_fit,
    sofa_fit,
    full_fit
  )

  if (
    !all(
      vapply(
        fits,
        function(x) isTRUE(
          x$converged
        ),
        logical(1)
      )
    )
  ) {
    stop(
      database,
      ": at least one S12 logistic model did not converge."
    )
  }

  linear_vcov <-
    get_inference_vcov(
      linear_fit,
      analysis_data,
      database
    )

  age_vcov <-
    get_inference_vcov(
      age_fit,
      analysis_data,
      database
    )

  sofa_vcov <-
    get_inference_vcov(
      sofa_fit,
      analysis_data,
      database
    )

  full_vcov <-
    get_inference_vcov(
      full_fit,
      analysis_data,
      database
    )

  age_wald <-
    joint_wald_test(
      age_fit,
      age_vcov,
      c(
        "age_rcs_nl1",
        "age_rcs_nl2"
      )
    )

  sofa_wald <-
    joint_wald_test(
      sofa_fit,
      sofa_vcov,
      c(
        "sofa_rcs_nl1",
        "sofa_rcs_nl2"
      )
    )

  both_wald <-
    joint_wald_test(
      full_fit,
      full_vcov,
      c(
        "age_rcs_nl1",
        "age_rcs_nl2",
        "sofa_rcs_nl1",
        "sofa_rcs_nl2"
      )
    )

  make_linearity_row <- function(
    label,
    spline_fit,
    wald_row
  ) {

    p_value <-
      wald_row$P_value_numeric[[1]]

    tibble(
      Cohort = database,
      Covariate_specification = label,

      Delta_AIC =
        stats::AIC(spline_fit) -
        stats::AIC(linear_fit),

      Wald_chisq =
        wald_row$Wald_chisq[[1]],

      Wald_df =
        wald_row$df[[1]],

      Wald_chisq_df = sprintf(
        "%.2f (%d)",
        wald_row$Wald_chisq[[1]],
        wald_row$df[[1]]
      ),

      P_value_numeric =
        p_value,

      P_value =
        format_p(
          p_value
        ),

      Diagnostic_conclusion =
        ifelse(
          !is.na(p_value) &&
            p_value < 0.05,
          "Evidence of nonlinearity; inspect spline specification",
          ifelse(
            label ==
              "Age and nonrenal SOFA score jointly",
            "No evidence favoring spline terms",
            "No evidence of nonlinearity"
          )
        )
    )
  }

  panel_a <- bind_rows(
    make_linearity_row(
      "Age",
      age_fit,
      age_wald
    ),

    make_linearity_row(
      "First-day nonrenal SOFA score",
      sofa_fit,
      sofa_wald
    ),

    make_linearity_row(
      "Age and nonrenal SOFA score jointly",
      full_fit,
      both_wald
    )
  )

  exposure_linear <-
    extract_exposure_effects(
      linear_fit,
      linear_vcov,
      database,
      "Primary model with linear terms"
    )

  exposure_spline <-
    extract_exposure_effects(
      full_fit,
      full_vcov,
      database,
      "Restricted cubic spline model"
    )

  influence <-
    make_influence_results(
      linear_fit,
      analysis_data,
      database
    )

  knots <- tibble(
    Cohort = database,
    Variable = c(
      "Age",
      "Nonrenal SOFA"
    ),
    Knot_1 = c(
      age_knots[1],
      sofa_knots[1]
    ),
    Knot_2 = c(
      age_knots[2],
      sofa_knots[2]
    ),
    Knot_3 = c(
      age_knots[3],
      sofa_knots[3]
    ),
    Knot_4 = c(
      age_knots[4],
      sofa_knots[4]
    )
  )

  list(
    panel_a = panel_a,
    exposure = bind_rows(
      exposure_linear,
      exposure_spline
    ),
    influence_summary =
      influence$summary,
    influence_top =
      influence$top,
    knots = knots,
    linear_fit = linear_fit
  )
}

# ------------------------------------------------------------------------------
# 6. Build publication-facing S12 panels
# ------------------------------------------------------------------------------

build_s12_tables <- function(
  mimic_result,
  eicu_result
) {

  panel_a <- bind_rows(
    mimic_result$panel_a,
    eicu_result$panel_a
  ) %>%
    mutate(
      `ΔAIC` =
        round(
          Delta_AIC,
          2
        ),

      `Wald χ² (df) for nonlinearity` =
        Wald_chisq_df,

      `P value` =
        P_value,

      `Diagnostic conclusion` =
        Diagnostic_conclusion
    ) %>%
    select(
      Cohort,
      `Covariate specification` =
        Covariate_specification,
      `ΔAIC`,
      `Wald χ² (df) for nonlinearity`,
      `P value`,
      `Diagnostic conclusion`
    )

  exposure <- bind_rows(
    mimic_result$exposure,
    eicu_result$exposure
  )

  panel_b <- exposure %>%
    select(
      Cohort,
      Model_specification,
      Comparison,
      OR_95CI
    ) %>%
    tidyr::pivot_wider(
      names_from = Comparison,
      values_from = OR_95CI
    ) %>%
    rename(
      `Model specification` =
        Model_specification,
      `Resolved early AKI, adjusted OR (95% CI)` =
        `Resolved early AKI`,
      `Ongoing early AKI, adjusted OR (95% CI)` =
        `Ongoing early AKI`
    ) %>%
    arrange(
      factor(
        Cohort,
        levels = c(
          "MIMIC-IV",
          "eICU-CRD"
        )
      ),
      factor(
        `Model specification`,
        levels = c(
          "Primary model with linear terms",
          "Restricted cubic spline model"
        )
      )
    )

  influence_summary <- bind_rows(
    mimic_result$influence_summary,
    eicu_result$influence_summary
  )

  panel_c1 <- influence_summary %>%
    transmute(
      Cohort,

      `Cook's distance flags, n (%)` =
        sprintf(
          "%d (%.1f)",
          Cook_flag_n,
          Cook_flag_pct
        ),

      `Leverage flags, n (%)` =
        sprintf(
          "%d (%.1f)",
          Leverage_flag_n,
          Leverage_flag_pct
        ),

      `Absolute standardized deviance residual >3, n (%)` =
        sprintf(
          "%d (%.1f)",
          Residual_flag_n,
          Residual_flag_pct
        ),

      `Any influence flag, n (%)` =
        sprintf(
          "%d (%.1f)",
          Any_influence_flag_n,
          Any_influence_flag_pct
        )
    )

  panel_c2 <- influence_summary %>%
    transmute(
      Cohort,

      `Maximum Cook's distance` =
        sprintf(
          "%.4f",
          Maximum_Cooks_distance
        ),

      `Maximum leverage` =
        sprintf(
          "%.4f",
          Maximum_leverage
        ),

      `Maximum absolute standardized deviance residual` =
        sprintf(
          "%.3f",
          Maximum_absolute_standardized_residual
        )
    )

  list(
    panel_a = panel_a,
    panel_b = panel_b,
    panel_c1 = panel_c1,
    panel_c2 = panel_c2,
    influence_top = bind_rows(
      mimic_result$influence_top,
      eicu_result$influence_top
    ),
    knots = bind_rows(
      mimic_result$knots,
      eicu_result$knots
    )
  )
}

# ==============================================================================
# SUPPLEMENTARY TABLE S13
# ==============================================================================

# ------------------------------------------------------------------------------
# 7. GVIF helpers
# ------------------------------------------------------------------------------

pretty_vif_term <- function(term) {

  labels <- c(
    primary_exposure =
      "Early AKI trajectory",

    age =
      "Age, per year",

    female_flag =
      "Female sex",

    ckd_flag =
      "Chronic kidney disease",

    heart_failure_flag =
      "Heart failure",

    diabetes_flag =
      "Diabetes mellitus",

    chronic_liver_disease_flag =
      "Chronic liver disease",

    malignancy_flag =
      "Malignancy",

    standard_nonrenal_sofa_24h =
      "First-day nonrenal SOFA score"
  )

  if (
    term %in%
      names(labels)
  ) {
    return(
      unname(
        labels[term]
      )
    )
  }

  term
}

extract_gvif <- function(
  fit,
  database
) {

  v <- car::vif(
    fit
  )

  if (
    is.matrix(v) ||
      is.data.frame(v)
  ) {

    v <- as.data.frame(
      v,
      check.names = FALSE
    )

    v$Term <-
      rownames(v)

    rownames(v) <- NULL

    gvif_col <- grep(
      "^GVIF$",
      names(v),
      value = TRUE
    )

    df_col <- grep(
      "^Df$",
      names(v),
      value = TRUE
    )

    adjusted_col <- grep(
      "GVIF\\^\\(1/\\(2\\*Df\\)\\)",
      names(v),
      value = TRUE
    )

    if (
      length(gvif_col) != 1L ||
        length(df_col) != 1L
    ) {
      stop(
        database,
        ": unexpected car::vif() output."
      )
    }

    if (
      length(adjusted_col) == 1L
    ) {
      adjusted <-
        v[[adjusted_col]]
    } else {
      adjusted <-
        v[[gvif_col]]^(
          1 /
            (
              2 *
                v[[df_col]]
            )
        )
    }

    out <- tibble(
      Database = database,
      Term = v$Term,
      df = as.integer(
        v[[df_col]]
      ),
      GVIF = as.numeric(
        v[[gvif_col]]
      ),
      Adjusted_GVIF =
        as.numeric(
          adjusted
        )
    )

  } else {

    # Fallback if all terms are 1-df and car returns a vector.
    out <- tibble(
      Database = database,
      Term = names(v),
      df = 1L,
      GVIF = as.numeric(v),
      Adjusted_GVIF =
        sqrt(
          as.numeric(v)
        )
    )
  }

  out %>%
    mutate(
      Covariate = vapply(
        Term,
        pretty_vif_term,
        character(1)
      )
    )
}

build_s13_table <- function(
  mimic_fit,
  eicu_fit
) {

  mimic_vif <-
    extract_gvif(
      mimic_fit,
      "MIMIC-IV"
    )

  eicu_vif <-
    extract_gvif(
      eicu_fit,
      "eICU-CRD"
    )

  # Verify df is identical across databases for each model term.
  df_check <-
    mimic_vif %>%
    select(
      Term,
      mimic_df = df
    ) %>%
    full_join(
      eicu_vif %>%
        select(
          Term,
          eicu_df = df
        ),
      by = "Term"
    )

  if (
    any(
      is.na(
        df_check$mimic_df
      )
    ) ||
      any(
        is.na(
          df_check$eicu_df
        )
      ) ||
      any(
        df_check$mimic_df !=
          df_check$eicu_df
      )
  ) {
    stop(
      "S13: degrees of freedom differ between MIMIC-IV and eICU-CRD for at least one term."
    )
  }

  variable_order <- c(
    "primary_exposure",
    "age",
    "female_flag",
    "ckd_flag",
    "heart_failure_flag",
    "diabetes_flag",
    "chronic_liver_disease_flag",
    "malignancy_flag",
    "standard_nonrenal_sofa_24h"
  )

  mimic_wide <-
    mimic_vif %>%
    select(
      Term,
      Covariate,
      df,
      MIMIC_GVIF = GVIF,
      MIMIC_Adjusted_GVIF =
        Adjusted_GVIF
    )

  eicu_wide <-
    eicu_vif %>%
    select(
      Term,
      eICU_GVIF = GVIF,
      eICU_Adjusted_GVIF =
        Adjusted_GVIF
    )

  mimic_wide %>%
    left_join(
      eicu_wide,
      by = "Term"
    ) %>%
    mutate(
      order =
        match(
          Term,
          variable_order
        ),

      `MIMIC-IV GVIF` =
        round(
          MIMIC_GVIF,
          2
        ),

      `MIMIC-IV Adjusted GVIF` =
        round(
          MIMIC_Adjusted_GVIF,
          2
        ),

      `eICU-CRD GVIF` =
        round(
          eICU_GVIF,
          2
        ),

      `eICU-CRD Adjusted GVIF` =
        round(
          eICU_Adjusted_GVIF,
          2
        )
    ) %>%
    arrange(order) %>%
    select(
      Covariate,
      df,
      `MIMIC-IV GVIF`,
      `MIMIC-IV Adjusted GVIF`,
      `eICU-CRD GVIF`,
      `eICU-CRD Adjusted GVIF`
    )
}


# ==============================================================================
# 3. Load and independently validate primary data from R/01
# ==============================================================================
stop_if_missing_file(MIMIC_PRIMARY_RDS)
stop_if_missing_file(EICU_PRIMARY_RDS)
mimic_primary <- prepare_primary_data(readRDS(MIMIC_PRIMARY_RDS), "MIMIC-IV")
eicu_primary <- prepare_primary_data(readRDS(EICU_PRIMARY_RDS), "eICU-CRD")

for (cohort in list(mimic_primary, eicu_primary)) {
  if (anyNA(cohort$icu_stay_id) || anyDuplicated(cohort$icu_stay_id)) {
    stop("Primary cohort contains a missing/duplicate ICU stay identifier.")
  }
}

# ==============================================================================
# 4. Re-run diagnostic models (S12); evaluate GVIF from SAME linear fits (S13)
# ==============================================================================
mimic_s12 <- run_s12_for_database(mimic_primary, "MIMIC-IV")
eicu_s12 <- run_s12_for_database(eicu_primary, "eICU-CRD")
s12 <- build_s12_tables(mimic_s12, eicu_s12)
s13_table <- build_s13_table(mimic_s12$linear_fit, eicu_s12$linear_fit)

# ==============================================================================
# 5. Compare displayed results with FINAL manuscript S12/S13 (QC ONLY)
#    Published targets never enter the estimation, model fit or patient selection.
# ==============================================================================

# S12 Panel A: displayed delta-AIC, Wald chi-square(df), and p-value.
expected_s12_a <- tibble::tribble(
  ~Cohort, ~Covariate_specification, ~Delta_AIC, ~Wald_chisq_df, ~P_value,
  "MIMIC-IV", "Age",                                      3.72,  "0.28 (2)", "0.870",
  "MIMIC-IV", "First-day nonrenal SOFA score",            -0.11,  "4.09 (2)", "0.129",
  "MIMIC-IV", "Age and nonrenal SOFA score jointly",      3.53,  "4.45 (4)", "0.348",
  "eICU-CRD", "Age",                                       3.97,  "0.03 (2)", "0.983",
  "eICU-CRD", "First-day nonrenal SOFA score",             3.37,  "0.43 (2)", "0.807",
  "eICU-CRD", "Age and nonrenal SOFA score jointly",       7.33,  "0.46 (4)", "0.977"
)

# Read display columns by meaning; the original table used mathematical glyphs.
s12_a_obs <- s12$panel_a
s12_delta_col <- grep("AIC", names(s12_a_obs), value = TRUE)[1]
s12_wald_col <- grep("Wald", names(s12_a_obs), value = TRUE)[1]
s12_p_col <- grep("P value", names(s12_a_obs), value = TRUE)[1]
if (anyNA(c(s12_delta_col, s12_wald_col, s12_p_col))) {
  stop("Unexpected S12 Panel A display columns; inspect original build_s12_tables().")
}
s12_a_qc <- s12_a_obs %>%
  transmute(
    Cohort = Cohort,
    Covariate_specification = as.character(`Covariate specification`),
    Delta_AIC_observed = as.numeric(.data[[s12_delta_col]]),
    Wald_chisq_df_observed = as.character(.data[[s12_wald_col]]),
    P_value_observed = as.character(.data[[s12_p_col]])
  ) %>%
  left_join(expected_s12_a, by = c("Cohort", "Covariate_specification")) %>%
  mutate(
    Delta_AIC_match = !is.na(Delta_AIC) & abs(Delta_AIC_observed - Delta_AIC) < 0.00001,
    Wald_match = !is.na(Wald_chisq_df) & Wald_chisq_df_observed == Wald_chisq_df,
    P_match = !is.na(P_value) & P_value_observed == P_value,
    Panel = "S12 A",
    All_match = Delta_AIC_match & Wald_match & P_match
  )

expected_s12_b <- tibble::tribble(
  ~Cohort, ~Model_specification, ~Resolved_expected, ~Ongoing_expected,
  "MIMIC-IV", "Primary model with linear terms", "1.20 (0.95-1.52)", "2.61 (2.16-3.17)",
  "MIMIC-IV", "Restricted cubic spline model", "1.20 (0.96-1.52)", "2.60 (2.15-3.15)",
  "eICU-CRD", "Primary model with linear terms", "1.50 (1.13-1.99)", "2.35 (1.84-3.00)",
  "eICU-CRD", "Restricted cubic spline model", "1.50 (1.13-1.98)", "2.35 (1.84-3.00)"
)
s12_b_qc <- s12$panel_b %>%
  transmute(
    Cohort,
    Model_specification = as.character(`Model specification`),
    Resolved_observed = as.character(`Resolved early AKI, adjusted OR (95% CI)`),
    Ongoing_observed = as.character(`Ongoing early AKI, adjusted OR (95% CI)`)
  ) %>%
  left_join(expected_s12_b, by = c("Cohort", "Model_specification")) %>%
  mutate(
    Resolved_match = !is.na(Resolved_expected) & Resolved_observed == Resolved_expected,
    Ongoing_match = !is.na(Ongoing_expected) & Ongoing_observed == Ongoing_expected,
    Panel = "S12 B", All_match = Resolved_match & Ongoing_match
  )

# S12 C1: counts with percentages; S12 C2: maxima.
expected_s12_c1 <- tibble::tribble(
  ~Cohort, ~Cook_expected, ~Leverage_expected, ~Residual_expected, ~Any_expected,
  "MIMIC-IV", "823 (10.8)", "935 (12.3)", "0 (0.0)", "1482 (19.4)",
  "eICU-CRD", "724 (9.4)", "961 (12.4)", "0 (0.0)", "1452 (18.8)"
)
# Source table uses thousands separators only in manuscript typography, not CSV.
s12_c1_qc <- s12$panel_c1 %>%
  transmute(Cohort,
            Cook_observed = as.character(.data[[names(s12$panel_c1)[2]]]),
            Leverage_observed = as.character(.data[[names(s12$panel_c1)[3]]]),
            Residual_observed = as.character(.data[[names(s12$panel_c1)[4]]]),
            Any_observed = as.character(.data[[names(s12$panel_c1)[5]]])) %>%
  left_join(expected_s12_c1, by = "Cohort") %>%
  mutate(
    Cook_match = !is.na(Cook_expected) & Cook_observed == Cook_expected,
    Leverage_match = !is.na(Leverage_expected) & Leverage_observed == Leverage_expected,
    Residual_match = !is.na(Residual_expected) & Residual_observed == Residual_expected,
    Any_match = !is.na(Any_expected) & Any_observed == Any_expected,
    Panel = "S12 C1", All_match = Cook_match & Leverage_match & Residual_match & Any_match
  )
expected_s12_c2 <- tibble::tribble(
  ~Cohort, ~Cook_expected, ~Leverage_expected, ~Residual_expected,
  "MIMIC-IV", "0.0031", "0.0105", "2.820",
  "eICU-CRD", "0.0049", "0.0175", "2.733"
)
s12_c2_qc <- s12$panel_c2 %>%
  transmute(Cohort,
            Cook_observed = as.character(.data[[names(s12$panel_c2)[2]]]),
            Leverage_observed = as.character(.data[[names(s12$panel_c2)[3]]]),
            Residual_observed = as.character(.data[[names(s12$panel_c2)[4]]])) %>%
  left_join(expected_s12_c2, by = "Cohort") %>%
  mutate(
    Cook_match = !is.na(Cook_expected) & Cook_observed == Cook_expected,
    Leverage_match = !is.na(Leverage_expected) & Leverage_observed == Leverage_expected,
    Residual_match = !is.na(Residual_expected) & Residual_observed == Residual_expected,
    Panel = "S12 C2", All_match = Cook_match & Leverage_match & Residual_match
  )

# One concise panel-level summary; detailed comparisons remain above.
published_s12_qc <- dplyr::bind_rows(
  s12_a_qc %>% transmute(Panel, Row = paste(Cohort, Covariate_specification), All_match),
  s12_b_qc %>% transmute(Panel, Row = paste(Cohort, Model_specification), All_match),
  s12_c1_qc %>% transmute(Panel, Row = Cohort, All_match),
  s12_c2_qc %>% transmute(Panel, Row = Cohort, All_match)
)

# S13 Table: GVIF and df as printed in final Supplementary Table S13.
expected_s13 <- tibble::tribble(
  ~Covariate, ~df, ~MIMIC_GVIF, ~MIMIC_adjusted, ~eICU_GVIF, ~eICU_adjusted,
  "Early AKI trajectory",                2L, 1.13, 1.03, 1.05, 1.01,
  "Age, per year",                      1L, 1.18, 1.09, 1.08, 1.04,
  "Female sex",                         1L, 1.03, 1.01, 1.01, 1.01,
  "Chronic kidney disease",             1L, 1.18, 1.09, 1.08, 1.04,
  "Heart failure",                      1L, 1.13, 1.07, 1.07, 1.03,
  "Diabetes mellitus",                  1L, 1.06, 1.03, 1.05, 1.02,
  "Chronic liver disease",              1L, 1.13, 1.06, 1.03, 1.01,
  "Malignancy",                          1L, 1.05, 1.03, 1.02, 1.01,
  "First-day nonrenal SOFA score",       1L, 1.15, 1.07, 1.08, 1.04
)

s13_observed <- s13_table
s13_qc <- s13_observed %>%
  transmute(
    Covariate = as.character(Covariate),
    df_observed = as.integer(df),
    MIMIC_GVIF_observed = as.numeric(`MIMIC-IV GVIF`),
    MIMIC_adjusted_observed = as.numeric(`MIMIC-IV Adjusted GVIF`),
    eICU_GVIF_observed = as.numeric(`eICU-CRD GVIF`),
    eICU_adjusted_observed = as.numeric(`eICU-CRD Adjusted GVIF`)
  ) %>%
  left_join(expected_s13, by = "Covariate") %>%
  mutate(
    df_match = !is.na(df) & df_observed == df,
    MIMIC_GVIF_match = !is.na(MIMIC_GVIF) & abs(MIMIC_GVIF_observed - MIMIC_GVIF) < 0.00001,
    MIMIC_adjusted_match = !is.na(MIMIC_adjusted) & abs(MIMIC_adjusted_observed - MIMIC_adjusted) < 0.00001,
    eICU_GVIF_match = !is.na(eICU_GVIF) & abs(eICU_GVIF_observed - eICU_GVIF) < 0.00001,
    eICU_adjusted_match = !is.na(eICU_adjusted) & abs(eICU_adjusted_observed - eICU_adjusted) < 0.00001,
    All_match = df_match & MIMIC_GVIF_match & MIMIC_adjusted_match & eICU_GVIF_match & eICU_adjusted_match
  )
if (nrow(s13_qc) != nrow(expected_s13)) {
  stop("S13 expected nine covariates; received ", nrow(s13_qc), ".")
}
if (!isTRUE(all(published_s12_qc$All_match)) || !isTRUE(all(s13_qc$All_match))) {
  warning(
    "One or more displayed S12/S13 statistics differ from the final manuscript. ",
    "Inspect the published-values QC CSV files. No model results were forced."
  )
}

# A concise model-eligibility QC with no row-level identifiers.
model_counts_qc <- tibble::tibble(
  Cohort = c("MIMIC-IV", "eICU-CRD"),
  N = c(nrow(mimic_primary), nrow(eicu_primary)),
  Deaths = c(sum(mimic_primary$hospital_death_after_60h),
             sum(eicu_primary$hospital_death_after_60h)),
  Clusters = c(NA_integer_, length(unique(eicu_primary$hospital_id))),
  Primary_model_parameters = c(length(stats::coef(mimic_s12$linear_fit)),
                               length(stats::coef(eicu_s12$linear_fit))),
  Spline_knots_per_variable = SPLINE_KNOTS,
  S12_rows_matching = rep(sum(published_s12_qc$All_match, na.rm = TRUE), 2L),
  S12_rows_total = rep(nrow(published_s12_qc), 2L),
  S13_rows_matching = rep(sum(s13_qc$All_match, na.rm = TRUE), 2L),
  S13_rows_total = rep(nrow(s13_qc), 2L)
)

# ==============================================================================
# 6. Export aggregate-only results
# ==============================================================================
write_csv_bom(s12$panel_a, "TableS12_PanelA_linearity.csv")
write_csv_bom(s12$panel_b, "TableS12_PanelB_spline_stability.csv")
write_csv_bom(s12$panel_c1, "TableS12_PanelC1_influence_flags.csv")
write_csv_bom(s12$panel_c2, "TableS12_PanelC2_influence_maxima.csv")
write_csv_bom(s12$knots, "TableS12_spline_knots_QC.csv")
write_csv_bom(s13_table, "TableS13_multicollinearity.csv")
write_csv_bom(s12_a_qc, "TableS12_PanelA_published_QC.csv")
write_csv_bom(s12_b_qc, "TableS12_PanelB_published_QC.csv")
write_csv_bom(s12_c1_qc, "TableS12_PanelC1_published_QC.csv")
write_csv_bom(s12_c2_qc, "TableS12_PanelC2_published_QC.csv")
write_csv_bom(published_s12_qc, "TableS12_published_values_QC.csv")
write_csv_bom(s13_qc, "TableS13_published_values_QC.csv")
write_csv_bom(model_counts_qc, "TableS12_S13_model_counts_QC.csv")

if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  sheet_map <- list(
    "S12_A_Linearity" = s12$panel_a,
    "S12_B_Splines" = s12$panel_b,
    "S12_C1_Flags" = s12$panel_c1,
    "S12_C2_Maxima" = s12$panel_c2,
    "S12_Knots_QC" = s12$knots,
    "S13_GVIF" = s13_table,
    "S12_A_Published_QC" = s12_a_qc,
    "S12_B_Published_QC" = s12_b_qc,
    "S12_C1_Published_QC" = s12_c1_qc,
    "S12_C2_Published_QC" = s12_c2_qc,
    "S12_Published_QC" = published_s12_qc,
    "S13_Published_QC" = s13_qc,
    "Model_counts_QC" = model_counts_qc
  )
  for (sheet_name in names(sheet_map)) {
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet_name, sheet_map[[sheet_name]])
    openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet_name,
      cols = seq_len(ncol(sheet_map[[sheet_name]])), widths = "auto")
  }
  openxlsx::saveWorkbook(
    wb, file.path(OUTPUT_DIR, "TableS12_S13_model_diagnostics.xlsx"),
    overwrite = TRUE
  )
} else {
  message("Optional package 'openxlsx' not installed; CSV outputs available; XLSX omitted.")
}

# ==============================================================================
# 7. Analysis manifest / environment record
# ==============================================================================
manifest <- tibble::tibble(
  Item = c(
    "script", "source_script", "manuscript_tables", "mimic_primary_input",
    "eicu_primary_input", "primary_formula", "covariance_MIMIC",
    "covariance_eICU", "spline_knots", "spline_knot_selection",
    "Cook_flag", "leverage_flag", "deviance_residual_flag",
    "GVIF_source", "patient_level_export", "R_version"
  ),
  Value = c(
    "05_model_diagnostics_collinearity.R",
    "04_generate_S4_S5_S6_revised_fixed_reference_v3.R (original S4 + S5)",
    "S12 and S13", MIMIC_PRIMARY_RDS, EICU_PRIMARY_RDS,
    paste(deparse(primary_formula), collapse = " "),
    "Model-based vcov", "sandwich::vcovCL cluster=hospital_id type=HC1",
    as.character(SPLINE_KNOTS),
    "Original author's choose_rcs_knots: quantile(type=2), fallback for discrete SOFA",
    "Cook's distance > 4/N", "Leverage > 2*p/N",
    "Absolute standardized deviance residual > 3",
    "car::vif on primary linear logistic regression",
    "None (only aggregate files)", as.character(getRversion())
  )
)
write_csv_bom(manifest, "analysis_manifest_S12_S13.csv")
writeLines(
  capture.output(sessionInfo()),
  con = file.path(OUTPUT_DIR, "sessionInfo_S12_S13.txt"),
  useBytes = TRUE
)

cat("\n==============================================================\n")
cat("Supplementary Tables S12 and S13 - model diagnostics complete\n")
cat("MIMIC-IV: N=", nrow(mimic_primary), ", deaths=",
    sum(mimic_primary$hospital_death_after_60h), "\n", sep = "")
cat("eICU-CRD: N=", nrow(eicu_primary), ", deaths=",
    sum(eicu_primary$hospital_death_after_60h), "\n", sep = "")
cat("S12 manuscript QC: ", sum(published_s12_qc$All_match, na.rm = TRUE), "/",
    nrow(published_s12_qc), " rows match\n", sep = "")
cat("S13 manuscript QC: ", sum(s13_qc$All_match, na.rm = TRUE), "/",
    nrow(s13_qc), " covariates match\n", sep = "")
cat("Output directory: ", OUTPUT_DIR, "\n", sep = "")
cat("==============================================================\n")
