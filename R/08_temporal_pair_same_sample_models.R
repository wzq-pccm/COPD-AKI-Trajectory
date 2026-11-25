# ==============================================================================
# COPD early creatinine-defined AKI trajectory study
# R/08_temporal_pair_same_sample_models.R
# MIMIC-IV v3.1 + eICU-CRD v2.0 | FINAL Supplementary Table S7 Panel C
#
# PUBLIC REPRODUCTION ADAPTATION OF:
#   09_Reviewer1_Temporal_Selection_Bias_and_RRT_Combined_v2.R
#   (section A: original vs strictly ordered observed-pair on SAME patients)
#
# Purpose:
#   On the very same strict-temporal-classifiable patients, compare adjusted
#   associations using (1) the frozen original fixed-reference AKI trajectory
#   and (2) the strictly observed-pair trajectory from public R07.
#   Identical outcome, covariates, ICU stays and hospital clusters are used for
#   both models. A difference in OR is NOT interpreted causally.
#
# Official manuscript Panel C ONLY:
#   - Primary fixed-reference / temporally paired classifications.
#   - Baseline category: no early AKI.
#   - Adjustment: age, female sex, CKD, heart failure, diabetes, chronic liver
#     disease, malignancy, first-day nonrenal SOFA (same as primary model).
#   - MIMIC-IV: model-based GLM standard errors.
#   - eICU-CRD: hospital-clustered sandwich HC1 standard errors.
#   - Wald 95% CIs: exp(beta +/- 1.96*SE), matching source R09.
#   - No IPW, RRT joint exclusion, additional covariates or alternative
#     temporal comparator scenarios in this formal manuscript script.
#
# REQUIRED INPUTS FROM R07 (LOCAL, NOT FOR GITHUB):
#   output/R_analysis_output/07_temporal_pair_reclassification/
#     mimic_temporal_pair_joined.rds
#     eicu_temporal_pair_joined.rds
#     TableS7_PanelA_numeric.csv
#
# AGGREGATE OUTPUTS (NO PATIENT/STAY IDS):
#   output/R_analysis_output/08_temporal_pair_same_sample_models/
#     TableS7_PanelC_same_patient_OR.csv
#     TableS7_PanelC_numeric.csv
#     TableS7_PanelC_cohort_QC.csv
#     TableS7_PanelC_published_values_QC.csv
#     TableS7_PanelC_all_coefficients.csv
#     08_run_notes.txt
#
# Run from repository root: Rscript R/08_temporal_pair_same_sample_models.R
# Optionally set environment COPD_AKI_PROJECT_ROOT=/path/to/repository.
# No automatic package installation, no fixed ID lists, no patient-level export.
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)

# 0. PORTABLE SETTINGS ---------------------------------------------------------
ROOT_OVERRIDE <- Sys.getenv("COPD_AKI_PROJECT_ROOT", unset = "")
PROJECT_ROOT <- normalizePath(
  if (nzchar(ROOT_OVERRIDE)) ROOT_OVERRIDE else ".",
  winslash = "/", mustWork = TRUE
)
RDS_DIR <- file.path(PROJECT_ROOT, "output", "R_analysis_output")
INPUT_DIR <- file.path(RDS_DIR, "07_temporal_pair_reclassification")
OUTPUT_DIR <- file.path(RDS_DIR, "08_temporal_pair_same_sample_models")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

STOP_ON_FROZEN_SAMPLE_MISMATCH <- TRUE
STOP_ON_PUBLISHED_ESTIMATE_MISMATCH <- FALSE  # QC only; never force estimates.
RUN_INTERNAL_UNIT_TESTS <- TRUE

INPUTS <- c(
  "MIMIC-IV" = file.path(INPUT_DIR, "mimic_temporal_pair_joined.rds"),
  "eICU-CRD" = file.path(INPUT_DIR, "eicu_temporal_pair_joined.rds")
)
PANEL_A_INPUT <- file.path(INPUT_DIR, "TableS7_PanelA_numeric.csv")
missing_inputs <- c(INPUTS[!file.exists(INPUTS)],
                    if (!file.exists(PANEL_A_INPUT)) PANEL_A_INPUT)
if (length(missing_inputs)) {
  stop("Missing R07 output file(s): ", paste(missing_inputs, collapse = "; "),
       ". Run R/07_temporal_pair_reclassification.R first, with ",
       "SAVE_LOCAL_JOINED_RDS <- TRUE. Do not publish those patient-level RDS files.")
}

if (!requireNamespace("sandwich", quietly = TRUE)) {
  stop("R package 'sandwich' is required for eICU hospital-clustered HC1 ",
       "standard errors. Install it before running this script.")
}

LOG <- character(0)
WARNINGS <- character(0)
msg <- function(...) {
  s <- paste0(...)
  message(s)
  LOG <<- c(LOG, s)
  invisible(s)
}
warn_keep <- function(...) {
  s <- paste0(...)
  warning(s, call. = FALSE)
  WARNINGS <<- unique(c(WARNINGS, s))
  invisible(s)
}
need <- function(x, fields, description) {
  missing <- setdiff(fields, names(x))
  if (length(missing)) {
    stop(description, " missing field(s): ", paste(missing, collapse = ", "))
  }
}
number <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) return(as.numeric(x))
  if (is.logical(x)) return(as.numeric(x))
  txt <- trimws(as.character(x))
  txt[txt %in% c("", "NA", "N/A", "NULL", "null", "NaN")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", "", txt, fixed = TRUE)))
}
identifier <- function(x) {
  txt <- trimws(as.character(x))
  txt[txt %in% c("", "NA", "N/A", "NULL", "null")] <- NA_character_
  decimal_int <- !is.na(txt) & grepl("^[0-9]+[.]0+$", txt)
  txt[decimal_int] <- sub("[.]0+$", "", txt[decimal_int])
  txt
}
check_ids <- function(x, description) {
  if (anyNA(x) || any(!nzchar(x))) stop(description, ": missing stay IDs")
  if (anyDuplicated(x)) stop(description, ": duplicate stay IDs")
}
write_summary <- function(x, filename) {
  if (!is.data.frame(x)) stop("Summary must be a data.frame: ", filename)
  if (any(grepl("^(icu_id|patientunitstayid|stay_id|subject_id|hadm_id)$",
                names(x), ignore.case = TRUE))) {
    stop("Refusing to write stay/patient identifier column to public-style summary: ",
         filename)
  }
  utils::write.csv(x, file.path(OUTPUT_DIR, filename), row.names = FALSE,
                   na = "", fileEncoding = "UTF-8")
  msg("Saved aggregate summary: ", filename)
}

COVARS <- c(
  "age", "female_flag", "ckd_flag", "heart_failure_flag",
  "diabetes_flag", "chronic_liver_disease_flag", "malignancy_flag",
  "standard_nonrenal_sofa_24h"
)
EXPOSURE_LEVELS <- c(
  "No early AKI", "Resolved early AKI", "Ongoing early AKI"
)
MODEL_FORMULA <- stats::as.formula(paste(
  "hospital_death_after_60h ~ analysis_exposure +",
  paste(COVARS, collapse = " + ")
))

FROZEN <- data.frame(
  Database = c("MIMIC-IV", "eICU-CRD"),
  Full_N = c(7794L, 8495L),
  Primary_N = c(7627L, 7738L),
  Primary_deaths = c(832L, 709L),
  Strict_classifiable_N = c(6558L, 5627L),
  Strict_classifiable_deaths = c(732L, 575L),
  Strict_no_AKI_N = c(4920L, 4722L),
  Strict_resolved_N = c(678L, 381L),
  Strict_ongoing_N = c(960L, 524L),
  stringsAsFactors = FALSE
)

# 1. RESTORE R07 JOINED COHORTS; CHECK SAME-PATIENT SAMPLE --------------------
PANEL_A <- utils::read.csv(PANEL_A_INPUT, stringsAsFactors = FALSE,
                           check.names = FALSE)
need(PANEL_A, c("Database", "original_primary_N", "classifiable_N",
                "classifiable_deaths", "no_AKI_N", "resolved_N", "ongoing_N"),
     "R07 S7 Panel A")
if (anyDuplicated(PANEL_A$Database)) stop("R07 Panel A has duplicate cohort rows")

REQUIRED <- c(
  "icu_id", "database", "hospital_id", "primary_analysis_flag",
  "primary_exposure_code", "nearest__strict_code",
  "hospital_death_after_60h", COVARS
)
NUMERIC_COLUMNS <- c(
  "primary_analysis_flag", "primary_exposure_code", "nearest__strict_code",
  "hospital_death_after_60h", COVARS
)

DATA <- list()
QC <- list()
for (db in names(INPUTS)) {
  x <- readRDS(INPUTS[[db]])
  if (!is.data.frame(x)) stop(db, ": R07 joined RDS is not a data.frame")
  need(x, REQUIRED, paste(db, "R07 joined RDS"))
  x$icu_id <- identifier(x$icu_id)
  x$hospital_id <- identifier(x$hospital_id)
  check_ids(x$icu_id, db)
  if (any(is.na(x$database)) || !all(as.character(x$database) == db)) {
    stop(db, ": wrong database label in R07 patient-level file")
  }
  for (field in NUMERIC_COLUMNS) {
    original <- x[[field]]
    x[[field]] <- number(original)
    # Reject silent conversion of nonempty, non-NA source strings to NA.
    bad <- !is.na(original) & !is.na(as.character(original)) &
      nzchar(trimws(as.character(original))) & is.na(x[[field]])
    if (any(bad)) stop(db, ": nonnumeric values in ", field)
  }
  if (nrow(x) != FROZEN$Full_N[FROZEN$Database == db]) {
    stop(db, ": wrong full landmark sample size in R07 RDS")
  }
  if (any(!is.na(x$primary_analysis_flag) &
          !x$primary_analysis_flag %in% 0:1)) {
    stop(db, ": invalid primary_analysis_flag")
  }
  primary <- x[
    !is.na(x$primary_analysis_flag) & x$primary_analysis_flag == 1 &
      !is.na(x$primary_exposure_code) & x$primary_exposure_code %in% 0:2,
    , drop = FALSE
  ]
  check_ids(primary$icu_id, paste(db, "original primary cohort"))
  if (any(!is.na(primary$nearest__strict_code) &
          !primary$nearest__strict_code %in% 0:2)) {
    stop(db, ": invalid strict temporal classification code")
  }
  if (!all(primary$hospital_death_after_60h %in% 0:1)) {
    stop(db, ": original primary outcome is missing or not binary")
  }
  if (!all(stats::complete.cases(primary[, COVARS, drop = FALSE]))) {
    stop(db, ": missing model covariates in original primary cohort")
  }
  if (db == "eICU-CRD" &&
      (anyNA(primary$hospital_id) || any(!nzchar(primary$hospital_id)))) {
    stop("eICU original primary sample lacks hospital cluster identifiers")
  }

  paired <- primary[!is.na(primary$nearest__strict_code), , drop = FALSE]
  check_ids(paired$icu_id, paste(db, "strict-classifiable cohort"))
  if (any(!paired$nearest__strict_code %in% 0:2)) {
    stop(db, ": strict-classifiable cohort contains invalid classification")
  }
  if (!all(stats::complete.cases(
    paired[, c("hospital_death_after_60h", COVARS), drop = FALSE]))) {
    stop(db, ": incomplete outcome/covariate in paired sample")
  }
  if (length(unique(paired$primary_exposure_code)) != 3L ||
      length(unique(paired$nearest__strict_code)) != 3L) {
    stop(db, ": all three exposure categories must be present in both models")
  }

  n_strict <- as.integer(table(factor(paired$nearest__strict_code,
                                      levels = 0:2)))
  observed <- c(
    Full_N = nrow(x),
    Primary_N = nrow(primary),
    Primary_deaths = sum(primary$hospital_death_after_60h == 1),
    Strict_classifiable_N = nrow(paired),
    Strict_classifiable_deaths = sum(paired$hospital_death_after_60h == 1),
    Strict_no_AKI_N = n_strict[1],
    Strict_resolved_N = n_strict[2],
    Strict_ongoing_N = n_strict[3]
  )
  expected <- FROZEN[FROZEN$Database == db, names(observed), drop = FALSE]
  if (nrow(expected) != 1L) stop(db, ": missing frozen published cohort reference")
  expected <- as.numeric(unlist(expected[1, , drop = FALSE], use.names = FALSE))
  names(expected) <- names(observed)
  bad <- names(observed)[as.numeric(observed) != expected]
  if (length(bad)) {
    note <- paste0(db, " frozen sample mismatch in: ",
                   paste(bad, collapse = ", "))
    if (isTRUE(STOP_ON_FROZEN_SAMPLE_MISMATCH)) stop(note)
    warn_keep(note)
  }
  row7 <- PANEL_A[PANEL_A$Database == db, , drop = FALSE]
  if (nrow(row7) != 1L) stop(db, ": expected one Panel A row from R07")
  r07_fields <- c("original_primary_N", "classifiable_N",
                  "classifiable_deaths", "no_AKI_N", "resolved_N", "ongoing_N")
  r08_observed <- c(nrow(primary), nrow(paired),
                    sum(paired$hospital_death_after_60h == 1), n_strict)
  if (any(as.numeric(unlist(row7[1, r07_fields, drop = FALSE], use.names = FALSE)) !=
          as.numeric(r08_observed))) {
    stop(db, ": R07 patient-level RDS disagrees with R07 Panel A aggregate. ",
         "Do not proceed before resolving exported-file version mismatch.")
  }

  QC[[db]] <- data.frame(
    Cohort = db,
    R07_landmark_N = nrow(x),
    Original_primary_N = nrow(primary),
    Original_primary_deaths = sum(primary$hospital_death_after_60h == 1),
    Strict_classifiable_N = nrow(paired),
    Strict_classifiable_deaths = sum(paired$hospital_death_after_60h == 1),
    Strict_indeterminate_N = nrow(primary) - nrow(paired),
    Original_no_AKI_in_paired = sum(paired$primary_exposure_code == 0),
    Original_resolved_in_paired = sum(paired$primary_exposure_code == 1),
    Original_ongoing_in_paired = sum(paired$primary_exposure_code == 2),
    Strict_no_AKI = n_strict[1],
    Strict_resolved = n_strict[2],
    Strict_ongoing = n_strict[3],
    N_changed_classification = sum(
      paired$primary_exposure_code != paired$nearest__strict_code
    ),
    Hospital_clusters = if (db == "eICU-CRD")
      length(unique(paired$hospital_id)) else NA_integer_,
    Original_model_N = nrow(paired),
    Strict_model_N = nrow(paired),
    Original_model_deaths = sum(paired$hospital_death_after_60h == 1),
    Strict_model_deaths = sum(paired$hospital_death_after_60h == 1),
    stringsAsFactors = FALSE
  )
  DATA[[db]] <- paired
  msg(db, ": verified identical candidate sample for BOTH exposures: ",
      nrow(paired), " patients / ",
      sum(paired$hospital_death_after_60h == 1), " deaths")
}

# 2. REGRESSION ENGINE FROM SOURCE R09 / PRIMARY MODEL -------------------------
fit_same_sample <- function(data, db, exposure_field, label) {
  need(data, c("icu_id", "hospital_id", "hospital_death_after_60h",
               exposure_field, COVARS),
       paste(db, label, "same-sample fit"))
  if (anyNA(data[[exposure_field]]) ||
      any(!data[[exposure_field]] %in% 0:2)) {
    stop(db, " / ", label, ": invalid/missing exposure")
  }
  model_data <- data
  model_data$analysis_exposure <- factor(
    model_data[[exposure_field]], levels = 0:2, labels = EXPOSURE_LEVELS
  )
  # No listwise omission: both models MUST use every selected patient.
  formula_fields <- all.vars(MODEL_FORMULA)
  if (!all(stats::complete.cases(model_data[, formula_fields, drop = FALSE]))) {
    stop(db, " / ", label, ": missing model field")
  }
  if (length(unique(model_data$analysis_exposure)) != 3L) {
    stop(db, " / ", label, ": three exposure levels are required")
  }
  captured <- character(0)
  fit <- withCallingHandlers(
    stats::glm(
      formula = MODEL_FORMULA, data = model_data,
      family = stats::binomial(link = "logit"),
      na.action = stats::na.fail, model = TRUE, x = TRUE, y = TRUE
    ),
    warning = function(w) {
      captured <<- c(captured, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  if (!isTRUE(fit$converged)) stop(db, " / ", label, ": glm did not converge")
  if (stats::nobs(fit) != nrow(model_data)) {
    stop(db, " / ", label, ": glm unexpectedly dropped patients")
  }
  if (length(captured)) {
    warn_keep(db, " / ", label, ": glm warning(s): ",
              paste(unique(captured), collapse = " | "))
  }
  if (anyNA(stats::coef(fit))) {
    stop(db, " / ", label, ": aliased coefficients; examine design matrix")
  }

  covariance <- if (db == "eICU-CRD") {
    if (anyNA(model_data$hospital_id) ||
        any(!nzchar(model_data$hospital_id))) {
      stop("Missing eICU hospital cluster IDs")
    }
    if (length(unique(model_data$hospital_id)) < 2L) {
      stop("Fewer than two eICU hospital clusters")
    }
    sandwich::vcovCL(fit, cluster = model_data$hospital_id, type = "HC1")
  } else {
    stats::vcov(fit)
  }
  if (!all(is.finite(covariance))) {
    stop(db, " / ", label, ": nonfinite covariance matrix")
  }
  beta <- stats::coef(fit)
  terms <- paste0("analysis_exposure", EXPOSURE_LEVELS[2:3])
  if (!all(terms %in% names(beta)) ||
      !all(terms %in% rownames(covariance))) {
    stop(db, " / ", label, ": cannot locate exposure coefficients")
  }
  se_all <- sqrt(diag(covariance))
  if (any(!is.finite(se_all)) || any(se_all <= 0)) {
    stop(db, " / ", label, ": invalid standard errors")
  }
  all_coef <- data.frame(
    Cohort = db, `Exposure classification` = label,
    Term = names(beta), Beta = as.numeric(beta),
    SE = as.numeric(se_all),
    OR = exp(as.numeric(beta)),
    CI95_lower = exp(as.numeric(beta) - 1.96 * se_all),
    CI95_upper = exp(as.numeric(beta) + 1.96 * se_all),
    P_value = 2 * stats::pnorm(abs(as.numeric(beta) / se_all),
                               lower.tail = FALSE),
    N = nrow(model_data),
    Deaths = sum(model_data$hospital_death_after_60h == 1),
    SE_method = if (db == "eICU-CRD")
      "Hospital-clustered robust HC1" else "Model-based",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  results <- all_coef[match(terms, all_coef$Term), , drop = FALSE]
  if (anyNA(results$Term)) stop(db, " / ", label, ": lost exposure results")
  results$Comparison <- c("Resolved vs no early AKI",
                          "Ongoing vs no early AKI")
  results <- results[, c(
    "Cohort", "Exposure classification", "Comparison", "N", "Deaths",
    "SE_method", "Beta", "SE", "OR", "CI95_lower", "CI95_upper", "P_value"
  ), drop = FALSE]
  rownames(results) <- NULL
  list(results = results, all_coefficients = all_coef,
       model_stays = model_data$icu_id, model_deaths = model_data$hospital_death_after_60h)
}

# 3. INTERNAL DETERMINISTIC SYNTHETIC TESTS (NOT PAPER RESULTS) ---------------
if (isTRUE(RUN_INTERNAL_UNIT_TESTS)) {
  set.seed(80908L)
  n_test <- 900L
  synth <- data.frame(
    icu_id = as.character(seq_len(n_test)),
    hospital_id = as.character(rep(seq_len(45L), length.out = n_test)),
    hospital_death_after_60h = integer(n_test),
    primary_exposure_code = rep(0:2, length.out = n_test),
    nearest__strict_code = rep(c(0L, 2L, 1L), length.out = n_test),
    age = sample(45:90, n_test, replace = TRUE),
    female_flag = stats::rbinom(n_test, 1, 0.5),
    ckd_flag = stats::rbinom(n_test, 1, 0.2),
    heart_failure_flag = stats::rbinom(n_test, 1, 0.3),
    diabetes_flag = stats::rbinom(n_test, 1, 0.35),
    chronic_liver_disease_flag = stats::rbinom(n_test, 1, 0.08),
    malignancy_flag = stats::rbinom(n_test, 1, 0.18),
    standard_nonrenal_sofa_24h = stats::rpois(n_test, 3),
    stringsAsFactors = FALSE
  )
  eta <- -3 + 0.40 * (synth$primary_exposure_code == 1) +
    0.90 * (synth$primary_exposure_code == 2) +
    0.020 * (synth$age - 65) +
    0.12 * synth$standard_nonrenal_sofa_24h
  synth$hospital_death_after_60h <- stats::rbinom(
    n_test, size = 1L, prob = stats::plogis(eta)
  )
  for (db in names(INPUTS)) {
    old_test <- fit_same_sample(synth, db, "primary_exposure_code",
                                "Primary fixed-reference")
    new_test <- fit_same_sample(synth, db, "nearest__strict_code",
                                "Temporally paired")
    if (nrow(old_test$results) != 2L || nrow(new_test$results) != 2L ||
        !identical(old_test$model_stays, new_test$model_stays) ||
        !all(is.finite(c(old_test$results$OR, new_test$results$OR)))) {
      stop("Synthetic same-patient regression test failed for ", db)
    }
  }
  rm(synth, eta, old_test, new_test, n_test)
  msg("Internal synthetic test passed: both covariance methods and same sample")
}

# 4. FIT TWO CLASSIFICATIONS WITH IDENTICAL PATIENTS --------------------------
NUMERIC <- list()
ALL_COEFFICIENTS <- list()
for (db in names(DATA)) {
  paired <- DATA[[db]]
  original <- fit_same_sample(paired, db, "primary_exposure_code",
                              "Primary fixed-reference")
  strict <- fit_same_sample(paired, db, "nearest__strict_code",
                            "Temporally paired")
  # A direct ID and outcome vector equality check is local only; never export it.
  if (!identical(original$model_stays, strict$model_stays) ||
      !identical(original$model_deaths, strict$model_deaths)) {
    stop(db, ": original and strict models have different patients/outcomes")
  }
  if (!identical(original$results$N, strict$results$N) ||
      !identical(original$results$Deaths, strict$results$Deaths)) {
    stop(db, ": original and strict models have different N/deaths")
  }
  NUMERIC[[db]] <- rbind(original$results, strict$results)
  ALL_COEFFICIENTS[[db]] <- rbind(original$all_coefficients,
                                  strict$all_coefficients)
  msg(db, ": primary and temporal adjusted models fitted; ",
      nrow(paired), " identical ICU stays")
}
NUMERIC <- do.call(rbind, NUMERIC)
ALL_COEFFICIENTS <- do.call(rbind, ALL_COEFFICIENTS)
COHORT_QC <- do.call(rbind, QC)
rownames(NUMERIC) <- NULL
rownames(ALL_COEFFICIENTS) <- NULL
rownames(COHORT_QC) <- NULL

# 5. FORMAT FINAL SUPPLEMENTARY TABLE S7 PANEL C ------------------------------
formatted <- list()
for (db in names(DATA)) {
  for (definition in c("Primary fixed-reference", "Temporally paired")) {
    subset <- NUMERIC[
      NUMERIC$Cohort == db & NUMERIC[["Exposure classification"]] == definition,
      , drop = FALSE
    ]
    if (nrow(subset) != 2L) stop(db, ": missing S7 Panel C model contrasts")
    make_ci <- function(z) sprintf("%.2f (%.2f-%.2f)",
                                   z$OR, z$CI95_lower, z$CI95_upper)
    a <- subset[subset$Comparison == "Resolved vs no early AKI", , drop = FALSE]
    b <- subset[subset$Comparison == "Ongoing vs no early AKI", , drop = FALSE]
    if (nrow(a) != 1L || nrow(b) != 1L) stop("Unexpected exposure contrast")
    formatted[[length(formatted) + 1L]] <- data.frame(
      Cohort = db,
      `Exposure classification` = definition,
      `N/deaths` = paste0(a$N, "/", a$Deaths),
      `Resolved vs no early AKI, adjusted OR (95% CI)` = make_ci(a),
      `Ongoing vs no early AKI, adjusted OR (95% CI)` = make_ci(b),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
}
PANEL_C <- do.call(rbind, formatted)
rownames(PANEL_C) <- NULL

# 6. FROZEN PUBLISHED VALUES: CHECK ONLY; NEVER REPLACE MODEL OUTPUT ---------
EXPECTED_ESTIMATES <- data.frame(
  Cohort = rep(c("MIMIC-IV", "eICU-CRD"), each = 4L),
  Definition = rep(rep(c("Primary fixed-reference", "Temporally paired"),
                       each = 2L), times = 2L),
  Comparison = rep(c("Resolved vs no early AKI", "Ongoing vs no early AKI"),
                   times = 4L),
  Expected_OR = c(1.21, 2.75, 1.33, 2.67,
                  1.80, 2.62, 1.61, 2.90),
  Expected_CI95_lower = c(0.90, 2.25, 1.02, 2.19,
                          1.30, 2.03, 1.18, 2.26),
  Expected_CI95_upper = c(1.62, 3.37, 1.72, 3.24,
                          2.49, 3.39, 2.20, 3.71),
  stringsAsFactors = FALSE
)
PUBLISHED_QC <- list()
for (i in seq_len(nrow(EXPECTED_ESTIMATES))) {
  e <- EXPECTED_ESTIMATES[i, , drop = FALSE]
  r <- NUMERIC[
    NUMERIC$Cohort == e$Cohort &
      NUMERIC[["Exposure classification"]] == e$Definition &
      NUMERIC$Comparison == e$Comparison, , drop = FALSE
  ]
  if (nrow(r) != 1L) stop("Cannot uniquely match S7 Panel C published row ", i)
  fields <- c("OR", "CI95_lower", "CI95_upper")
  for (j in seq_along(fields)) {
    field <- fields[j]
    expected_name <- paste0("Expected_", field)
    actual <- r[[field]][1]
    expected <- e[[expected_name]][1]
    PUBLISHED_QC[[length(PUBLISHED_QC) + 1L]] <- data.frame(
      Cohort = e$Cohort,
      `Exposure classification` = e$Definition,
      Comparison = e$Comparison,
      Statistic = field,
      Published_rounded = sprintf("%.2f", expected),
      Recalculated_unrounded = actual,
      Recalculated_rounded = sprintf("%.2f", actual),
      Match_to_published_2dp = sprintf("%.2f", actual) == sprintf("%.2f", expected),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
}
PUBLISHED_QC <- do.call(rbind, PUBLISHED_QC)
rownames(PUBLISHED_QC) <- NULL
mismatch_n <- sum(!PUBLISHED_QC$Match_to_published_2dp)
if (mismatch_n) {
  note <- paste0("S7 Panel C published-value QC mismatches (at 2 d.p.): ",
                 mismatch_n, ". See detailed QC; never overwrite results.")
  if (isTRUE(STOP_ON_PUBLISHED_ESTIMATE_MISMATCH)) stop(note)
  warn_keep(note)
} else {
  msg("All published S7 Panel C OR/CI rounded values matched")
}

# 7. WRITE AGGREGATE RESULTS ONLY ---------------------------------------------
write_summary(PANEL_C, "TableS7_PanelC_same_patient_OR.csv")
write_summary(NUMERIC, "TableS7_PanelC_numeric.csv")
write_summary(COHORT_QC, "TableS7_PanelC_cohort_QC.csv")
write_summary(PUBLISHED_QC, "TableS7_PanelC_published_values_QC.csv")
write_summary(ALL_COEFFICIENTS, "TableS7_PanelC_all_coefficients.csv")

writeLines(c(
  "Supplementary Table S7 Panel C: same-patient comparison.",
  "Source: R07 private joined RDS with original primary and nearest-prior strict temporal exposures.",
  "No RRT or IPW restrictions; no reclassification of indeterminate cases to no AKI.",
  "The same strict-classifiable patients, outcome and eight covariates are used twice.",
  "MIMIC-IV: model-based standard errors; eICU-CRD: hospital-clustered HC1.",
  "Wald CI = exp(beta +/- 1.96*SE); exposure reference = no early AKI.",
  "All published values are QC comparisons only, never imposed on estimates.",
  paste0("Number of rounded published OR/CI mismatches: ", mismatch_n),
  "Only aggregate CSVs and this text log are written; no patient-level export.",
  "Do not commit source R07 joined RDS or any credentialed patient data.",
  "", "Run messages:", LOG,
  "", "Warnings:", if (length(WARNINGS)) WARNINGS else "None",
  "", "R session information:", capture.output(utils::sessionInfo())
), file.path(OUTPUT_DIR, "08_run_notes.txt"), useBytes = TRUE)
msg("Finished R08; S7 Panel C aggregate results in: ", OUTPUT_DIR)
