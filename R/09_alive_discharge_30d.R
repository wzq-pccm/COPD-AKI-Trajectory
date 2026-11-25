# ==============================================================================
# COPD early creatinine-defined AKI trajectory study
# R/09_alive_discharge_30d.R
# MIMIC-IV v3.1 + eICU-CRD v2.0 | FINAL Supplementary Table S4
#
# PUBLIC REPRODUCTION ADAPTATION OF:
#   10_Reviewer2_Secondary_Clinical_Outcomes.R
#   (the binary alive hospital discharge within 30 days analyses only)
#
# PURPOSE
#   S4 Panel A: alive discharge by day 30 / hospital death before day 30 /
#               still hospitalized at day 30, by original AKI trajectory.
#   S4 Panel B: adjusted odds of discharge alive by day 30, compared with
#               no early AKI, among all original primary-analysis patients.
#   S4 Panel C: ongoing vs resolved early AKI, additionally adjusting for
#               peak creatinine-defined AKI stage (stage 2-3 vs stage 1).
#
# Endpoint is measured from the ICU +60-hour landmark. Discharge alive at or
# before 30 days is 1. In-hospital death and continued hospitalization on
# day 30 are 0. This endpoint is not renal recovery or a causal treatment
# effect. The original source also evaluated Cox and survivor-only LOS/Gamma
# models; those UNREPORTED explorations are excluded from this formal S4 file.
#
# INPUTS FROM R01 (local, DO NOT upload to GitHub):
#   output/R_analysis_output/mimic_primary_clean.rds
#   output/R_analysis_output/eicu_primary_clean.rds
#
# MAIN AGGREGATE OUTPUTS (no patient/stay identifiers):
#   output/R_analysis_output/09_alive_discharge_30d/
#     TableS4_PanelA_disposition.csv
#     TableS4_PanelB_primary_adjusted.csv
#     TableS4_PanelC_within_AKI.csv
#     TableS4_model_numeric.csv
#     TableS4_cohort_time_QC.csv
#     TableS4_published_values_QC.csv
#     09_run_notes.txt
#
# Run from project root:
#   Rscript R/09_alive_discharge_30d.R
# Optional environment variable: COPD_AKI_PROJECT_ROOT=/path/to/project.
# This script never installs packages and never exports patient-level rows.
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999, warn = 1,
        contrasts = c("contr.treatment", "contr.poly"))

# 0. PATHS AND CONFIGURATION -------------------------------------------------
ROOT_OVERRIDE <- Sys.getenv("COPD_AKI_PROJECT_ROOT", unset = "")
PROJECT_ROOT <- normalizePath(
  if (nzchar(ROOT_OVERRIDE)) ROOT_OVERRIDE else ".",
  winslash = "/", mustWork = TRUE
)
RDS_DIR <- file.path(PROJECT_ROOT, "output", "R_analysis_output")
OUTPUT_DIR <- file.path(RDS_DIR, "09_alive_discharge_30d")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

INPUTS <- c(
  "MIMIC-IV" = file.path(RDS_DIR, "mimic_primary_clean.rds"),
  "eICU-CRD" = file.path(RDS_DIR, "eicu_primary_clean.rds")
)
missing_inputs <- INPUTS[!file.exists(INPUTS)]
if (length(missing_inputs)) {
  stop("Missing R01 primary cohort RDS file(s):\n",
       paste(missing_inputs, collapse = "\n"),
       "\nRun R/01_import_harmonize_qc.R before R09.")
}
if (!requireNamespace("sandwich", quietly = TRUE)) {
  stop("The R package 'sandwich' is required for eICU hospital-clustered HC1 ",
       "standard errors. Install it before running this script.")
}

DAY_HORIZON <- 30
TIME_TOLERANCE_DAYS <- 1e-6
STOP_ON_FROZEN_COHORT_MISMATCH <- TRUE
STOP_ON_PUBLISHED_VALUE_MISMATCH <- FALSE  # published values are QC ONLY
RUN_SELF_TESTS <- TRUE

LOG <- character(0)
MODEL_WARNINGS <- character(0)
log_msg <- function(...) {
  s <- paste0(...)
  message(s)
  LOG <<- c(LOG, s)
  invisible(s)
}
keep_warning <- function(...) {
  s <- paste0(...)
  warning(s, call. = FALSE)
  MODEL_WARNINGS <<- unique(c(MODEL_WARNINGS, s))
  invisible(s)
}
check_fields <- function(x, columns, context) {
  absent <- setdiff(columns, names(x))
  if (length(absent)) {
    stop(context, ": required column(s) missing: ", paste(absent, collapse = ", "))
  }
}
num <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) return(as.numeric(x))
  z <- trimws(as.character(x))
  z[z %in% c("", "NA", "N/A", "NULL", "null", "NaN")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", "", z, fixed = TRUE)))
}
identifier <- function(x) {
  z <- trimws(as.character(x))
  z[z %in% c("", "NA", "N/A", "NULL", "null")] <- NA_character_
  decimal_integer <- !is.na(z) & grepl("^[0-9]+[.]0+$", z)
  z[decimal_integer] <- sub("[.]0+$", "", z[decimal_integer])
  z
}
check_unique_ids <- function(x, context) {
  if (anyNA(x) || any(!nzchar(x))) stop(context, ": missing ICU identifiers")
  if (anyDuplicated(x)) stop(context, ": duplicated ICU identifiers")
}
write_aggregate <- function(data, filename) {
  if (!is.data.frame(data)) stop("Output must be a data.frame: ", filename)
  sensitive_columns <- c(
    "stay_id", "subject_id", "hadm_id", "patientunitstayid", "uniquepid",
    "patienthealthsystemstayid", "icu_stay_id", "patient_id", "hospital_stay_id"
  )
  if (any(tolower(names(data)) %in% sensitive_columns)) {
    stop("Refusing to write identifier-bearing aggregate: ", filename)
  }
  utils::write.csv(data, file.path(OUTPUT_DIR, filename),
                   row.names = FALSE, na = "", fileEncoding = "UTF-8")
  log_msg("Wrote ", filename)
}
format_n_pct <- function(n, total) {
  sprintf("%d (%.1f)", as.integer(n), 100 * n / total)
}
format_or <- function(or, lo, hi) {
  sprintf("%.2f (%.2f-%.2f)", or, lo, hi)
}

COVARS <- c(
  "age", "female_flag", "ckd_flag", "heart_failure_flag",
  "diabetes_flag", "chronic_liver_disease_flag", "malignancy_flag",
  "standard_nonrenal_sofa_24h"
)
LABELS <- c("No early AKI", "Resolved early AKI", "Ongoing early AKI")

# Values from FINAL Supplementary Table S4 are for independent checks, NOT
# for fitting models or modifying measured data.
EXPECTED_COHORT <- data.frame(
  Database = c("MIMIC-IV", "eICU-CRD"),
  N = c(7627L, 7738L),
  Deaths_after_60h = c(832L, 709L),
  No_early_AKI = c(5796L, 6644L),
  Resolved_early_AKI = c(898L, 465L),
  Ongoing_early_AKI = c(933L, 629L),
  Alive_discharge_by_day30 = c(6579L, 6913L),
  stringsAsFactors = FALSE
)
EXPECTED_PANEL_A <- data.frame(
  Database = rep(c("MIMIC-IV", "eICU-CRD"), each = 3),
  Trajectory = rep(LABELS, times = 2),
  N = c(5796L, 898L, 933L, 6644L, 465L, 629L),
  Alive_discharge_by_day30 = c(5184L, 742L, 653L, 6045L, 389L, 479L),
  Death_before_day30 = c(456L, 110L, 223L, 502L, 61L, 131L),
  Still_hospitalized_day30 = c(156L, 46L, 57L, 97L, 15L, 19L),
  stringsAsFactors = FALSE
)
EXPECTED_MODELS <- data.frame(
  Database = c("MIMIC-IV", "MIMIC-IV", "eICU-CRD", "eICU-CRD",
               "MIMIC-IV", "eICU-CRD"),
  Panel = c(rep("B", 4), rep("C", 2)),
  Comparison = c("Resolved early AKI vs no early AKI",
                 "Ongoing early AKI vs no early AKI",
                 "Resolved early AKI vs no early AKI",
                 "Ongoing early AKI vs no early AKI",
                 "Ongoing early AKI vs resolved early AKI",
                 "Ongoing early AKI vs resolved early AKI"),
  OR_expected = c(0.75, 0.38, 0.63, 0.42, 0.52, 0.66),
  CI_low_expected = c(0.61, 0.32, 0.48, 0.34, 0.41, 0.48),
  CI_high_expected = c(0.92, 0.46, 0.83, 0.52, 0.66, 0.91),
  stringsAsFactors = FALSE
)

# 1. ENDPOINT RULES AND BOUNDARY SELF-TESTS ----------------------------------
make_outcome_flags <- function(days_after_60h, death_after_60h,
                               horizon = DAY_HORIZON) {
  stopifnot(length(days_after_60h) == length(death_after_60h))
  if (any(!is.finite(days_after_60h) | days_after_60h <= 0) ||
      anyNA(death_after_60h) || any(!(death_after_60h %in% 0:1))) {
    stop("Invalid follow-up time or in-hospital death flag")
  }
  list(
    alive_discharge_by_day30 = as.integer(
      death_after_60h == 0 & days_after_60h <= horizon
    ),
    death_before_day30 = as.integer(
      death_after_60h == 1 & days_after_60h <= horizon
    ),
    still_hospitalized_day30 = as.integer(days_after_60h > horizon)
  )
}
if (RUN_SELF_TESTS) {
  test <- make_outcome_flags(c(3, 4, 30, 30.001, 6), c(0, 1, 0, 0, 1))
  stopifnot(
    identical(test$alive_discharge_by_day30, c(1L, 0L, 1L, 0L, 0L)),
    identical(test$death_before_day30, c(0L, 1L, 0L, 0L, 1L)),
    identical(test$still_hospitalized_day30, c(0L, 0L, 0L, 1L, 0L)),
    all(test$alive_discharge_by_day30 + test$death_before_day30 +
          test$still_hospitalized_day30 == 1L)
  )
  log_msg("Endpoint self-tests passed: inclusive day 30 and competing death.")
}

# 2. INPUT VALIDATION AND RECONCILIATION ------------------------------------
# Independently reconstruct time from SQL source fields (not from the endpoint
# we are about to compute). Failure indicates a wrong join or input version.
parse_sql_timestamp <- function(x, context) {
  if (inherits(x, "POSIXct")) return(x)
  txt <- trimws(as.character(x))
  txt <- sub("Z$", "", txt)
  txt <- sub("T", " ", txt, fixed = TRUE)
  txt[txt %in% c("", "NA", "N/A", "NULL", "null")] <- NA_character_
  out <- suppressWarnings(as.POSIXct(txt, format = "%Y-%m-%d %H:%M:%OS",
                                    tz = "UTC"))
  if (anyNA(out)) {
    stop(context, ": cannot parse a source hospital outcome/landmark timestamp")
  }
  out
}

prepare_primary <- function(file, database) {
  d <- readRDS(file)
  if (!is.data.frame(d)) stop(database, ": R01 output is not a data.frame")
  required <- c(
    "icu_stay_id", "primary_analysis_flag", "primary_exposure_code",
    "hospital_death_after_60h", "followup_days_after_60h", COVARS,
    "peak_aki_stage_0_48h",
    if (database == "MIMIC-IV") {
      c("landmark_60h_time", "outcome_or_discharge_time", "discharge_location")
    } else {
      c("hospital_id", "hospitaldischargeoffset", "hospitaldischargestatus")
    }
  )
  check_fields(d, required, database)
  d$icu_stay_id <- identifier(d$icu_stay_id)
  check_unique_ids(d$icu_stay_id, database)
  if (database == "eICU-CRD") {
    d$hospital_id <- identifier(d$hospital_id)
    if (anyNA(d$hospital_id) || any(!nzchar(d$hospital_id))) {
      stop("eICU-CRD: missing hospital_id prevents hospital-clustered HC1 SE")
    }
  }
  cols_numeric <- unique(c("primary_analysis_flag", "primary_exposure_code",
                           "hospital_death_after_60h", "followup_days_after_60h",
                           "peak_aki_stage_0_48h", COVARS,
                           if (database == "eICU-CRD") "hospitaldischargeoffset"))
  for (v in cols_numeric) d[[v]] <- num(d[[v]])

  if (anyNA(d$primary_analysis_flag) || any(d$primary_analysis_flag != 1) ||
      anyNA(d$primary_exposure_code) ||
      any(!(d$primary_exposure_code %in% 0:2))) {
    stop(database, ": this script requires R01's PRIMARY cohort RDS, not the full cohort")
  }
  if (anyNA(d$hospital_death_after_60h) ||
      any(!(d$hospital_death_after_60h %in% 0:1)) ||
      any(!is.finite(d$followup_days_after_60h)) ||
      any(d$followup_days_after_60h <= 0)) {
    stop(database, ": invalid outcome or follow-up time")
  }
  if (!all(stats::complete.cases(d[, COVARS, drop = FALSE]))) {
    stop(database, ": missing primary model covariates")
  }

  if (database == "MIMIC-IV") {
    end_time <- parse_sql_timestamp(d$outcome_or_discharge_time,
                                    "MIMIC-IV outcome_or_discharge_time")
    landmark <- parse_sql_timestamp(d$landmark_60h_time,
                                    "MIMIC-IV landmark_60h_time")
    independently_calculated_days <- as.numeric(
      difftime(end_time, landmark, units = "days")
    )
    discharge_text <- toupper(trimws(as.character(d$discharge_location)))
    missing_text <- is.na(discharge_text) | discharge_text %in% c("", "NA", "NULL")
    disposition_conflict <- !missing_text &
      ((discharge_text == "DIED") != (d$hospital_death_after_60h == 1))
  } else {
    independently_calculated_days <-
      (d$hospitaldischargeoffset - 3600) / 1440
    discharge_text <- toupper(trimws(as.character(d$hospitaldischargestatus)))
    missing_text <- is.na(discharge_text) | discharge_text %in% c("", "NA", "NULL")
    disposition_conflict <- !missing_text &
      ((discharge_text == "EXPIRED") != (d$hospital_death_after_60h == 1))
  }
  if (any(!is.finite(independently_calculated_days)) ||
      any(abs(independently_calculated_days - d$followup_days_after_60h) >
          TIME_TOLERANCE_DAYS)) {
    stop(database, ": source time does not match followup_days_after_60h. ",
         "Review the SQL exports and R01 merging before using S4 results.")
  }

  states <- make_outcome_flags(d$followup_days_after_60h,
                               d$hospital_death_after_60h)
  d$alive_discharge_by_day30 <- states$alive_discharge_by_day30
  d$death_before_day30 <- states$death_before_day30
  d$still_hospitalized_day30 <- states$still_hospitalized_day30
  d$analysis_exposure <- factor(d$primary_exposure_code,
                                levels = 0:2, labels = LABELS)
  d$peak_stage_high <- as.integer(d$peak_aki_stage_0_48h >= 2)
  if (!all(d$alive_discharge_by_day30 + d$death_before_day30 +
           d$still_hospitalized_day30 == 1L)) {
    stop(database, ": 30-day event categories do not partition the cohort")
  }

  g <- tabulate(as.integer(d$primary_exposure_code) + 1L, nbins = 3)
  observed <- data.frame(
    Database = database, N = nrow(d),
    Deaths_after_60h = sum(d$hospital_death_after_60h),
    No_early_AKI = g[1], Resolved_early_AKI = g[2],
    Ongoing_early_AKI = g[3],
    Alive_discharge_by_day30 = sum(d$alive_discharge_by_day30),
    stringsAsFactors = FALSE
  )
  expected <- EXPECTED_COHORT[
    EXPECTED_COHORT$Database == database, names(observed), drop = FALSE
  ]
  matched <- identical(
    as.numeric(unlist(observed[1, -1, drop = FALSE], use.names = FALSE)),
    as.numeric(unlist(expected[1, -1, drop = FALSE], use.names = FALSE))
  )
  if (!matched && STOP_ON_FROZEN_COHORT_MISMATCH) {
    stop(database, ": primary cohort or 30-day outcome counts do not match ",
         "the final manuscript. Inspect R01 and the SQL export files.")
  }
  qc <- data.frame(
    observed,
    Expected_N = expected$N,
    Expected_deaths = expected$Deaths_after_60h,
    Expected_alive_discharge_day30 = expected$Alive_discharge_by_day30,
    Published_cohort_check_pass = matched,
    Recomputed_time_mismatch_N = sum(abs(independently_calculated_days -
                                           d$followup_days_after_60h) >
                                       TIME_TOLERANCE_DAYS),
    Missing_discharge_status_text_N = sum(missing_text),
    Discharge_text_conflict_N = sum(disposition_conflict),
    Min_followup_days = min(d$followup_days_after_60h),
    Max_followup_days = max(d$followup_days_after_60h),
    N_over_180_days = sum(d$followup_days_after_60h > 180),
    N_over_365_days = sum(d$followup_days_after_60h > 365)
  )
  log_msg(database, ": N=", nrow(d), ", deaths=",
          sum(d$hospital_death_after_60h), ", alive discharge by day 30=",
          sum(d$alive_discharge_by_day30))
  list(data = d, qc = qc)
}

DATA <- lapply(names(INPUTS), function(db) prepare_primary(INPUTS[[db]], db))
names(DATA) <- names(INPUTS)
COHORT_QC <- do.call(rbind, lapply(DATA, `[[`, "qc"))
rownames(COHORT_QC) <- NULL

# 3. S4 PANEL A: THREE EXCLUSIVE 30-DAY DISPOSITION CATEGORIES --------------
make_panel_a <- function(d, database) {
  out <- lapply(0:2, function(code) {
    g <- d[d$primary_exposure_code == code, , drop = FALSE]
    n <- nrow(g)
    alive <- sum(g$alive_discharge_by_day30)
    dead <- sum(g$death_before_day30)
    still <- sum(g$still_hospitalized_day30)
    if (n != alive + dead + still) {
      stop(database, ": disposition counts do not sum to group N")
    }
    data.frame(
      Database = database,
      `Early AKI trajectory` = LABELS[code + 1L],
      N = n,
      `Discharged alive within 30 days, n (%)` = format_n_pct(alive, n),
      `Died before day 30, n` = dead,
      `Still hospitalized at day 30, n` = still,
      check.names = FALSE
    )
  })
  do.call(rbind, out)
}
PANEL_A <- do.call(rbind, lapply(names(DATA), function(db)
  make_panel_a(DATA[[db]]$data, db)))
rownames(PANEL_A) <- NULL

PANEL_A_NUMERIC <- do.call(rbind, lapply(names(DATA), function(db) {
  d <- DATA[[db]]$data
  do.call(rbind, lapply(0:2, function(code) {
    g <- d[d$primary_exposure_code == code, , drop = FALSE]
    data.frame(
      Database = db, Trajectory = LABELS[code + 1L], N = nrow(g),
      Alive_discharge_by_day30 = sum(g$alive_discharge_by_day30),
      Death_before_day30 = sum(g$death_before_day30),
      Still_hospitalized_day30 = sum(g$still_hospitalized_day30)
    )
  }))
}))
rownames(PANEL_A_NUMERIC) <- NULL

# 4. S4 PANELS B/C: ORIGINAL ADJUSTED LOGISTIC REGRESSIONS -------------------
# Keep the exact original Reviewer-2 binary model specification. For eICU,
# standard errors are hospital-clustered sandwich HC1; for MIMIC model-based.
fit_alive_discharge <- function(d, database, aki_only = FALSE) {
  if (aki_only) {
    d <- d[d$primary_exposure_code %in% 1:2, , drop = FALSE]
    if (anyNA(d$peak_aki_stage_0_48h) ||
        any(!(d$peak_aki_stage_0_48h %in% 1:3))) {
      stop(database, ": invalid peak AKI stage in within-AKI analysis")
    }
    d$peak_stage_high <- as.integer(d$peak_aki_stage_0_48h >= 2)
    d$recovery_status <- factor(d$primary_exposure_code,
                                levels = c(1, 2),
                                labels = c("Resolved early AKI", "Ongoing early AKI"))
    exposure_name <- "recovery_status"
    expected_terms <- "recovery_statusOngoing early AKI"
    comparisons <- "Ongoing early AKI vs resolved early AKI"
    panel <- "C"
  } else {
    exposure_name <- "analysis_exposure"
    expected_terms <- paste0("analysis_exposure", LABELS[2:3])
    comparisons <- c("Resolved early AKI vs no early AKI",
                     "Ongoing early AKI vs no early AKI")
    panel <- "B"
  }
  predictors <- c(exposure_name,
                  if (aki_only) "peak_stage_high" else character(), COVARS)
  needed <- c("alive_discharge_by_day30", predictors,
              if (database == "eICU-CRD") "hospital_id" else character())
  check_fields(d, needed, paste(database, "S4 Panel", panel))
  if (!all(stats::complete.cases(d[, needed, drop = FALSE]))) {
    stop(database, ": missing values would change the frozen Panel ", panel,
         " model sample. Review R01 input/QC.")
  }
  observed_groups <- sort(unique(d$primary_exposure_code))
  if (!identical(as.integer(observed_groups),
                 if (aki_only) c(1L, 2L) else 0:2)) {
    stop(database, ": one or more required trajectory groups absent")
  }
  formula <- stats::reformulate(predictors, response = "alive_discharge_by_day30")
  model_warnings <- character(0)
  fit <- withCallingHandlers(
    stats::glm(formula, data = d, family = stats::binomial(),
               na.action = stats::na.fail, model = TRUE),
    warning = function(w) {
      model_warnings <<- unique(c(model_warnings, conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )
  if (!isTRUE(fit$converged)) stop(database, ": S4 Panel ", panel,
                                  " logistic regression did not converge")
  if (length(model_warnings)) {
    keep_warning(database, "/Panel ", panel, ": ",
                 paste(model_warnings, collapse = " | "))
  }
  beta <- stats::coef(fit)
  if (any(!is.finite(beta))) {
    stop(database, ": non-finite logistic model coefficient")
  }
  covariance <- if (database == "eICU-CRD") {
    if (length(unique(d$hospital_id)) < 2L) {
      stop("eICU-CRD: fewer than 2 hospitals for clustered HC1 inference")
    }
    sandwich::vcovCL(fit, cluster = d$hospital_id, type = "HC1")
  } else {
    stats::vcov(fit)
  }
  if (!all(expected_terms %in% names(beta))) {
    stop(database, ": missing exposure contrast: ",
         paste(setdiff(expected_terms, names(beta)), collapse = ", "))
  }
  results <- lapply(seq_along(expected_terms), function(i) {
    term <- expected_terms[i]
    se <- sqrt(covariance[term, term])
    if (!is.finite(se) || se <= 0) stop("Invalid S4 model SE: ", term)
    b <- beta[[term]]
    data.frame(
      Database = database, Panel = panel,
      Comparison = comparisons[i],
      N = nrow(d),
      Alive_discharge_by_day30 = sum(d$alive_discharge_by_day30),
      OR = exp(b),
      CI_low = exp(b - 1.96 * se),
      CI_high = exp(b + 1.96 * se),
      P_value = 2 * stats::pnorm(abs(b / se), lower.tail = FALSE),
      Coefficient = b, SE = se,
      Standard_error_method = if (database == "MIMIC-IV")
        "Model-based" else "Hospital-clustered HC1",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, results)
}

MODEL_NUMERIC <- do.call(rbind, lapply(names(DATA), function(db) {
  d <- DATA[[db]]$data
  rbind(fit_alive_discharge(d, db, aki_only = FALSE),
        fit_alive_discharge(d, db, aki_only = TRUE))
}))
rownames(MODEL_NUMERIC) <- NULL
if (nrow(MODEL_NUMERIC) != 6L) {
  stop("Expected 6 S4 contrast estimates (2 primary + 1 AKI-only per database)")
}

# 5. S4 PANEL B / C MANUSCRIPT LAYOUT ----------------------------------------
make_panel_b <- function(db) {
  x <- MODEL_NUMERIC[MODEL_NUMERIC$Database == db &
                       MODEL_NUMERIC$Panel == "B", , drop = FALSE]
  if (nrow(x) != 2L) stop("Panel B needs two AKI comparisons: ", db)
  res <- x[grepl("^Resolved", x$Comparison), , drop = FALSE]
  ong <- x[grepl("^Ongoing", x$Comparison), , drop = FALSE]
  if (nrow(res) != 1L || nrow(ong) != 1L) stop("Bad Panel B contrasts: ", db)
  data.frame(
    Cohort = db,
    `N / alive discharge within 30 days` = paste0(x$N[1], " / ",
                                                  x$Alive_discharge_by_day30[1]),
    `Resolved early AKI vs no early AKI, adjusted OR (95% CI)` =
      format_or(res$OR, res$CI_low, res$CI_high),
    `Ongoing early AKI vs no early AKI, adjusted OR (95% CI)` =
      format_or(ong$OR, ong$CI_low, ong$CI_high),
    check.names = FALSE
  )
}
PANEL_B <- do.call(rbind, lapply(names(DATA), make_panel_b))
rownames(PANEL_B) <- NULL

PANEL_C <- do.call(rbind, lapply(names(DATA), function(db) {
  x <- MODEL_NUMERIC[MODEL_NUMERIC$Database == db &
                       MODEL_NUMERIC$Panel == "C", , drop = FALSE]
  if (nrow(x) != 1L) stop("Panel C needs one AKI comparison: ", db)
  data.frame(
    Cohort = db,
    `N / alive discharge within 30 days` =
      paste0(x$N, " / ", x$Alive_discharge_by_day30),
    Comparison = "Ongoing early AKI vs resolved early AKI",
    `Adjusted OR (95% CI)` = format_or(x$OR, x$CI_low, x$CI_high),
    check.names = FALSE
  )
}))
rownames(PANEL_C) <- NULL

# 6. PUBLISHED-RESULT QC (DOES NOT ALTER RESULTS) ---------------------------
observed_a <- merge(PANEL_A_NUMERIC, EXPECTED_PANEL_A,
                    by = c("Database", "Trajectory"),
                    suffixes = c("_observed", "_expected"), sort = FALSE)
if (nrow(observed_a) != nrow(EXPECTED_PANEL_A)) {
  stop("S4 Panel A expected/observed comparison row mismatch")
}
for (field in c("N", "Alive_discharge_by_day30", "Death_before_day30",
                "Still_hospitalized_day30")) {
  observed_a[[paste0(field, "_pass")]] <-
    observed_a[[paste0(field, "_observed")]] ==
    observed_a[[paste0(field, "_expected")]]
}

observed_models <- merge(
  MODEL_NUMERIC, EXPECTED_MODELS,
  by = c("Database", "Panel", "Comparison"), sort = FALSE
)
if (nrow(observed_models) != nrow(EXPECTED_MODELS)) {
  stop("S4 Panel B/C expected/observed comparison row mismatch")
}
for (metric in c("OR", "CI_low", "CI_high")) {
  observed_models[[paste0(metric, "_round2_pass")]] <-
    round(observed_models[[metric]], 2) ==
    observed_models[[paste0(metric, "_expected")]]
}

pass_a <- all(unlist(observed_a[grep("_pass$", names(observed_a))]))
pass_b <- all(unlist(observed_models[grep("_round2_pass$", names(observed_models))]))
if (!pass_a || !pass_b) {
  keep_warning("One or more S4 values differ from final reported table. ",
               "Review the published-value QC files; never adjust results to match.")
  if (STOP_ON_PUBLISHED_VALUE_MISMATCH) {
    stop("Published-value QC mismatch; inspect result tables")
  }
}

# 7. WRITE AGGREGATE-ONLY OUTPUTS -------------------------------------------
write_aggregate(PANEL_A, "TableS4_PanelA_disposition.csv")
write_aggregate(PANEL_B, "TableS4_PanelB_primary_adjusted.csv")
write_aggregate(PANEL_C, "TableS4_PanelC_within_AKI.csv")
write_aggregate(MODEL_NUMERIC, "TableS4_model_numeric.csv")
write_aggregate(PANEL_A_NUMERIC, "TableS4_PanelA_numeric_QC.csv")
write_aggregate(COHORT_QC, "TableS4_cohort_time_QC.csv")
write_aggregate(observed_a, "TableS4_PanelA_published_values_QC.csv")
write_aggregate(observed_models, "TableS4_published_values_QC.csv")

if (requireNamespace("openxlsx", quietly = TRUE)) {
  openxlsx::write.xlsx(
    list(PanelA = PANEL_A, PanelB = PANEL_B, PanelC = PANEL_C,
         Model_numeric = MODEL_NUMERIC,
         Cohort_QC = COHORT_QC),
    file = file.path(OUTPUT_DIR, "TableS4_alive_discharge_30d.xlsx"),
    overwrite = TRUE
  )
  log_msg("Wrote optional TableS4_alive_discharge_30d.xlsx")
}

note <- c(
  "COPD-AKI trajectory: 30-day discharge alive after the ICU +60h landmark",
  "Final manuscript: Supplementary Table S4, Panels A-C",
  "Inputs: R01 mimic_primary_clean.rds, eicu_primary_clean.rds",
  "No SQL rerun or new AKI exposure classification is performed in R09.",
  "Endpoint: alive discharge from hospital at or before 30 days after 60h.",
  "Hospital death and still hospitalized at day 30 are coded as not discharged alive.",
  "Panel B: primary adjustment covariates; reference=no early AKI.",
  "Panel C: early AKI only, plus peak AKI stage 2-3 (vs 1); reference=resolved.",
  "MIMIC: model-based Wald SE; eICU: hospital-clustered HC1 robust Wald SE.",
  "The OR describes odds of hospital discharge alive by day 30; NOT kidney recovery.",
  "Original reviewer script included exploratory Cox and survivor-only LOS models.",
  "Those models are not part of the final manuscript S4 and are excluded here.",
  "Original and derived patient-level datasets are never exported by R09.",
  paste("S4 Panel A published counts match:", pass_a),
  paste("S4 Panel B/C published rounded estimates match:", pass_b),
  paste("R version:", R.version.string),
  paste("sandwich version:", as.character(utils::packageVersion("sandwich"))),
  paste("Run timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "Notes and validation warnings:",
  if (length(MODEL_WARNINGS)) unique(MODEL_WARNINGS) else "None",
  "",
  "Run log:", LOG
)
writeLines(note, file.path(OUTPUT_DIR, "09_run_notes.txt"), useBytes = TRUE)

log_msg("SUCCESS: S4 Panels A-C generated from original primary cohorts.")
