# ==============================================================================
# EXPLORATORY / NOT REPORTED IN THE FINAL MANUSCRIPT
# COPD early creatinine-defined AKI trajectory: time-to-discharge and LOS
# MIMIC-IV v3.1 and eICU-CRD v2.0 | 60-hour landmark
#
# Adapted from the author's original:
#   10_Reviewer2_Secondary_Clinical_Outcomes.R
#
# The final Supplementary Table S4 binary 30-day alive-discharge outcome is
# reproduced by R/09_alive_discharge_30d.R, NOT by this exploratory file.
# This optional script focuses on two UNREPORTED analyses:
# (A) Cause-specific Cox regression for alive discharge within 30 days,
#     with hospital death treated as a competing terminal event and censored
#     at its observed event time; do not interpret the cause-specific HR as a
#     cumulative incidence/risk ratio.
# (B) Gamma(log-link) regression for remaining hospital days AFTER the 60h
#     landmark, restricted to patients ultimately discharged ALIVE. This
#     survivor-only analysis is selected on a post-exposure outcome and is
#     not a causal effect of AKI trajectory on LOS or kidney recovery.
# Also preserves source-status conflict and survivor >180-day LOS sensitivity.
#
# INPUT: R01 patient-level primary RDS (LOCAL ONLY, NEVER GIT COMMIT).
# Run from repository root, after R01:
#   Rscript exploratory/secondary_cox_los.R
# Optional: COPD_AKI_PROJECT_ROOT=/path/to/repository
# Needs: survival, sandwich. No patient-level output is written.
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999, warn = 1,
        contrasts = c("contr.treatment", "contr.poly"))
ROOT_OVERRIDE <- Sys.getenv("COPD_AKI_PROJECT_ROOT", unset = "")
PROJECT_ROOT <- normalizePath(
  if (nzchar(ROOT_OVERRIDE)) ROOT_OVERRIDE else ".",
  winslash = "/", mustWork = TRUE
)
RDS_DIR <- file.path(PROJECT_ROOT, "output", "R_analysis_output")
OUTPUT_DIR <- file.path(RDS_DIR, "exploratory_secondary_cox_los")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

DAY_HORIZON <- 30
TAIL_SENSITIVITY_LIMIT_DAYS <- 180
TIME_TOLERANCE_DAYS <- 1e-6
RUN_INTERNAL_TESTS <- TRUE
COVARS <- c(
  "age", "female_flag", "ckd_flag", "heart_failure_flag",
  "diabetes_flag", "chronic_liver_disease_flag", "malignancy_flag",
  "standard_nonrenal_sofa_24h"
)
EXPOSURE_LABELS <- c("No early AKI", "Resolved early AKI", "Ongoing early AKI")
EXPECTED <- data.frame(
  Database = c("MIMIC-IV", "eICU-CRD"),
  Primary_N = c(7627L, 7738L), Deaths = c(832L, 709L),
  Alive_discharge_by_day30_N = c(6579L, 6913L),
  No_AKI = c(5796L, 6644L), Resolved = c(898L, 465L),
  Ongoing = c(933L, 629L), stringsAsFactors = FALSE
)
INPUT_RDS <- c(
  "MIMIC-IV" = file.path(RDS_DIR, "mimic_primary_clean.rds"),
  "eICU-CRD" = file.path(RDS_DIR, "eicu_primary_clean.rds")
)
if (any(!file.exists(INPUT_RDS))) {
  stop("Run R/01_import_harmonize_qc.R first; primary RDS missing: ",
       paste(INPUT_RDS[!file.exists(INPUT_RDS)], collapse = "; "))
}
for (package in c("survival", "sandwich")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Install required R package before running: ", package)
  }
}

LOG <- character()
WARNINGS <- character()
log_note <- function(...) {
  line <- paste0(...)
  message(line)
  LOG <<- c(LOG, line)
  invisible(line)
}
flag_warning <- function(...) {
  line <- paste0(...)
  warning(line, call. = FALSE)
  WARNINGS <<- unique(c(WARNINGS, line))
  invisible(line)
}
require_fields <- function(x, cols, label) {
  miss <- setdiff(cols, names(x))
  if (length(miss)) stop(label, " missing columns: ", paste(miss, collapse = ", "))
}
number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  v <- trimws(as.character(x))
  v[v %in% c("", "NA", "N/A", "NULL", "null", "NaN")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", "", v, fixed = TRUE)))
}
identifier <- function(x) {
  v <- trimws(as.character(x))
  v[v %in% c("", "NA", "N/A", "NULL", "null")] <- NA_character_
  ok <- !is.na(v) & grepl("^[0-9]+[.]0+$", v)
  v[ok] <- sub("[.]0+$", "", v[ok])
  v
}
bind_tables <- function(xs) {
  xs <- Filter(function(z) is.data.frame(z) && nrow(z) > 0L, xs)
  if (!length(xs)) return(data.frame())
  z <- do.call(rbind, xs)
  rownames(z) <- NULL
  z
}
quant <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, p, na.rm = TRUE, names = FALSE))
}
write_table <- function(x, filename) {
  if (!is.data.frame(x)) stop("Expected aggregate data.frame: ", filename)
  if (any(names(x) %in% c("icu_id", "stay_id", "patientunitstayid",
                          "subject_id", "uniquepid"))) {
    stop("Do not export patient identifiers: ", filename)
  }
  utils::write.csv(x, file.path(OUTPUT_DIR, filename), row.names = FALSE,
                   na = "", fileEncoding = "UTF-8")
  log_note("Saved: ", filename)
}

# --- Identical 30-day endpoint definition to original Reviewer 2 R10 ----------
make_outcome_flags <- function(t, death, horizon = DAY_HORIZON) {
  stopifnot(length(t) == length(death))
  if (any(!is.finite(t) | t <= 0) || anyNA(death) ||
      any(!(death %in% 0:1))) stop("Invalid terminal follow-up time/death")
  list(
    discharged_alive = as.integer(death == 0),
    alive_discharge_by_day30 = as.integer(death == 0 & t <= horizon),
    death_before_day30 = as.integer(death == 1 & t <= horizon),
    still_in_hospital_day30 = as.integer(t > horizon),
    time_capped_day30 = pmin(t, horizon)
  )
}
if (RUN_INTERNAL_TESTS) {
  z <- make_outcome_flags(c(3, 4, 30, 30.001, 6), c(0, 1, 0, 0, 1))
  stopifnot(identical(z$alive_discharge_by_day30, c(1L, 0L, 1L, 0L, 0L)))
  stopifnot(identical(z$death_before_day30, c(0L, 1L, 0L, 0L, 1L)))
  stopifnot(identical(z$still_in_hospital_day30, c(0L, 0L, 0L, 1L, 0L)))
  stopifnot(all(z$alive_discharge_by_day30 + z$death_before_day30 +
                z$still_in_hospital_day30 == 1L))
}

# --- Verify terminal time where source fields survive R01 harmonization ------
independent_followup_qc <- function(x, db) {
  status <- "source_fields_unavailable"
  diff <- rep(NA_real_, nrow(x))
  if (db == "MIMIC-IV" &&
      all(c("outcome_or_discharge_time", "landmark_60h_time") %in% names(x))) {
    t1 <- as.POSIXct(x$outcome_or_discharge_time, tz = "UTC")
    t0 <- as.POSIXct(x$landmark_60h_time, tz = "UTC")
    if (!anyNA(t0) && !anyNA(t1)) {
      independent <- as.numeric(difftime(t1, t0, units = "days"))
      diff <- x$followup_days_after_60h - independent
      status <- "verified_source_timestamps"
    } else status <- "timestamp_parse_incomplete"
  } else if (db == "eICU-CRD" && "hospitaldischargeoffset" %in% names(x)) {
    off <- number(x$hospitaldischargeoffset)
    if (!anyNA(off)) {
      independent <- (off - 3600) / 1440
      diff <- x$followup_days_after_60h - independent
      status <- "verified_source_offset"
    } else status <- "discharge_offset_missing"
  }
  if (status %in% c("verified_source_timestamps", "verified_source_offset") &&
      any(!is.finite(diff) | abs(diff) > TIME_TOLERANCE_DAYS)) {
    stop(db, ": terminal follow-up time disagrees with SQL source timestamps")
  }
  data.frame(Database = db, Time_check = status, N = nrow(x),
             Max_absolute_difference_days = if (all(is.na(diff))) NA_real_ else
               max(abs(diff), na.rm = TRUE))
}

# --- Original cohort and outcome construction --------------------------------
load_primary <- function(path, db) {
  x <- readRDS(path)
  if (!is.data.frame(x)) stop(db, ": R01 primary RDS not a data.frame")
  require_fields(x, c("primary_exposure_code", "hospital_death_after_60h",
     "followup_days_after_60h", "peak_aki_stage_0_48h", COVARS),
     paste(db, "R01 primary data"))
  id_col <- if (db == "MIMIC-IV") "stay_id" else "patientunitstayid"
  require_fields(x, id_col, db)
  x$.internal_id <- identifier(x[[id_col]])
  if (anyNA(x$.internal_id) || anyDuplicated(x$.internal_id)) {
    stop(db, ": missing/duplicate patient-level stay ID")
  }
  x$hospital_id <- if (db == "MIMIC-IV") rep("MIMIC", nrow(x)) else {
    if ("hospitalid" %in% names(x)) identifier(x$hospitalid) else {
      require_fields(x, "hospital_id", db); identifier(x$hospital_id)
    }
  }
  if (db == "eICU-CRD" && anyNA(x$hospital_id)) {
    stop("Missing eICU hospital ID for cluster robust models")
  }
  num_fields <- c("primary_exposure_code", "hospital_death_after_60h",
    "followup_days_after_60h", "peak_aki_stage_0_48h", COVARS)
  for (field in num_fields) x[[field]] <- number(x[[field]])
  if (anyNA(x$primary_exposure_code) ||
      any(!x$primary_exposure_code %in% 0:2) ||
      anyNA(x$hospital_death_after_60h) ||
      any(!x$hospital_death_after_60h %in% 0:1) ||
      any(!stats::complete.cases(x[, COVARS, drop = FALSE]))) {
    stop(db, ": invalid cohort labels, mortality, or model covariates")
  }
  f <- make_outcome_flags(x$followup_days_after_60h,
                          x$hospital_death_after_60h)
  for (nm in names(f)) x[[nm]] <- f[[nm]]
  x$analysis_exposure <- factor(x$primary_exposure_code, levels = 0:2,
                                labels = EXPOSURE_LABELS)
  x$recovery_status <- factor(x$primary_exposure_code, levels = 1:2,
                              labels = EXPOSURE_LABELS[2:3])
  x$peak_stage_high <- as.integer(x$peak_aki_stage_0_48h >= 2)
  expected <- EXPECTED[EXPECTED$Database == db, , drop = FALSE]
  observed <- c(N = nrow(x), Deaths = sum(x$hospital_death_after_60h == 1),
    Alive_day30 = sum(x$alive_discharge_by_day30 == 1),
    No_AKI = sum(x$primary_exposure_code == 0),
    Resolved = sum(x$primary_exposure_code == 1),
    Ongoing = sum(x$primary_exposure_code == 2))
  aim <- c(N = expected$Primary_N, Deaths = expected$Deaths,
    Alive_day30 = expected$Alive_discharge_by_day30_N,
    No_AKI = expected$No_AKI, Resolved = expected$Resolved,
    Ongoing = expected$Ongoing)
  if (!identical(as.numeric(observed), as.numeric(aim))) {
    stop(db, ": frozen primary / day30 counts do not match manuscript")
  }
  log_note(db, ": N=", nrow(x), ", alive discharge by day30=",
           sum(x$alive_discharge_by_day30))
  x
}
DATA <- lapply(names(INPUT_RDS), function(db) load_primary(INPUT_RDS[[db]], db))
names(DATA) <- names(INPUT_RDS)

# --- Aggregate descriptive outcome data --------------------------------------
describe_outcomes <- function(d, db) {
  rows <- list()
  for (code in 0:2) {
    g <- d[d$primary_exposure_code == code, , drop = FALSE]
    survivors <- g$followup_days_after_60h[g$discharged_alive == 1]
    rows[[length(rows) + 1L]] <- data.frame(
      Database = db, Trajectory = EXPOSURE_LABELS[code + 1L], N = nrow(g),
      Deaths = sum(g$hospital_death_after_60h == 1),
      Alive_discharge_by_day7_N = sum(g$discharged_alive == 1 &
                                        g$followup_days_after_60h <= 7),
      Alive_discharge_by_day14_N = sum(g$discharged_alive == 1 &
                                         g$followup_days_after_60h <= 14),
      Alive_discharge_by_day30_N = sum(g$alive_discharge_by_day30 == 1),
      Survivor_N = length(survivors),
      Survivor_remaining_LOS_median_days = quant(survivors, .5),
      Survivor_remaining_LOS_p25_days = quant(survivors, .25),
      Survivor_remaining_LOS_p75_days = quant(survivors, .75),
      Survivor_remaining_LOS_mean_days = mean(survivors),
      Survivor_remaining_LOS_gt180_N = sum(survivors > TAIL_SENSITIVITY_LIMIT_DAYS)
    )
  }
  bind_tables(rows)
}

# --- Cox and Gamma engines: preserve original Reviewer 2 model conventions --
ADJ_RHS <- paste(c("analysis_exposure", COVARS), collapse = " + ")
AKI_RHS <- paste(c("recovery_status", "peak_stage_high", COVARS),
                 collapse = " + ")
MODEL_KINDS <- c("Alive_discharge_cause_specific_Cox",
                 "Survivor_remaining_LOS_Gamma")

fit_endpoint <- function(df, db, scenario, model_kind, aki_only = FALSE) {
  d <- df
  if (aki_only) d <- d[d$primary_exposure_code %in% 1:2, , drop = FALSE]
  if (model_kind == "Survivor_remaining_LOS_Gamma") {
    d <- d[d$discharged_alive == 1, , drop = FALSE]
  }
  rhs <- if (aki_only) AKI_RHS else ADJ_RHS
  if (model_kind == "Alive_discharge_cause_specific_Cox") {
    form <- stats::as.formula(paste(
      "survival::Surv(time_capped_day30, alive_discharge_by_day30) ~", rhs))
  } else if (model_kind == "Survivor_remaining_LOS_Gamma") {
    form <- stats::as.formula(paste("followup_days_after_60h ~", rhs))
  } else stop("Unexpected model kind: ", model_kind)
  needed <- all.vars(form)
  if (db == "eICU-CRD") needed <- union(needed, "hospital_id")
  complete <- stats::complete.cases(d[, needed, drop = FALSE])
  if (any(!complete)) flag_warning(db, "/", scenario, "/", model_kind,
                                   " dropped ", sum(!complete), " incomplete cases")
  d <- d[complete, , drop = FALSE]
  if (nrow(d) < 80L || length(unique(d[[if (aki_only) "recovery_status" else
                                            "analysis_exposure"]])) < 2L) {
    flag_warning(db, "/", scenario, "/", model_kind, ": insufficient sample")
    return(list(table = data.frame(), ph = data.frame()))
  }
  if (model_kind == "Survivor_remaining_LOS_Gamma" &&
      any(!is.finite(d$followup_days_after_60h) |
          d$followup_days_after_60h <= 0)) {
    stop("Gamma survivor-LOS model contains invalid followup days")
  }
  warnings_here <- character()
  fit <- tryCatch(withCallingHandlers({
    if (model_kind == "Alive_discharge_cause_specific_Cox") {
      if (db == "eICU-CRD") {
        survival::coxph(form, data = d, ties = "efron",
                       cluster = d$hospital_id, robust = TRUE,
                       na.action = stats::na.fail, model = TRUE, x = TRUE)
      } else {
        survival::coxph(form, data = d, ties = "efron",
                       na.action = stats::na.fail, model = TRUE, x = TRUE)
      }
    } else {
      stats::glm(form, data = d, family = stats::Gamma(link = "log"),
                 na.action = stats::na.fail, model = TRUE)
    }
  }, warning = function(w) {
    warnings_here <<- c(warnings_here, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) e)
  if (inherits(fit, "error") ||
      (inherits(fit, "glm") && !isTRUE(fit$converged))) {
    flag_warning(db, "/", scenario, "/", model_kind, ": fit failed/nonconvergent")
    return(list(table = data.frame(), ph = data.frame()))
  }
  if (length(warnings_here)) flag_warning(db, "/", scenario, "/", model_kind,
                                         " fitting warning: ", paste(unique(warnings_here), collapse = " | "))
  covariance <- tryCatch({
    if (model_kind == "Alive_discharge_cause_specific_Cox") {
      stats::vcov(fit)  # eICU robust cluster covariance is in coxph fit
    } else if (db == "eICU-CRD") {
      sandwich::vcovCL(fit, cluster = d$hospital_id, type = "HC1")
    } else sandwich::vcovHC(fit, type = "HC1")
  }, error = function(e) e)
  if (inherits(covariance, "error")) {
    flag_warning(db, "/", scenario, "/", model_kind, ": covariance failed")
    return(list(table = data.frame(), ph = data.frame()))
  }
  beta <- stats::coef(fit)
  prefix <- if (aki_only) "recovery_status" else "analysis_exposure"
  terms <- grep(paste0("^", prefix), names(beta), value = TRUE)
  out <- lapply(terms, function(term) {
    if (!term %in% rownames(covariance) ||
        !is.finite(beta[[term]]) ||
        !is.finite(covariance[term, term]) || covariance[term, term] <= 0) return(NULL)
    se <- sqrt(covariance[term, term]); b <- beta[[term]]
    data.frame(Database = db, Scenario = scenario, Model = model_kind,
      Population = if (aki_only) "Early AKI only" else
        if (model_kind == "Survivor_remaining_LOS_Gamma") "Alive-discharge survivors" else
          "All primary-analysis patients",
      Contrast = substring(term, nchar(prefix) + 1L),
      Estimate_type = if (model_kind == "Alive_discharge_cause_specific_Cox")
        "Cause-specific hazard ratio" else "Ratio of conditional mean LOS",
      Estimate = exp(b), CI95_low = exp(b - 1.96 * se),
      CI95_high = exp(b + 1.96 * se),
      P_value = 2 * stats::pnorm(abs(b / se), lower.tail = FALSE),
      N = nrow(d), Alive_discharge_by_day30_N = sum(d$alive_discharge_by_day30),
      Discharged_alive_N = sum(d$discharged_alive),
      SE_method = if (db == "eICU-CRD") "Hospital-clustered robust" else
        if (model_kind == "Survivor_remaining_LOS_Gamma") "HC1 robust" else
          "Model-based")
  })
  ph <- data.frame()
  if (model_kind == "Alive_discharge_cause_specific_Cox") {
    z <- tryCatch(survival::cox.zph(fit), error = function(e) e)
    if (inherits(z, "error")) {
      flag_warning(db, "/", scenario, ": Cox PH diagnostic failed")
    } else {
      tab <- as.data.frame(z$table)
      tab$Term <- rownames(tab)
      tab$Database <- db
      tab$Scenario <- scenario
      rownames(tab) <- NULL
      ph <- tab[, c("Database", "Scenario", "Term",
                    setdiff(names(tab), c("Database", "Scenario", "Term"))), drop = FALSE]
    }
  }
  list(table = bind_tables(out), ph = ph)
}

# --- Source discharge status QC (never overwrite source mortality) -----------
disposition_qc <- function(x, db) {
  if (db == "MIMIC-IV" && "discharge_location" %in% names(x)) {
    loc <- toupper(trimws(as.character(x$discharge_location)))
    loc[loc == "" | is.na(loc)] <- NA_character_
    conflict <- !is.na(loc) & ((loc == "DIED") !=
                                    (x$hospital_death_after_60h == 1))
    available <- sum(!is.na(loc))
  } else if (db == "eICU-CRD" && "hospitaldischargestatus" %in% names(x)) {
    status <- tolower(trimws(as.character(x$hospitaldischargestatus)))
    status[status == "" | is.na(status)] <- NA_character_
    conflict <- !is.na(status) & ((status == "expired") !=
                                         (x$hospital_death_after_60h == 1))
    available <- sum(!is.na(status))
  } else {
    return(list(summary = data.frame(Database = db, Status_source = "unavailable",
                                     N = nrow(x), Records_with_status = 0L,
                                     Conflicting_records = NA_integer_),
                flags = rep(NA, nrow(x))))
  }
  list(summary = data.frame(Database = db, Status_source = "available",
                            N = nrow(x), Records_with_status = available,
                            Conflicting_records = sum(conflict)), flags = conflict)
}

# --- Execute original exploratory analyses -----------------------------------
DESCRIPTIVE <- list(); TIME_QC <- list(); STATUS_QC <- list(); MODELS <- list()
PH_DIAGNOSTICS <- list(); SENSITIVITIES <- list(); WITHIN_AKI <- list()
for (db in names(DATA)) {
  d <- DATA[[db]]
  TIME_QC[[db]] <- independent_followup_qc(d, db)
  STATUS_QC[[db]] <- disposition_qc(d, db)$summary
  DESCRIPTIVE[[db]] <- describe_outcomes(d, db)
  for (kind in MODEL_KINDS) {
    result <- fit_endpoint(d, db, "Frozen_primary", kind)
    MODELS[[length(MODELS) + 1L]] <- result$table
    PH_DIAGNOSTICS[[length(PH_DIAGNOSTICS) + 1L]] <- result$ph
    within <- fit_endpoint(d, db, "Within_early_AKI_peak_adjusted", kind,
                           aki_only = TRUE)
    WITHIN_AKI[[length(WITHIN_AKI) + 1L]] <- within$table
    PH_DIAGNOSTICS[[length(PH_DIAGNOSTICS) + 1L]] <- within$ph
  }
  # Source text is only supporting QC. It never replaces mortality source.
  conflict <- disposition_qc(d, db)$flags
  if (!all(is.na(conflict)) && any(conflict)) {
    clean <- d[!conflict, , drop = FALSE]
    for (kind in MODEL_KINDS) {
      result <- fit_endpoint(clean, db, "Exclude_discharge_status_conflict", kind)
      SENSITIVITIES[[length(SENSITIVITIES) + 1L]] <- result$table
    }
  }
  # The original tail sensitivity keeps all nonsurvivors in the input but
  # Gamma internally restricts to survivors with remaining LOS <= 180 days.
  short_survivors <- d[d$discharged_alive == 0 |
                         d$followup_days_after_60h <= TAIL_SENSITIVITY_LIMIT_DAYS,
                       , drop = FALSE]
  result <- fit_endpoint(short_survivors, db, "Survivor_LOS_at_most_180d",
                         "Survivor_remaining_LOS_Gamma")
  SENSITIVITIES[[length(SENSITIVITIES) + 1L]] <- result$table
}

write_table(bind_tables(DESCRIPTIVE), "Exploratory_discharge_LOS_descriptive.csv")
write_table(bind_tables(MODELS), "Exploratory_Cox_Gamma_adjusted_models.csv")
write_table(bind_tables(WITHIN_AKI), "Exploratory_Cox_Gamma_within_AKI.csv")
write_table(bind_tables(PH_DIAGNOSTICS), "Exploratory_Cox_PH_diagnostics.csv")
write_table(bind_tables(SENSITIVITIES), "Exploratory_Cox_Gamma_sensitivities.csv")
write_table(bind_tables(TIME_QC), "Exploratory_followup_time_QC.csv")
write_table(bind_tables(STATUS_QC), "Exploratory_discharge_status_QC.csv")

writeLines(c(
  "EXPLORATORY RESEARCH RECORD - NOT FINAL MANUSCRIPT RESULTS",
  "Source: 10_Reviewer2_Secondary_Clinical_Outcomes.R",
  "Official S4 binary 30-day alive discharge is reproduced by R09, not here.",
  "Cause-specific Cox: hospital death is censored at the terminal time,",
  "alive discharge is an event only if <=30 days after the 60h landmark.",
  "HR is NOT a ratio of cumulative probabilities of alive discharge.",
  "Gamma(log) uses time from landmark to hospital end among ALIVE",
  "discharges only, not all patients. Survivor selection/collider bias applies.",
  "MIMIC Cox SE model-based; eICU Cox SE hospital-clustered robust.",
  "Gamma SE: MIMIC HC1; eICU hospital-clustered HC1.",
  "The 180-day tail sensitivity and source-status QC retain original design.",
  "No patient identifiers or patient-level records are exported.",
  "Do not treat these explorations as proof of causal kidney recovery.",
  "", "Warnings:", if (length(WARNINGS)) WARNINGS else "None recorded",
  "", "Log:", LOG, "", capture.output(utils::sessionInfo())
), file.path(OUTPUT_DIR, "exploratory_run_notes.txt"), useBytes = TRUE)
log_note("Exploratory cause-specific Cox and Gamma LOS completed: ", OUTPUT_DIR)
