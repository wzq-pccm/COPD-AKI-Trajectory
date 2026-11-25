# ==============================================================================
# COPD early creatinine-defined AKI trajectory study
# R/07_temporal_pair_reclassification.R
# MIMIC-IV v3.1 + eICU-CRD v2.0 | FINAL Supplementary Table S7 Panels A and B
#
# PUBLIC REPRODUCTION ADAPTATION OF:
#   08_Reviewer1_Strict_KDIGO_Temporal_Reclassification_v3.R
#
# Scientific algorithm (scr_stage/get_hit/classify_one and deterministic tests)
# is copied unchanged from the author's strict observed-pair sensitivity script.
# The OFFICIAL manuscript S7 implementation uses the NEAREST QUALIFYING PRIOR
# MEASURED sCr comparator only. Exploratory lowest/fixed modes are not reported
# by this script. The original fixed-reference primary exposure is never
# overwritten or used as a temporal-pair comparator for the official result.
#
# Strict observed-pair definitions:
#   * Early sCr event: ICU hours 0-48, inclusive.
#   * Strictly preceding observed sCr comparator, not a future low.
#   * sCr rise >=0.3 mg/dL in <=48h, OR >=1.5x in <=168h (7 days).
#   * For early AKI: closest-to-hour-48 assessment within hours 36-60, but
#     STRICTLY LATER than first qualifying sCr measurement.
#   * Resolved when both fixed-to-temporal-anchor increase criteria are below
#     thresholds at assessment; otherwise ongoing.
#   * No valid comparator, no monitored assessment, or original AKI without
#     verified strict pair => INDETERMINATE, not no early AKI.
#   * No-early-AKI requires a temporal comparator and a 36-60h measurement.
#   * No urine output or RRT criterion is added; this is a sensitivity analysis,
#     not a complete clinical KDIGO diagnosis.
#
# INPUTS (from R01 and the two SQL serial-creatinine final SELECT exports):
#   output/R_analysis_output/mimic_full_clean.rds
#   output/R_analysis_output/eicu_full_clean.rds
#   data/sql_exports/mimiciv/07_serial_creatinine.csv  (or .xlsx/.xls)
#   data/sql_exports/eicu/08_serial_creatinine.csv    (or .xlsx/.xls)
#
# OUTPUT (aggregate, disclosure-review before Git publication):
#   TableS7_PanelA_classification.csv
#   TableS7_PanelA_numeric.csv
#   TableS7_PanelB_reclassification.csv
#   TableS7_PanelB_long.csv
#   TableS7_indeterminate_reasons.csv
#   TableS7_cohort_source_QC.csv
#   TableS7_published_values_QC.csv
#   07_run_notes.txt
#
# PRIVATE LOCAL OUTPUT FOR R08 (DO NOT COMMIT, contains individual stay data):
#   mimic_temporal_pair_joined.rds
#   eicu_temporal_pair_joined.rds
#
# Run from repository root: Rscript R/07_temporal_pair_reclassification.R
# Optional environment setting COPD_AKI_PROJECT_ROOT=/path/to/repository
# No automatic package installation and no Windows-specific absolute paths.
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)

# 0. PORTABLE SETTINGS ---------------------------------------------------------
ROOT_OVERRIDE <- Sys.getenv("COPD_AKI_PROJECT_ROOT", unset = "")
PROJECT_ROOT <- normalizePath(
  if (nzchar(ROOT_OVERRIDE)) ROOT_OVERRIDE else ".",
  winslash = "/", mustWork = TRUE
)
RDS_DIR <- file.path(PROJECT_ROOT, "output", "R_analysis_output")
SQL_DIR <- file.path(PROJECT_ROOT, "data", "sql_exports")
OUTPUT_DIR <- file.path(RDS_DIR, "07_temporal_pair_reclassification")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

STOP_ON_FROZEN_COHORT_MISMATCH <- TRUE
STOP_ON_EXPECTED_TABLE_MISMATCH <- FALSE   # Report discrepancies; do not force data.
RUN_INTERNAL_UNIT_TESTS <- TRUE
SAVE_LOCAL_JOINED_RDS <- TRUE            # Needed by R08 for S7 Panel C.
SCR_TOL <- 1e-9
HOUR_TOL <- 1e-8

RDS_INPUT <- c(
  "MIMIC-IV" = file.path(RDS_DIR, "mimic_full_clean.rds"),
  "eICU-CRD" = file.path(RDS_DIR, "eicu_full_clean.rds")
)
if (any(!file.exists(RDS_INPUT))) {
  stop("Missing R01 full cohort RDS: ",
       paste(RDS_INPUT[!file.exists(RDS_INPUT)], collapse = "; "),
       ". Run R/01_import_harmonize_qc.R first.")
}

resolve_export <- function(database_directory, stem) {
  directory <- file.path(SQL_DIR, database_directory)
  candidates <- file.path(directory, paste0(stem, c(".csv", ".xlsx", ".xls")))
  hits <- candidates[file.exists(candidates)]
  if (length(hits) != 1L) {
    stop("Exactly one SQL export required for ", stem, " under ", directory,
         "; found ", length(hits),
         ". Export the SQL FINAL SELECT as UTF-8 CSV (preferred) or Excel.")
  }
  normalizePath(hits, winslash = "/", mustWork = TRUE)
}
SCR_INPUT <- c(
  "MIMIC-IV" = resolve_export("mimiciv", "07_serial_creatinine"),
  "eICU-CRD" = resolve_export("eicu", "08_serial_creatinine")
)

needed_packages <- "readr"
missing_packages <- needed_packages[
  !vapply(needed_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install R package(s) before executing: ",
       paste(missing_packages, collapse = ", "))
}
if (any(tolower(tools::file_ext(SCR_INPUT)) %in% c("xlsx", "xls")) &&
    !requireNamespace("readxl", quietly = TRUE)) {
  stop("readxl is required for Excel inputs; alternatively export SQL as CSV.")
}

log_lines <- character(0)
msg <- function(...) {
  statement <- paste0(...)
  message(statement)
  log_lines <<- c(log_lines, statement)
  invisible(statement)
}
need <- function(x, columns, label) {
  absent <- setdiff(columns, names(x))
  if (length(absent)) stop(label, " missing column(s): ", paste(absent, collapse = ", "))
}
num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  y <- trimws(as.character(x))
  y[y %in% c("", "NA", "N/A", "NULL", "null", "NaN")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", "", y, fixed = TRUE)))
}
ids <- function(x) {
  y <- trimws(as.character(x))
  y[y %in% c("", "NA", "NULL", "null")] <- NA_character_
  decimal_int <- !is.na(y) & grepl("^[0-9]+[.]0+$", y)
  y[decimal_int] <- sub("[.]0+$", "", y[decimal_int])
  y
}
flag <- function(x) !is.na(x) & x == 1
unique_id <- function(x, label) {
  if (anyNA(x) || any(!nzchar(x))) stop(label, ": missing stay ID")
  if (anyDuplicated(x)) stop(label, ": duplicate stay ID")
}
check_set <- function(expected, observed, label) {
  missing <- setdiff(unique(expected), unique(observed))
  extra <- setdiff(unique(observed), unique(expected))
  if (length(missing) || length(extra)) {
    stop(label, ": stay-ID set mismatch: missing=", length(missing),
         ", extra=", length(extra))
  }
}
read_text <- function(path) {
  ext <- tolower(tools::file_ext(path))
  na_words <- c("", "NA", "N/A", "NULL", "null", "NaN")
  if (ext == "csv") {
    z <- readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character()),
      na = na_words, show_col_types = FALSE, progress = FALSE,
      name_repair = "minimal", locale = readr::locale(encoding = "UTF-8")
    )
  } else if (ext %in% c("xlsx", "xls")) {
    z <- readxl::read_excel(
      path, col_types = "text", sheet = 1, na = na_words,
      .name_repair = "minimal"
    )
  } else {
    stop("Unsupported file extension: ", ext)
  }
  z <- as.data.frame(z, check.names = FALSE)
  names(z) <- trimws(names(z))
  if (anyDuplicated(names(z))) stop("Duplicate field names in ", path)
  z
}
write_summary <- function(x, filename) {
  if (!is.data.frame(x)) stop("Aggregate output must be a data.frame: ", filename)
  utils::write.csv(x, file.path(OUTPUT_DIR, filename), row.names = FALSE,
                   na = "", fileEncoding = "UTF-8")
  msg("Saved summary: ", filename)
}

# 1. LOAD THE FROZEN R01 COHORTS ---------------------------------------------
NUM_FIELDS <- c(
  "primary_analysis_flag", "primary_exposure_code", "eligible_at_60h_flag",
  "hospital_death_after_60h", "reference_scr", "reference_scr_hours_from_icu",
  "aki_onset_hours_from_icu", "assessment_offset_from_48h_36_60h",
  "peak_aki_stage_0_48h", "scr_measurement_n_0_48h",
  "scr_measurement_n_36_60h", "age", "female_flag", "ckd_flag",
  "heart_failure_flag", "diabetes_flag", "chronic_liver_disease_flag",
  "malignancy_flag", "standard_nonrenal_sofa_24h"
)
COVARS <- c(
  "age", "female_flag", "ckd_flag", "heart_failure_flag",
  "diabetes_flag", "chronic_liver_disease_flag", "malignancy_flag",
  "standard_nonrenal_sofa_24h"
)
EXPECTED_FROZEN <- data.frame(
  Database = c("MIMIC-IV", "eICU-CRD"),
  Full_N = c(7794L, 8495L), Primary_N = c(7627L, 7738L),
  Deaths = c(832L, 709L), No_AKI = c(5796L, 6644L),
  Resolved = c(898L, 465L), Ongoing = c(933L, 629L),
  stringsAsFactors = FALSE
)
load_cohort <- function(path, database) {
  x <- readRDS(path)
  if (!is.data.frame(x)) stop(database, " R01 full RDS is not a data.frame")
  id_field <- if (database == "MIMIC-IV") "stay_id" else "patientunitstayid"
  need(x, c(id_field, "primary_analysis_flag", "primary_exposure_code",
            "hospital_death_after_60h", "reference_scr_source", NUM_FIELDS),
       paste(database, "R01 cohort"))
  for (field in intersect(NUM_FIELDS, names(x))) x[[field]] <- num(x[[field]])
  x$icu_id <- ids(x[[id_field]])
  x$database <- database
  if (database == "MIMIC-IV") {
    x$hospital_id <- rep("MIMIC", nrow(x))
  } else {
    need(x, "hospitalid", "eICU R01 cohort")
    x$hospital_id <- ids(x$hospitalid)
  }
  unique_id(x$icu_id, database)
  if (database == "eICU-CRD" && anyNA(x$hospital_id)) {
    stop("eICU cohort has missing hospitalid; R08 clustering would fail")
  }
  p <- x[flag(x$primary_analysis_flag) &
           !is.na(x$primary_exposure_code) & x$primary_exposure_code %in% 0:2,
         , drop = FALSE]
  if (!all(p$hospital_death_after_60h %in% c(0, 1))) {
    stop(database, ": nonbinary/missing primary outcome")
  }
  if (!all(stats::complete.cases(p[, COVARS, drop = FALSE]))) {
    stop(database, ": original primary adjustment covariates are incomplete")
  }
  observed <- c(
    Full_N = nrow(x), Primary_N = nrow(p),
    Deaths = sum(p$hospital_death_after_60h == 1),
    No_AKI = sum(p$primary_exposure_code == 0),
    Resolved = sum(p$primary_exposure_code == 1),
    Ongoing = sum(p$primary_exposure_code == 2)
  )
  expected <- as.numeric(unlist(EXPECTED_FROZEN[
    EXPECTED_FROZEN$Database == database, names(observed), drop = FALSE
  ], use.names = FALSE))
  names(expected) <- names(observed)
  if (!identical(as.numeric(observed), expected)) {
    explanation <- paste(names(observed)[observed != expected], collapse = ", ")
    if (STOP_ON_FROZEN_COHORT_MISMATCH) {
      stop(database, ": R01 frozen cohort mismatch in: ", explanation)
    }
    warning(database, ": R01 frozen cohort mismatch in: ", explanation)
  }
  msg("Verified R01 ", database, ": landmark=", nrow(x),
      ", original primary=", nrow(p))
  x
}
COHORT <- lapply(names(RDS_INPUT), function(db) load_cohort(RDS_INPUT[[db]], db))
names(COHORT) <- names(RDS_INPUT)

# 2. READ INDEPENDENT LONG-FORMAT sCr EVENTS ---------------------------------
# This is adapted from R08-v3; all SQL rows (including has_scr_event=0) first
# participate in the stay-set check. Only observed positive sCr measurements
# participate in the strict temporal-pair algorithm.
read_scr <- function(path, database, expected_ids) {
  z <- read_text(path)
  idcol <- if (database == "MIMIC-IV") "stay_id" else "patientunitstayid"
  labcol <- if (database == "MIMIC-IV") "labevent_id" else "labid"
  tcol <- if (database == "MIMIC-IV") "scr_hours_from_icu" else
    "labresultoffset_minutes_from_index_icu"
  need(z, c(idcol, labcol, tcol, "has_scr_event", "scr_numeric_as_extracted"),
       paste(database, "sCr long"))
  z$icu_id <- ids(z[[idcol]])
  check_set(expected_ids, z$icu_id, paste(database, "sCr long"))
  has <- num(z$has_scr_event)
  e <- data.frame(
    icu_id = z$icu_id, event_id = ids(z[[labcol]]),
    hour = num(z[[tcol]]), scr = num(z$scr_numeric_as_extracted),
    stringsAsFactors = FALSE
  )
  if (database == "eICU-CRD") e$hour <- e$hour / 60
  e <- e[!is.na(has) & has == 1 & is.finite(e$hour) &
           is.finite(e$scr) & e$scr > 0, , drop = FALSE]
  if (nrow(e) && anyNA(e$event_id)) {
    stop(database, ": observed sCr row has missing lab event ID")
  }
  duplicates <- sum(duplicated(e[c("icu_id", "event_id")]))
  if (duplicates > 0L) {
    warning(database, ": ", duplicates,
            " duplicate stay/event pairs; keeping the first, as in original R08-v3")
    e <- e[!duplicated(e[c("icu_id", "event_id")]), , drop = FALSE]
  }
  e <- e[order(e$icu_id, e$hour, e$event_id), , drop = FALSE]
  rownames(e) <- NULL
  list(
    events = e,
    by_stay = split(e[, c("event_id", "hour", "scr"), drop = FALSE],
                    e$icu_id, drop = TRUE),
    duplicate_n = duplicates,
    exported_rows = nrow(z)
  )
}
LABS <- lapply(names(SCR_INPUT), function(db) {
  msg("Reading long-format creatinine events for ", db)
  read_scr(SCR_INPUT[[db]], db, COHORT[[db]]$icu_id)
})
names(LABS) <- names(SCR_INPUT)
EMPTY_EVENTS <- data.frame(
  event_id = character(0), hour = numeric(0), scr = numeric(0),
  stringsAsFactors = FALSE
)

# 3. SOURCE ORIGINAL: TEMPORAL-PAIR DETECTION / UNIT TESTS --------------------
# 3. TEMPORAL-PAIR DETECTION
# ---------------------------------------------------------------------------
# Staging is based on a qualifying preceding sCr (not a future reference).
# Creatinine stage 3: >=3x reference OR >=4.0 mg/dL with acute +0.3 evidence.
# We do not claim to stage RRT- or urine-output-based AKI here.
scr_stage <- function(current, reference, abs_valid, rel_valid) {
  ratio <- current / reference
  if (rel_valid && ratio >= 3 - SCR_TOL) return(3L)
  if (abs_valid && current >= 4 - SCR_TOL &&
      current - reference >= 0.3 - SCR_TOL) return(3L)
  if (rel_valid && ratio >= 2 - SCR_TOL) return(2L)
  if ((rel_valid && ratio >= 1.5 - SCR_TOL) || abs_valid) return(1L)
  0L
}

# For each current sCr, find strictly earlier qualifying witnessed reference.
# mode = nearest/lowest compares ALL earlier actual measurements; mode = fixed
# uses only the original frozen reference and verifies time lag and value.
get_hit <- function(e, j, mode, ref_scr, ref_hour) {
  curr_t <- e$hour[j]
  curr_v <- e$scr[j]
  if (mode == "fixed") {
    if (!is.finite(ref_scr) || !is.finite(ref_hour) || ref_scr <= 0)
      return(NULL)
    lag <- curr_t - ref_hour
    if (lag <= HOUR_TOL || lag > 168 + HOUR_TOL) return(NULL)
    ab <- lag <= 48 + HOUR_TOL && curr_v - ref_scr >= 0.3 - SCR_TOL
    re <- curr_v >= 1.5 * ref_scr - SCR_TOL
    if (!ab && !re) return(NULL)
    return(list(onset_hour = curr_t, onset_scr = curr_v,
                onset_event_id = e$event_id[j], witness_hour = ref_hour,
                witness_scr = ref_scr, witness_event_id = "original_fixed_reference",
                abs_flag = as.integer(ab), rel_flag = as.integer(re),
                stage = scr_stage(curr_v, ref_scr, ab, re)))
  }
  prior <- which(e$hour < curr_t - HOUR_TOL &
                 e$hour >= curr_t - 168 - HOUR_TOL)
  if (!length(prior)) return(NULL)
  lag <- curr_t - e$hour[prior]
  ab <- lag <= 48 + HOUR_TOL & curr_v - e$scr[prior] >= 0.3 - SCR_TOL
  re <- curr_v >= 1.5 * e$scr[prior] - SCR_TOL
  eligible <- which(ab | re)
  if (!length(eligible)) return(NULL)
  pi <- prior[eligible]
  # Deterministic tie breaks: later time then lower sCr; or lowest sCr then
  # later time. This choice changes the RECOVERY anchor, not the onset test.
  ord <- if (mode == "nearest") {
    order(-e$hour[pi], e$scr[pi], e$event_id[pi])
  } else if (mode == "lowest") {
    order(e$scr[pi], -e$hour[pi], e$event_id[pi])
  } else stop("Unknown detection mode: ", mode)
  pick <- eligible[ord[1]]
  k <- prior[pick]
  return(list(onset_hour = curr_t, onset_scr = curr_v,
              onset_event_id = e$event_id[j], witness_hour = e$hour[k],
              witness_scr = e$scr[k], witness_event_id = e$event_id[k],
              abs_flag = as.integer(ab[pick]), rel_flag = as.integer(re[pick]),
              stage = scr_stage(curr_v, e$scr[k], ab[pick], re[pick])))
}

# Assess observed onset in [0,48], then a later sCr in [36,60].
# Output code 0=no AKI; 1=resolved; 2=ongoing; NA=indeterminate.
# When ORIGINAL group is AKI-positive but temporal evidence is not reproduced,
# do NOT move the person into the no-AKI reference group; use indeterminate.
classify_one <- function(e, mode, orig_code, orig_ref, orig_ref_hour) {
  if (is.null(e)) e <- EMPTY_EVENTS
  e <- e[is.finite(e$hour) & is.finite(e$scr) & e$scr > 0 &
           e$hour >= -168 & e$hour <= 60,
         , drop = FALSE]
  if (nrow(e) > 1) e <- e[order(e$hour, e$event_id), , drop = FALSE]
  time <- e$hour
  early <- which(time >= 0 & time <= 48)
  late <- which(time >= 36 & time <= 60)
  pre <- which(time >= -168 & time < 0)
  n_early <- length(unique(time[early]))
  n_late <- length(unique(time[late]))
  n_pre <- length(unique(time[pre]))
  # Actual temporal comparator availability for each strategy.
  can_compare <- FALSE
  if (length(early)) {
    if (mode == "fixed") {
      can_compare <- is.finite(orig_ref) && orig_ref > 0 &&
        is.finite(orig_ref_hour) &&
        any(time[early] - orig_ref_hour > HOUR_TOL &
            time[early] - orig_ref_hour <= 168 + HOUR_TOL)
    } else {
      can_compare <- any(vapply(early, function(j) {
        any(time < time[j] - HOUR_TOL &
            time >= time[j] - 168 - HOUR_TOL)
      }, logical(1)))
    }
  }
  # Early hit search, ordered chronologically; do not look at any future value.
  hits <- lapply(early, function(j) get_hit(e, j, mode, orig_ref, orig_ref_hour))
  yes <- which(!vapply(hits, is.null, logical(1)))
  positive_n <- length(yes)
  hit <- if (positive_n) hits[[yes[1]]] else NULL
  peak <- if (positive_n) max(vapply(hits[yes], function(z) z$stage, integer(1))) else 0L
  # Late NEW AKI signal is descriptive only; it is not an early-onset event.
  after48 <- which(time > 48 & time <= 60)
  late_hit <- any(vapply(after48, function(j) {
    !is.null(get_hit(e, j, mode, orig_ref, orig_ref_hour))
  }, logical(1)))
  onset_t <- if (is.null(hit)) NA_real_ else hit$onset_hour
  onset_scr <- if (is.null(hit)) NA_real_ else hit$onset_scr
  onset_event <- if (is.null(hit)) NA_character_ else hit$onset_event_id
  anchor_scr <- if (is.null(hit)) NA_real_ else hit$witness_scr
  anchor_t <- if (is.null(hit)) NA_real_ else hit$witness_hour
  anchor_event <- if (is.null(hit)) NA_character_ else hit$witness_event_id
  abs_flag <- if (is.null(hit)) NA_integer_ else hit$abs_flag
  rel_flag <- if (is.null(hit)) NA_integer_ else hit$rel_flag
  assessment_t <- NA_real_
  assessment_scr <- NA_real_
  assessment_event <- NA_character_
  assessment_gap <- NA_real_
  code_monitored <- NA_integer_
  code_earlyonly <- NA_integer_
  reason <- NA_character_
  if (positive_n) {
    future <- late[time[late] > onset_t + HOUR_TOL]
    if (!length(future)) {
      reason <- "early_AKI_no_strictly_later_sCr_in_36_60h"
    } else {
      priority <- order(abs(time[future] - 48), time[future], e$event_id[future])
      k <- future[priority[1]]
      assessment_t <- time[k]
      assessment_scr <- e$scr[k]
      assessment_event <- e$event_id[k]
      assessment_gap <- assessment_t - onset_t
      resolved <- assessment_scr < anchor_scr + 0.3 - SCR_TOL &&
                  assessment_scr < 1.5 * anchor_scr - SCR_TOL
      code_monitored <- if (resolved) 1L else 2L
      code_earlyonly <- code_monitored
    }
  } else if (!is.na(orig_code) && orig_code %in% 1:2) {
    reason <- "original_AKI_not_supported_by_strict_prior_sCr_pair"
  } else if (!can_compare) {
    reason <- "no_valid_earlier_sCr_comparator_for_early_window"
  } else if (n_late == 0) {
    # Monitored no-AKI requires an actual 36-60h measurement. A relaxed
    # sensitivity retains 0-48h comparison evidence without that requirement.
    code_earlyonly <- 0L
    reason <- "no_late_36_60h_sCr_for_monitored_no_AKI"
  } else {
    code_monitored <- 0L
    code_earlyonly <- 0L
  }
  # Repeated-lab rows at one timestamp do not count as separate time points.
  if (positive_n && !is.finite(assessment_gap) && is.na(reason))
    stop("Internal error: positive episode lacks outcome status reason")
  data.frame(
    strict_code = code_monitored, strict_code_earlyonly = code_earlyonly,
    indeterminate_reason = reason,
    early_scr_timepoints_n = n_early, late_scr_timepoints_n = n_late,
    early_scr_event_n = length(early), late_scr_event_n = length(late),
    preicu_scr_timepoints_n = n_pre, comparator_available_flag = as.integer(can_compare),
    early_temporal_positive_event_n = positive_n,
    late_new_temporal_positive_flag = as.integer(late_hit),
    strict_onset_hour = onset_t, strict_onset_scr = onset_scr,
    strict_onset_event_id = onset_event,
    strict_witness_hour = anchor_t, strict_witness_scr = anchor_scr,
    strict_witness_event_id = anchor_event,
    strict_first_absolute_flag = abs_flag, strict_first_relative_flag = rel_flag,
    strict_peak_stage_0_48h = peak,
    strict_assessment_hour = assessment_t, strict_assessment_scr = assessment_scr,
    strict_assessment_event_id = assessment_event,
    strict_onset_assessment_gap_h = assessment_gap,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# 3B. SMALL DETERMINISTIC TESTS: demonstrate that no future baseline is used
# ---------------------------------------------------------------------------
if (RUN_INTERNAL_UNIT_TESTS) {
  make_test <- function(hours, values) {
    data.frame(event_id = as.character(seq_along(hours)),
               hour = hours, scr = values, stringsAsFactors = FALSE)
  }
  t1 <- classify_one(make_test(c(-1, 6, 48), c(1, 1.4, 1)),
                     "nearest", 1L, 1, -1)
  stopifnot(identical(t1$strict_code, 1L),
            abs(t1$strict_onset_hour - 6) < HOUR_TOL,
            abs(t1$strict_assessment_hour - 48) < HOUR_TOL)
  # A LOWER FUTURE value cannot validate an earlier HIGH sCr as AKI.
  t2 <- classify_one(make_test(c(0, 10, 47), c(2, 1, 1)),
                     "nearest", 1L, 1, 10)
  stopifnot(is.na(t2$strict_code), t2$early_temporal_positive_event_n == 0L)
  # At hour 48, +0.4 mg/dL over hour 0 is a valid 48-hour AKI event;
  # the onset sample itself cannot double as a post-onset recovery sample.
  t3 <- classify_one(make_test(c(0, 48), c(1, 1.4)),
                     "nearest", 1L, 1, 0)
  stopifnot(is.na(t3$strict_code),
            identical(t3$early_temporal_positive_event_n, 1L),
            identical(t3$indeterminate_reason,
                      "early_AKI_no_strictly_later_sCr_in_36_60h"))
  # Boundary regression: hour -1 to hour 48 is 49h, NOT <=48h;
  # an isolated +0.4 mg/dL rise must not be accepted as acute KDIGO.
  t3b <- classify_one(make_test(c(-1, 48), c(1, 1.4)),
                      "nearest", 1L, 1, -1)
  stopifnot(is.na(t3b$strict_code),
            identical(t3b$early_temporal_positive_event_n, 0L),
            identical(t3b$indeterminate_reason,
                      "original_AKI_not_supported_by_strict_prior_sCr_pair"))
  # +0.3 relative to a >48-hour-old value is NOT sufficient for absolute KDIGO.
  t4 <- classify_one(make_test(c(-120, 1, 49), c(1, 1.35, 1.1)),
                     "nearest", 0L, 1, -120)
  stopifnot(t4$early_temporal_positive_event_n == 0L,
            identical(t4$strict_code, 0L))
  # Relative rise >=1.5 within 7 days is eligible even if >48h apart.
  t5 <- classify_one(make_test(c(-100, 1, 49), c(1, 1.6, 1)),
                     "nearest", 1L, 1, -100)
  stopifnot(identical(t5$strict_code, 1L),
            identical(t5$strict_first_relative_flag, 1L))
  # Pair choice is explicitly a sensitivity parameter, not an invisible fix.
  e6 <- make_test(c(-4, 0, 10, 48), c(0.8, 1.0, 1.5, 1.2))
  t6a <- classify_one(e6, "nearest", 1L, 0.8, -4)
  t6b <- classify_one(e6, "lowest", 1L, 0.8, -4)
  stopifnot(identical(t6a$strict_code, 1L),
            identical(t6b$strict_code, 2L))
  msg("Built-in temporal logic unit tests passed.")
}

# ---------------------------------------------------------------------------

# 4. OFFICIAL S7: NEAREST QUALIFYING PRIOR PAIR -------------------------------
# Only the nearest-mode output is used in Panels A/B and passed to the later
# R08 same-sample analysis. The other modes in the retained algorithm functions
# are not used to construct manuscript tables.
compute_nearest <- function(x, by_stay, database) {
  msg("Strictly paired sCr: ", database, ", N=", nrow(x))
  rows <- lapply(seq_len(nrow(x)), function(i) {
    observed <- by_stay[[x$icu_id[i]]]
    classify_one(
      observed, mode = "nearest",
      orig_code = x$primary_exposure_code[i],
      orig_ref = x$reference_scr[i],
      orig_ref_hour = x$reference_scr_hours_from_icu[i]
    )
  })
  z <- do.call(rbind, rows)
  rownames(z) <- NULL
  names(z) <- paste0("nearest__", names(z))
  if (nrow(z) != nrow(x)) stop(database, ": event reclassification row mismatch")
  data.frame(x, z, stringsAsFactors = FALSE, check.names = FALSE)
}

RESULTS <- list()
SOURCE_QC <- list()
for (db in names(COHORT)) {
  x <- compute_nearest(COHORT[[db]], LABS[[db]]$by_stay, db)
  code <- x$nearest__strict_code
  onset <- x$nearest__strict_onset_hour
  witness <- x$nearest__strict_witness_hour
  assessment_gap <- x$nearest__strict_onset_assessment_gap_h
  if (any(!is.na(code) & !code %in% 0:2)) stop(db, ": invalid strict exposure code")
  if (any(is.finite(witness) & is.finite(onset) &
          witness >= onset - HOUR_TOL)) {
    stop(db, ": temporal invariant failed: onset witness not strictly earlier")
  }
  if (any(is.finite(assessment_gap) & assessment_gap <= HOUR_TOL)) {
    stop(db, ": temporal invariant failed: assessment not later than onset")
  }
  if (any(!is.na(code) & code %in% 1:2 & !is.finite(assessment_gap))) {
    stop(db, ": classified AKI with no valid later assessment")
  }
  if (any(!is.na(code) & code == 0 &
          (x$nearest__comparator_available_flag != 1 |
           x$nearest__late_scr_timepoints_n < 1), na.rm = TRUE)) {
    stop(db, ": no-AKI case lacks required measured comparator or late sCr")
  }
  # Exact source laboratory event counts must match the frozen Stage 2 fields;
  # if not, the public SQL output is not the source used for the original run.
  mismatch_early <- !is.na(x$scr_measurement_n_0_48h) &
    x$scr_measurement_n_0_48h != x$nearest__early_scr_event_n
  mismatch_late <- !is.na(x$scr_measurement_n_36_60h) &
    x$scr_measurement_n_36_60h != x$nearest__late_scr_event_n
  if (any(mismatch_early) || any(mismatch_late)) {
    stop(db, ": event-count mismatch versus primary extraction: 0-48h=",
         sum(mismatch_early), "; 36-60h=", sum(mismatch_late),
         ". Compare SQL serial creatinine and Stage 2 exports.")
  }

  # The fixed-reference match is an informational audit, not a requirement
  # for the strictly observed-pair nearest analysis.
  ref_found <- vapply(seq_len(nrow(x)), function(i) {
    value <- x$reference_scr[i]
    reference_hour <- x$reference_scr_hours_from_icu[i]
    if (!is.finite(value) || !is.finite(reference_hour)) return(NA_integer_)
    d <- LABS[[db]]$by_stay[[x$icu_id[i]]]
    if (is.null(d)) return(0L)
    as.integer(any(abs(d$hour - reference_hour) <= 0.01 &
                     abs(d$scr - value) <= 1e-5))
  }, integer(1))
  x$original_reference_observed_in_long_flag <- ref_found
  x$original_onset_assessment_hour <- ifelse(
    x$primary_exposure_code %in% 1:2,
    48 + x$assessment_offset_from_48h_36_60h, NA_real_
  )
  x$original_onset_assessment_gap_h <-
    x$original_onset_assessment_hour - x$aki_onset_hours_from_icu

  e <- LABS[[db]]$events
  SOURCE_QC[[db]] <- data.frame(
    Database = db,
    landmark_stays = nrow(x),
    primary_stays = sum(flag(x$primary_analysis_flag)),
    exported_long_rows = LABS[[db]]$exported_rows,
    distinct_stays_with_valid_scr = length(unique(e$icu_id)),
    real_scr_events = nrow(e),
    duplicate_stay_event_pairs_removed = LABS[[db]]$duplicate_n,
    scr_event_rows_0_48h = sum(e$hour >= 0 & e$hour <= 48),
    scr_event_rows_36_60h = sum(e$hour >= 36 & e$hour <= 60),
    frozen_reference_not_matched_n = sum(ref_found == 0, na.rm = TRUE),
    event_count_mismatch_0_48h = sum(mismatch_early),
    event_count_mismatch_36_60h = sum(mismatch_late),
    stringsAsFactors = FALSE
  )
  RESULTS[[db]] <- x
  msg(db, ": strict pair calculation finished")
}

# 5. FINAL TABLE S7 PANELS A AND B -------------------------------------------
ORIGINAL_LEVELS <- c("No early AKI", "Resolved early AKI", "Ongoing early AKI")
STRICT_LEVELS <- c(ORIGINAL_LEVELS, "Indeterminate")
label_code <- function(code) {
  output <- rep("Indeterminate", length(code))
  output[!is.na(code) & code == 0] <- STRICT_LEVELS[1]
  output[!is.na(code) & code == 1] <- STRICT_LEVELS[2]
  output[!is.na(code) & code == 2] <- STRICT_LEVELS[3]
  output
}
fmt_nd <- function(n, death) paste0(n, "/", death)

panel_a_num <- list()
panel_a_format <- list()
panel_b <- list()
panel_b_long <- list()
reason_tables <- list()

for (db in names(RESULTS)) {
  full <- RESULTS[[db]]
  x <- full[
    flag(full$primary_analysis_flag) &
      !is.na(full$primary_exposure_code) & full$primary_exposure_code %in% 0:2,
    , drop = FALSE
  ]
  x$strict_label <- label_code(x$nearest__strict_code)
  classified <- x[!is.na(x$nearest__strict_code), , drop = FALSE]
  n <- nrow(x)
  nk <- nrow(classified)
  di <- n - nk
  category_n <- as.integer(table(factor(
    classified$nearest__strict_code, levels = 0:2
  )))
  category_deaths <- vapply(0:2, function(k) {
    as.integer(sum(classified$nearest__strict_code == k &
                     classified$hospital_death_after_60h == 1))
  }, integer(1))
  nd <- sum(classified$hospital_death_after_60h == 1)
  panel_a_num[[db]] <- data.frame(
    Database = db,
    original_primary_N = n,
    classifiable_N = nk,
    classifiable_deaths = nd,
    indeterminate_N = di,
    indeterminate_pct = round(di / n * 100, 1),
    no_AKI_N = category_n[1],
    no_AKI_deaths = category_deaths[1],
    resolved_N = category_n[2],
    resolved_deaths = category_deaths[2],
    ongoing_N = category_n[3],
    ongoing_deaths = category_deaths[3],
    stringsAsFactors = FALSE
  )
  panel_a_format[[db]] <- data.frame(
    Cohort = db,
    `Classifiable N/deaths` = fmt_nd(nk, nd),
    `Indeterminate n (%)` = sprintf("%d (%.1f%%)", di, di / n * 100),
    `No early AKI N/deaths` = fmt_nd(category_n[1], category_deaths[1]),
    `Resolved early AKI N/deaths` = fmt_nd(category_n[2], category_deaths[2]),
    `Ongoing early AKI N/deaths` = fmt_nd(category_n[3], category_deaths[3]),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  original <- factor(label_code(x$primary_exposure_code),
                     levels = ORIGINAL_LEVELS)
  new <- factor(x$strict_label, levels = STRICT_LEVELS)
  matrix_counts <- table(Original = original, Reclassified = new)
  if (sum(matrix_counts) != n || any(rowSums(matrix_counts) <= 0)) {
    stop(db, ": invalid S7 Panel B contingency table")
  }
  table_wide <- data.frame(
    Cohort = rep(db, length(ORIGINAL_LEVELS)),
    `Original trajectory` = ORIGINAL_LEVELS,
    `No early AKI` = as.integer(matrix_counts[, STRICT_LEVELS[1]]),
    `Resolved early AKI` = as.integer(matrix_counts[, STRICT_LEVELS[2]]),
    `Ongoing early AKI` = as.integer(matrix_counts[, STRICT_LEVELS[3]]),
    Indeterminate = as.integer(matrix_counts[, STRICT_LEVELS[4]]),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  panel_b[[db]] <- table_wide
  long <- as.data.frame(matrix_counts, stringsAsFactors = FALSE)
  long$Database <- db
  panel_b_long[[db]] <- long[, c("Database", "Original", "Reclassified", "Freq")]

  reasons <- x$nearest__indeterminate_reason[
    is.na(x$nearest__strict_code)
  ]
  if (anyNA(reasons) || any(!nzchar(reasons))) {
    stop(db, ": indeterminate cases with no coded reason")
  }
  reason_n <- as.data.frame(table(Reason = reasons), stringsAsFactors = FALSE)
  reason_n$Database <- db
  reason_tables[[db]] <- reason_n[, c("Database", "Reason", "Freq")]
}

PANEL_A_NUMERIC <- do.call(rbind, panel_a_num)
PANEL_A <- do.call(rbind, panel_a_format)
PANEL_B <- do.call(rbind, panel_b)
PANEL_B_LONG <- do.call(rbind, panel_b_long)
REASONS <- do.call(rbind, reason_tables)
SOURCE_QC_FRAME <- do.call(rbind, SOURCE_QC)
rownames(PANEL_A_NUMERIC) <- NULL
rownames(PANEL_A) <- NULL
rownames(PANEL_B) <- NULL
rownames(PANEL_B_LONG) <- NULL
rownames(REASONS) <- NULL
rownames(SOURCE_QC_FRAME) <- NULL

# 6. REPRODUCTION CHECKS AGAINST FINAL SUPPLEMENTARY TABLE S7 -----------------
# Expected published values are used for QC ONLY. They never enter the
# reclassification function, do not reassign patients and cannot fix a value.
EXPECTED_A <- data.frame(
  Database = c("MIMIC-IV", "eICU-CRD"),
  classifiable_N = c(6558L, 5627L),
  classifiable_deaths = c(732L, 575L),
  indeterminate_N = c(1069L, 2111L),
  no_AKI_N = c(4920L, 4722L),
  no_AKI_deaths = c(400L, 389L),
  resolved_N = c(678L, 381L),
  resolved_deaths = c(89L, 57L),
  ongoing_N = c(960L, 524L),
  ongoing_deaths = c(243L, 129L),
  stringsAsFactors = FALSE
)
EXPECTED_B <- data.frame(
  Cohort = rep(c("MIMIC-IV", "eICU-CRD"), each = 3L),
  `Original trajectory` = rep(ORIGINAL_LEVELS, times = 2L),
  `No early AKI` = c(4920, 0, 0, 4722, 0, 0),
  `Resolved early AKI` = c(181, 460, 37, 112, 254, 15),
  `Ongoing early AKI` = c(138, 36, 786, 62, 12, 450),
  Indeterminate = c(557, 402, 110, 1748, 199, 164),
  check.names = FALSE, stringsAsFactors = FALSE
)
qc_records <- list()
for (i in seq_len(nrow(EXPECTED_A))) {
  db <- EXPECTED_A$Database[i]
  observed <- PANEL_A_NUMERIC[PANEL_A_NUMERIC$Database == db, , drop = FALSE]
  for (col in setdiff(names(EXPECTED_A), "Database")) {
    qc_records[[length(qc_records) + 1L]] <- data.frame(
      Panel = "S7A", Cohort = db, Original = "", Variable = col,
      Expected = EXPECTED_A[[col]][i], Actual = observed[[col]][1],
      Pass = (EXPECTED_A[[col]][i] == observed[[col]][1]),
      stringsAsFactors = FALSE
    )
  }
}
for (i in seq_len(nrow(EXPECTED_B))) {
  db <- EXPECTED_B$Cohort[i]
  old <- EXPECTED_B[["Original trajectory"]][i]
  observed <- PANEL_B[PANEL_B$Cohort == db &
                        PANEL_B[["Original trajectory"]] == old, , drop = FALSE]
  if (nrow(observed) != 1L) stop("S7 Panel B QC could not identify unique row")
  for (col in STRICT_LEVELS) {
    qc_records[[length(qc_records) + 1L]] <- data.frame(
      Panel = "S7B", Cohort = db, Original = old, Variable = col,
      Expected = EXPECTED_B[[col]][i], Actual = observed[[col]][1],
      Pass = EXPECTED_B[[col]][i] == observed[[col]][1],
      stringsAsFactors = FALSE
    )
  }
}
PUBLISHED_QC <- do.call(rbind, qc_records)
rownames(PUBLISHED_QC) <- NULL
fail_n <- sum(!PUBLISHED_QC$Pass)
if (fail_n > 0) {
  note <- paste0(fail_n,
                 " aggregate S7 published-value QC mismatches. See QC CSV; ",
                 "do not overwrite or force the results.")
  if (STOP_ON_EXPECTED_TABLE_MISMATCH) stop(note)
  warning(note)
} else {
  msg("All expected S7 A/B published summary values matched.")
}

# 7. WRITE AGGREGATE RESULTS; PRIVATE RDS ONLY UNDER LOCAL output/ -----------
write_summary(PANEL_A, "TableS7_PanelA_classification.csv")
write_summary(PANEL_A_NUMERIC, "TableS7_PanelA_numeric.csv")
write_summary(PANEL_B, "TableS7_PanelB_reclassification.csv")
write_summary(PANEL_B_LONG, "TableS7_PanelB_long.csv")
write_summary(REASONS, "TableS7_indeterminate_reasons.csv")
write_summary(SOURCE_QC_FRAME, "TableS7_cohort_source_QC.csv")
write_summary(PUBLISHED_QC, "TableS7_published_values_QC.csv")

if (isTRUE(SAVE_LOCAL_JOINED_RDS)) {
  saveRDS(RESULTS[["MIMIC-IV"]],
          file.path(OUTPUT_DIR, "mimic_temporal_pair_joined.rds"),
          compress = "xz")
  saveRDS(RESULTS[["eICU-CRD"]],
          file.path(OUTPUT_DIR, "eicu_temporal_pair_joined.rds"),
          compress = "xz")
  msg("Saved private local stay-level RDS for R08 Panel C (never publish).")
}

writeLines(c(
  "S7 Panel A/B: strictly ordered OBSERVED sCr pair reclassification.",
  "Nearest qualifying prior observed sCr anchor, not fixed reference.",
  "48h absolute rise >=0.3 mg/dL OR 168h relative rise >=1.5x.",
  "Early event occurs in ICU hours 0-48; assessment strictly later in 36-60.",
  "No event with adequate observed comparator and later monitoring -> no AKI.",
  "Otherwise lack of evidence can be INDETERMINATE; never impute no AKI.",
  "Main fixed-reference exposure is left intact; original primary cohort frozen.",
  "Results S7 A/B are aggregate. Do not publish patient-level long labs/RDS.",
  "R08 will use private joined RDS to fit same-patient original vs temporal models.",
  "Source validation: event counts, temporal invariants and stay-set matching.",
  paste0("Published aggregate QC mismatch count: ", fail_n),
  "", log_lines, "", capture.output(utils::sessionInfo())
), file.path(OUTPUT_DIR, "07_run_notes.txt"), useBytes = TRUE)
msg("Finished R07. Aggregate outputs: ", OUTPUT_DIR)
