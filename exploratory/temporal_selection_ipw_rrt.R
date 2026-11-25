# ==============================================================================
# EXPLORATORY / NOT REPORTED IN THE FINAL MANUSCRIPT
# COPD early creatinine-defined AKI trajectory: selection/IPW and joint RRT
# MIMIC-IV v3.1 and eICU-CRD v2.0 | 60-hour landmark
#
# Adapted from the author's original:
#   09_Reviewer1_Temporal_Selection_Bias_and_RRT_Combined_v2.R
#
# The published Supplementary Table S7 Panels A-C are reproduced by R07/R08.
# This optional script examines selection into classifiable temporally paired
# trajectories, stabilized inverse-probability-of-classification weighting,
# and exploratory joint restrictions by RRT record evidence. It uses ONLY the
# nearest-prior observed-sCr temporal classification from the public R07.
# Other anchor/scenario experiments in the original private R09 are not run.
#
# IMPORTANT LIMITATIONS:
# * Indeterminate does NOT mean no AKI; unknown classifications are not imputed.
# * IPW does NOT resolve structural positivity or unknown exposure categories.
# * IPW variance estimates do not propagate estimation of selection weights.
# * RRT timestamps indicate charting/documentation, not therapy initiation.
# * No RRT record does not prove no dialysis. No causal effect is estimated.
# * All outputs are aggregate; keep input RDS and SQL event data OUT of GitHub.
#
# Run from repository root, AFTER R01 and R07:
#   Rscript exploratory/temporal_selection_ipw_rrt.R
# Optional: COPD_AKI_PROJECT_ROOT=/path/to/repository
# Needs: sandwich, readr (readxl only if XLSX exports are used).
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)
ROOT_OVERRIDE <- Sys.getenv("COPD_AKI_PROJECT_ROOT", unset = "")
PROJECT_ROOT <- normalizePath(
  if (nzchar(ROOT_OVERRIDE)) ROOT_OVERRIDE else ".",
  winslash = "/", mustWork = TRUE
)
ANALYSIS_DIR <- file.path(PROJECT_ROOT, "output", "R_analysis_output")
R07_DIR <- file.path(ANALYSIS_DIR, "07_temporal_pair_reclassification")
SQL_EXPORT_DIR <- file.path(PROJECT_ROOT, "data", "sql_exports")
OUTPUT_DIR <- file.path(ANALYSIS_DIR, "exploratory_temporal_selection_ipw_rrt")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

IPW_PROBABILITY_CLIP <- c(0.02, 0.98)
IPW_WEIGHT_TRIM_QUANTILES <- c(0.01, 0.99)
TIME_TOL_HOURS <- 1e-8
RUN_INTERNAL_TESTS <- TRUE
COVARS <- c(
  "age", "female_flag", "ckd_flag", "heart_failure_flag",
  "diabetes_flag", "chronic_liver_disease_flag", "malignancy_flag",
  "standard_nonrenal_sofa_24h"
)
EXPOSURE_LABELS <- c("No early AKI", "Resolved early AKI", "Ongoing early AKI")
EXPECTED <- data.frame(
  Database = c("MIMIC-IV", "eICU-CRD"),
  Landmark_N = c(7794L, 8495L), Primary_N = c(7627L, 7738L),
  Primary_deaths = c(832L, 709L), Strict_classified_N = c(6558L, 5627L),
  Strong_RRT_0_60_N = c(116L, 128L), stringsAsFactors = FALSE
)
INPUT_RDS <- c(
  "MIMIC-IV" = file.path(R07_DIR, "mimic_temporal_pair_joined.rds"),
  "eICU-CRD" = file.path(R07_DIR, "eicu_temporal_pair_joined.rds")
)
if (any(!file.exists(INPUT_RDS))) {
  stop("R07 patient-level local RDS files are missing. Run R/07 first: ",
       paste(INPUT_RDS[!file.exists(INPUT_RDS)], collapse = "; "))
}
for (p in c("sandwich", "readr")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    stop("Install the required R package before running: ", p)
  }
}

resolve_export <- function(folder, stem) {
  base <- file.path(SQL_EXPORT_DIR, folder)
  paths <- file.path(base, paste0(stem, c(".csv", ".xlsx", ".xls")))
  found <- paths[file.exists(paths)]
  if (length(found) != 1L) {
    stop("Expected exactly one SQL export for ", stem, " under ", base,
         "; found ", length(found), ".")
  }
  found
}
RRT_INPUT <- c(
  "MIMIC-IV" = resolve_export("mimiciv", "08_rrt_events"),
  "eICU-CRD" = resolve_export("eicu", "09_rrt_events")
)
if (any(tolower(tools::file_ext(RRT_INPUT)) %in% c("xlsx", "xls")) &&
    !requireNamespace("readxl", quietly = TRUE)) {
  stop("Install readxl for Excel event exports, or export the SQL final SELECT as CSV.")
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
  if (length(miss)) stop(label, " is missing column(s): ", paste(miss, collapse = ", "))
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
flag1 <- function(x) !is.na(x) & x == 1
check_unique <- function(v, label) {
  if (anyNA(v) || any(!nzchar(v)) || anyDuplicated(v)) {
    stop(label, ": missing or duplicated stay IDs")
  }
}
check_same_set <- function(a, b, label) {
  missing <- setdiff(unique(a), unique(b))
  extra <- setdiff(unique(b), unique(a))
  if (length(missing) || length(extra)) {
    stop(label, ": stay set differs; missing=", length(missing),
         ", extra=", length(extra))
  }
}
bind_tables <- function(xs) {
  xs <- Filter(function(z) is.data.frame(z) && nrow(z) > 0L, xs)
  if (!length(xs)) return(data.frame())
  z <- do.call(rbind, xs)
  rownames(z) <- NULL
  z
}
q <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, p, names = FALSE))
}
safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}
write_table <- function(x, filename) {
  if (!is.data.frame(x)) stop("Output is not a data.frame: ", filename)
  if (any(names(x) %in% c("icu_id", "stay_id", "patientunitstayid", "subject_id"))) {
    stop("Refusing to export an individual identifier: ", filename)
  }
  utils::write.csv(x, file.path(OUTPUT_DIR, filename), row.names = FALSE,
                   na = "", fileEncoding = "UTF-8")
  log_note("Saved: ", filename)
}
read_sql_export <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    z <- readr::read_csv(
      path, col_types = readr::cols(.default = readr::col_character()),
      na = c("", "NA", "N/A", "NULL", "null", "NaN"),
      show_col_types = FALSE, progress = FALSE, name_repair = "minimal"
    )
  } else if (ext %in% c("xlsx", "xls")) {
    z <- readxl::read_excel(path, col_types = "text", sheet = 1,
                            .name_repair = "minimal")
  } else stop("Unsupported SQL export file extension: ", ext)
  z <- as.data.frame(z, check.names = FALSE)
  names(z) <- trimws(names(z))
  if (anyDuplicated(names(z))) stop("Duplicate column names: ", path)
  z
}

# --- Local R07 joined data: nearest prior observed sCr only -------------------
load_joined <- function(path, db) {
  x <- readRDS(path)
  if (!is.data.frame(x)) stop(db, " R07 RDS is not a data.frame")
  cols <- c("icu_id", "hospital_id", "primary_analysis_flag",
            "primary_exposure_code", "hospital_death_after_60h",
            "reference_scr_source", "nearest__strict_code",
            "nearest__strict_assessment_hour", "nearest__indeterminate_reason",
            "nearest__strict_peak_stage_0_48h", "peak_aki_stage_0_48h",
            "scr_measurement_n_0_48h", "scr_measurement_n_36_60h", COVARS)
  require_fields(x, cols, paste(db, "R07 joined cohort"))
  x$icu_id <- identifier(x$icu_id)
  x$hospital_id <- identifier(x$hospital_id)
  check_unique(x$icu_id, db)
  numeric_fields <- setdiff(cols, c("icu_id", "hospital_id",
                                  "reference_scr_source", "nearest__indeterminate_reason"))
  for (col in numeric_fields) x[[col]] <- number(x[[col]])
  expected <- EXPECTED[EXPECTED$Database == db, , drop = FALSE]
  if (nrow(x) != expected$Landmark_N) stop(db, ": wrong landmark N")
  if (db == "eICU-CRD" && anyNA(x$hospital_id)) stop(db, ": missing hospital cluster")
  p <- x[flag1(x$primary_analysis_flag) & x$primary_exposure_code %in% 0:2,
         , drop = FALSE]
  if (nrow(p) != expected$Primary_N ||
      sum(p$hospital_death_after_60h == 1) != expected$Primary_deaths ||
      sum(!is.na(p$nearest__strict_code)) != expected$Strict_classified_N) {
    stop(db, ": original/strict cohort mismatch with finalized analysis")
  }
  if (anyNA(p$hospital_death_after_60h) ||
      any(!p$hospital_death_after_60h %in% 0:1)) stop(db, ": invalid mortality flags")
  if (any(!stats::complete.cases(p[, COVARS, drop = FALSE]))) {
    stop(db, ": missing primary model covariates")
  }
  x
}
COHORTS <- lapply(names(INPUT_RDS), function(db) load_joined(INPUT_RDS[[db]], db))
names(COHORTS) <- names(INPUT_RDS)

# --- RRT event-level extraction: adapted from original Reviewer 1 R09 ---------
rrt_summary <- function(path, db, full_ids) {
  z <- read_sql_export(path)
  if (db == "MIMIC-IV") {
    require_fields(z, c("index_stay_id", "hours_from_index_icu",
      "has_rrt_evidence_record", "direct_treatment_procedure_flag",
      "derived_active_dialysis_flag"), "MIMIC RRT SQL export")
    id <- identifier(z$index_stay_id)
    hour <- number(z$hours_from_index_icu)
    candidate <- flag1(number(z$has_rrt_evidence_record))
    direct <- flag1(number(z$direct_treatment_procedure_flag))
    derived <- flag1(number(z$derived_active_dialysis_flag))
    flow <- rep(FALSE, nrow(z))
    strong <- direct | derived
  } else {
    require_fields(z, c("patientunitstayid", "event_hours_from_index_icu",
      "has_rrt_candidate_record", "rrt_treatment_documented_flag",
      "rrt_dialysis_flow_flag", "rrt_cumulative_balance_change_flag",
      "rrt_therapy_or_dialysis_flow_evidence_flag"), "eICU RRT SQL export")
    id <- identifier(z$patientunitstayid)
    hour <- number(z$event_hours_from_index_icu)
    candidate <- flag1(number(z$has_rrt_candidate_record))
    direct <- flag1(number(z$rrt_treatment_documented_flag))
    derived <- flag1(number(z$rrt_cumulative_balance_change_flag))
    flow <- flag1(number(z$rrt_dialysis_flow_flag))
    strong <- flag1(number(z$rrt_therapy_or_dialysis_flow_evidence_flag))
  }
  if (anyNA(id)) stop(db, ": missing IDs in RRT long export")
  check_same_set(full_ids, id, db)
  out <- data.frame(icu_id = full_ids, stringsAsFactors = FALSE)
  in_0_60 <- is.finite(hour) & hour >= 0 & hour <= 60
  before_60 <- is.finite(hour) & hour <= 60
  mark_any <- function(mask, nm) {
    out[[nm]] <<- as.integer(out$icu_id %in% unique(id[mask]))
  }
  mark_count <- function(mask, nm) {
    tt <- table(id[mask]); vals <- as.integer(tt[match(out$icu_id, names(tt))])
    vals[is.na(vals)] <- 0L; out[[nm]] <<- vals
  }
  mark_first <- function(mask, nm) {
    if (!any(mask)) {
      out[[nm]] <<- rep(NA_real_, nrow(out))
    } else {
      first <- tapply(hour[mask], id[mask], min)
      out[[nm]] <<- as.numeric(first[match(out$icu_id, dimnames(first)[[1]])])
    }
  }
  mark_any(candidate & in_0_60, "rrt_candidate_any_0_60")
  mark_any(strong & in_0_60, "rrt_strong_any_0_60")
  mark_any(direct & in_0_60, "rrt_direct_any_0_60")
  mark_any(derived & in_0_60, "rrt_derived_or_cumulative_any_0_60")
  mark_any(flow & in_0_60, "rrt_flow_any_0_60")
  mark_count(strong & in_0_60, "rrt_strong_record_count_0_60")
  mark_count(candidate & in_0_60, "rrt_candidate_record_count_0_60")
  mark_first(strong & before_60, "rrt_first_strong_before60_hour")
  mark_first(candidate & before_60, "rrt_first_candidate_before60_hour")
  check_unique(out$icu_id, paste(db, " RRT per-stay summary"))
  out
}

prepare_analysis <- function(db) {
  x <- COHORTS[[db]]
  rr <- rrt_summary(RRT_INPUT[[db]], db, x$icu_id)
  idx <- match(x$icu_id, rr$icu_id)
  d <- cbind(x, rr[idx, setdiff(names(rr), "icu_id"), drop = FALSE])
  if (nrow(d) != nrow(x) || anyDuplicated(d$icu_id)) stop(db, ": RRT join changed rows")
  d <- d[flag1(d$primary_analysis_flag) & d$primary_exposure_code %in% 0:2,
         , drop = FALSE]
  d$strict_classified <- as.integer(!is.na(d$nearest__strict_code))
  d$orig_group <- factor(d$primary_exposure_code, levels = 0:2,
                         labels = c("NoAKI", "Resolved", "Ongoing"))
  d$preicu_reference_flag <- as.integer(grepl("^pre_icu", d$reference_scr_source))
  # 'Before boundary' means <=60h in no-AKI, or <= individual later assessment
  # in strict early AKI. Negative relative ICU hours are permitted.
  boundary <- ifelse(d$nearest__strict_code == 0, 60,
                     d$nearest__strict_assessment_hour)
  d$rrt_strong_before_strict_boundary <- as.integer(
    is.finite(d$rrt_first_strong_before60_hour) &
      is.finite(boundary) &
      d$rrt_first_strong_before60_hour <= boundary + TIME_TOL_HOURS)
  d$rrt_candidate_before_strict_boundary <- as.integer(
    is.finite(d$rrt_first_candidate_before60_hour) &
      is.finite(boundary) &
      d$rrt_first_candidate_before60_hour <= boundary + TIME_TOL_HOURS)
  d$rrt_strong_before_strict_boundary[is.na(d$nearest__strict_code)] <- NA_integer_
  d$rrt_candidate_before_strict_boundary[is.na(d$nearest__strict_code)] <- NA_integer_
  expected <- EXPECTED$Strong_RRT_0_60_N[EXPECTED$Database == db]
  observed <- sum(d$rrt_strong_any_0_60 == 1)
  if (observed != expected) {
    stop(db, ": 0-60h strong RRT evidence differs from original Reviewer 1: ",
         observed, " vs ", expected)
  }
  log_note(db, ": N primary=", nrow(d), "; strict classifiable=",
           sum(d$strict_classified), "; strong RRT(0-60h)=", observed)
  d
}

# --- Selection description and observed indeterminate reasons ----------------
selection_descriptive <- function(d, db) {
  d$Original_noAKI <- as.integer(d$primary_exposure_code == 0)
  d$Original_resolved <- as.integer(d$primary_exposure_code == 1)
  d$Original_ongoing <- as.integer(d$primary_exposure_code == 2)
  fields <- c(COVARS, "scr_measurement_n_0_48h", "scr_measurement_n_36_60h",
              "hospital_death_after_60h", "preicu_reference_flag",
              "Original_noAKI", "Original_resolved", "Original_ongoing")
  out <- list()
  for (nm in fields) for (group in c("All_primary", "Classifiable", "Indeterminate")) {
    keep <- switch(group, All_primary = rep(TRUE, nrow(d)),
                   Classifiable = d$strict_classified == 1,
                   Indeterminate = d$strict_classified == 0)
    v <- number(d[[nm]][keep])
    out[[length(out) + 1L]] <- data.frame(
      Database = db, Variable = nm, Group = group, N = sum(keep),
      N_nonmissing = sum(is.finite(v)), Mean_or_fraction = safe_mean(v),
      SD = stats::sd(v, na.rm = TRUE), Median = q(v, 0.5),
      P25 = q(v, 0.25), P75 = q(v, 0.75))
  }
  bind_tables(out)
}
reason_summary <- function(d, db) {
  x <- d[d$strict_classified == 0, , drop = FALSE]
  reason <- as.character(x$nearest__indeterminate_reason)
  reason[is.na(reason) | !nzchar(reason)] <- "undocumented_indeterminate_reason"
  z <- aggregate(rep(1L, length(reason)), by = list(
    Original_group = EXPOSURE_LABELS[x$primary_exposure_code + 1L],
    Reason = reason), FUN = sum)
  names(z)[3] <- "N"
  z$Database <- db
  z[, c("Database", "Original_group", "Reason", "N"), drop = FALSE]
}

# --- Original stabilized IPW formulas ----------------------------------------
SELECTION_FORMULA <- stats::as.formula(paste0(
  "strict_classified ~ orig_group + ", paste(COVARS, collapse = " + "),
  " + preicu_reference_flag"))
wmean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}
wvar <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  mu <- wmean(x[ok], w[ok])
  sum(w[ok] * (x[ok] - mu)^2) / sum(w[ok])
}
effective_n <- function(w) {
  w <- w[is.finite(w) & w > 0]
  if (!length(w)) return(NA_real_)
  sum(w)^2 / sum(w^2)
}
selection_weighting <- function(d, db) {
  required <- all.vars(SELECTION_FORMULA)
  require_fields(d, required, paste(db, "IPW predictors"))
  if (any(!stats::complete.cases(d[, required, drop = FALSE]))) {
    flag_warning(db, ": missing IPW predictors; skipping weighted models")
    return(NULL)
  }
  notes <- character()
  fit <- tryCatch(withCallingHandlers(
    stats::glm(SELECTION_FORMULA, data = d, family = stats::binomial(),
               na.action = stats::na.fail, model = TRUE),
    warning = function(w) {
      notes <<- c(notes, conditionMessage(w)); invokeRestart("muffleWarning")
    }), error = function(e) e)
  if (inherits(fit, "error") || !isTRUE(fit$converged)) {
    flag_warning(db, ": IPW propensity model failed/convergence problem")
    return(NULL)
  }
  if (length(notes)) flag_warning(db, ": IPW fitting warnings: ", paste(unique(notes), collapse = " | "))
  ps <- as.numeric(stats::predict(fit, newdata = d, type = "response"))
  if (any(!is.finite(ps))) { flag_warning(db, ": nonfinite selection probabilities"); return(NULL) }
  selected <- d$strict_classified == 1
  p0 <- mean(selected)
  ps_clip <- pmax(IPW_PROBABILITY_CLIP[1], pmin(IPW_PROBABILITY_CLIP[2], ps))
  raw <- p0 / ps_clip[selected]
  trim <- as.numeric(stats::quantile(raw, IPW_WEIGHT_TRIM_QUANTILES,
                                     names = FALSE, na.rm = TRUE))
  w <- pmax(trim[1], pmin(trim[2], raw))
  d$ipw_stabilized_trimmed <- NA_real_
  d$ipw_stabilized_trimmed[selected] <- w
  qc <- data.frame(Database = db, N_primary = nrow(d), N_selected = sum(selected),
    N_indeterminate = sum(!selected), Selection_rate = p0,
    Probability_min = min(ps), Probability_median = q(ps, 0.5),
    Probability_max = max(ps), Prob_lt_0_05_N = sum(ps < 0.05),
    Prob_gt_0_95_N = sum(ps > 0.95),
    Prob_clipped_N = sum(ps < IPW_PROBABILITY_CLIP[1] |
                           ps > IPW_PROBABILITY_CLIP[2]),
    Weight_min = min(w), Weight_median = q(w, 0.5), Weight_max = max(w),
    Effective_N = effective_n(w), Trim_lower = trim[1], Trim_upper = trim[2],
    stringsAsFactors = FALSE)
  coef <- data.frame(Database = db, Term = rownames(summary(fit)$coefficients),
                     summary(fit)$coefficients, row.names = NULL, check.names = FALSE)
  d$Original_noAKI <- as.integer(d$primary_exposure_code == 0)
  d$Original_resolved <- as.integer(d$primary_exposure_code == 1)
  d$Original_ongoing <- as.integer(d$primary_exposure_code == 2)
  fields <- c(COVARS, "preicu_reference_flag", "Original_noAKI",
              "Original_resolved", "Original_ongoing")
  balance <- lapply(fields, function(f) {
    x <- number(d[[f]])
    pop <- mean(x)
    unm <- mean(x[selected])
    wtm <- wmean(x[selected], w)
    sd_un <- sqrt((stats::var(x) + stats::var(x[selected])) / 2)
    sd_wt <- sqrt((stats::var(x) + wvar(x[selected], w)) / 2)
    data.frame(Database = db, Variable = f, Primary_mean = pop,
      Classifiable_mean = unm, Classifiable_IPW_mean = wtm,
      SMD_unweighted = if (is.finite(sd_un) && sd_un > 0) (unm - pop) / sd_un else NA_real_,
      SMD_weighted = if (is.finite(sd_wt) && sd_wt > 0) (wtm - pop) / sd_wt else NA_real_)
  })
  list(data = d, qc = qc, coefficients = coef, balance = bind_tables(balance))
}

# --- Original and strict mortality regression models -------------------------
fit_adjusted <- function(x, db, scenario, exposure, weights = NULL, aki_only = FALSE) {
  d <- x
  if (aki_only) {
    d <- d[!is.na(d[[exposure]]) & d[[exposure]] %in% 1:2, , drop = FALSE]
    d$recovery_status <- factor(d[[exposure]], levels = 1:2,
                                labels = c("Resolved", "Ongoing"))
    d$peak_stage_high <- as.integer(d$nearest__strict_peak_stage_0_48h >= 2)
    form <- stats::as.formula(paste0(
      "hospital_death_after_60h ~ recovery_status + peak_stage_high + ",
      paste(COVARS, collapse = " + ")))
    prefix <- "recovery_status"
  } else {
    d$analysis_exposure <- factor(d[[exposure]], levels = 0:2,
                                  labels = EXPOSURE_LABELS)
    form <- stats::as.formula(paste0(
      "hospital_death_after_60h ~ analysis_exposure + ",
      paste(COVARS, collapse = " + ")))
    prefix <- "analysis_exposure"
  }
  needed <- all.vars(form)
  if (db == "eICU-CRD") needed <- union(needed, "hospital_id")
  if (!is.null(weights)) {
    d$.glm_weight_exploratory <- number(d[[weights]])
    needed <- union(needed, ".glm_weight_exploratory")
  }
  valid <- stats::complete.cases(d[, needed, drop = FALSE])
  if (any(!valid)) flag_warning(db, "/", scenario, ": discarded ", sum(!valid), " incomplete records")
  d <- d[valid, , drop = FALSE]
  if (nrow(d) < 100L || length(unique(d$hospital_death_after_60h)) != 2L ||
      length(unique(d[[if (aki_only) "recovery_status" else "analysis_exposure"]])) < 2L) {
    flag_warning(db, "/", scenario, ": insufficient sample or group variation")
    return(data.frame())
  }
  captured <- character()
  fit <- tryCatch(withCallingHandlers({
    if (is.null(weights)) {
      stats::glm(form, data = d, family = stats::binomial(),
                 na.action = stats::na.fail, model = TRUE)
    } else {
      stats::glm(form, data = d, family = stats::quasibinomial(),
                 weights = .glm_weight_exploratory,
                 na.action = stats::na.fail, model = TRUE)
    }
  }, warning = function(w) {
    captured <<- c(captured, conditionMessage(w)); invokeRestart("muffleWarning")
  }), error = function(e) e)
  if (inherits(fit, "error") || !isTRUE(fit$converged)) {
    flag_warning(db, "/", scenario, ": glm failed or did not converge")
    return(data.frame())
  }
  if (length(captured)) flag_warning(db, "/", scenario, ": ", paste(unique(captured), collapse = " | "))
  V <- tryCatch({
    if (db == "eICU-CRD") {
      sandwich::vcovCL(fit, cluster = d$hospital_id, type = "HC1")
    } else if (!is.null(weights)) {
      sandwich::vcovHC(fit, type = "HC1")
    } else stats::vcov(fit)
  }, error = function(e) e)
  if (inherits(V, "error")) { flag_warning(db, "/", scenario, ": covariance failed"); return(data.frame()) }
  b <- stats::coef(fit)
  terms <- grep(paste0("^", prefix), names(b), value = TRUE)
  rows <- lapply(terms, function(term) {
    if (!term %in% rownames(V) || !is.finite(b[[term]]) ||
        !is.finite(V[term, term]) || V[term, term] <= 0) return(NULL)
    se <- sqrt(V[term, term]); beta <- b[[term]]
    data.frame(Database = db, Scenario = scenario, Exposure = exposure,
      Population = if (aki_only) "Early AKI only" else "Strictly classifiable",
      Weighting = if (is.null(weights)) "Unweighted" else "Truncated stabilized IPW",
      Comparison = substring(term, nchar(prefix) + 1L),
      N = nrow(d), Deaths = sum(d$hospital_death_after_60h == 1),
      SE_method = if (db == "eICU-CRD") "Hospital-clustered HC1" else
        if (is.null(weights)) "Model-based" else "Sandwich HC1 (estimated weights not propagated)",
      OR = exp(beta), CI95_low = exp(beta - 1.96 * se),
      CI95_high = exp(beta + 1.96 * se),
      P_value = 2 * stats::pnorm(abs(beta / se), lower.tail = FALSE))
  })
  bind_tables(rows)
}

if (RUN_INTERNAL_TESTS) {
  stopifnot(identical(EXPOSURE_LABELS, c("No early AKI", "Resolved early AKI", "Ongoing early AKI")))
  stopifnot(isTRUE(all.equal(effective_n(rep(1, 5)), 5)))
  stopifnot(isTRUE(all.equal(
    min(0.98, max(0.02, 0.01)), 0.02)))
}

# --- Execute exploratory analyses; no patient-level export -------------------
SELECTION <- list(); REASONS <- list(); IPW_QC <- list(); IPW_COEF <- list()
IPW_BALANCE <- list(); IPW_OR <- list(); RRT_GROUPS <- list()
RRT_COUNTS <- list(); RRT_MODELS <- list()
SCENARIOS <- c("No_RRT_exclusion", "Exclude_strong_RRT_0_60",
               "Exclude_strong_RRT_before_strict_boundary",
               "Exclude_candidate_RRT_before_strict_boundary")
FILTER_FIELDS <- c(NA_character_, "rrt_strong_any_0_60",
                   "rrt_strong_before_strict_boundary",
                   "rrt_candidate_before_strict_boundary")

for (db in names(COHORTS)) {
  d <- prepare_analysis(db)
  SELECTION[[db]] <- selection_descriptive(d, db)
  REASONS[[db]] <- reason_summary(d, db)
  selected <- d[d$strict_classified == 1, , drop = FALSE]
  if (nrow(selected) != EXPECTED$Strict_classified_N[EXPECTED$Database == db]) {
    stop(db, ": unexpected strict classified N")
  }
  ipw <- selection_weighting(d, db)
  if (!is.null(ipw)) {
    IPW_QC[[db]] <- ipw$qc
    IPW_COEF[[db]] <- ipw$coefficients
    IPW_BALANCE[[db]] <- ipw$balance
    w <- ipw$data[ipw$data$strict_classified == 1, , drop = FALSE]
    IPW_OR[[length(IPW_OR) + 1L]] <- fit_adjusted(
      w, db, "Nearest_prior_pair_IPW", "primary_exposure_code",
      weights = "ipw_stabilized_trimmed")
    IPW_OR[[length(IPW_OR) + 1L]] <- fit_adjusted(
      w, db, "Nearest_prior_pair_IPW", "nearest__strict_code",
      weights = "ipw_stabilized_trimmed")
  }
  IPW_OR[[length(IPW_OR) + 1L]] <- fit_adjusted(
    selected, db, "Nearest_prior_pair_unweighted", "primary_exposure_code")
  IPW_OR[[length(IPW_OR) + 1L]] <- fit_adjusted(
    selected, db, "Nearest_prior_pair_unweighted", "nearest__strict_code")

  for (code in 0:2) {
    ss <- selected[selected$nearest__strict_code == code, , drop = FALSE]
    RRT_GROUPS[[length(RRT_GROUPS) + 1L]] <- data.frame(
      Database = db, Strict_group = EXPOSURE_LABELS[code + 1L], N = nrow(ss),
      Strong_RRT_0_60_N = sum(ss$rrt_strong_any_0_60),
      Strong_RRT_before_boundary_N = sum(ss$rrt_strong_before_strict_boundary),
      Candidate_RRT_before_boundary_N = sum(ss$rrt_candidate_before_strict_boundary),
      Direct_treatment_0_60_N = sum(ss$rrt_direct_any_0_60),
      Flow_record_0_60_N = sum(ss$rrt_flow_any_0_60))
  }
  for (i in seq_along(SCENARIOS)) {
    field <- FILTER_FIELDS[i]
    s <- selected
    if (!is.na(field)) {
      if (anyNA(s[[field]])) stop(db, "/", SCENARIOS[i], ": unresolved RRT flags")
      s <- s[s[[field]] != 1, , drop = FALSE]
    }
    RRT_COUNTS[[length(RRT_COUNTS) + 1L]] <- data.frame(
      Database = db, Scenario = SCENARIOS[i], N = nrow(s),
      Deaths = sum(s$hospital_death_after_60h == 1),
      No_AKI = sum(s$nearest__strict_code == 0),
      Resolved = sum(s$nearest__strict_code == 1),
      Ongoing = sum(s$nearest__strict_code == 2))
    RRT_MODELS[[length(RRT_MODELS) + 1L]] <- fit_adjusted(
      s, db, SCENARIOS[i], "nearest__strict_code")
    RRT_MODELS[[length(RRT_MODELS) + 1L]] <- fit_adjusted(
      s, db, paste0(SCENARIOS[i], "_AKI_only_peak_adjusted"),
      "nearest__strict_code", aki_only = TRUE)
  }
}

write_table(bind_tables(SELECTION), "Exploratory_classifiability_descriptive.csv")
write_table(bind_tables(REASONS), "Exploratory_indeterminate_reasons.csv")
write_table(bind_tables(IPW_QC), "Exploratory_IPW_weight_diagnostics.csv")
write_table(bind_tables(IPW_COEF), "Exploratory_IPW_selection_model.csv")
write_table(bind_tables(IPW_BALANCE), "Exploratory_IPW_balance.csv")
write_table(bind_tables(IPW_OR), "Exploratory_IPW_adjusted_OR.csv")
write_table(bind_tables(RRT_GROUPS), "Exploratory_RRT_by_strict_group.csv")
write_table(bind_tables(RRT_COUNTS), "Exploratory_RRT_joint_sample_counts.csv")
write_table(bind_tables(RRT_MODELS), "Exploratory_RRT_joint_adjusted_OR.csv")

writeLines(c(
  "EXPLORATORY RESEARCH RECORD - NOT FINAL MANUSCRIPT RESULTS",
  "Source: 09_Reviewer1_Temporal_Selection_Bias_and_RRT_Combined_v2.R",
  "Inputs: R07 private joined RDS + both public SQL RRT event exports.",
  "Only the R07 nearest prior temporal classification is used.",
  "Selection model excludes 36-60h monitoring due structural selection concerns.",
  "IPW: P(classifiable | original group, eight adjustment covariates,",
  "pre-ICU reference), probability clip [0.02, 0.98], stabilized selected",
  "weights truncated at 1st and 99th percentiles.",
  "Weighted outcome: quasibinomial GLM, HC1 or hospital-clustered HC1 SE.",
  "Estimated propensity uncertainty is not incorporated into the SE.",
  "RRT is charted evidence, NOT verified RRT treatment initiation time.",
  "No individual patient rows/IDs are written by this script.",
  "Do not interpret as a causal effect or a proof of eliminated selection bias.",
  "", "Warnings:", if (length(WARNINGS)) WARNINGS else "None recorded",
  "", "Log:", LOG, "", capture.output(utils::sessionInfo())
), file.path(OUTPUT_DIR, "exploratory_run_notes.txt"), useBytes = TRUE)
log_note("Exploratory temporal selection/IPW/RRT scripts finished. Outputs: ", OUTPUT_DIR)
