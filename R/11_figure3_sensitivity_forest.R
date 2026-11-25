# ============================================================================
# COPD early creatinine-defined AKI trajectory study
# R/11_figure3_sensitivity_forest.R
# Figure 3: Primary and four prespecified sensitivity analyses
# MIMIC-IV v3.1 and eICU-CRD v2.0
#
# Public reproducibility edition of the author's:
#   Figure3_sensitivity_forest_Arial_v3.R
# The author's drawing script internally called this Figure 4; this file uses
# the FINAL manuscript numbering (Figure 3).
#
# INPUT CONTRACT: Run R/03_primary_models_and_sensitivities.R first.
# This script consumes FOUR aggregate adjusted-estimate CSVs from Script 03.
# It does not refit logistic regressions or access individual patient data.
# Analysis blocks are: primary, 42-54-h assessment, measured pre-ICU reference,
# additional IMV/vasopressor adjustment, and AECOPD-restricted cohort.
# Each block has two contrasts (resolved and ongoing vs no early AKI) and two
# cohorts (MIMIC-IV and eICU-CRD): 5 x 2 x 2 = 20 plotted estimates.
#
# Script 03 applies model-based Wald intervals in MIMIC-IV and hospital-
# clustered HC1 robust intervals in eICU-CRD. The plot preserves them as-is.
# Manuscript estimates below are used EXCLUSIVELY as QC targets. They never
# replace or adjust the actual output of Script 03.
#
# Run from the project root:
#   Rscript R/11_figure3_sensitivity_forest.R
# Or set the COPD_AKI_PROJECT_ROOT environment variable.
# An optional display-font override is COPD_AKI_FIG_FONT=sans.
# ============================================================================

options(stringsAsFactors = FALSE, scipen = 999)

# ------------------------------------------------------------------------------
# 0. Portable paths, outputs, and original plot visual system
# ------------------------------------------------------------------------------

root_override <- Sys.getenv("COPD_AKI_PROJECT_ROOT", unset = "")
PROJECT_ROOT <- normalizePath(
  if (nzchar(root_override)) root_override else ".",
  winslash = "/", mustWork = TRUE
)
R_OUTPUT_DIR <- file.path(PROJECT_ROOT, "output", "R_analysis_output")
MODEL_DIR <- file.path(R_OUTPUT_DIR, "03_primary_models_and_sensitivities")
OUTPUT_DIR <- file.path(R_OUTPUT_DIR, "11_figure3_sensitivity_forest")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

PRIMARY_FILE <- file.path(MODEL_DIR, "Primary_logistic_adjusted_exposure_results.csv")
CORE_SENS_FILE <- file.path(MODEL_DIR, "Core_sensitivity_adjusted_exposure_results.csv")
ORGAN_SUPPORT_FILE <- file.path(MODEL_DIR, "Sensitivity_IMV_vasopressor_adjusted_exposure_results.csv")
AECOPD_FILE <- file.path(MODEL_DIR, "Optional_AECOPD_adjusted_exposure_results.csv")

PNG_FILE <- file.path(OUTPUT_DIR, "Figure3_sensitivity_forest.png")
PDF_FILE <- file.path(OUTPUT_DIR, "Figure3_sensitivity_forest.pdf")
CSV_FILE <- file.path(OUTPUT_DIR, "Figure3_sensitivity_forest_data.csv")
QC_FILE <- file.path(OUTPUT_DIR, "Figure3_sensitivity_forest_QC.csv")
NOTES_FILE <- file.path(OUTPUT_DIR, "Figure3_run_notes.txt")

# If TRUE, a discrepancy prevents rendering but leaves the detailed QC CSV.
STRICT_MANUSCRIPT_QC <- TRUE

FONT_FAMILY <- Sys.getenv("COPD_AKI_FIG_FONT", unset = "Arial")
if (!nzchar(FONT_FAMILY)) FONT_FAMILY <- "Arial"
COL_RESOLVED <- "#0072B2"
COL_ONGOING  <- "#D55E00"
COL_REFERENCE <- "#666666"
COL_GRID <- "#E9E9E9"
COL_TEXT <- "#222222"
COL_MUTED <- "#555555"
SHAPE_MIMIC <- 16
SHAPE_EICU <- 15
FOREST_BREAKS_RAW <- c(0.5, 1, 2, 4)
FOREST_LIMITS_RAW <- c(0.45, 4.80)

ANALYSIS_LEVELS <- c(
  "Primary analysis",
  "Narrow assessment window (42-54 h)",
  "Measured pre-ICU reference sCr only",
  "Additional adjustment for IMV + vasopressor",
  "AECOPD-restricted cohort"
)

# ------------------------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------------------------

required_packages <- c(
  "ggplot2",
  "dplyr",
  "tibble",
  "readr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them before running this script (or use renv::restore())."
  )
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tibble)
  library(readr)
})

# ------------------------------------------------------------------------------
# 2. Input schema checks and exact exposure-estimate extraction
# ------------------------------------------------------------------------------

check_file_exists <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Script 03 aggregate file is missing: ", path,
      "\nRun R/03_primary_models_and_sensitivities.R first."
    )
  }
}

read_model_results <- function(path, source_label) {
  dat <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  required_columns <- c(
    "Database", "Analysis_ID", "Model", "SE_method", "Term_label",
    "OR", "CI_low", "CI_high", "P_value_numeric", "N", "Events"
  )
  missing_columns <- setdiff(required_columns, names(dat))
  if (length(missing_columns) > 0L) {
    stop(
      source_label, " is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  dat
}

# Extract exactly one adjusted estimate per (database, exposure contrast).
# Filtering by Analysis_ID and Model protects against accidental inclusion of
# unadjusted, AKI-only, or unrelated sensitivity model estimates.
extract_exposure_rows <- function(dat, analysis_label, analysis_id, model_name) {
  out <- dat %>%
    filter(
      Analysis_ID == analysis_id,
      Model == model_name,
      Database %in% c("MIMIC-IV", "eICU-CRD"),
      grepl("^(Resolved|Ongoing) early AKI", Term_label)
    ) %>%
    mutate(
      Analysis = analysis_label,
      trajectory = case_when(
        grepl("^Resolved early AKI", Term_label) ~ "Resolved early AKI",
        grepl("^Ongoing early AKI", Term_label) ~ "Ongoing early AKI",
        TRUE ~ NA_character_
      )
    ) %>%
    transmute(
      Analysis, Analysis_ID, Model, Database, trajectory,
      OR = as.numeric(OR),
      lower = as.numeric(CI_low),
      upper = as.numeric(CI_high),
      p_value = as.numeric(P_value_numeric),
      N = as.integer(N),
      Events = as.integer(Events),
      SE_method = as.character(SE_method)
    )

  expected_keys <- c(
    "MIMIC-IV | Resolved early AKI", "MIMIC-IV | Ongoing early AKI",
    "eICU-CRD | Resolved early AKI", "eICU-CRD | Ongoing early AKI"
  )
  actual_keys <- paste(out$Database, out$trajectory, sep = " | ")
  if (nrow(out) != 4L || anyDuplicated(actual_keys) ||
      !setequal(actual_keys, expected_keys)) {
    stop(
      analysis_label, " requires exactly four distinct cohort/contrast rows. ",
      "Observed: ", paste(actual_keys, collapse = "; ")
    )
  }
  if (any(!is.finite(out$OR) | out$OR <= 0) ||
      any(!is.finite(out$lower) | out$lower <= 0) ||
      any(!is.finite(out$upper) | out$upper <= 0) ||
      any(out$lower > out$OR | out$OR > out$upper) ||
      any(!is.finite(out$p_value) | out$p_value < 0 | out$p_value > 1) ||
      any(is.na(out$N) | out$N <= 0) ||
      any(is.na(out$Events) | out$Events < 0 | out$Events > out$N)) {
    stop(analysis_label, " contains invalid OR/CI, P value or sample counts.")
  }
  out
}

# ------------------------------------------------------------------------------
# 3. Read the four aggregate result files written by Script 03
# ------------------------------------------------------------------------------

for (path in c(PRIMARY_FILE, CORE_SENS_FILE, ORGAN_SUPPORT_FILE, AECOPD_FILE)) {
  check_file_exists(path)
}
primary_raw <- read_model_results(PRIMARY_FILE, "Primary analysis")
core_sens_raw <- read_model_results(CORE_SENS_FILE, "Core sensitivity analyses")
organ_raw <- read_model_results(ORGAN_SUPPORT_FILE, "Organ support adjustment")
aecopd_raw <- read_model_results(AECOPD_FILE, "AECOPD-restricted analysis")

# ------------------------------------------------------------------------------
# 4. Assemble 20 model-derived estimates without refitting regressions
# ------------------------------------------------------------------------------

forest_dat <- bind_rows(
  extract_exposure_rows(
    primary_raw, "Primary analysis", "Primary_36_60h", "Adjusted"
  ),
  extract_exposure_rows(
    core_sens_raw, "Narrow assessment window (42-54 h)",
    "Narrow_42_54h", "Adjusted"
  ),
  extract_exposure_rows(
    core_sens_raw, "Measured pre-ICU reference sCr only",
    "PreICU_reference_only", "Adjusted"
  ),
  extract_exposure_rows(
    organ_raw, "Additional adjustment for IMV + vasopressor",
    "Primary_plus_IMV_vasopressor", "Adjusted + IMV + vasopressor"
  ),
  extract_exposure_rows(
    aecopd_raw, "AECOPD-restricted cohort", "AECOPD_restricted", "Adjusted"
  )
) %>%
  mutate(
    Analysis = factor(Analysis, levels = ANALYSIS_LEVELS),
    Database = factor(Database, levels = c("MIMIC-IV", "eICU-CRD")),
    trajectory = factor(
      trajectory, levels = c("Resolved early AKI", "Ongoing early AKI")
    )
  )

if (nrow(forest_dat) != 20L ||
    anyNA(forest_dat$Analysis) ||
    anyDuplicated(paste(forest_dat$Analysis, forest_dat$Database, forest_dat$trajectory))) {
  stop("Figure 3 requires exactly 20 distinct adjusted estimates: 5 x 2 x 2.")
}

# ------------------------------------------------------------------------------
# 5. Fixed row layout
# ------------------------------------------------------------------------------

row_layout <- tribble(
  ~trajectory,          ~Analysis,                                         ~Database,   ~y,
  "Resolved early AKI", "Primary analysis",                               "MIMIC-IV", 14.80,
  "Resolved early AKI", "Primary analysis",                               "eICU-CRD", 14.30,
  "Resolved early AKI", "Narrow assessment window (42-54 h)",             "MIMIC-IV", 13.40,
  "Resolved early AKI", "Narrow assessment window (42-54 h)",             "eICU-CRD", 12.90,
  "Resolved early AKI", "Measured pre-ICU reference sCr only",            "MIMIC-IV", 12.00,
  "Resolved early AKI", "Measured pre-ICU reference sCr only",            "eICU-CRD", 11.50,
  "Resolved early AKI", "Additional adjustment for IMV + vasopressor",    "MIMIC-IV", 10.60,
  "Resolved early AKI", "Additional adjustment for IMV + vasopressor",    "eICU-CRD", 10.10,
  "Resolved early AKI", "AECOPD-restricted cohort",                       "MIMIC-IV",  9.20,
  "Resolved early AKI", "AECOPD-restricted cohort",                       "eICU-CRD",  8.70,
  "Ongoing early AKI",  "Primary analysis",                               "MIMIC-IV",  7.20,
  "Ongoing early AKI",  "Primary analysis",                               "eICU-CRD",  6.70,
  "Ongoing early AKI",  "Narrow assessment window (42-54 h)",             "MIMIC-IV",  5.80,
  "Ongoing early AKI",  "Narrow assessment window (42-54 h)",             "eICU-CRD",  5.30,
  "Ongoing early AKI",  "Measured pre-ICU reference sCr only",            "MIMIC-IV",  4.40,
  "Ongoing early AKI",  "Measured pre-ICU reference sCr only",            "eICU-CRD",  3.90,
  "Ongoing early AKI",  "Additional adjustment for IMV + vasopressor",    "MIMIC-IV",  3.00,
  "Ongoing early AKI",  "Additional adjustment for IMV + vasopressor",    "eICU-CRD",  2.50,
  "Ongoing early AKI",  "AECOPD-restricted cohort",                       "MIMIC-IV",  1.60,
  "Ongoing early AKI",  "AECOPD-restricted cohort",                       "eICU-CRD",  1.10
) %>%
  mutate(
    Analysis = factor(Analysis, levels = ANALYSIS_LEVELS),
    Database = factor(Database, levels = c("MIMIC-IV", "eICU-CRD")),
    trajectory = factor(
      trajectory,
      levels = c("Resolved early AKI", "Ongoing early AKI")
    )
  )

forest_dat <- forest_dat %>%
  left_join(row_layout, by = c("trajectory", "Analysis", "Database")) %>%
  arrange(desc(y)) %>%
  mutate(
    estimate_label = sprintf("%.2f (%.2f\u2013%.2f)", OR, lower, upper),
    x = log10(OR),
    x_lower = log10(lower),
    x_upper = log10(upper)
  )

if (any(is.na(forest_dat$y))) {
  stop("Figure 3 row layout is incomplete.")
}

analysis_labels <- row_layout %>%
  group_by(trajectory, Analysis) %>%
  summarise(y = mean(y), .groups = "drop") %>%
  mutate(
    label = as.character(Analysis),
    fontface = if_else(Analysis == "Primary analysis", "bold", "plain")
  )

group_headers <- tibble(
  trajectory = factor(
    c("Resolved early AKI", "Ongoing early AKI"),
    levels = c("Resolved early AKI", "Ongoing early AKI")
  ),
  # Keep the resolved title position from v2. Place the ongoing title at the
  # midpoint between the last resolved row (y = 8.70) and first ongoing row
  # (y = 7.20) so the two trajectory blocks have balanced visual spacing.
  y = c(15.78, 7.95),
  label = c(
    "Resolved early AKI vs no early AKI",
    "Ongoing early AKI vs no early AKI"
  )
)

# ------------------------------------------------------------------------------
# 6. Frozen manuscript-facing QC
# ------------------------------------------------------------------------------

EXPECTED_DISPLAY <- tribble(
  ~trajectory,          ~Analysis,                                      ~Database,   ~expected_label,
  "Resolved early AKI", "Primary analysis",                            "MIMIC-IV", "1.20 (0.95\u20131.52)",
  "Resolved early AKI", "Primary analysis",                            "eICU-CRD", "1.50 (1.13\u20131.99)",
  "Resolved early AKI", "Narrow assessment window (42-54 h)",          "MIMIC-IV", "1.35 (1.04\u20131.76)",
  "Resolved early AKI", "Narrow assessment window (42-54 h)",          "eICU-CRD", "1.66 (1.20\u20132.32)",
  "Resolved early AKI", "Measured pre-ICU reference sCr only",         "MIMIC-IV", "1.28 (0.80\u20132.05)",
  "Resolved early AKI", "Measured pre-ICU reference sCr only",         "eICU-CRD", "1.73 (1.14\u20132.62)",
  "Resolved early AKI", "Additional adjustment for IMV + vasopressor", "MIMIC-IV", "1.23 (0.97\u20131.55)",
  "Resolved early AKI", "Additional adjustment for IMV + vasopressor", "eICU-CRD", "1.48 (1.12\u20131.96)",
  "Resolved early AKI", "AECOPD-restricted cohort",                    "MIMIC-IV", "0.89 (0.53\u20131.48)",
  "Resolved early AKI", "AECOPD-restricted cohort",                    "eICU-CRD", "1.67 (0.93\u20132.98)",
  "Ongoing early AKI",  "Primary analysis",                            "MIMIC-IV", "2.61 (2.16\u20133.17)",
  "Ongoing early AKI",  "Primary analysis",                            "eICU-CRD", "2.35 (1.84\u20133.00)",
  "Ongoing early AKI",  "Narrow assessment window (42-54 h)",          "MIMIC-IV", "2.78 (2.25\u20133.44)",
  "Ongoing early AKI",  "Narrow assessment window (42-54 h)",          "eICU-CRD", "2.88 (2.04\u20134.07)",
  "Ongoing early AKI",  "Measured pre-ICU reference sCr only",         "MIMIC-IV", "3.14 (2.26\u20134.36)",
  "Ongoing early AKI",  "Measured pre-ICU reference sCr only",         "eICU-CRD", "2.49 (1.83\u20133.38)",
  "Ongoing early AKI",  "Additional adjustment for IMV + vasopressor", "MIMIC-IV", "2.54 (2.09\u20133.09)",
  "Ongoing early AKI",  "Additional adjustment for IMV + vasopressor", "eICU-CRD", "2.32 (1.81\u20132.97)",
  "Ongoing early AKI",  "AECOPD-restricted cohort",                    "MIMIC-IV", "2.77 (1.81\u20134.26)",
  "Ongoing early AKI",  "AECOPD-restricted cohort",                    "eICU-CRD", "2.28 (1.57\u20133.31)"
) %>%
  mutate(
    Analysis = factor(Analysis, levels = ANALYSIS_LEVELS),
    Database = factor(Database, levels = c("MIMIC-IV", "eICU-CRD")),
    trajectory = factor(
      trajectory,
      levels = c("Resolved early AKI", "Ongoing early AKI")
    )
  )

# Check manuscript OR/CI values as displayed (two-decimal rounding), and
# check the expected sample counts and SE specification for each analysis.
expected_counts <- tibble::tribble(
  ~Analysis, ~Database, ~Expected_N, ~Expected_Deaths,
  "Primary analysis", "MIMIC-IV", 7627L, 832L,
  "Primary analysis", "eICU-CRD", 7738L, 709L,
  "Narrow assessment window (42-54 h)", "MIMIC-IV", 7113L, 762L,
  "Narrow assessment window (42-54 h)", "eICU-CRD", 7143L, 619L,
  "Measured pre-ICU reference sCr only", "MIMIC-IV", 2288L, 263L,
  "Measured pre-ICU reference sCr only", "eICU-CRD", 3293L, 359L,
  "Additional adjustment for IMV + vasopressor", "MIMIC-IV", 7627L, 832L,
  "Additional adjustment for IMV + vasopressor", "eICU-CRD", 7738L, 709L,
  "AECOPD-restricted cohort", "MIMIC-IV", 1492L, 188L,
  "AECOPD-restricted cohort", "eICU-CRD", 2946L, 243L
)

qc_display <- forest_dat %>%
  mutate(
    Analysis = as.character(Analysis),
    Database = as.character(Database),
    trajectory = as.character(trajectory)
  ) %>%
  left_join(
    EXPECTED_DISPLAY %>%
      mutate(
        Analysis = as.character(Analysis),
        Database = as.character(Database),
        trajectory = as.character(trajectory)
      ),
    by = c("trajectory", "Analysis", "Database")
  ) %>%
  left_join(expected_counts, by = c("Analysis", "Database")) %>%
  mutate(
    Expected_SE = if_else(
      Database == "MIMIC-IV", "Model-based", "Hospital-clustered robust HC1"
    ),
    OR_CI_matches = !is.na(expected_label) & estimate_label == expected_label,
    N_matches = !is.na(Expected_N) & N == Expected_N,
    Deaths_match = !is.na(Expected_Deaths) & Events == Expected_Deaths,
    SE_matches = SE_method == Expected_SE,
    All_matches = OR_CI_matches & N_matches & Deaths_match & SE_matches
  ) %>%
  select(
    trajectory, Analysis, Database, Analysis_ID, Model,
    N, Events, SE_method, OR, lower, upper, p_value,
    estimate_label, expected_label, Expected_N, Expected_Deaths, Expected_SE,
    OR_CI_matches, N_matches, Deaths_match, SE_matches, All_matches
  )

# Write QC before a potential error, so differences can be investigated.
readr::write_csv(qc_display, QC_FILE)
if (isTRUE(STRICT_MANUSCRIPT_QC) && !isTRUE(all(qc_display$All_matches))) {
  print(qc_display, n = Inf, width = Inf)
  stop(
    "Figure 3 inputs differ from final manuscript Table S2; see ", QC_FILE,
    ". No published values have been substituted for model results."
  )
}

# Aggregate model estimates only. No stay IDs or patient-level records.
readr::write_csv(
  forest_dat %>%
    select(
      trajectory, Analysis, Database, Analysis_ID, Model,
      N, Events, SE_method, OR, lower, upper, p_value, estimate_label
    ),
  CSV_FILE
)

# ------------------------------------------------------------------------------
# 7. Single-canvas plotting coordinates
# ------------------------------------------------------------------------------

X_FOREST_MIN <- log10(FOREST_LIMITS_RAW[1])
X_FOREST_MAX <- log10(FOREST_LIMITS_RAW[2])
X_BREAKS <- log10(FOREST_BREAKS_RAW)

X_ANALYSIS <- -2.72
X_COHORT <- -1.02
X_VALUES <- 1.03

Y_HEADER <- 16.42
Y_AXIS <- 0.44
Y_AXIS_TICKS_BOTTOM <- 0.57
Y_AXIS_TICKS_TOP <- 0.75
Y_AXIS_LABEL <- -0.02
Y_LIMITS <- c(-0.55, 16.70)

plot_x_limits <- c(X_ANALYSIS, 1.45)

# ------------------------------------------------------------------------------
# 8. Draw figure on a single ggplot canvas
# ------------------------------------------------------------------------------

final_plot <- ggplot() +
  # Column headers
  annotate(
    "text",
    x = X_ANALYSIS,
    y = Y_HEADER,
    label = "Analysis",
    hjust = 0,
    vjust = 0.5,
    family = FONT_FAMILY,
    fontface = "bold",
    size = 3.45,
    colour = COL_TEXT
  ) +
  annotate(
    "text",
    x = X_COHORT,
    y = Y_HEADER,
    label = "Cohort",
    hjust = 0,
    vjust = 0.5,
    family = FONT_FAMILY,
    fontface = "bold",
    size = 3.45,
    colour = COL_TEXT
  ) +
  annotate(
    "text",
    x = X_VALUES,
    y = Y_HEADER,
    label = "aOR (95% CI)",
    hjust = 0,
    vjust = 0.5,
    family = FONT_FAMILY,
    fontface = "bold",
    size = 3.45,
    colour = COL_TEXT
  ) +
  # Group headers
  geom_text(
    data = group_headers,
    aes(x = X_ANALYSIS, y = y, label = label, colour = trajectory),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0.5,
    family = FONT_FAMILY,
    fontface = "bold",
    size = 3.45,
    show.legend = FALSE
  ) +
  # Analysis labels
  geom_text(
    data = analysis_labels,
    aes(x = X_ANALYSIS + 0.04, y = y, label = label, fontface = fontface),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0.5,
    family = FONT_FAMILY,
    size = 3.10,
    colour = COL_TEXT,
    lineheight = 0.95,
    show.legend = FALSE
  ) +
  # Cohort labels
  geom_text(
    data = forest_dat,
    aes(x = X_COHORT, y = y, label = Database),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0.5,
    family = FONT_FAMILY,
    size = 3.02,
    colour = COL_TEXT,
    show.legend = FALSE
  ) +
  # Right-side numerical values
  geom_text(
    data = forest_dat,
    aes(x = X_VALUES, y = y, label = estimate_label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0.5,
    family = FONT_FAMILY,
    size = 3.02,
    colour = COL_TEXT,
    show.legend = FALSE
  ) +
  # Forest grid lines
  geom_segment(
    data = tibble(x = X_BREAKS),
    aes(x = x, xend = x, y = 0.95, yend = 15.35),
    inherit.aes = FALSE,
    linewidth = 0.35,
    colour = COL_GRID
  ) +
  # Reference line at OR = 1
  annotate(
    "segment",
    x = log10(1),
    xend = log10(1),
    y = 0.95,
    yend = 15.35,
    linewidth = 0.65,
    linetype = "dashed",
    colour = COL_REFERENCE
  ) +
  # CI lines
  geom_segment(
    data = forest_dat,
    aes(x = x_lower, xend = x_upper, y = y, yend = y, colour = trajectory),
    inherit.aes = FALSE,
    linewidth = 0.90,
    lineend = "round",
    show.legend = FALSE
  ) +
  # Points
  geom_point(
    data = forest_dat,
    aes(x = x, y = y, colour = trajectory, shape = Database),
    inherit.aes = FALSE,
    size = 3.20,
    stroke = 0.65,
    show.legend = FALSE
  ) +
  scale_shape_manual(
    values = c("MIMIC-IV" = SHAPE_MIMIC, "eICU-CRD" = SHAPE_EICU)
  ) +
  scale_color_manual(
    values = c(
      "Resolved early AKI" = COL_RESOLVED,
      "Ongoing early AKI" = COL_ONGOING
    )
  ) +
  # Manual bottom forest axis
  annotate(
    "segment",
    x = X_FOREST_MIN,
    xend = X_FOREST_MAX,
    y = Y_AXIS_TICKS_BOTTOM,
    yend = Y_AXIS_TICKS_BOTTOM,
    linewidth = 0.45,
    colour = COL_MUTED
  ) +
  geom_segment(
    data = tibble(x = X_BREAKS),
    aes(x = x, xend = x, y = Y_AXIS_TICKS_BOTTOM, yend = Y_AXIS_TICKS_TOP),
    inherit.aes = FALSE,
    linewidth = 0.45,
    colour = COL_MUTED
  ) +
  geom_text(
    data = tibble(
      x = X_BREAKS,
      y = Y_AXIS,
      label = c("0.5", "1", "2", "4")
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    family = FONT_FAMILY,
    size = 3.00,
    colour = COL_TEXT,
    vjust = 1,
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = mean(c(X_FOREST_MIN, X_FOREST_MAX)),
    y = Y_AXIS_LABEL,
    label = "Adjusted odds ratio",
    family = FONT_FAMILY,
    fontface = "bold",
    size = 3.35,
    colour = COL_TEXT,
    vjust = 1
  ) +
  coord_cartesian(
    xlim = plot_x_limits,
    ylim = Y_LIMITS,
    clip = "off"
  ) +
  theme_void(base_family = FONT_FAMILY) +
  theme(
    text = element_text(family = FONT_FAMILY, colour = COL_TEXT),
    plot.margin = margin(t = 10, r = 16, b = 34, l = 10)
  )

# ------------------------------------------------------------------------------
# 9. Export
# ------------------------------------------------------------------------------

ggsave(
  filename = PNG_FILE,
  plot = final_plot,
  width = 8.8,
  height = 8.3,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = PDF_FILE,
  plot = final_plot,
  width = 8.8,
  height = 8.3,
  units = "in",
  device = if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf,
  bg = "white"
)

# ------------------------------------------------------------------------------
# 10. Reproducibility notes and console report
# ------------------------------------------------------------------------------

notes <- c(
  "Figure 3: primary and four prespecified sensitivity analyses",
  "Source: Script 03 aggregate adjusted model results, no patient-level data.",
  paste0("Input directory: ", normalizePath(MODEL_DIR, winslash = "/", mustWork = TRUE)),
  paste0("Input files: ", paste(basename(c(PRIMARY_FILE, CORE_SENS_FILE, ORGAN_SUPPORT_FILE, AECOPD_FILE)), collapse = "; ")),
  paste0("Displayed estimates: ", nrow(forest_dat)),
  paste0("Font: ", FONT_FAMILY),
  "MIMIC-IV intervals: model-based (upstream Script 03).",
  "eICU-CRD intervals: hospital-clustered robust HC1 (upstream Script 03).",
  paste0("Manuscript-facing QC all passed: ", all(qc_display$All_matches)),
  "Final manuscript values are for validation only, never plotting substitutions.",
  paste0("R version: ", R.version.string),
  paste0("ggplot2: ", as.character(utils::packageVersion("ggplot2"))),
  paste0("dplyr: ", as.character(utils::packageVersion("dplyr")))
)
writeLines(notes, NOTES_FILE, useBytes = TRUE)

cat("\n============================================================\n")
cat("Figure 3 completed: primary and sensitivity forest plot\n")
cat("============================================================\n\n")
print(
  forest_dat %>%
    select(trajectory, Analysis, Database, N, Events, OR, lower, upper, estimate_label),
  n = Inf, width = Inf
)
cat("\nPNG: ", normalizePath(PNG_FILE, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("PDF: ", normalizePath(PDF_FILE, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("CSV: ", normalizePath(CSV_FILE, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("QC: ", normalizePath(QC_FILE, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("Run notes: ", normalizePath(NOTES_FILE, winslash = "/", mustWork = FALSE), "\n", sep = "")
