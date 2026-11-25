# ============================================================================
# COPD early creatinine-defined AKI trajectory study
# R/10_figure2_primary_forest.R
# Figure 2: Primary adjusted associations with post-landmark mortality
# MIMIC-IV v3.1 and eICU-CRD v2.0
#
# Reproducibility edition of the original author's:
#   Figure2_primary_adjusted_forest_Arial_v2.R
#
# IMPORTANT INPUT CONTRACT
#   This script reads the aggregate adjusted estimates produced by Script 03.
#   It DOES NOT refit logistic regression, access patient-level data, or alter
#   the exposure/standard-error definitions.
#
# Source (run Script 03 before this script):
#   output/R_analysis_output/03_primary_models_and_sensitivities/
#       Primary_logistic_adjusted_exposure_results.csv
#
# Model fitted upstream in Script 03:
#   age + female sex + CKD + heart failure + diabetes + chronic liver
#   disease + malignancy + first-day nonrenal SOFA.
#   MIMIC: model-based Wald 95% CIs.
#   eICU: hospital-clustered robust HC1 Wald 95% CIs.
#
# Outputs:
#   output/R_analysis_output/10_figure2_primary_forest/
#       Figure2_primary_adjusted_forest.png (600 dpi)
#       Figure2_primary_adjusted_forest.pdf (vector PDF)
#       Figure2_primary_adjusted_forest_data.csv (four aggregate estimates)
#       Figure2_primary_adjusted_forest_QC.csv (comparison to manuscript)
#       Figure2_run_notes.txt
#
# Run from project root:
#   Rscript R/10_figure2_primary_forest.R
# Or set COPD_AKI_PROJECT_ROOT to the project directory.
#
# Published numbers are used only for QC. They NEVER replace the estimates
# produced by Script 03. No patient identifiers or individual records exported.
# ============================================================================

options(stringsAsFactors = FALSE, scipen = 999)

# ----------------------------------------------------------------------------
# 0. Portable paths and display settings
# ----------------------------------------------------------------------------

root_override <- Sys.getenv("COPD_AKI_PROJECT_ROOT", unset = "")
PROJECT_ROOT <- normalizePath(
  if (nzchar(root_override)) root_override else ".",
  winslash = "/", mustWork = TRUE
)

R_OUTPUT_DIR <- file.path(PROJECT_ROOT, "output", "R_analysis_output")
PRIMARY_CSV <- file.path(
  R_OUTPUT_DIR, "03_primary_models_and_sensitivities",
  "Primary_logistic_adjusted_exposure_results.csv"
)
OUTPUT_DIR <- file.path(R_OUTPUT_DIR, "10_figure2_primary_forest")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

PNG_FILE <- file.path(OUTPUT_DIR, "Figure2_primary_adjusted_forest.png")
PDF_FILE <- file.path(OUTPUT_DIR, "Figure2_primary_adjusted_forest.pdf")
CSV_FILE <- file.path(OUTPUT_DIR, "Figure2_primary_adjusted_forest_data.csv")
QC_FILE <- file.path(OUTPUT_DIR, "Figure2_primary_adjusted_forest_QC.csv")
NOTES_FILE <- file.path(OUTPUT_DIR, "Figure2_run_notes.txt")

# TRUE stops if the rounded R03 estimates or model metadata differ from the
# manuscript; the QC CSV is written first so discrepancies can be inspected.
STRICT_MANUSCRIPT_QC <- TRUE

# Original author's Arial-based publication visual system. If Arial is not
# available on the local machine, set COPD_AKI_FIG_FONT=sans before running.
FONT_FAMILY <- Sys.getenv("COPD_AKI_FIG_FONT", unset = "Arial")
if (!nzchar(FONT_FAMILY)) FONT_FAMILY <- "Arial"

COL_RESOLVED <- "#0072B2"
COL_ONGOING <- "#D55E00"
COL_REFERENCE <- "#666666"
COL_GRID <- "#E9E9E9"
COL_TEXT <- "#222222"
COL_MUTED <- "#555555"
SHAPE_MIMIC <- 16
SHAPE_EICU <- 15
FOREST_BREAKS <- c(0.5, 1, 2, 4)
FOREST_LIMITS <- c(0.45, 4.25)

# ----------------------------------------------------------------------------
# 1. Packages: no automatic installation
# ----------------------------------------------------------------------------

required_packages <- c("ggplot2", "dplyr", "tibble", "readr", "patchwork")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them first (or use renv::restore())."
  )
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tibble)
  library(readr)
  library(patchwork)
})

# ----------------------------------------------------------------------------
# 2. Read exactly the four PRIMARY adjusted exposure estimates from R03
# ----------------------------------------------------------------------------

if (!file.exists(PRIMARY_CSV)) {
  stop(
    "Script 03 aggregate results not found: ", PRIMARY_CSV,
    "\nRun R/03_primary_models_and_sensitivities.R first."
  )
}

model_results <- readr::read_csv(
  PRIMARY_CSV,
  show_col_types = FALSE,
  progress = FALSE
)

required_columns <- c(
  "Database", "Analysis_ID", "Model", "SE_method", "Term_label",
  "OR", "CI_low", "CI_high", "P_value_numeric", "N", "Events"
)
missing_columns <- setdiff(required_columns, names(model_results))
if (length(missing_columns)) {
  stop(
    "Script 03 CSV is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

primary_rows <- model_results %>%
  filter(
    Analysis_ID == "Primary_36_60h",
    Model == "Adjusted"
  ) %>%
  mutate(
    trajectory = case_when(
      startsWith(Term_label, "Resolved early AKI") ~ "Resolved early AKI",
      startsWith(Term_label, "Ongoing early AKI") ~ "Ongoing early AKI",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(trajectory))

expected_keys <- paste(
  rep(c("MIMIC-IV", "eICU-CRD"), each = 2L),
  rep(c("Resolved early AKI", "Ongoing early AKI"), times = 2L),
  sep = " | "
)
observed_keys <- paste(
  primary_rows$Database, primary_rows$trajectory, sep = " | "
)
if (nrow(primary_rows) != 4L ||
    anyDuplicated(observed_keys) ||
    !setequal(observed_keys, expected_keys)) {
  stop(
    "Figure 2 requires exactly one adjusted estimate for each ",
    "database x trajectory contrast (four rows). Found: ",
    paste(observed_keys, collapse = "; ")
  )
}

forest_dat <- primary_rows %>%
  transmute(
    Database = as.character(Database),
    trajectory = as.character(trajectory),
    OR = as.numeric(OR),
    lower = as.numeric(CI_low),
    upper = as.numeric(CI_high),
    p_value = as.numeric(P_value_numeric),
    N = as.integer(N),
    Events = as.integer(Events),
    SE_method = as.character(SE_method)
  )

if (any(!is.finite(forest_dat$OR)) ||
    any(!is.finite(forest_dat$lower)) ||
    any(!is.finite(forest_dat$upper)) ||
    any(forest_dat$lower <= 0) ||
    any(forest_dat$lower > forest_dat$OR) ||
    any(forest_dat$OR > forest_dat$upper) ||
    any(!is.finite(forest_dat$p_value)) ||
    any(forest_dat$p_value < 0 | forest_dat$p_value > 1) ||
    any(is.na(forest_dat$N) | forest_dat$N <= 0) ||
    any(is.na(forest_dat$Events) | forest_dat$Events < 0 | forest_dat$Events > forest_dat$N)) {
  stop("Invalid OR, 95% CI, p-value, or cohort counts in Script 03 results.")
}

# Keep plotting positions and color/shape conventions identical to the
# author's original forest plot (previously mislabeled Figure 3 internally).
row_layout <- tibble::tribble(
  ~trajectory,           ~Database,  ~y,
  "Resolved early AKI", "MIMIC-IV", 4.0,
  "Resolved early AKI", "eICU-CRD", 3.2,
  "Ongoing early AKI",  "MIMIC-IV", 1.6,
  "Ongoing early AKI",  "eICU-CRD", 0.8
)

forest_dat <- forest_dat %>%
  left_join(row_layout, by = c("trajectory", "Database")) %>%
  arrange(desc(y)) %>%
  mutate(
    estimate_label = sprintf("%.2f (%.2f\u2013%.2f)", OR, lower, upper),
    trajectory = factor(
      trajectory,
      levels = c("Resolved early AKI", "Ongoing early AKI")
    ),
    Database = factor(Database, levels = c("MIMIC-IV", "eICU-CRD"))
  )
if (any(is.na(forest_dat$y))) {
  stop("A primary estimate could not be assigned a figure row position.")
}

# ----------------------------------------------------------------------------
# 3. Manuscript-facing quality control (compares, never overwrites)
# ----------------------------------------------------------------------------

expected_primary <- tibble::tribble(
  ~Database, ~trajectory, ~Expected_N, ~Expected_Deaths, ~Expected_SE,
  ~Expected_OR, ~Expected_low, ~Expected_high,
  "MIMIC-IV", "Resolved early AKI", 7627L, 832L, "Model-based", 1.20, 0.95, 1.52,
  "eICU-CRD", "Resolved early AKI", 7738L, 709L, "Hospital-clustered robust HC1", 1.50, 1.13, 1.99,
  "MIMIC-IV", "Ongoing early AKI",  7627L, 832L, "Model-based", 2.61, 2.16, 3.17,
  "eICU-CRD", "Ongoing early AKI",  7738L, 709L, "Hospital-clustered robust HC1", 2.35, 1.84, 3.00
)

qc <- forest_dat %>%
  mutate(
    Database = as.character(Database),
    trajectory = as.character(trajectory)
  ) %>%
  left_join(expected_primary, by = c("Database", "trajectory")) %>%
  mutate(
    OR_matches = sprintf("%.2f", OR) == sprintf("%.2f", Expected_OR),
    CI_low_matches = sprintf("%.2f", lower) == sprintf("%.2f", Expected_low),
    CI_high_matches = sprintf("%.2f", upper) == sprintf("%.2f", Expected_high),
    N_matches = N == Expected_N,
    Deaths_match = Events == Expected_Deaths,
    SE_method_matches = SE_method == Expected_SE,
    All_matches = OR_matches & CI_low_matches & CI_high_matches &
      N_matches & Deaths_match & SE_method_matches
  ) %>%
  select(
    Database, trajectory, N, Events, SE_method, OR, lower, upper,
    Expected_N, Expected_Deaths, Expected_SE,
    Expected_OR, Expected_low, Expected_high,
    OR_matches, CI_low_matches, CI_high_matches,
    N_matches, Deaths_match, SE_method_matches, All_matches
  )
readr::write_csv(qc, QC_FILE)

if (isTRUE(STRICT_MANUSCRIPT_QC) && !isTRUE(all(qc$All_matches))) {
  print(qc, n = Inf, width = Inf)
  stop(
    "Figure 2 QC does not match the manuscript. Inspect ", QC_FILE,
    ". Estimates have NOT been replaced with published numbers."
  )
}

# Only aggregated four-row model results are exported; never patient-level data.
readr::write_csv(
  forest_dat %>%
    select(Database, trajectory, N, Events, SE_method,
           OR, lower, upper, p_value, estimate_label),
  CSV_FILE
)

# ------------------------------------------------------------------------------
# 4. Shared plotting data
# ------------------------------------------------------------------------------

group_headers <- tibble(
  x = 0,
  y = c(4.72, 2.32),
  label = c(
    "Resolved early AKI vs no early AKI",
    "Ongoing early AKI vs no early AKI"
  ),
  colour = c(COL_RESOLVED, COL_ONGOING)
)

column_header_y <- 5.22
plot_y_limits <- c(0.35, 5.45)

# ------------------------------------------------------------------------------
# 5. Left panel: trajectory blocks and cohort labels
# ------------------------------------------------------------------------------

left_panel <- ggplot() +
  geom_text(
    data = group_headers,
    aes(x = x, y = y, label = label, colour = colour),
    hjust = 0,
    fontface = "bold",
    family = FONT_FAMILY,
    size = 3.45,
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  geom_text(
    data = forest_dat,
    aes(x = 0.08, y = y, label = Database),
    hjust = 0,
    family = FONT_FAMILY,
    size = 3.35,
    colour = COL_TEXT
  ) +
  annotate(
    "text",
    x = 0,
    y = column_header_y,
    label = "Cohort",
    hjust = 0,
    family = FONT_FAMILY,
    fontface = "bold",
    size = 3.45,
    colour = COL_TEXT
  ) +
  coord_cartesian(
    xlim = c(0, 1),
    ylim = plot_y_limits,
    clip = "off"
  ) +
  theme_void(base_family = FONT_FAMILY) +
  theme(
    plot.margin = margin(t = 8, r = 3, b = 30, l = 2)
  )

# ------------------------------------------------------------------------------
# 6. Middle panel: forest plot
# ------------------------------------------------------------------------------

forest_panel <- ggplot(forest_dat, aes(y = y)) +
  geom_vline(
    xintercept = FOREST_BREAKS,
    linewidth = 0.35,
    colour = COL_GRID
  ) +
  geom_vline(
    xintercept = 1,
    linewidth = 0.65,
    linetype = "dashed",
    colour = COL_REFERENCE
  ) +
  geom_segment(
    aes(
      x = lower,
      xend = upper,
      yend = y,
      colour = trajectory
    ),
    linewidth = 0.9,
    lineend = "round",
    show.legend = FALSE
  ) +
  geom_point(
    aes(
      x = OR,
      colour = trajectory,
      shape = Database
    ),
    size = 3.4,
    stroke = 0.65,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Resolved early AKI" = COL_RESOLVED,
      "Ongoing early AKI" = COL_ONGOING
    )
  ) +
  scale_shape_manual(
    values = c(
      "MIMIC-IV" = SHAPE_MIMIC,
      "eICU-CRD" = SHAPE_EICU
    )
  ) +
  scale_x_log10(
    breaks = FOREST_BREAKS,
    labels = c("0.5", "1", "2", "4"),
    limits = FOREST_LIMITS,
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = plot_y_limits,
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Adjusted odds ratio",
    y = NULL
  ) +
  theme_minimal(
    base_size = 11,
    base_family = FONT_FAMILY
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(
      family = FONT_FAMILY,
      size = 10.3,
      colour = COL_TEXT
    ),
    axis.title.x = element_text(
      family = FONT_FAMILY,
      face = "bold",
      size = 10.8,
      colour = COL_TEXT,
      margin = margin(t = 9)
    ),
    axis.ticks.x = element_line(
      linewidth = 0.45,
      colour = COL_MUTED
    ),
    plot.margin = margin(t = 8, r = 8, b = 16, l = 8)
  )

# ------------------------------------------------------------------------------
# 7. Right panel: numerical estimates
# ------------------------------------------------------------------------------

right_panel <- ggplot() +
  geom_text(
    data = forest_dat,
    aes(x = 0, y = y, label = estimate_label),
    hjust = 0,
    family = FONT_FAMILY,
    size = 3.35,
    colour = COL_TEXT
  ) +
  annotate(
    "text",
    x = 0,
    y = column_header_y,
    label = "aOR (95% CI)",
    hjust = 0,
    family = FONT_FAMILY,
    fontface = "bold",
    size = 3.45,
    colour = COL_TEXT
  ) +
  coord_cartesian(
    xlim = c(0, 1),
    ylim = plot_y_limits,
    clip = "off"
  ) +
  theme_void(base_family = FONT_FAMILY) +
  theme(
    plot.margin = margin(t = 8, r = 2, b = 30, l = 0)
  )

# ------------------------------------------------------------------------------
# 8. Combine panels
# ------------------------------------------------------------------------------

final_plot <- left_panel + forest_panel + right_panel +
  plot_layout(widths = c(1.95, 3.65, 1.45)) &
  theme(
    text = element_text(family = FONT_FAMILY)
  )

# ------------------------------------------------------------------------------
# 9. Export
# ------------------------------------------------------------------------------

ggsave(
  filename = PNG_FILE,
  plot = final_plot,
  width = 8.0,
  height = 4.6,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = PDF_FILE,
  plot = final_plot,
  width = 8.0,
  height = 4.6,
  units = "in",
  device = if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf,
  bg = "white"
)

# ----------------------------------------------------------------------------
# 10. Run notes and console report
# ----------------------------------------------------------------------------

notes <- c(
  "Figure 2: primary adjusted forest plot",
  paste0("Input: ", normalizePath(PRIMARY_CSV, winslash = "/", mustWork = TRUE)),
  "Input is Script 03 aggregate result CSV, not individual patient data.",
  paste0("Font: ", FONT_FAMILY),
  "MIMIC-IV standard errors: model-based (computed in Script 03).",
  "eICU-CRD standard errors: hospital-clustered robust HC1 (computed in Script 03).",
  paste0("Manuscript QC all passed: ", all(qc$All_matches)),
  "Underlying Script 03 results are never altered to force a manuscript match.",
  paste0("R version: ", R.version.string),
  paste0("ggplot2: ", as.character(utils::packageVersion("ggplot2"))),
  paste0("patchwork: ", as.character(utils::packageVersion("patchwork")))
)
writeLines(notes, NOTES_FILE, useBytes = TRUE)

cat("\n============================================================\n")
cat("Figure 2 completed: primary adjusted forest plot\n")
cat("============================================================\n\n")
print(
  forest_dat %>%
    select(Database, trajectory, N, Events, OR, lower, upper, estimate_label),
  n = Inf, width = Inf
)
cat("\nPNG: ", normalizePath(PNG_FILE, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("PDF: ", normalizePath(PDF_FILE, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("CSV: ", normalizePath(CSV_FILE, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("QC: ", normalizePath(QC_FILE, winslash = "/", mustWork = FALSE), "\n", sep = "")
