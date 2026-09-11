# =============================================================================
# IIEQI / RAGR-CAGR REPLICATION SCRIPT — REVIEWER REVISION
# =============================================================================
# Purpose:
#   1. Reproduce the IIEQI from one compiled CSV input.
#   2. Correct the PISA CAGR window to 2018-2022 (actual observations only).
#   3. Make classification rules and exceptions explicit.
#   4. Recalculate and audit equity scores transparently.
#   5. Treat the 2024 equity reversal explicitly in scenario projections.
#   6. Produce a formatted Appendix A sensitivity table and worked examples.
#
# Input:
#   ikpii_master_data_FINAL_PISA2025_REVIEWER_FIXED.csv
#
# Output:
#   ikpii_annual_scores.csv
#   ragr_cagr_diagnostic.csv
#   equity_selected_years.csv
#   scenario_projections.csv
#   sensitivity_appendixA.csv
#   worked_examples.csv
#   validation_checks.txt
#
# NOTE:
#   PISA 2025 was officially released on 8 September 2026. The 2025 IIEQI
#   baseline therefore uses the observed official PISA 2025 scores:
#   Reading = 365, Mathematics = 364, Science = 389.
#   The historical PISA CAGR window remains 2018-2022, as prespecified in
#   the manuscript, so the newly released 2025 observation is not silently
#   substituted into the historical momentum estimate.
# =============================================================================

options(stringsAsFactors = FALSE)

# Set working directory to the folder containing the CSV and this script.
setwd("C:/Users/irdha/OneDrive/0. PERKULIAHAN/Artikel/Manuscript Cogent/R cogent education")

INPUT_FILE <- "ikpii_master_data_FINAL_PISA2025_REVIEWER_FIXED.csv"
OUTPUT_DIR <- "outputs"
dir.create(OUTPUT_DIR, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. READ ONE MASTER CSV
# -----------------------------------------------------------------------------
dat <- read.csv(INPUT_FILE, check.names = FALSE)

required_cols <- c("dataset","indicator","year","value","role","source_note")
stopifnot(all(required_cols %in% names(dat)))

get_annual <- function() {
  x <- subset(dat, dataset == "IIEQI_annual" & role == "annual_index_input")
  wide <- reshape(x[, c("indicator","year","value")],
                  idvar = "year", timevar = "indicator", direction = "wide")
  names(wide) <- sub("^value\\.", "", names(wide))
  wide[order(wide$year), ]
}

annual <- get_annual()

required_indicators <- c(
  "PISA Reading","PISA Mathematics","PISA Science",
  "AN Literacy","AN Numeracy","APK Tertiary",
  "APS 16-18","Urban-Rural Gap","GPI"
)
stopifnot(all(required_indicators %in% names(annual)))

# -----------------------------------------------------------------------------
# 2. LOCKED NORMALIZATION PARAMETERS
# -----------------------------------------------------------------------------
# Floors/ceilings are fixed before calculating the annual index and are not
# re-estimated year by year. This preserves temporal comparability.
norm <- data.frame(
  indicator = c(
    "PISA Reading","PISA Mathematics","PISA Science",
    "AN Literacy","AN Numeracy","APK Tertiary","APS 16-18",
    "Urban-Rural Gap"
  ),
  floor = c(320,320,350,48,27,25.26,55.05,0.50),
  ceiling = c(402,391,403,100,100,32.89,76.20,15.32),
  direction = c("positive","positive","positive","positive","positive",
                "positive","positive","negative")
)

minmax <- function(x, lo, hi) {
  if (hi <= lo) stop("Invalid normalization bounds.")
  pmax(0, pmin(100, (x - lo) / (hi - lo) * 100))
}

score_indicator <- function(x, indicator) {
  if (indicator == "GPI") {
    # UNESCO parity interval used in the manuscript.
    # 0.97-1.03 = full score; deviations of 0.10 beyond either boundary
    # receive zero, with linear interpolation.
    score <- ifelse(
      x >= 0.97 & x <= 1.03, 100,
      ifelse(x > 1.03,
             pmax(0, 100 * (1 - (x - 1.03)/0.10)),
             pmax(0, 100 * (1 - (0.97 - x)/0.10)))
    )
    return(score)
  }

  p <- norm[norm$indicator == indicator, ]
  if (nrow(p) != 1) stop(paste("No normalization rule for", indicator))

  if (p$direction == "positive") {
    return(minmax(x, p$floor, p$ceiling))
  } else {
    # Lower gap = better equity.
    return(pmax(0, pmin(100,
                         (p$ceiling - x)/(p$ceiling - p$floor)*100)))
  }
}

# -----------------------------------------------------------------------------
# 3. CALCULATE INDICATOR, DIMENSION, AND COMPOSITE SCORES
# -----------------------------------------------------------------------------
for (ind in required_indicators) {
  annual[[paste0(ind, "_score")]] <- score_indicator(annual[[ind]], ind)
}

annual$D1a_PISA <- rowMeans(
  annual[, paste0(c("PISA Reading","PISA Mathematics","PISA Science"), "_score")]
)
annual$D1b_AN <- rowMeans(
  annual[, paste0(c("AN Literacy","AN Numeracy"), "_score")]
)
annual$D2_Access <- rowMeans(
  annual[, paste0(c("APK Tertiary","APS 16-18"), "_score")]
)
annual$D3_Equity <- rowMeans(
  annual[, paste0(c("Urban-Rural Gap","GPI"), "_score")]
)
annual$IIEQI <- rowMeans(
  annual[, c("D1a_PISA","D1b_AN","D2_Access","D3_Equity")]
)

# -----------------------------------------------------------------------------
# 4. EXPLICIT CLASSIFICATION HIERARCHY
# -----------------------------------------------------------------------------
# Pre-specified order:
#   (1) Internal Anomaly: long-term target is below the 2029 milestone.
#   (2) Trend Reversal: historical CAGR < 0.
#   (3) Trend Reversal: gap > 5 pp.
#   (4) Significant Acceleration: 2 < gap <= 5 pp.
#   (5) Moderate Acceleration: 0 < gap <= 2 pp.
#   (6) Trend Sufficient: gap <= 0 pp.
#
# The negative-CAGR rule is retained because a positive target requires first
# reversing a declining observed trajectory. The Internal Anomaly is reported
# separately because it describes inconsistency within the planning sequence,
# not the magnitude of acceleration.
classify_status <- function(gap, cagr, target_2029, target_2045) {
  if (target_2045 < target_2029) return("Internal Anomaly")
  if (cagr < 0) return("Trend Reversal")
  if (gap > 5) return("Trend Reversal")
  if (gap > 2) return("Significant Acceleration")
  if (gap > 0) return("Moderate Acceleration")
  return("Trend Sufficient")
}

# -----------------------------------------------------------------------------
# 5. RAGR-CAGR DIAGNOSTIC
# -----------------------------------------------------------------------------
cagr <- function(start, end, years) {
  if (start <= 0 || end <= 0 || years <= 0) return(NA_real_)
  ((end/start)^(1/years) - 1) * 100
}

ragr <- function(base, target, years) {
  if (base <= 0 || target <= 0 || years <= 0) return(NA_real_)
  ((target/base)^(1/years) - 1) * 100
}

hist_start <- c(371,379,396,53.39,32.93,31.19)
hist_end   <- c(359,366,383,68.62,67.35,32.89)
hist_years <- c(4,4,4,4,4,4)   # PISA 2018->2022; AN/APK 2021->2025

target_df <- data.frame(
  Indicator = c("PISA Reading","PISA Mathematics","PISA Science",
                "AN Literacy","AN Numeracy","APK Tertiary"),
  Base_2025 = c(365,364,389,68.62,67.35,32.89),
  T_2029 = c(409,416,426,76.62,75.35,38.04),
  T_2045 = c(485,490,487,75.73,68.72,60.00),

# Planning-target base values must match the observed 2025 baseline used by the
# revised manuscript and the IIEQI annual dataset.
pt <- subset(dat, dataset == "Planning_target" & role == "base_2025")
stopifnot(all(pt$value[match(target_df$Indicator, pt$indicator)] == target_df$Base_2025))
  CAGR = mapply(cagr, hist_start, hist_end, hist_years),
  CAGR_window = c("2018-2022","2018-2022","2018-2022",
                  "2021-2025","2021-2025","2021-2025")
)

target_df$RAGR_2029 <- mapply(ragr, target_df$Base_2025,
                             target_df$T_2029, MoreArgs=list(years=4))
target_df$RAGR_2045 <- mapply(ragr, target_df$Base_2025,
                             target_df$T_2045, MoreArgs=list(years=20))
target_df$Gap_2029 <- target_df$RAGR_2029 - target_df$CAGR
target_df$Gap_2045 <- target_df$RAGR_2045 - target_df$CAGR

target_df$Status_2029 <- mapply(
  classify_status, target_df$Gap_2029, target_df$CAGR,
  target_df$T_2029, target_df$T_2045
)
target_df$Status_2045 <- mapply(
  classify_status, target_df$Gap_2045, target_df$CAGR,
  target_df$T_2029, target_df$T_2045
)

# -----------------------------------------------------------------------------
# 6. EQUITY AUDIT: REPRODUCIBLE SELECTED-YEAR CALCULATION
# -----------------------------------------------------------------------------
equity_years <- c(2009,2014,2021,2022,2023,2024)
equity_raw <- data.frame(
  Year = equity_years,
  Total_APS = c(55.05,70.13,70.74,72.88,73.07,74.35),
  Urban_APS = c(62.84,74.68,73.90,75.79,75.61,77.60),
  Rural_APS = c(47.52,65.29,66.54,69.04,69.51,69.59),
  UR_Gap = c(15.32,9.39,7.36,6.75,6.10,8.01),
  GPI = c(0.972,1.013,1.021,1.044,1.042,1.048)
)
equity_raw$UR_Score <- score_indicator(equity_raw$UR_Gap, "Urban-Rural Gap")
equity_raw$GPI_Score <- score_indicator(equity_raw$GPI, "GPI")
equity_raw$Equity_Score <- rowMeans(equity_raw[,c("UR_Score","GPI_Score")])

# Transparent worked check for the 2022 equity score.
check_2022 <- subset(equity_raw, Year == 2022)
expected_2022_ur <- (15.32 - 6.75)/(15.32 - 0.50)*100
expected_2022_gpi <- score_indicator(1.044, "GPI")
expected_2022_eq <- mean(c(expected_2022_ur, expected_2022_gpi))
if (abs(check_2022$UR_Score - expected_2022_ur) > 1e-8 ||
    abs(check_2022$GPI_Score - expected_2022_gpi) > 1e-8 ||
    abs(check_2022$Equity_Score - expected_2022_eq) > 1e-8) {
  stop("Equity calculation failed the 2022 reproducibility check.")
}

# Historical convergence benchmark 2009->2023:
# This is NOT treated as a forecast after the 2024 reversal. It is a conditional
# benchmark for a recovery pathway.
equity_2009_2023_rate <- cagr(15.32, 6.10, 14)

# -----------------------------------------------------------------------------
# 7. SCENARIO PROJECTIONS — EXPLICIT COUNTERFACTUALS
# -----------------------------------------------------------------------------
# The scenarios are not probability forecasts.
#
# Quality/access indicators:
#   Conservative = 0% of historical CAGR (hold at 2025)
#   Moderate    = 50% of historical CAGR
#   Optimistic  = 100% of historical CAGR
#
# Because PISA historical CAGR is negative, upward PISA projections are not
# generated by mechanically scaling a decline. PISA therefore remains at the
# 2025 carry-forward baseline in all three scenarios.
#
# Equity:
#   The 2024 widening (6.10 -> 8.01 pp) is acknowledged explicitly.
#   The 2009-2023 convergence rate is used only as a CONDITIONAL recovery
#   benchmark starting from the 2025 observed gap of 8.01 pp.
#   GPI moves toward the 1.03 upper parity boundary at 2.5% (Moderate) or
#   5% (Optimistic) of the remaining deviation per year.
#
# This avoids treating the 2024 reversal as if it had never occurred.

scenario_k <- c(Conservative=0, Moderate=0.5, Optimistic=1.0)

project_value <- function(base, cagr_pct, years, k, upper=Inf) {
  x <- base * (1 + (cagr_pct/100)*k)^years
  pmin(x, upper)
}

# Baseline 2025 dimension values
base <- annual[annual$year == 2025, ]

# Historical CAGRs for AN/APK only (positive trajectories)
cagr_map <- setNames(target_df$CAGR, target_df$Indicator)

project_quality_access <- function(years, k) {
  pisa_d1a <- base$D1a_PISA  # negative historical CAGR -> no mechanical upward scaling

  an_lit <- project_value(base$`AN Literacy`, cagr_map["AN Literacy"], years, k, 100)
  an_num <- project_value(base$`AN Numeracy`, cagr_map["AN Numeracy"], years, k, 100)
  apk    <- project_value(base$`APK Tertiary`, cagr_map["APK Tertiary"], years, k, 100)
  aps    <- base$`APS 16-18`  # locked ceiling already reached

  an_lit_s <- score_indicator(an_lit, "AN Literacy")
  an_num_s <- score_indicator(an_num, "AN Numeracy")
  apk_s    <- score_indicator(apk, "APK Tertiary")
  aps_s    <- score_indicator(aps, "APS 16-18")

  list(
    D1a = pisa_d1a,
    D1b = mean(c(an_lit_s, an_num_s)),
    D2 = mean(c(apk_s, aps_s))
  )
}

project_equity <- function(years, scenario) {
  if (scenario == "Conservative") {
    ur_gap <- 8.01
    gpi <- 1.052
  } else {
    # Apply the historical convergence benchmark only as a conditional
    # recovery pathway from the post-reversal 2025 baseline.
    ur_gap <- max(0.50, 8.01 * (1 + equity_2009_2023_rate/100)^years)
    gpi_rate <- ifelse(scenario == "Moderate", 0.025, 0.05)
    # Gradual movement toward the upper parity boundary.
    gpi <- 1.03 + (1.052 - 1.03) * (1 - gpi_rate)^years
  }

  ur_s <- score_indicator(ur_gap, "Urban-Rural Gap")
  gpi_s <- score_indicator(gpi, "GPI")

  list(
    UR_gap = ur_gap,
    GPI = gpi,
    D3 = mean(c(ur_s, gpi_s))
  )
}

scenario_out <- list()
ii <- 1
for (s in names(scenario_k)) {
  for (yr in c(2029,2045)) {
    n <- yr - 2025
    qa <- project_quality_access(n, scenario_k[s])
    eq <- project_equity(n, s)
    scenario_out[[ii]] <- data.frame(
      Scenario=s, Year=yr,
      D1a_PISA=qa$D1a,
      D1b_AN=qa$D1b,
      D2_Access=qa$D2,
      D3_Equity=eq$D3,
      UR_Gap=eq$UR_gap,
      GPI=eq$GPI
    )
    ii <- ii + 1
  }
}
scenario_df <- do.call(rbind, scenario_out)
scenario_df$IIEQI <- rowMeans(scenario_df[,c("D1a_PISA","D1b_AN","D2_Access","D3_Equity")])

# -----------------------------------------------------------------------------
# 8. SENSITIVITY ANALYSIS — APPENDIX A
# -----------------------------------------------------------------------------
sensitivity <- data.frame(
  Year = annual$year,
  Equal = annual$IIEQI,
  Cov_Informed = annual$D1a_PISA*0.30 + annual$D1b_AN*0.25 +
                 annual$D2_Access*0.25 + annual$D3_Equity*0.20,
  Qual_Heavy = annual$D1a_PISA*0.30 + annual$D1b_AN*0.30 +
               annual$D2_Access*0.25 + annual$D3_Equity*0.15,
  Equity_Heavy = annual$D1a_PISA*0.20 + annual$D1b_AN*0.20 +
                 annual$D2_Access*0.25 + annual$D3_Equity*0.35
)
sensitivity$Max_Dev <- apply(abs(sensitivity[,2:5] -
                                   sensitivity$Equal), 1, max)
sensitivity$Mean_Dev <- rowMeans(abs(sensitivity[,2:5] -
                                       sensitivity$Equal))

# -----------------------------------------------------------------------------
# 9. WORKED NUMERICAL EXAMPLES — REVIEWER REQUEST
# -----------------------------------------------------------------------------
wr_pisa <- data.frame(
  Indicator="PISA Reading",
  Historical_Start=371,
  Historical_End=359,
  Historical_Years=4,
  Target_2029=409,
  Target_Horizon=4
)
wr_pisa$CAGR <- cagr(371,359,4)
wr_pisa$RAGR_2029 <- ragr(359,409,4)
wr_pisa$Gap_2029 <- wr_pisa$RAGR_2029 - wr_pisa$CAGR
wr_pisa$Classification <- classify_status(
  wr_pisa$Gap_2029, wr_pisa$CAGR, 409, 485
)

wr_an <- data.frame(
  Indicator="AN Literacy",
  Historical_Start=53.39,
  Historical_End=68.62,
  Historical_Years=4,
  Target_2029=76.62,
  Target_Horizon=4
)
wr_an$CAGR <- cagr(53.39,68.62,4)
wr_an$RAGR_2029 <- ragr(68.62,76.62,4)
wr_an$Gap_2029 <- wr_an$RAGR_2029 - wr_an$CAGR
wr_an$Classification <- classify_status(
  wr_an$Gap_2029, wr_an$CAGR, 76.62, 75.73
)

worked <- rbind(wr_pisa, wr_an)

# -----------------------------------------------------------------------------
# 10. VALIDATION CHECKS
# -----------------------------------------------------------------------------
checks <- character()

checks <- c(checks, sprintf(
  "2025 IIEQI = %.2f (official PISA 2025 baseline; previous reference was 67.33)", 
  base$IIEQI
))
checks <- c(checks, sprintf(
  "PISA Reading CAGR 2018-2022 = %.3f%%", target_df$CAGR[1]
))
checks <- c(checks, sprintf(
  "PISA Mathematics CAGR 2018-2022 = %.3f%%", target_df$CAGR[2]
))
checks <- c(checks, sprintf(
  "PISA Science CAGR 2018-2022 = %.3f%%", target_df$CAGR[3]
))
checks <- c(checks, sprintf(
  "2022 equity score = %.3f", check_2022$Equity_Score
))
checks <- c(checks, sprintf(
  "2009-2023 urban-rural gap convergence benchmark = %.3f%% p.a.",
  equity_2009_2023_rate
))
checks <- c(checks, sprintf(
  "Sensitivity overall mean deviation = %.3f pts",
  mean(sensitivity$Mean_Dev)
))
checks <- c(checks, sprintf(
  "Sensitivity overall max deviation = %.3f pts",
  max(sensitivity$Max_Dev)
))
checks <- c(checks,
  "PISA 2025 baseline updated to official values released 8 September 2026 (Kemendikdasmen/OECD); historical PISA CAGR remains 2018-2022."
)

# -----------------------------------------------------------------------------
# 11. EXPORT REPLICATION OUTPUTS
# -----------------------------------------------------------------------------
write.csv(annual, file.path(OUTPUT_DIR,"ikpii_annual_scores.csv"), row.names=FALSE)
write.csv(target_df, file.path(OUTPUT_DIR,"ragr_cagr_diagnostic.csv"), row.names=FALSE)
write.csv(equity_raw, file.path(OUTPUT_DIR,"equity_selected_years.csv"), row.names=FALSE)
write.csv(scenario_df, file.path(OUTPUT_DIR,"scenario_projections.csv"), row.names=FALSE)
write.csv(sensitivity, file.path(OUTPUT_DIR,"sensitivity_appendixA.csv"), row.names=FALSE)
write.csv(worked, file.path(OUTPUT_DIR,"worked_examples.csv"), row.names=FALSE)
writeLines(checks, file.path(OUTPUT_DIR,"validation_checks.txt"))

cat("\n=== IIEQI REVIEWER-REVISION REPLICATION ===\n")
print(annual[,c("year","D1a_PISA","D1b_AN","D2_Access","D3_Equity","IIEQI")],
      row.names=FALSE, digits=4)

cat("\n=== RAGR-CAGR DIAGNOSTIC ===\n")
print(target_df[,c("Indicator","Base_2025","T_2029","RAGR_2029",
                   "CAGR","Gap_2029","Status_2029",
                   "T_2045","RAGR_2045","Gap_2045","Status_2045")],
      row.names=FALSE, digits=4)

cat("\n=== EQUITY SELECTED YEARS ===\n")
print(equity_raw[,c("Year","UR_Gap","UR_Score","GPI","GPI_Score","Equity_Score")],
      row.names=FALSE, digits=4)

cat("\n=== SCENARIO PROJECTIONS ===\n")
print(scenario_df, row.names=FALSE, digits=4)

cat("\n=== WORKED EXAMPLES ===\n")
print(worked, row.names=FALSE, digits=4)

cat("\n=== VALIDATION ===\n")
cat(paste(checks, collapse="\n"), "\n")

cat("\nOutputs written to:", normalizePath(OUTPUT_DIR), "\n")
# =============================================================================
# END
# =============================================================================
