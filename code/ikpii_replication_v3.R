# =============================================================================
# IIEQI / RAGR-CAGR REPLICATION SCRIPT — v3 (FULLY CSV-DRIVEN; R1 REVISION)
# =============================================================================
# Purpose:
#   1. Reproduce the IIEQI from one compiled CSV input.
#   2. Correct the PISA CAGR window to 2018-2022 (actual observations only).
#   3. Make classification rules and exceptions explicit.
#   4. Recalculate and audit equity scores transparently.
#   5. Treat the 2024-2025 equity reversal explicitly in scenario projections.
#   6. Produce a formatted Appendix A sensitivity table and worked examples.
#
# CHANGES FROM THE PRIOR VERSION (ikpii_replication_reviewer_revision_FIXED.R):
#   - RAGR-CAGR historical anchors and planning targets (target_df) are now
#     READ from the CSV's CAGR_anchor / Planning_target sections instead of
#     being hardcoded a second time in this script. Previously the CSV rows
#     existed but were never actually consumed — this is now fixed.
#   - The equity historical table (equity_raw) is now READ from the CSV's new
#     Equity_history section (Total/Urban/Rural/Male/Female APS by year)
#     instead of being hardcoded. Urban-Rural Gap and GPI are DERIVED from
#     those raw figures rather than typed in as separate numbers, which is
#     what let a stale 2024 Urban-Rural Gap value (8.01) get silently
#     carried into 2025 in the prior version. This also means equity_raw now
#     automatically includes 2025, once the CSV provides it.
#   - Scenario projections' Conservative case and the equity recovery
#     pathway's starting gap now reference the actual 2025 observed
#     Urban-Rural Gap / GPI from `annual` dynamically, instead of hardcoded
#     literals (8.01 / 1.052) that would otherwise need to be remembered and
#     manually kept in sync.
#   - Worked examples are now built directly from target_df (the same table
#     behind Table 4A/4B) instead of being recomputed from separately
#     hardcoded historical figures. This fixes a bug in the prior version,
#     where the PISA Reading worked example used the stale pre-2025 baseline
#     (359) instead of the updated official 2025 baseline (365) for the
#     RAGR_2029 calculation, producing a worked-example RAGR/Gap that
#     contradicted Table 4A.
#
# CHANGES IN v3 (revision round 1):
#   - INPUT_FILE is now located in data/ (or the working directory), so the
#     script runs from the repository root as documented in the README.
#   - Appendix A: Mean_Dev is now the mean absolute deviation of the THREE
#     alternative weighting schemes from the Equal-weight baseline (the
#     Equal scheme's own zero deviation is no longer averaged in), matching
#     the manuscript.
#   - Appendix B: PISA CAGR window sensitivity (2018-2022, 2022-2025,
#     2018-2025) -> appendixB_pisa_windows.csv
#   - Appendix C: floor sensitivity, k-grid, equity-scenario variants,
#     threshold sensitivity, and D2 ceiling anchoring -> appendixC_*.csv
#   - Equity sensitivity (-3.90 percent per year, 2009-2025) reported.
#   - Table 1 (normalization parameters, with observed minima/maxima and
#     derivation rules) -> table1_normalization_parameters.csv
#   - Failure-mode mapping (Section 3.4 rules) added to the diagnostic output.
#   - The PISA 2018-2022 window is the specification fixed in the original
#     analysis, before the PISA 2025 release; it is NOT described as
#     "pre-specified" in a registered sense (see manuscript Section 3.4).
#
# Input:
#   ikpii_master_data_FINAL_PISA2025_v2.csv (in data/)
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
#   The historical PISA CAGR window remains 2018-2022, the window fixed in
#   the original analysis before the 2025 release, so the newly released
#   2025 observation is not silently substituted into the historical
#   momentum estimate (alternative windows: Appendix B).
# =============================================================================

options(stringsAsFactors = FALSE)

setwd(".")

INPUT_FILE <- if (file.exists(file.path("data","ikpii_master_data_FINAL_PISA2025_v2.csv"))) file.path("data","ikpii_master_data_FINAL_PISA2025_v2.csv") else "ikpii_master_data_FINAL_PISA2025_v2.csv"
OUTPUTS_DIR <- "output"
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
classify_status <- function(gap, cagr, target_2029, target_2045) {
  if (target_2045 < target_2029) return("Internal Anomaly")
  if (cagr < 0) return("Trend Reversal")
  if (gap > 5) return("Trend Reversal")
  if (gap > 2) return("Significant Acceleration")
  if (gap > 0) return("Moderate Acceleration")
  return("Trend Sufficient")
}

# -----------------------------------------------------------------------------
# 5. RAGR-CAGR DIAGNOSTIC — historical anchors and targets READ FROM CSV
# -----------------------------------------------------------------------------
cagr <- function(start, end, years) {
  if (start <= 0 || end <= 0 || years <= 0) return(NA_real_)
  ((end/start)^(1/years) - 1) * 100
}

ragr <- function(base, target, years) {
  if (base <= 0 || target <= 0 || years <= 0) return(NA_real_)
  ((target/base)^(1/years) - 1) * 100
}

anchor <- subset(dat, dataset == "CAGR_anchor")
anchor_start <- subset(anchor, role == "historical_start")[, c("indicator","year","value")]
names(anchor_start) <- c("Indicator","Year_Start","Historical_Start")
anchor_end <- subset(anchor, role == "historical_end")[, c("indicator","year","value")]
names(anchor_end) <- c("Indicator","Year_End","Historical_End")
anchor_df <- merge(anchor_start, anchor_end, by = "Indicator")
anchor_df$Historical_Years <- anchor_df$Year_End - anchor_df$Year_Start
anchor_df$CAGR_window <- paste0(anchor_df$Year_Start, "-", anchor_df$Year_End)

pt <- subset(dat, dataset == "Planning_target")
pt_base <- subset(pt, role == "base_2025")[, c("indicator","value")]
names(pt_base) <- c("Indicator","Base_2025")
pt_2029 <- subset(pt, role == "target_2029")[, c("indicator","value")]
names(pt_2029) <- c("Indicator","T_2029")
pt_2045 <- subset(pt, role == "target_2045")[, c("indicator","value")]
names(pt_2045) <- c("Indicator","T_2045")

target_df <- Reduce(function(x, y) merge(x, y, by = "Indicator"),
                     list(anchor_df, pt_base, pt_2029, pt_2045))

# Preserve the manuscript's presentation order (Table 4A then Table 4B).
indicator_order <- c("PISA Reading","PISA Mathematics","PISA Science",
                      "AN Literacy","AN Numeracy","APK Tertiary")
target_df <- target_df[match(indicator_order, target_df$Indicator), ]
rownames(target_df) <- NULL

target_df$CAGR <- mapply(cagr, target_df$Historical_Start,
                          target_df$Historical_End, target_df$Historical_Years)
target_df$RAGR_2029 <- mapply(ragr, target_df$Base_2025,
                               target_df$T_2029, MoreArgs = list(years = 4))
target_df$RAGR_2045 <- mapply(ragr, target_df$Base_2025,
                               target_df$T_2045, MoreArgs = list(years = 20))
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
# 6. EQUITY AUDIT — READ FROM CSV, GAP AND GPI DERIVED (NOT HARDCODED)
# -----------------------------------------------------------------------------
eq <- subset(dat, dataset == "Equity_history")
eq_wide <- reshape(eq[, c("indicator","year","value")],
                    idvar = "year", timevar = "indicator", direction = "wide")
names(eq_wide) <- sub("^value\\.", "", names(eq_wide))
eq_wide <- eq_wide[order(eq_wide$year), ]
names(eq_wide)[names(eq_wide) == "year"] <- "Year"
equity_raw <- eq_wide

# Urban-Rural Gap and GPI are DERIVED from the raw Perkotaan/Perdesaan and
# male/female APS figures, each rounded to 2dp before combining — matching
# how the underlying BPS Susenas figures are officially published, and how
# every prior year in this series was actually computed. This is what
# prevents a stale carry-forward value (e.g. 2024's gap silently reused for
# 2025) from ever entering the pipeline undetected again.
equity_raw$UR_Gap <- round(round(equity_raw$Urban_APS, 2) - round(equity_raw$Rural_APS, 2), 2)
equity_raw$GPI <- round(round(equity_raw$Female_APS, 2) / round(equity_raw$Male_APS, 2), 3)

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

# Historical convergence benchmark 2009->2023, sourced from equity_raw itself
# (not hardcoded), so it always reflects whatever is actually in the CSV.
gap_2009 <- equity_raw$UR_Gap[equity_raw$Year == 2009]
gap_2023 <- equity_raw$UR_Gap[equity_raw$Year == 2023]
equity_2009_2023_rate <- cagr(gap_2009, gap_2023, 2023 - 2009)

# -----------------------------------------------------------------------------
# 7. SCENARIO PROJECTIONS — EXPLICIT COUNTERFACTUALS
# -----------------------------------------------------------------------------
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
#   The 2024-2025 widening is read from the actual 2025 observation (via
#   `annual`), not hardcoded, so a future data update propagates automatically.
#   The 2009-2023 convergence rate is used only as a CONDITIONAL recovery
#   benchmark starting from the 2025 observed gap.
#   GPI moves toward the 1.03 upper parity boundary at 2.5% (Moderate) or
#   5% (Optimistic) of the remaining deviation per year.

scenario_k <- c(Conservative=0, Moderate=0.5, Optimistic=1.0)

project_value <- function(base, cagr_pct, years, k, upper=Inf) {
  x <- base * (1 + (cagr_pct/100)*k)^years
  pmin(x, upper)
}

# Baseline 2025 dimension values
base <- annual[annual$year == 2025, ]

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
    ur_gap <- base$`Urban-Rural Gap`
    gpi <- base$GPI
  } else {
    ur_gap <- max(0.50, base$`Urban-Rural Gap` * (1 + equity_2009_2023_rate/100)^years)
    gpi_rate <- ifelse(scenario == "Moderate", 0.025, 0.05)
    gpi <- 1.03 + (base$GPI - 1.03) * (1 - gpi_rate)^years
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
    eq2 <- project_equity(n, s)
    scenario_out[[ii]] <- data.frame(
      Scenario=s, Year=yr,
      D1a_PISA=qa$D1a,
      D1b_AN=qa$D1b,
      D2_Access=qa$D2,
      D3_Equity=eq2$D3,
      UR_Gap=eq2$UR_gap,
      GPI=eq2$GPI
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
sensitivity$Max_Dev <- apply(abs(sensitivity[,3:5] -
                                   sensitivity$Equal), 1, max)
# Mean absolute deviation of the THREE alternative schemes (columns 3:5)
# from the Equal-weight baseline (column 2).
sensitivity$Mean_Dev <- rowMeans(abs(sensitivity[,3:5] -
                                       sensitivity$Equal))

# -----------------------------------------------------------------------------
# 9. WORKED NUMERICAL EXAMPLES — BUILT DIRECTLY FROM target_df
# -----------------------------------------------------------------------------
# Building these from target_df (rather than recomputing from separately
# hardcoded historical figures) guarantees the worked example can never again
# drift out of sync with Table 4A/4B, as happened previously when the PISA
# Reading example used the stale pre-2025 baseline for RAGR_2029.
build_worked_example <- function(indicator_name) {
  row <- target_df[target_df$Indicator == indicator_name, ]
  data.frame(
    Indicator = row$Indicator,
    Historical_Start = row$Historical_Start,
    Historical_End = row$Historical_End,
    Historical_Years = row$Historical_Years,
    Target_2029 = row$T_2029,
    Target_Horizon = 4,
    CAGR = row$CAGR,
    RAGR_2029 = row$RAGR_2029,
    Gap_2029 = row$Gap_2029,
    Classification = row$Status_2029
  )
}

worked <- rbind(
  build_worked_example("PISA Reading"),
  build_worked_example("AN Literacy")
)

# -----------------------------------------------------------------------------
# 10. VALIDATION CHECKS
# -----------------------------------------------------------------------------
checks <- character()

checks <- c(checks, sprintf(
  "2025 IIEQI = %.2f (official PISA 2025 baseline; previous reference was 67.33)",
  base$IIEQI
))
checks <- c(checks, sprintf(
  "PISA Reading CAGR 2018-2022 = %.3f%%", target_df$CAGR[target_df$Indicator=="PISA Reading"]
))
checks <- c(checks, sprintf(
  "PISA Mathematics CAGR 2018-2022 = %.3f%%", target_df$CAGR[target_df$Indicator=="PISA Mathematics"]
))
checks <- c(checks, sprintf(
  "PISA Science CAGR 2018-2022 = %.3f%%", target_df$CAGR[target_df$Indicator=="PISA Science"]
))
checks <- c(checks, sprintf(
  "2022 equity score = %.3f", check_2022$Equity_Score
))
checks <- c(checks, sprintf(
  "2025 equity score = %.3f (Urban-Rural Gap = %.2f, GPI = %.3f)",
  equity_raw$Equity_Score[equity_raw$Year==2025],
  equity_raw$UR_Gap[equity_raw$Year==2025],
  equity_raw$GPI[equity_raw$Year==2025]
))
checks <- c(checks, sprintf(
  "2009-2023 urban-rural gap convergence benchmark = %.3f%% p.a.",
  equity_2009_2023_rate
))
checks <- c(checks, sprintf(
  "Sensitivity overall mean deviation (three alternative schemes) = %.3f pts",
  mean(sensitivity$Mean_Dev)
))
checks <- c(checks, sprintf(
  "Sensitivity overall max deviation = %.3f pts",
  max(sensitivity$Max_Dev)
))
checks <- c(checks,
  "PISA 2025 baseline updated to official values released 8 September 2026 (Kemendikdasmen/OECD); historical PISA CAGR remains 2018-2022."
)
checks <- c(checks,
  sprintf("Equity_history now covers %d years (%s), read from CSV rather than hardcoded.",
          nrow(equity_raw), paste(equity_raw$Year, collapse=", "))
)


# -----------------------------------------------------------------------------
# 10b. FAILURE-MODE MAPPING (manuscript Section 3.4)
# -----------------------------------------------------------------------------
# Status rules (sequential): R1 Internal Anomaly; R2 Trend Reversal (CAGR < 0);
# R3 Trend Reversal (gap > 5); R4 Significant Acceleration; R5 Moderate
# Acceleration; else Trend Sufficient. Failure modes are assigned by rule:
#   planning-data misalignment  : R1
#   structural stagnation       : R2 (CAGR < 0)
#   aspirational overreach      : CAGR >= 0 and gap > 2 (R3 or R4)
#   none flagged                : Moderate Acceleration, Trend Sufficient
failure_mode <- function(gap, cagr, target_2029, target_2045) {
  if (target_2045 < target_2029) return("Planning-data misalignment")
  if (cagr < 0) return("Structural stagnation")
  if (gap > 2) return("Aspirational overreach")
  "None flagged"
}
target_df$Mode_2029 <- mapply(failure_mode, target_df$Gap_2029, target_df$CAGR,
                              target_df$T_2029, target_df$T_2045)
target_df$Mode_2045 <- mapply(failure_mode, target_df$Gap_2045, target_df$CAGR,
                              target_df$T_2029, target_df$T_2045)

# -----------------------------------------------------------------------------
# 12. TABLE 1 — NORMALIZATION PARAMETERS WITH OBSERVED RANGES
# -----------------------------------------------------------------------------
pisa_hist <- subset(dat, dataset == "PISA_history")
obs_range <- function(ind) {
  x <- subset(pisa_hist, indicator == ind)
  c(min = min(x$value), min_year = x$year[which.min(x$value)][1],
    max = max(x$value), max_year = x$year[which.max(x$value)][1])
}
t1 <- norm[norm$indicator != "Urban-Rural Gap", c("indicator","floor","ceiling")]
t1 <- rbind(t1, data.frame(indicator = "Urban-Rural Gap", floor = 0.50, ceiling = 15.32))
t1$obs_min <- NA; t1$obs_min_year <- NA; t1$obs_max <- NA; t1$obs_max_year <- NA
for (ind in c("PISA Reading","PISA Mathematics","PISA Science")) {
  r <- obs_range(ind); i <- t1$indicator == ind
  t1[i, c("obs_min","obs_min_year","obs_max","obs_max_year")] <- r
}
an_anchor <- subset(anchor, indicator %in% c("AN Literacy","AN Numeracy"))
for (ind in c("AN Literacy","AN Numeracy")) {
  i <- t1$indicator == ind
  t1$obs_min[i] <- anchor_df$Historical_Start[anchor_df$Indicator == ind]; t1$obs_min_year[i] <- 2021
  t1$obs_max[i] <- anchor_df$Historical_End[anchor_df$Indicator == ind];   t1$obs_max_year[i] <- 2025
}
# APK: 2015-2025 minimum reported by BPS (25.26); full series not deposited.
i <- t1$indicator == "APK Tertiary"; t1$obs_min[i] <- 25.26; t1$obs_max[i] <- 32.89; t1$obs_max_year[i] <- 2025
i <- t1$indicator == "APS 16-18"
t1$obs_min[i] <- min(equity_raw$Total_APS); t1$obs_min_year[i] <- equity_raw$Year[which.min(equity_raw$Total_APS)]
t1$obs_max[i] <- max(equity_raw$Total_APS); t1$obs_max_year[i] <- equity_raw$Year[which.max(equity_raw$Total_APS)]
i <- t1$indicator == "Urban-Rural Gap"
t1$obs_min[i] <- min(equity_raw$UR_Gap); t1$obs_min_year[i] <- equity_raw$Year[which.min(equity_raw$UR_Gap)]
t1$obs_max[i] <- max(equity_raw$UR_Gap); t1$obs_max_year[i] <- equity_raw$Year[which.max(equity_raw$UR_Gap)]
t1$floor_rule <- c(rep("Round-number floor 30-40 points below the lowest observed score (judgmental)",3),
                   rep("Round-number floor 5-6 points below the first (lowest) observation (judgmental)",2),
                   "Observed minimum over 2015-2025", "Observed minimum over 2009-2025",
                   "Policy floor (0.50 pp)")
t1$ceiling_rule <- c(rep("Highest observed score, 2000-2025",3),
                     rep("Theoretical maximum (100%)",2),
                     "Observed maximum (2025)", "Observed maximum (2025)",
                     "Widest observed gap (2009)")
t1 <- t1[match(c("PISA Reading","PISA Mathematics","PISA Science","AN Literacy","AN Numeracy",
                 "APK Tertiary","APS 16-18","Urban-Rural Gap"), t1$indicator), ]
t1$clipping_rule <- "Normalized scores are clipped to [0, 100]"
t1$clipping_rule[t1$indicator == "Urban-Rural Gap"] <- "Inverted (smaller gap scores higher); normalized scores clipped to [0, 100]"

# -----------------------------------------------------------------------------
# 13. APPENDIX B — PISA CAGR WINDOW SENSITIVITY
# -----------------------------------------------------------------------------
ph <- function(ind, yr) subset(pisa_hist, indicator == ind & year == yr)$value
winB <- list()
for (ind in c("PISA Reading","PISA Mathematics","PISA Science")) {
  row <- target_df[target_df$Indicator == ind, ]
  for (w in list(c(2018,2022), c(2022,2025), c(2018,2025))) {
    cg <- cagr(ph(ind, w[1]), ph(ind, w[2]), w[2]-w[1])
    g29 <- row$RAGR_2029 - cg; g45 <- row$RAGR_2045 - cg
    winB[[length(winB)+1]] <- data.frame(
      Indicator = ind, Window = paste0(w[1], "-", w[2]),
      Main = (w[1]==2018 && w[2]==2022), CAGR = cg,
      Gap_2029 = g29, Status_2029 = classify_status(g29, cg, row$T_2029, row$T_2045),
      Gap_2045 = g45, Status_2045 = classify_status(g45, cg, row$T_2029, row$T_2045))
  }
}
appendixB <- do.call(rbind, winB)

# -----------------------------------------------------------------------------
# 14. APPENDIX C — ROBUSTNESS
# -----------------------------------------------------------------------------
# Generic re-scoring with alternative normalization parameters.
score_ind_p <- function(x, indicator, normdf) {
  if (indicator == "GPI") return(score_indicator(x, "GPI"))
  p <- normdf[normdf$indicator == indicator, ]
  if (p$direction == "positive") pmax(0, pmin(100, (x - p$floor)/(p$ceiling - p$floor)*100))
  else pmax(0, pmin(100, (p$ceiling - x)/(p$ceiling - p$floor)*100))
}
iieqi_p <- function(row, normdf) {
  s <- function(ind) score_ind_p(row[[ind]], ind, normdf)
  d1a <- mean(c(s("PISA Reading"), s("PISA Mathematics"), s("PISA Science")))
  d1b <- mean(c(s("AN Literacy"), s("AN Numeracy")))
  d2  <- mean(c(s("APK Tertiary"), s("APS 16-18")))
  d3  <- mean(c(s("Urban-Rural Gap"), s("GPI")))
  c(D1a = d1a, D1b = d1b, D2 = d2, D3 = d3, IIEQI = mean(c(d1a, d1b, d2, d3)))
}
c1 <- list()
shift_grid <- list(list("PISA floors", "PISA", c(-20,-10,0,10,20)),
                   list("AN floors", "AN", c(-5,0,5)))
for (g in shift_grid) for (sh in g[[3]]) {
  nd <- norm
  idx <- if (g[[2]] == "PISA") grepl("^PISA", nd$indicator) else grepl("^AN", nd$indicator)
  nd$floor[idx] <- nd$floor[idx] + sh
  r22 <- iieqi_p(annual[annual$year == 2022, ], nd)
  r25 <- iieqi_p(annual[annual$year == 2025, ], nd)
  c1[[length(c1)+1]] <- data.frame(Parameter = g[[1]], Floor_shift = sh,
    D1a_2025 = r25["D1a"], D1b_2025 = r25["D1b"], IIEQI_2022 = r22["IIEQI"],
    IIEQI_2025 = r25["IIEQI"], Gain_2022_2025 = r25["IIEQI"] - r22["IIEQI"], row.names = NULL)
}
appendixC1 <- do.call(rbind, c1)

# Scenario engine with explicit parameters (reproduces Table 5 by default).
scenario_p <- function(k, years, eq_rate, gpi_rate, eq_mode = "recovery") {
  qa <- project_quality_access(years, k)
  if (eq_mode == "hold") { gap <- base$`Urban-Rural Gap`; gpi <- base$GPI }
  else {
    gap <- max(0.50, base$`Urban-Rural Gap` * (1 + eq_rate/100)^years)
    gpi <- 1.03 + (base$GPI - 1.03) * (1 - gpi_rate)^years
  }
  d3 <- mean(c(score_indicator(gap, "Urban-Rural Gap"), score_indicator(gpi, "GPI")))
  c(D1a = qa$D1a, D1b = qa$D1b, D2 = qa$D2, D3 = d3,
    IIEQI = mean(c(qa$D1a, qa$D1b, qa$D2, d3)))
}
# C2: k-grid (equity held at the Moderate rule so that only k varies)
c2 <- list()
for (k in c(0.25, 0.5, 0.75, 1.0)) for (yr in c(2029, 2045)) {
  r <- scenario_p(k, yr - 2025, equity_2009_2023_rate, 0.025)
  lit <- min(100, base$`AN Literacy` * (1 + cagr_map["AN Literacy"]/100 * k)^(yr-2025))
  num <- min(100, base$`AN Numeracy` * (1 + cagr_map["AN Numeracy"]/100 * k)^(yr-2025))
  c2[[length(c2)+1]] <- data.frame(k = k, Year = yr, AN_Literacy = lit, AN_Numeracy = num,
                                   D1b = r["D1b"], IIEQI = r["IIEQI"], row.names = NULL)
}
appendixC2 <- do.call(rbind, c2)
# C3: equity variants
gap_2014 <- equity_raw$UR_Gap[equity_raw$Year == 2014]
rate_0925 <- cagr(gap_2009, equity_raw$UR_Gap[equity_raw$Year == 2025], 2025 - 2009)
rate_1423 <- cagr(gap_2014, gap_2023, 2023 - 2014)
variants <- list(
  list("Main: 2009-2023 rate", equity_2009_2023_rate, "recovery"),
  list("2014-2023 rate", rate_1423, "recovery"),
  list("2009-2025 rate", rate_0925, "recovery"),
  list("No equity recovery (held at 2025)", 0, "hold"))
c3 <- list()
for (v in variants) for (sc in c("Moderate","Optimistic")) for (yr in c(2029, 2045)) {
  r <- scenario_p(ifelse(sc == "Moderate", 0.5, 1.0), yr - 2025, v[[2]],
                  ifelse(sc == "Moderate", 0.025, 0.05), v[[3]])
  c3[[length(c3)+1]] <- data.frame(Variant = v[[1]], Gap_rate = v[[2]], Scenario = sc, Year = yr,
                                   D3 = r["D3"], IIEQI = r["IIEQI"], row.names = NULL)
}
appendixC3 <- do.call(rbind, c3)
# C4: classification-threshold sensitivity
classify_thr <- function(gap, cagr, t29, t45, lo, hi) {
  if (t45 < t29) return("Internal Anomaly")
  if (cagr < 0) return("Trend Reversal")
  if (gap > hi) return("Trend Reversal")
  if (gap > lo) return("Significant Acceleration")
  if (gap > 0) return("Moderate Acceleration")
  "Trend Sufficient"
}
c4 <- list()
for (th in list(c(1.5,4), c(2,5), c(2.5,6))) for (i in seq_len(nrow(target_df))) {
  r <- target_df[i, ]
  c4[[length(c4)+1]] <- data.frame(Lower = th[1], Upper = th[2], Indicator = r$Indicator,
    Status_2029 = classify_thr(r$Gap_2029, r$CAGR, r$T_2029, r$T_2045, th[1], th[2]),
    Status_2045 = classify_thr(r$Gap_2045, r$CAGR, r$T_2029, r$T_2045, th[1], th[2]))
}
appendixC4 <- do.call(rbind, c4)
# C5: D2 ceiling anchored to the RPJPN tertiary target (60%)
nd <- norm; nd$ceiling[nd$indicator == "APK Tertiary"] <- 60
c5 <- data.frame(Ceiling = c("Observed 2025 maximum (main)", "APK ceiling = RPJPN target (60%)"),
  IIEQI_2022 = c(iieqi_p(annual[annual$year==2022,], norm)["IIEQI"], iieqi_p(annual[annual$year==2022,], nd)["IIEQI"]),
  IIEQI_2025 = c(iieqi_p(annual[annual$year==2025,], norm)["IIEQI"], iieqi_p(annual[annual$year==2025,], nd)["IIEQI"]),
  D2_2025 = c(iieqi_p(annual[annual$year==2025,], norm)["D2"], iieqi_p(annual[annual$year==2025,], nd)["D2"]))
c5$Gain <- c5$IIEQI_2025 - c5$IIEQI_2022
appendixC5 <- c5

checks <- c(checks, sprintf("2009-2025 urban-rural gap convergence rate = %.3f%% p.a.; 2014-2023 = %.3f%% p.a.", rate_0925, rate_1423))

# -----------------------------------------------------------------------------
# 11. EXPORT REPLICATION OUTPUTS
# -----------------------------------------------------------------------------
write.csv(annual, file.path(OUTPUT_DIR,"ikpii_annual_scores.csv"), row.names=FALSE)
write.csv(target_df, file.path(OUTPUT_DIR,"ragr_cagr_diagnostic.csv"), row.names=FALSE)
write.csv(equity_raw, file.path(OUTPUT_DIR,"equity_selected_years.csv"), row.names=FALSE)
write.csv(scenario_df, file.path(OUTPUT_DIR,"scenario_projections.csv"), row.names=FALSE)
write.csv(sensitivity, file.path(OUTPUT_DIR,"sensitivity_appendixA.csv"), row.names=FALSE)
write.csv(worked, file.path(OUTPUT_DIR,"worked_examples.csv"), row.names=FALSE)
write.csv(t1, file.path(OUTPUT_DIR,"table1_normalization_parameters.csv"), row.names=FALSE)
write.csv(appendixB, file.path(OUTPUT_DIR,"appendixB_pisa_windows.csv"), row.names=FALSE)
write.csv(appendixC1, file.path(OUTPUT_DIR,"appendixC1_floor_sensitivity.csv"), row.names=FALSE)
write.csv(appendixC2, file.path(OUTPUT_DIR,"appendixC2_k_grid.csv"), row.names=FALSE)
write.csv(appendixC3, file.path(OUTPUT_DIR,"appendixC3_equity_variants.csv"), row.names=FALSE)
write.csv(appendixC4, file.path(OUTPUT_DIR,"appendixC4_threshold_sensitivity.csv"), row.names=FALSE)
write.csv(appendixC5, file.path(OUTPUT_DIR,"appendixC5_d2_ceiling.csv"), row.names=FALSE)
writeLines(checks, file.path(OUTPUT_DIR,"validation_checks.txt"))

cat("\n=== IIEQI REPLICATION (v3, CSV-driven) ===\n")
print(annual[,c("year","D1a_PISA","D1b_AN","D2_Access","D3_Equity","IIEQI")],
      row.names=FALSE, digits=4)

cat("\n=== RAGR-CAGR DIAGNOSTIC ===\n")
print(target_df[,c("Indicator","Base_2025","T_2029","RAGR_2029",
                   "CAGR","Gap_2029","Status_2029",
                   "T_2045","RAGR_2045","Gap_2045","Status_2045")],
      row.names=FALSE, digits=4)

cat("\n=== EQUITY SELECTED YEARS (now includes 2025) ===\n")
print(equity_raw[,c("Year","Total_APS","Urban_APS","Rural_APS","UR_Gap","GPI","Equity_Score")],
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
