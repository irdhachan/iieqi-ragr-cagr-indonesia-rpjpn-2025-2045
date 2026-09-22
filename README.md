# iieqi-ragr-cagr-indonesia-rpjpn-2025-2045

Replication materials for:

> "Long-Term Education Planning Under Data Uncertainty: A RAGR-CAGR Diagnostic Framework for Indonesia's National Long-Term Development Plan (RPJPN) 2025-2045" (manuscript under revision, *Cogent Education*).

The Indonesian Inclusive Education Quality Index (IIEQI) and the RAGR-CAGR diagnostic are applied to a single-country case (Indonesia). The scenario projections are conditional planning illustrations, not statistical forecasts.

## Repository structure

| Path | Content |
|---|---|
| `data/ikpii_master_data_FINAL_PISA2025_v2.csv` | Compiled dataset (one long-format CSV): annual IIEQI inputs 2022-2025, CAGR anchors, planning targets, equity history 2009-2025 (total/urban/rural/male/female APS), PISA history 2000-2025 |
| `code/ikpii_replication_v3.R` | R script that reproduces every table and appendix from the CSV (run from the repository root: `Rscript code/ikpii_replication_v3.R`) |
| `outputs/` | Generated tables (see below) |

## Outputs and manuscript cross-reference

| Output file | Manuscript element |
|---|---|
| `table1_normalization_parameters.csv` | Table 1 (observed minimum/maximum, floor, ceiling, derivation rules, clipping) |
| `ikpii_annual_scores.csv` | Table 2 |
| `equity_selected_years.csv` | Table 3 (includes male/female APS, component scores) |
| `ragr_cagr_diagnostic.csv` | Tables 4A/4B (and failure-mode mapping, Section 3.4) |
| `worked_examples.csv` | Box 2 |
| `scenario_projections.csv` | Table 5 |
| `sensitivity_appendixA.csv` | Table A1 (Max./Mean deviation of the three alternative weighting schemes from Equal weights) |
| `appendixB_pisa_windows.csv` | Table B1 |
| `appendixC1_floor_sensitivity.csv` ... `appendixC5_d2_ceiling.csv` | Tables C1-C5 |
| `validation_checks.txt` | Consistency checks |

## Methodological notes

- PISA 2025 baseline (released 8 September 2026): Reading 365, Mathematics 364, Science 389.
- Historical PISA CAGR uses the 2018-2022 window, which was the specification fixed in the original analysis before the PISA 2025 release. PISA 2025 enters as the 2025 baseline, not in the trajectory estimate. Alternative windows are reported in Appendix B.
- AN and APK CAGRs use 2021-2025 (four years); these are short windows and are indicative only.
- Floors for PISA and AN are round-number judgments below the observed minimum (see `table1_normalization_parameters.csv`); floor sensitivity is in Appendix C1.
- The APK 2015-2025 series and the APS series for years other than those in `Equity_history` are not deposited; the APK minimum (25.26) used for its floor is taken from the BPS series cited in the manuscript.
- Scenario coefficient k (0, 0.5, 1) is an analyst-specified planning parameter, not an estimated coefficient.

## Citation

Please cite the associated manuscript and the archived version of this repository (Zenodo DOI: 10.5281/zenodo.22720766; cite the version DOI of the release matching the final revision).

## Licence

See `LICENSE`.
