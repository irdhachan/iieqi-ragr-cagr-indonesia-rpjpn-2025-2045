# iieqi-ragr-cagr-indonesia-rpjpn-2025-2045
Replication materials for the RAGR-CAGR diagnostic framework and Indonesian Inclusive Education Quality Index (IIEQI) applied to Indonesia's RPJPN 2025-2045.
# Replication Materials for the RAGR-CAGR Diagnostic Framework

This repository contains the replication materials for:

"Long-Term Education Planning Under Data Uncertainty:
A RAGR-CAGR Diagnostic Framework for Indonesia's National
Long-Term Development Plan (RPJPN) 2025-2045"

## Contents

### Data
The `data/` folder contains the compiled dataset used to construct
the Indonesian Inclusive Education Quality Index (IIEQI) and
calculate the RAGR-CAGR diagnostics.

### Code
The `code/` folder contains the R script used to reproduce:

- IIEQI annual scores
- RAGR-CAGR diagnostics
- equity calculations
- scenario projections
- sensitivity analysis
- worked numerical examples
- validation checks

### Output
The `output/` folder contains the generated replication tables.

## Data Sources

The dataset is compiled from publicly available:

- OECD PISA data
- Rapor Pendidikan / Asesmen Nasional
- BPS education statistics
- Susenas
- RPJPN 2025-2045
- RPJMN 2025-2029
- Kemendikdasmen strategic planning documents

## Methodological Notes

PISA 2025 values used in the IIEQI baseline are:

- Reading: 365
- Mathematics: 364
- Science: 389

The historical PISA CAGR is calculated using the 2018-2022
assessment cycles only.

AN and APK CAGR calculations use the 2021-2025 period.

The IIEQI uses a locked normalization baseline and equal weighting
across four dimensions.

The scenario projections are diagnostic counterfactuals rather
than statistical forecasts.

## Reproducibility

To reproduce the analysis:

1. Download the dataset from `data/`.
2. Open the R script in `code/`.
3. Run the R script from the repository root.
4. Run the script.
5. Replication outputs will be generated in `output/`.

## Citation

Please cite the associated manuscript when using these materials.
