# Trends and Determinants of Maternal Mortality in Brazil, 2000–2024

**Interrupted time-series analysis of maternal mortality using national vital registration data**

## Overview

This repository contains the analytical code, publication-ready figures, and manuscript for a comprehensive study of maternal mortality trends in Brazil (2000–2024). The study uses data from the Brazilian Mortality Information System (SIM) and the Livebirth Information System (SINASC) to:

- Quantify temporal trends using Prais-Winsten regression (APC with 95% CI)
- Assess the impact of the Rede Cegonha programme (2011) and COVID-19 (2020–2021) via interrupted time-series (ITS) analysis
- Examine inequalities by region, state, race/ethnicity, age, and education
- Analyse cause-specific maternal mortality over time
- Generate exploratory ARIMA forecasts

## Repository Structure

```
.
├── 01_download_datasus.R           # Download SIM/SINASC microdata (2000–2024)
├── 02_analise_dados_reais.R        # Analysis with real data + age standardisation
├── 03_figuras_publicacao.R         # Publication-quality figures (Lancet style)
├── analise_mortalidade_materna.R   # Full analysis with simulated data (standalone)
├── manuscript_lancet.docx          # Complete manuscript (Word)
├── manuscript_lancet.docx.md       # Manuscript source (Markdown)
├── resultados/
│   ├── figuras/                    # Figures (exploratory, with titles)
│   ├── figuras_publicacao/         # Figures (publication, no grids/titles, 600 dpi)
│   ├── tabelas/                    # Tables (HTML + Excel)
│   └── suplementar/                # Supplementary material
└── dados_datasus/                  # Downloaded DATASUS data (not tracked by git)
```

## How to Reproduce

### 1. Download data from DATASUS

```r
Rscript 01_download_datasus.R
```

This downloads SIM (deaths) and SINASC (livebirths) microdata for all 27 Brazilian states, 2000–2024. Requires the `microdatasus` R package and an internet connection. Downloads are saved as `.rds` files in `dados_datasus/`.

### 2. Run analysis with real data

```r
Rscript 02_analise_dados_reais.R
```

Performs all analyses (descriptive tables, Prais-Winsten, ITS, crude/adjusted models, ARIMA forecasts) using real DATASUS data. Includes age standardisation and validation against published WHO/GBD/Ministry of Health estimates.

### 3. Generate publication figures

```r
Rscript 03_figuras_publicacao.R
```

Produces 9 figures in PNG (600 dpi), PDF, and TIFF formats, styled for The Lancet (no gridlines, no titles, clean axes).

### Standalone (simulated data)

```r
Rscript analise_mortalidade_materna.R
```

Runs the full analysis with realistic simulated data based on published literature values. Useful for testing the pipeline without downloading DATASUS data.

## R Dependencies

```r
install.packages(c(
  "tidyverse", "lmtest", "sandwich", "nlme", "forecast",
  "broom", "gt", "gtsummary", "patchwork", "scales",
  "viridis", "RColorBrewer", "writexl", "microdatasus"
))
```

## Key Findings (Simulated Data)

| Period | APC (95% CI) | Trend |
|--------|-------------|-------|
| 2000–2010 (pre-Rede Cegonha) | −2.94% (−3.13; −2.74) | Declining |
| 2011–2019 (post-Rede Cegonha) | +0.70% (0.06; 1.34) | Stagnation |
| 2020–2021 (COVID-19) | ITS β = +0.606, p < 0.001 | Sharp increase |

## Data Sources

- **SIM** (Sistema de Informações sobre Mortalidade): [DATASUS](https://datasus.saude.gov.br/)
- **SINASC** (Sistema de Informações sobre Nascidos Vivos): [DATASUS](https://datasus.saude.gov.br/)
- ICD-10 codes O00–O95, O98–O99 (official MMR definition)

## Citation

> [Authors]. Trends, determinants, and the impact of the COVID-19 pandemic on maternal mortality in Brazil, 2000–2024: an interrupted time-series analysis. [Journal]. [Year].

## License

This project is licensed under the MIT License.
