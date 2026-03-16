<p align="center">
  <strong>Trends, inequalities, and the impact of Rede Cegonha and COVID-19</strong><br>
  <em>A national interrupted time-series analysis</em>
</p>

<p align="center">
  <a href="https://datasus.saude.gov.br/"><img src="https://img.shields.io/badge/Data-DATASUS-blue?style=flat-square" alt="DATASUS"></a>
  <a href="https://svs.aids.gov.br/daent/centrais-de-conteudos/paineis-de-monitoramento/mortalidade/materna/"><img src="https://img.shields.io/badge/Panel-Maternal%20Mortality-red?style=flat-square" alt="Painel MM"></a>
  <a href="https://www.who.int/publications/i/item/9789240068759"><img src="https://img.shields.io/badge/WHO-MMR%20Estimates%202023-green?style=flat-square" alt="WHO"></a>
  <a href="https://www.r-project.org/"><img src="https://img.shields.io/badge/Made%20with-R%204.5-276DC3?style=flat-square&logo=r" alt="R"></a>
  <a href="https://github.com/Audency/maternal-mortality-brazil-2000-2024/blob/main/LICENSE"><img src="https://img.shields.io/badge/License-MIT-yellow?style=flat-square" alt="License"></a>
</p>

<p align="center">
  <img src="resultados/figuras_publicacao/figure1.png" width="700" alt="Maternal Mortality Trends in Brazil, 2000–2023">
</p>

<h1 align="center">Maternal Mortality in Brazil, 2000–2024</h1>

---

### Quick Links

| Resource | Link |
|----------|------|
| DATASUS — Mortality (SIM) | [datasus.saude.gov.br](https://datasus.saude.gov.br/) |
| DATASUS — Livebirths (SINASC) | [datasus.saude.gov.br](https://datasus.saude.gov.br/) |
| Maternal Mortality Panel (MS/SVS) | [svs.aids.gov.br](https://svs.aids.gov.br/daent/centrais-de-conteudos/paineis-de-monitoramento/mortalidade/materna/) |
| WHO Maternal Mortality Estimates | [who.int](https://www.who.int/publications/i/item/9789240068759) |
| GBD Maternal Mortality | [healthdata.org](https://www.healthdata.org/results/gbd_summaries/2021/maternal-disorders-level-2-cause) |
| SDG 3.1 Target | [sdgs.un.org](https://sdgs.un.org/goals/goal3) |

---

## Overview

This repository contains the full analytical pipeline, publication-ready figures, and manuscript for a comprehensive study of maternal mortality trends in Brazil (2000–2024). Using national vital registration data from SIM and SINASC, we:

- Quantify temporal trends using **Prais-Winsten regression** (APC with 95% CI)
- Assess the impact of the **Rede Cegonha** programme (2011) and **COVID-19** (2020–2021) via **interrupted time-series (ITS)** analysis
- Examine inequalities by region, state, race/ethnicity, age, and education
- Analyse cause-specific maternal mortality and respiratory diseases (COVID-19, SARS, pneumonia)
- Calculate **Years of Potential Life Lost (YPLL)**
- Generate **5-year ARIMA forecasts** (2024–2028)

## Key Findings

<p align="center">
  <img src="resultados/figuras_publicacao/figure8.png" width="650" alt="ITS Model">
</p>

| Period | APC (95% CI) | Trend |
|--------|-------------|-------|
| 2000–2010 (pre-Rede Cegonha) | **−2.94%** (−3.13; −2.74) | Declining |
| 2011–2019 (post-Rede Cegonha) | **+0.70%** (0.06; 1.34) | Stagnation |
| 2020–2021 (COVID-19) | ITS β = **+0.606**, p < 0.001 | Sharp increase |

**Key inequalities:**
- Indigenous women: **2.5x** higher MMR than White women
- Black women: **1.9x** higher MMR than White women
- North region: **1.35x** the national average
- No education: **4x** MMR compared to university-educated women

## Selected Figures

<table>
<tr>
<td><img src="resultados/figuras_publicacao/figure2.png" width="400" alt="By Region"></td>
<td><img src="resultados/figuras_publicacao/figure3.png" width="400" alt="By Race"></td>
</tr>
<tr>
<td align="center"><em>Figure 2. MMR by region</em></td>
<td align="center"><em>Figure 3. MMR by race/ethnicity</em></td>
</tr>
<tr>
<td><img src="resultados/figuras_publicacao/figure5.png" width="400" alt="Causes"></td>
<td><img src="resultados/figuras_publicacao/figure9.png" width="400" alt="Forecast"></td>
</tr>
<tr>
<td align="center"><em>Figure 5. Causes of maternal death</em></td>
<td align="center"><em>Figure 9. 5-year ARIMA forecast</em></td>
</tr>
</table>

## State-Level Heatmap

<p align="center">
  <img src="resultados/figuras_publicacao/figure6.png" width="700" alt="Heatmap UF">
  <br><em>Figure 6. MMR by federative unit and year — clear North-South gradient and COVID-19 spike visible across all states</em>
</p>

## Years of Potential Life Lost (YPLL)

<table>
<tr>
<td><img src="resultados/figuras_publicacao/figure10.png" width="400" alt="YPLL"></td>
<td><img src="resultados/figuras_publicacao/figure11.png" width="400" alt="Respiratory"></td>
</tr>
<tr>
<td align="center"><em>Figure 10. Total YPLL — 76% surge in 2021</em></td>
<td align="center"><em>Figure 11. Respiratory diseases in maternal deaths</em></td>
</tr>
</table>

> **144,000 years of life lost** in 2021 alone (vs. 82,000 in 2019) — each maternal death represents ~51 years of potential life lost, reflecting the young age of victims (mean: 30 years).

## Interrupted Time-Series Results

| Parameter | Coefficient (β) | p-value | Interpretation |
|-----------|:---------------:|:-------:|----------------|
| Pre-intervention trend | −0.030 | <0.001 | Declining MMR before any intervention |
| **Rede Cegonha (level)** | **−0.060** | **0.002** | Immediate 6% reduction in MMR |
| Rede Cegonha (slope) | +0.039 | <0.001 | Decline slowed post-implementation |
| **COVID-19 (2020–2021)** | **+0.606** | **<0.001** | **83% increase** in MMR during pandemic |
| Post-COVID (level) | −0.323 | 0.202 | Recovery not yet statistically clear |

## Repository Structure

```
.
├── 01_download_datasus.R           # Download SIM/SINASC microdata (2000–2024)
├── 02_analise_dados_reais.R        # Analysis with real data + age standardisation
├── 03_figuras_publicacao.R         # Publication figures (Lancet style, 600 dpi)
├── 04_covariaveis_apvp.R          # Detailed covariates + YPLL analysis
├── analise_mortalidade_materna.R   # Full analysis (standalone, simulated data)
├── manuscript_lancet.docx          # Complete manuscript (Word)
├── manuscript_lancet.docx.md       # Manuscript source (Markdown)
├── resultados/
│   ├── figuras_publicacao/         # 12 figures (PNG 600dpi + PDF + TIFF)
│   ├── tabelas/                    # 10 tables (HTML + Excel)
│   └── suplementar/                # Supplementary material
└── dados_datasus/                  # Downloaded data (git-ignored)
```

## How to Reproduce

### 1. Install dependencies

```r
install.packages(c(
  "tidyverse", "lmtest", "sandwich", "nlme", "forecast",
  "broom", "gt", "gtsummary", "patchwork", "scales",
  "viridis", "RColorBrewer", "writexl", "microdatasus"
))
```

### 2. Download data from DATASUS

```r
Rscript 01_download_datasus.R
```

Downloads SIM (deaths) and SINASC (livebirths) microdata for all 27 states, 2000–2024. Memory-optimised: processes and aggregates year by year.

### 3. Run analyses

```r
Rscript 02_analise_dados_reais.R    # Main analysis + age standardisation
Rscript 03_figuras_publicacao.R     # Publication figures
Rscript 04_covariaveis_apvp.R      # Covariates + YPLL
```

### Standalone (no download needed)

```r
Rscript analise_mortalidade_materna.R   # Uses realistic simulated data
```

## Methods

| Method | Purpose | R Package |
|--------|---------|-----------|
| Prais-Winsten (GLS/AR1) | Temporal trend (APC) | `nlme` |
| Interrupted Time Series | Rede Cegonha + COVID-19 impact | `lmtest`, `sandwich` |
| Newey-West SE | Robust inference | `sandwich` |
| Direct standardisation | Age-adjusted MMR | Base R |
| ARIMA | 5-year exploratory forecast | `forecast` |
| YPLL | Premature mortality burden | Base R |

## Data Sources

| Source | Description | Access |
|--------|-------------|--------|
| **SIM** | Mortality Information System | [DATASUS](https://datasus.saude.gov.br/) |
| **SINASC** | Livebirth Information System | [DATASUS](https://datasus.saude.gov.br/) |
| **ICD-10** | O00–O95, O98–O99 (official MMR) | WHO |
| **CIDs resp.** | U07.1, B34.2 (COVID-19), J80 (SARS), J18.9/J15.9 (pneumonia) | WHO |

## Outputs

- **12 publication figures** (PNG 600 dpi + PDF + TIFF) — Lancet style
- **10 tables** (HTML + Excel) — descriptive, Prais-Winsten, ITS, crude/adjusted models, covariates, YPLL
- **Complete manuscript** in English (Lancet format, 38 references)
- **Supplementary material** — state-level tables, diagnostic plots, raw data

## Citation
Audencio Victor. Trends, inequalities, and the impact of Rede Cegonha and COVID-19 on maternal mortality in Brazil, 2000–2024: a national interrupted time-series analysis. *[Journal]*. [Year].

## License

This project is licensed under the MIT License.
