# Variation in uptake and dose reduction of CDK4/6 inhibitors for the treatment of breast cancer in England, 2019–2024: a descriptive observational study using OpenPrescribing Hospitals

Analysis code and outputs for a descriptive observational study of CDK4/6 inhibitor (palbociclib, ribociclib, and abemaciclib) uptake and dose reduction in NHS trusts in England, using OpenPrescribing Hospitals.

**Preprint:** [medRxiv](https://doi.org/10.64898/2026.08.05.26359678)

## Data sources

This project uses the **Secondary Care Medicines Dataset (SCMD)** from the NHS Business Services Authority, accessed via [OpenPrescribing Hospitals](https://hospitals.openprescribing.net/).

## Project structure

- `data/` — processed hospital CDK4/6 data, trust/cancer alliance lookups, and key dates
- `analysis/` — Python and R analysis scripts that produce the paper figures and tables
- `outputs/` — generated figures (PNG/SVG) and tables (CSV)
- `pyproject.toml` / `uv.lock` — Python dependencies (managed with [uv](https://github.com/astral-sh/uv))
- `renv.lock` — R dependencies (managed with [renv](https://rstudio.github.io/renv/))

## Usage

Install Python dependencies with uv:

```bash
uv sync
```

Install R dependencies with renv (from the project root in R):

```r
renv::restore()
```

Or open `op-hospitals-cdk46.Rproj` in RStudio, which activates renv via `.Rprofile`, then run `renv::restore()`.

Process the raw OpenPrescribing Hospitals extract:

```bash
uv run python analysis/process_data.py
```

This writes:

- `data/oph_data_11_25_processed.csv` — analysis-ready extract
- `outputs/excluded_trusts_ddd.csv` — DDDs for trusts excluded from analysis
- `outputs/trust_counts.csv` — counts of trusts mapped / unmapped / excluded

Main figure and table scripts:

| Script | Outputs |
|--------|---------|
| `analysis/Time_Line_Graph.R` | `outputs/TimeLineFigure.png`, `.svg` |
| `analysis/TimeLine_Split.R` | `outputs/EventTimeLineSplit.png`, `.svg` |
| `analysis/Dose_Reductions.R` | `outputs/DoseReductions.png`, `.svg` |
| `analysis/quantile_plots.py` | `outputs/QuantilePlots_combined.png`, `.svg`; `outputs/QuantilePlots_data.csv` |
| `analysis/Stacked_Bar_Cancer_Alliances.R` | `outputs/Cancer_Alliance_Stacked_Bar.png`, `.svg`; `outputs/Cancer_Alliance_Percentages_2021.csv`; `outputs/Cancer_Alliance_Percentages_2024.csv`; `outputs/Cancer_Alliance_Percentages_Change_2021_2024.csv` |
| `analysis/Create_Table1.R` | `outputs/Table1.csv` |
| `analysis/trust_cancer_alliance_table.py` | `outputs/trust_cancer_alliance_cdk46_ddds_2019_2024.csv` |

## Licence

MIT — see [LICENCE.txt](LICENCE.txt).
