# Trends and variation in the issuing of CDK4/6 inhibitors within NHS Trusts in England

This project examines the uptake of CDK4/6 inhibitors (palbociclib, ribociclib, and abemaciclib) within NHS Trusts in England, including the frequency of dose reductions, and assesses variation in their use both within NHS trusts and across cancer alliances in response to recent approvals and emerging evidence.

## Data Sources

This project makes use of the **Secondary Care Medicines Dataset (SCMD)** hosted by the NHS Business Services Authority. The SCMD contains processed pharmacy stock data from all NHS trusts in England, including cancer therapies. The data is accessed through the publicly available [OpenPrescribing Hospitals platform](https://hospitals.openprescribing.net/).

## Project Structure

This project uses both Python and R for data analysis.

- `data/`: Contains processed datasets including hospital-level CDK4/6 inhibitor data, cancer alliance boundaries, and key dates
- `r/`: R scripts and notebooks for data analysis, visualization, and report generation
  - `r/functions/`: Utility functions (e.g., ODS API access)
  - `r/r-notebook/`: R Markdown notebooks for various analyses
  - `r/outputs/`: Generated figures and tables
- `python/`: Python scripts and notebooks for data analysis
- `pyproject.toml`: Python project configuration and dependencies


## Usage

 The main analyses are conducted in R Markdown notebooks located in `r/r-notebook/`, which generate visualizations and reports examining:

- Temporal trends in CDK4/6 inhibitor usage
- Dose reduction patterns
- Geographic variation across cancer alliances
- Quantile plots showing distribution of usage
- Event timelines related to approvals and evidence

## Outputs

Generated outputs include:
- Timeline figures showing uptake over time
- Map visualizations of geographic variation
- Quantile plots showing distribution patterns
- Dose reduction analyses
- Statistical tables (Table 2)

All outputs are saved in `r/outputs/` in both PNG and SVG formats.

## References

This research contributes to understanding variation in the adoption of novel cancer treatments within the NHS and supports efforts to ensure rapid, widespread, and equitable uptake of effective therapies.
