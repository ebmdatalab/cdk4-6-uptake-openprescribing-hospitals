from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

import quantile_plots as qp


DRUGS = [
    ("Palbociclib", "pct_palbo", "% palbociclib"),
    ("Abemaciclib", "pct_abema", "% abemaciclib"),
    ("Ribociclib", "pct_ribo", "% ribociclib"),
]


def load_alliance_shares(data_path: Path) -> pd.DataFrame:
    cdk46 = pd.read_csv(data_path, parse_dates=["Date"])
    cdk46 = cdk46[cdk46["Date"] < pd.Timestamp("2025-01-01")]

    monthly = (
        cdk46.groupby(["Date", "VTM Name", "Cancer_Alliance"], as_index=False)["Value"]
        .sum()
        .rename(columns={"Date": "year_month", "VTM Name": "vtm_name", "Value": "qty"})
    )
    wide = monthly.pivot_table(
        index=["year_month", "Cancer_Alliance"],
        columns="vtm_name",
        values="qty",
        aggfunc="sum",
    ).reset_index()

    for drug, _, _ in DRUGS:
        if drug not in wide.columns:
            wide[drug] = 0
        wide[drug] = wide[drug].fillna(0)

    wide["total"] = wide["Palbociclib"] + wide["Abemaciclib"] + wide["Ribociclib"]
    wide["pct_abema"] = wide["Abemaciclib"] / wide["total"].replace(0, np.nan)
    wide["pct_ribo"] = wide["Ribociclib"] / wide["total"].replace(0, np.nan)
    wide["pct_palbo"] = wide["Palbociclib"] / wide["total"].replace(0, np.nan)
    return wide.rename(columns={"Cancer_Alliance": "new_code"})


def write_quantile_table(quantiles_by_drug: list[tuple[str, pd.DataFrame]], output_path: Path) -> None:
    tables = []
    for drug_name, quantiles in quantiles_by_drug:
        table = quantiles.copy()
        table["Drug"] = drug_name
        tables.append(table)
    quantile_table = pd.concat(tables, ignore_index=True)
    quantile_table = quantile_table[
        ["Drug", "year_month", "pct_05", "pct_10", "pct_25", "median", "pct_75", "pct_90", "pct_95"]
    ].rename(
        columns={
            "year_month": "Month",
            "pct_05": "5th percentile",
            "pct_10": "10th percentile",
            "pct_25": "25th percentile",
            "median": "Median",
            "pct_75": "75th percentile",
            "pct_90": "90th percentile",
            "pct_95": "95th percentile",
        }
    )
    quantile_table["Month"] = quantile_table["Month"].dt.strftime("%Y-%m")
    quantile_table.to_csv(output_path, index=False)


def create_alliance_plot(data_path: Path, output_dir: Path, key_dates_path: Path) -> None:
    alliance_data = load_alliance_shares(data_path)
    qp.EVENTS = qp.load_key_dates(key_dates_path)

    quantiles_by_drug = [
        (drug_name, qp.calculate_quantiles(alliance_data, pct_col))
        for drug_name, pct_col, _ in DRUGS
    ]

    fig, axes = plt.subplots(3, 1, figsize=(8, 12), sharex=True)
    plt.subplots_adjust(left=0.12, hspace=0.35, top=0.95)

    for index, ((drug_name, pct_col, ylabel), (_, quantiles)) in enumerate(
        zip(DRUGS, quantiles_by_drug)
    ):
        qp.plot_quantiles(
            axes[index],
            quantiles,
            alliance_data,
            pct_col,
            ylabel,
            add_legend_labels=(index == 0),
            show_event_labels=True,
            is_top_subplot=(index == 0),
            drug_name=drug_name,
        )
        axes[index].text(
            -0.08,
            0.98,
            chr(ord("a") + index),
            transform=axes[index].transAxes,
            fontsize=14,
            fontweight="bold",
            va="top",
            ha="right",
        )

    axes[2].set_xlabel("Month", fontsize=12)

    handles, labels = axes[0].get_legend_handles_labels()
    filtered = [(handle, label) for handle, label in zip(handles, labels) if label]
    fig.legend(
        [handle for handle, _ in filtered],
        [label for _, label in filtered],
        loc="lower center",
        ncol=2,
        frameon=False,
        fontsize=9,
        bbox_to_anchor=(0.5, 0.02),
    )

    plt.tight_layout(rect=[0, 0.06, 1, 1])

    output_dir.mkdir(parents=True, exist_ok=True)
    stem = output_dir / "QuantilePlots_cancer_alliances"
    fig.savefig(stem.with_suffix(".svg"), format="svg", dpi=300, bbox_inches="tight")
    fig.savefig(stem.with_suffix(".png"), format="png", dpi=200, bbox_inches="tight")
    write_quantile_table(quantiles_by_drug, output_dir / "QuantilePlots_cancer_alliances_data.csv")
    plt.close()

    print(f"Plots saved to {output_dir}")


if __name__ == "__main__":
    create_alliance_plot(
        Path("data/oph_data_11_25_processed.csv"),
        Path("outputs"),
        Path("data/KeyDates.csv"),
    )
