from pathlib import Path

import pandas as pd


DATA_PATH = Path("data") / "oph_data_11_25_processed.csv"
OUTPUT_PATH = Path("outputs") / "trust_cancer_alliance_cdk46_ddds_2019_2024.csv"
START_DATE = "2019-01-01"
END_DATE = "2024-12-01"


def main() -> None:
    data = pd.read_csv(DATA_PATH, parse_dates=["Date"])
    data = data[
        (data["Date"] >= pd.Timestamp(START_DATE))
        & (data["Date"] <= pd.Timestamp(END_DATE))
    ].copy()

    trust_table = (
        data.groupby(["Cancer_Alliance", "Trust Code", "Trust Name"], as_index=False)
        .agg(
            **{
                "Total CDK4/6 DDDs issued": (
                    "Value",
                    "sum",
                )
            }
        )
        .sort_values(
            ["Cancer_Alliance", "Total CDK4/6 DDDs issued", "Trust Name"],
            ascending=[True, False, True],
        )
        .rename(columns={"Cancer_Alliance": "Cancer Alliance"})
    )

    trust_table.to_csv(OUTPUT_PATH, index=False)


if __name__ == "__main__":
    main()
