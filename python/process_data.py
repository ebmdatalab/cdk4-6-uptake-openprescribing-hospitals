import pandas as pd
from pathlib import Path

DATA_DIR = Path("data")

raw_data_path = DATA_DIR / "oph_data_11_25.csv"
trust_information_path = DATA_DIR / "trust-information.csv"

RESTRICTED_TRUST_TYPES = [
    "Mental Health And Learning Disability",
    "Community",
]

def load_ods_ca_mapping(trust_information_path: Path) -> pd.DataFrame:
    """
    Load the ODS to Cancer Alliance mapping from trust-information,
    keeping only trusts with a cancer alliance assignment.
    """
    trust_info = pd.read_csv(trust_information_path)
    mapping = trust_info[
        trust_info["Cancer Alliance"].notna()
        & (trust_info["Cancer Alliance"] != "")
    ].copy()

    mapping["cancer_alliance_name"] = mapping["Cancer Alliance"]

    return mapping[["ODS code", "cancer_alliance_name"]].rename(
        columns={"ODS code": "ods_code"}
    )


def filter_data_by_date(
    data: pd.DataFrame, start_date: str, end_date: str
) -> pd.DataFrame:
    """
    Filter the data by date (inclusive)

    Args:
        data: The data to filter
        start_date: The start date to filter the data by
        end_date: The end date to filter the data by

    Returns:
        The filtered data
    """
    return data[(data["Date"] >= start_date) & (data["Date"] <= end_date)]


def get_restricted_trust_codes(trust_information_path: Path) -> set:
    """Return trust codes for mental health and community trusts."""
    trust_info = pd.read_csv(trust_information_path)
    return set(
        trust_info[trust_info["Trust type"].isin(RESTRICTED_TRUST_TYPES)][
            "ODS code"
        ].unique()
    )


def load_and_process_data(
    raw_data_path: Path, trust_information_path: Path
) -> pd.DataFrame:
    data = pd.read_csv(raw_data_path)
    ods_ca = load_ods_ca_mapping(trust_information_path)

    data = data.merge(ods_ca, left_on="Trust Code", right_on="ods_code", how="inner")
    data = data.drop(columns=["ods_code"])
    data = data.rename(columns={"cancer_alliance_name": "Cancer_Alliance"})

    restricted = get_restricted_trust_codes(trust_information_path)
    data = data[~data["Trust Code"].isin(restricted)]

    data = filter_data_by_date(data, "2019-01-01", "2024-12-01")
    return data


if __name__ == "__main__":
    raw_data = pd.read_csv(raw_data_path)
    ods_ca = load_ods_ca_mapping(trust_information_path)
    restricted = get_restricted_trust_codes(trust_information_path)

    original_trust_codes = set(raw_data["Trust Code"].unique())
    mapped_trust_codes = set(ods_ca["ods_code"].unique())
    unmapped_trusts = original_trust_codes - mapped_trust_codes
    mapped_and_restricted = (original_trust_codes & mapped_trust_codes) & restricted

    trust_counts = pd.DataFrame(
        {
            "Metric": [
                "Trusts in original data",
                "Trusts mapped to a Cancer Alliance",
                "Trusts in original data not mapped",
                "Trusts mapped but excluded (MH/Community)",
            ],
            "Count": [
                len(original_trust_codes),
                len(original_trust_codes & mapped_trust_codes),
                len(unmapped_trusts),
                len(mapped_and_restricted),
            ],
        }
    )
    trust_counts.to_csv(DATA_DIR / "trust_counts.csv", index=False)

    excluded_data = raw_data[
        raw_data["Trust Code"].isin(unmapped_trusts | mapped_and_restricted)
    ]
    all_vtms = raw_data["VTM Name"].unique()
    excluded_summary = (
        excluded_data.pivot_table(
            index=["Trust Code", "Trust Name"],
            columns="VTM Name",
            values="Value",
            aggfunc="sum",
            fill_value=0,
        )
        .reindex(columns=all_vtms, fill_value=0)
        .reset_index()
    )
    excluded_summary.to_csv(DATA_DIR / "excluded_trusts_ddd.csv", index=False)

    processed = load_and_process_data(
        raw_data_path, trust_information_path
    )
    processed.to_csv(DATA_DIR / "oph_data_11_25_processed.csv", index=False)