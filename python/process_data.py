import pandas as pd
from pathlib import Path
from typing import List

DATA_DIR = Path("data")

raw_data_path = DATA_DIR / "oph_data_11_25.csv"
eric_data_path = DATA_DIR / "eric_trust_categories.csv"

def process_eric_data(eric_data_path: Path) -> pd.DataFrame:
    """
    Process the ERIC data
    """

    restricted_categories = [
        "Mental Health And Learning Disability",
        "Community",
    ]

    eric_data = pd.read_csv(eric_data_path)
    eric_data_restricted = eric_data[~eric_data["trust_type"].isin(restricted_categories)]
    return eric_data_restricted["trust_code"].unique()

def filter_data_by_date(data: pd.DataFrame, start_date: str, end_date: str) -> pd.DataFrame:
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


def restrict_by_organisation(data: pd.DataFrame, organisations: List[str]) -> pd.DataFrame:
    """
    Restrict the data by organisation

    Args:
        data: The data to restrict
        organisations: List of trusts to restrict the data to

    Returns:
        The restricted data
    """

    return data[data["Trust Code"].isin(organisations)]


def load_and_process_data(raw_data_path: Path, eric_data_path: Path) -> pd.DataFrame:
    data = pd.read_csv(raw_data_path)
    trust_codes = process_eric_data(eric_data_path)
    data_filtered_by_date = filter_data_by_date(data, "2019-01-01", "2024-12-01")
    data_restricted_by_organisation = restrict_by_organisation(data_filtered_by_date, trust_codes)
    return data_restricted_by_organisation

if __name__ == "__main__":
    data = load_and_process_data(raw_data_path, eric_data_path)
    data.to_csv(DATA_DIR / "oph_data_11_25_processed.csv", index=False)