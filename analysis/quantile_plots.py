import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from matplotlib.ticker import NullFormatter
from pathlib import Path

COLOURS = {
    "Palbociclib": "#1f5189",
    "Abemaciclib": "#c50084",
    "Ribociclib": "#00b2a9",
    "Palbociclib_line": "#2a6ebb",
    "Abemaciclib_line": "#ff83d6",
    "Ribociclib_line": "#70fff8",
    "High_line": "#5e3baa",
    "Low_line": "#CD202C"
}

def load_key_dates(data_path: Path) -> pd.DataFrame:
    """Load and prepare key dates from CSV file."""
    key_dates = pd.read_csv(data_path)


    key_dates['date'] = key_dates['date'].astype(str).str.strip()
    key_dates['year_month'] = pd.to_datetime(key_dates['date'], format='%d/%m/%Y', errors='coerce')
    # Remove rows with invalid dates
    key_dates = key_dates[key_dates['year_month'].notna()].copy()
    key_dates['drug'] = key_dates['VTM']
    # Map text_pos to position (left/right, ignore 'above' for now)
    key_dates['position'] = key_dates['text_pos'].str.strip().map({'left': 'left', 'right': 'right', 'above': 'right'})
    # Use event_label if available, otherwise event
    if ' event_label' in key_dates.columns:
        key_dates['label'] = key_dates[' event_label']
    elif 'event_label' in key_dates.columns:
        key_dates['label'] = key_dates['event_label']
    else:
        key_dates['label'] = key_dates['event']
    # Replace <br/> with newlines for multi-line text
    key_dates['label'] = key_dates['label'].astype(str).str.replace('<br/>', '\n', regex=False)
    # Remove HR from labels (TimeLineFigure and QuantilePlots don't show HR)
    key_dates['label'] = key_dates['label'].str.replace(r'HR [0-9.]+ \([0-9.-]+\)\n?', '', regex=True)
    # Filter: date >= 2019 only (include all events)
    min_date = pd.to_datetime("2019-01-01")
    key_dates = key_dates[key_dates['year_month'] >= min_date].copy()
    # Add stagger based on order within each drug
    key_dates = key_dates.sort_values(['drug', 'year_month'])
    key_dates['stagger'] = key_dates.groupby('drug').cumcount()
    return key_dates[['year_month', 'drug', 'position', 'label', 'stagger']]


EVENTS = pd.DataFrame()

# Trusts to highlight
HIGHLIGHT_TRUSTS = {
    "Abemaciclib": {
        "RCB": {"colour": "#008000"},   # Dark green
        "RXP": {"colour": "#FF6600"}   # Bright orange
    },
    "Palbociclib": {
        "RAJ": {"colour": "#008000"},
        "R1H": {"colour": "#FF6600"}
    },
    "Ribociclib": {
        "RXP": {"colour": "#008000"},
        "RCB": {"colour": "#FF6600"}
    }
}


def load_and_prepare_data(data_path: Path) -> pd.DataFrame:
    """Load and prepare the trust data."""
   
    cdk46_data = pd.read_csv(data_path)
    cdk46_data['Date'] = pd.to_datetime(cdk46_data['Date'])
    
    # Filter dates before 2025
    cdk46_data = cdk46_data[cdk46_data['Date'] < pd.to_datetime("2025-01-01")]
    
    trust_data = (
        cdk46_data
        .groupby(['Date', 'VTM Name', 'Trust Code'])
        .agg({'Value': 'sum'})
        .reset_index()
        .rename(columns={'Date': 'year_month', 'VTM Name': 'vtm_name', 'Value': 'qty'})
    )
    
    trust_data_wide = trust_data.pivot_table(
        index=['year_month', 'Trust Code'],
        columns='vtm_name',
        values='qty',
        aggfunc='sum'
    ).reset_index()
    
    for drug in ['Palbociclib', 'Abemaciclib', 'Ribociclib']:
        if drug not in trust_data_wide.columns:
            trust_data_wide[drug] = 0
        trust_data_wide[drug] = trust_data_wide[drug].fillna(0)
    
    trust_data_wide['total'] = (
        trust_data_wide['Palbociclib'] + 
        trust_data_wide['Abemaciclib'] + 
        trust_data_wide['Ribociclib']
    )
    trust_data_wide['pct_abema'] = trust_data_wide['Abemaciclib'] / trust_data_wide['total'].replace(0, np.nan)
    trust_data_wide['pct_ribo'] = trust_data_wide['Ribociclib'] / trust_data_wide['total'].replace(0, np.nan)
    trust_data_wide['pct_palbo'] = trust_data_wide['Palbociclib'] / trust_data_wide['total'].replace(0, np.nan)
    
    return trust_data_wide.rename(columns={'Trust Code': 'new_code'})


def calculate_quantiles(data: pd.DataFrame, pct_col: str) -> pd.DataFrame:
    """Calculate quantiles for a percentage column."""
    quantiles = (
        data.groupby('year_month')[pct_col]
        .quantile([0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95], numeric_only=True)
        .unstack()
        .reset_index()
    )
    quantiles.columns = [
        'year_month', 'pct_05', 'pct_10', 'pct_25', 
        'median', 'pct_75', 'pct_90', 'pct_95'
    ]
    return quantiles


def wrap_text(text: str, max_chars: int = 25) -> str:
    """Wrap text to multiple lines if it exceeds max_chars."""
    words = text.split()
    lines = []
    current_line = []
    current_length = 0
    
    for word in words:
        word_length = len(word)
        if current_length + word_length + len(current_line) > max_chars and current_line:
            lines.append(' '.join(current_line))
            current_line = [word]
            current_length = word_length
        else:
            current_line.append(word)
            current_length += word_length
    
    if current_line:
        lines.append(' '.join(current_line))
    
    return '\n'.join(lines)


def plot_quantiles(ax, quantiles: pd.DataFrame, trust_data: pd.DataFrame, 
                   pct_col: str, ylabel: str, add_legend_labels: bool = True,
                   show_event_labels: bool = True, is_top_subplot: bool = False,
                   drug_name: str = None):
    """Plot quantiles on a given axis."""
  
    ax.fill_between(
        quantiles['year_month'], quantiles['pct_05'], quantiles['pct_95'],
        color='#D9D9D9', alpha=1, 
        label='5th-95th percentiles' if add_legend_labels else ''
    )
    ax.fill_between(
        quantiles['year_month'], quantiles['pct_10'], quantiles['pct_90'],
        color='#B3B3B3', alpha=1, 
        label='10th-90th percentiles' if add_legend_labels else ''
    )
    ax.fill_between(
        quantiles['year_month'], quantiles['pct_25'], quantiles['pct_75'],
        color='#808080', alpha=1, 
        label='25th-75th percentiles' if add_legend_labels else ''
    )
    
    ax.plot(
        quantiles['year_month'], quantiles['median'],
        color='red', linewidth=1.8, linestyle='solid', 
        label='Median' if add_legend_labels else ''
    )
    
    events_to_show = EVENTS.copy()
    if drug_name:
        events_to_show = events_to_show[events_to_show['drug'] == drug_name].copy()
     
        events_to_show = events_to_show.reset_index(drop=True)
        events_to_show['stagger'] = range(len(events_to_show))
    
    line_colour = COLOURS.get(f"{drug_name}_line", '#808080') if drug_name else '#808080'
    text_colour = COLOURS.get(drug_name, '#000000') if drug_name else '#000000'
    
    if show_event_labels:
       
        base_y_start = 0.90
        y_spacing = 0.10

        for idx, (_, event) in enumerate(events_to_show.iterrows()):
            y_horizontal_level = base_y_start - event['stagger'] * y_spacing
            
            if drug_name == "Palbociclib":
                y_horizontal_level = 0.35
            
            if drug_name == "Abemaciclib":
                event_date = event['year_month']
                if event_date >= pd.to_datetime("2022-06-01"):
                    if 'OS data' in str(event['label']):
                        y_horizontal_level = 0.90 - event['stagger'] * 0.05
                    elif 'TA810' in str(event['label']):
                        y_horizontal_level = 0.85 - event['stagger'] * 0.05
                # Last annotation (OS data May 2024): left and further up
                if event_date >= pd.to_datetime("2024-01-01") and 'OS data' in str(event['label']):
                    y_horizontal_level = 0.95
                    event = event.copy()
                    event['position'] = 'left'
            
            if drug_name == "Ribociclib" and 'OS data' in str(event['label']):
                # Last ribociclib annotation (OS data): go right
                event = event.copy()
                event['position'] = 'right'
            
            if drug_name == "Palbociclib" and 'OS data' in str(event['label']):
                y_horizontal_level = 0.30
            if drug_name == "Palbociclib" and 'TA619' in str(event['label']):
                # First palbociclib annotation: go right
                event = event.copy()
                event['position'] = 'right'
            
            ax.plot(
                [event['year_month'], event['year_month']],
                [0, y_horizontal_level],
                color=line_colour, linewidth=1.8, linestyle='-', clip_on=True
            )

            go_left = event['position'] == 'left'
            
            if go_left:
                x_offset_days = -(30 + event['stagger'] * 50)
            else:
                base_offset = 15
                stagger_offset = event['stagger'] * 25
                x_offset_days = base_offset + stagger_offset
            
            x_line_end = event['year_month'] + pd.Timedelta(days=x_offset_days)
            
            x_max = pd.to_datetime("2025-01-01")
            label_length_chars = len(str(event['label']).replace('\n', ''))
            text_width_days = label_length_chars * 5
            
            if not go_left and x_line_end + pd.Timedelta(days=text_width_days) > x_max:
                # Position label to the left of the line end if it would be cut off
                x_line_end = min(x_line_end, x_max - pd.Timedelta(days=text_width_days + 10))
            label = wrap_text(event['label'], max_chars=20)
            
            y_label_center = y_horizontal_level
            
            ax.plot(
                [event['year_month'], x_line_end],
                [y_horizontal_level, y_horizontal_level],
                color=line_colour, linewidth=1.8, linestyle='-', alpha=0.7,
                clip_on=True
            )
            
            text_ha = 'right' if go_left else 'left'
  
            x_max = pd.to_datetime("2025-01-01")
            margin_days = 60
            if not go_left and x_line_end > x_max - pd.Timedelta(days=margin_days):
                text_ha = 'right'
                x_line_end = x_max - pd.Timedelta(days=margin_days)
            
            ax.text(
                x_line_end, y_label_center,
                label,
                color=text_colour, fontsize=10, ha=text_ha, va='center',
                rotation=0,
                linespacing=1.2,
                clip_on=True
            )
    
    if drug_name and drug_name in HIGHLIGHT_TRUSTS:
        for trust_code, style in HIGHLIGHT_TRUSTS[drug_name].items():
            trust_subset = trust_data[trust_data['new_code'] == trust_code]
            if len(trust_subset) > 0:
                ax.plot(
                    trust_subset['year_month'], trust_subset[pct_col],
                    color=style['colour'],
                    linewidth=1.5,
                    linestyle='--',
                    gapcolor='white',
                    dash_capstyle='round',
                    label='',
                )
    
    ax.set_ylim(0, 1.0)
    ax.set_xlim(pd.to_datetime("2019-01-01"), pd.to_datetime("2025-01-01"))
    ax.set_ylabel(ylabel, fontsize=12, labelpad=8)
    ax.grid(True, linewidth=0.5, color='grey', alpha=0.5, which='major')
    ax.grid(True, linewidth=0.3, color='grey', alpha=0.3, which='minor', axis='x')
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
    ax.xaxis.set_major_locator(mdates.YearLocator())
    ax.xaxis.set_minor_locator(mdates.MonthLocator())
    ax.xaxis.set_minor_formatter(NullFormatter())
    ax.tick_params(axis='x', which='major', length=6, labelsize=11)
    ax.tick_params(axis='x', which='minor', length=3)
    
    ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda y, _: f'{y:.0%}'))
    ax.tick_params(axis='y', labelsize=11)


def create_combined_plot(data_path: Path, output_dir: Path, key_dates_path: Path = None):
    """Create a single figure with 3 subplots."""

    trust_data = load_and_prepare_data(data_path)

    if key_dates_path is None:
        key_dates_path = Path("data/KeyDates.csv")
    global EVENTS
    EVENTS = load_key_dates(key_dates_path)

    abema_quantiles = calculate_quantiles(trust_data, 'pct_abema')
    ribo_quantiles = calculate_quantiles(trust_data, 'pct_ribo')
    palbo_quantiles = calculate_quantiles(trust_data, 'pct_palbo')
    
    
    fig, axes = plt.subplots(3, 1, figsize=(8, 12), sharex=True)
    plt.subplots_adjust(left=0.12, hspace=0.35, top=0.95)

    plot_quantiles(
        axes[0], palbo_quantiles, trust_data, 'pct_palbo',
        "% palbociclib",
        add_legend_labels=True,
        show_event_labels=True,
        is_top_subplot=True,
        drug_name="Palbociclib"
    )
    
    plot_quantiles(
        axes[1], abema_quantiles, trust_data, 'pct_abema',
        "% abemaciclib",
        add_legend_labels=False,
        show_event_labels=True,
        is_top_subplot=False,
        drug_name="Abemaciclib"
    )
    
    plot_quantiles(
        axes[2], ribo_quantiles, trust_data, 'pct_ribo',
        "% ribociclib",
        add_legend_labels=False,
        show_event_labels=True,
        is_top_subplot=False,
        drug_name="Ribociclib"
    )
    
    axes[0].text(-0.08, 0.98, 'a', transform=axes[0].transAxes, 
                 fontsize=14, fontweight='bold', va='top', ha='right')
    axes[1].text(-0.08, 0.98, 'b', transform=axes[1].transAxes, 
                 fontsize=14, fontweight='bold', va='top', ha='right')
    axes[2].text(-0.08, 0.98, 'c', transform=axes[2].transAxes, 
                 fontsize=14, fontweight='bold', va='top', ha='right')
    
    axes[2].set_xlabel("Month", fontsize=12)
    
    handles, labels = axes[0].get_legend_handles_labels()
    
    filtered_handles = [h for h, l in zip(handles, labels) if l]
    filtered_labels = [l for l in labels if l]
    
    fig.legend(
        filtered_handles, filtered_labels,
        loc='lower center', ncol=2, frameon=False,
        fontsize=9, bbox_to_anchor=(0.5, 0.02)
    )
    
    plt.tight_layout(rect=[0, 0.08, 1, 1])
    
    output_dir.mkdir(parents=True, exist_ok=True)
    fig.savefig(output_dir / "QuantilePlots_combined.svg", format='svg', dpi=300, bbox_inches='tight')
    fig.savefig(output_dir / "QuantilePlots_combined.png", format='png', dpi=350, bbox_inches='tight')

    # Export quantile data table (median and percentile boundaries)
    quantile_tables = []
    for drug_name, quantiles in [
        ("Palbociclib", palbo_quantiles),
        ("Abemaciclib", abema_quantiles),
        ("Ribociclib", ribo_quantiles),
    ]:
        tbl = quantiles.copy()
        tbl["Drug"] = drug_name
        quantile_tables.append(tbl)
    quantile_table = pd.concat(quantile_tables, ignore_index=True)
    quantile_table = quantile_table[
        ["Drug", "year_month", "pct_05", "pct_10", "pct_25", "median", "pct_75", "pct_90", "pct_95"]
    ].rename(columns={
        "year_month": "Month",
        "pct_05": "5th percentile",
        "pct_10": "10th percentile",
        "pct_25": "25th percentile",
        "median": "Median",
        "pct_75": "75th percentile",
        "pct_90": "90th percentile",
        "pct_95": "95th percentile",
    })
    quantile_table["Month"] = quantile_table["Month"].dt.strftime("%Y-%m")
    quantile_table.to_csv(output_dir / "QuantilePlots_data.csv", index=False)

    print(f"Plots saved to {output_dir}")
    plt.close()


if __name__ == "__main__":
    data_path = Path("data/oph_data_11_25_processed.csv")
    key_dates_path = Path("data/KeyDates.csv")
    output_dir = Path("outputs")
    
    create_combined_plot(data_path, output_dir, key_dates_path)
